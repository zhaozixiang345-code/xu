import torch
import torch.nn as nn
import torch.nn.functional as F
from mambapy.mamba import Mamba, MambaConfig

class SimpleGCNLayer(nn.Module):
    def __init__(self, input_dim, output_dim):
        super().__init__()
        self.linear = nn.Linear(input_dim, output_dim)

    def forward(self, A, X):
        """
        A: [B, N, N]
        X: [B, N, H]
        Return: [B, N, H]
        """
        B, N, _ = A.size()
        A_hat = A + torch.eye(N, device=A.device).unsqueeze(0)  # [B, N, N]

        deg = A_hat.sum(dim=-1)               # [B, N]
        deg_inv_sqrt = deg.pow(-0.5)          # [B, N]
        deg_inv_sqrt[torch.isinf(deg_inv_sqrt)] = 0.0
        D_hat = torch.diag_embed(deg_inv_sqrt)

        A_norm = D_hat @ A_hat @ D_hat        # [B, N, N]
        out = A_norm @ X                      # [B, N, H]
        return self.linear(out)               # [B, N, H]


class GCNTemporalPredictor(nn.Module):
    def __init__(self, num_nodes, hidden_dim, out_steps, n_heads=4, dropout=0.1, use_gcn=False):
        super().__init__()
        self.out_steps = out_steps
        self.num_nodes = num_nodes
        self.hidden_dim = hidden_dim

        # 可训练节点嵌入
        self.node_embeddings = nn.Parameter(torch.empty(num_nodes, hidden_dim))
        nn.init.xavier_uniform_(self.node_embeddings)

        self.use_gcn = use_gcn

        # 图卷积层
        self.gcn = SimpleGCNLayer(hidden_dim, hidden_dim)

        # LayerNorm → 避免 Mamba 前爆炸
        self.ln = nn.LayerNorm(hidden_dim)

        # Mamba 时间建模
        config = MambaConfig(d_model=hidden_dim, n_layers=1)
        self.mamba = Mamba(config)

        # LSTM 时间建模（替换 Mamba）
        self.lstm = nn.LSTM(
            input_size=hidden_dim,
            hidden_size=hidden_dim,
            num_layers=1,
            batch_first=True,
            bidirectional=False
        )

        # Positional Encoding
        self.pos_encoding = nn.Parameter(torch.zeros(1, 100, hidden_dim))  # 支持最长 100 时间步

        # Transformer Encoder Layer
        encoder_layer = nn.TransformerEncoderLayer(
            d_model=hidden_dim,
            nhead=n_heads,
            dim_feedforward=4 * hidden_dim,
            dropout=dropout,
            batch_first=True
        )
        self.transformer = nn.TransformerEncoder(encoder_layer, num_layers=1)

        # 是否使用 GCN（地区矩阵）
        if use_gcn:
            self.gcn = SimpleGCNLayer(hidden_dim, hidden_dim)
        else:
            # 无 GCN：直接线性映射或恒等（仍需 H 维）
            self.node_proj = nn.Linear(hidden_dim, hidden_dim) 

        # 输出层
        self.out = nn.Linear(hidden_dim, out_steps * num_nodes * num_nodes)

    def forward(self, A_seq, local_embed_seq=None):
        """
        A_seq: [B, T, N, N] - 邻接矩阵序列
        local_embed_seq: [B, N, H] or [N, H] - 外部传入的节点嵌入
        Return: [B, out_steps, N, N]
        """
        B, T, N, _ = A_seq.shape
        H = self.hidden_dim

        # 使用外部嵌入或默认嵌入
        if local_embed_seq is not None:
            if local_embed_seq.dim() == 2:
                X = local_embed_seq.unsqueeze(0).expand(B, N, H)  # [B, N, H]
            elif local_embed_seq.dim() == 3:
                X = local_embed_seq  # [B, N, H]
            else:
                raise ValueError("local_embed_seq must be [N, H] or [B, N, H]")
        else:
            X = self.node_embeddings.unsqueeze(0).expand(B, N, H)  # 默认嵌入

        # # GCN 时序处理
        # gcn_outs = []
        # for t in range(T):
        #     A_t = A_seq[:, t, :, :]  # [B, N, N]
        #     h_t = self.gcn(A_t, X)   # [B, N, H]
        #     gcn_outs.append(h_t)

        # # [B, T, N, H] → [B, N, T, H]
        # gcn_seq = torch.stack(gcn_outs, dim=1).permute(0, 2, 1, 3)

        # --- GCN 或 无 GCN 分支 ---
        gcn_outs = []
        for t in range(T):
            if self.use_gcn:
                A_t = A_seq[:, t, :, :]  # [B, N, N]
                h_t = self.gcn(A_t, X)   # [B, N, H]
            else:
                # 无 GCN：直接投影节点嵌入（忽略 A_t）
                h_t = torch.tanh(self.node_proj(X))  # [B, N, H]
            gcn_outs.append(h_t)

        # [T, B, N, H] → [B, N, T, H]
        gcn_seq = torch.stack(gcn_outs, dim=1).permute(0, 2, 1, 3)  # [B, N, T, H]


        # 合并节点维度：[B*N, T, H]
        x = gcn_seq.reshape(B * N, T, H)

        # 加上位置编码（截取前 T 个）
        pos_enc = self.pos_encoding[:, :T, :]  # [1, T, H]
        # x = x + pos_enc

        # 加 LayerNorm
        x = self.ln(x)

        # 时间建模
        # x = self.mamba(x)       # [B*N, T, H]
        # h = x[:, -1, :]         # [B*N, H]

        # LSTM 时间建模
        lstm_out, (h_n, c_n) = self.lstm(x)          # h_n: [1, B*N, H]
        h = h_n[-1]           
         
        # Transformer 时间建模
        # x = self.transformer(x)               # [B*N, T, H]
        # h = x[:, -1, :]                       # 取最后时间步 [B*N, H]  

        # 输出预测
        out = self.out(h)       # [B*N, out_steps * N * N]
        out = out.view(B, N, self.out_steps, N, N)

        # 聚合所有节点预测（平均）
        out = out.mean(dim=1)   # [B, out_steps, N, N]

        return out
