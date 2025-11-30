import torch
import torch.nn as nn

class LocalLSTM(nn.Module):
    def __init__(self, input_dim, hidden_dim, output_dim):
        super().__init__()
        self.city_embed = nn.Parameter(torch.zeros(hidden_dim))  # 可训练嵌入
        nn.init.xavier_uniform_(self.city_embed.unsqueeze(0))    # 推荐初始化

        self.input_proj = nn.Linear(input_dim, hidden_dim)       # 仅映射输入特征
        self.lstm = nn.LSTM(hidden_dim, hidden_dim, batch_first=True)

        self.output_layer = nn.Sequential(
            nn.Linear(hidden_dim, output_dim),
            nn.Softplus()  # 保证输出非负
        )

    def forward(self, x):
        # x: [B, T, D]
        B, T, _ = x.shape

        x = self.input_proj(x)  # [B, T, hidden_dim]
        guidance = self.city_embed.unsqueeze(0).unsqueeze(0).expand(B, T, -1)  # [B, T, hidden_dim]
        x = x + guidance  # 融合嵌入信息

        h, _ = self.lstm(x)  # [B, T, hidden_dim]
        out = self.output_layer(h[:, -1])  # [B, output_dim]
        return out

    def get_hidden(self, x):
        with torch.no_grad():
            B, T, _ = x.shape
            x = self.input_proj(x)
            guidance = self.city_embed.unsqueeze(0).unsqueeze(0).expand(B, T, -1)
            x = x + guidance
            h, _ = self.lstm(x)
        return h[:, -1]  # [B, hidden_dim]
