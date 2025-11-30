import torch
import numpy as np
import random

from torch.utils.data import DataLoader
from models.local_model import LocalLSTM
from models.central_model import GCNTemporalPredictor
from datasets.flu_dataset import FluDataset
from datasets.migration_dataset import MigrationDataset
from trainer.federated_trainer import FederatedTrainer
from utils.metrics import mae, rmse
from utils.plot_predictions import *

def random_number():
    # 固定 Python 随机库
    random.seed(42)

    # 固定 NumPy 随机库
    np.random.seed(42)

    # 固定 PyTorch CPU 随机数
    torch.manual_seed(42)

    # 如果使用 GPU：
    torch.cuda.manual_seed(42)
    torch.cuda.manual_seed_all(42)  # 如果有多个 GPU

    # 确保卷积等操作确定性（牺牲一点速度）
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

# ----------------------
# 参数设置
# ----------------------
city_ids = list(range(31))
input_dim = 12
hidden_dim = 64
seq_len = 48
pred_len = 96
device = 'cuda' if torch.cuda.is_available() else 'cpu'

try:
    os.mkdir("checkpoints")
    os.mkdir("frames_chord")
    os.mkdir("migration_csv")
    os.mkdir("plots")
except OSError:
    pass

# ----------------------
# 模型初始化
# ----------------------
local_models = {
    cid: LocalLSTM(input_dim, hidden_dim, pred_len).to(device)
    for cid in city_ids
}
central_model = GCNTemporalPredictor(
    num_nodes=len(city_ids),
    hidden_dim=hidden_dim,
    out_steps=pred_len
).to(device)

# ----------------------
# 优化器与损失
# ----------------------
optimizers = {
    cid: torch.optim.Adam(local_models[cid].parameters(), lr=1e-3)
    for cid in city_ids
}
central_optimizer = torch.optim.Adam(central_model.parameters(), lr=1e-4)
loss_fn = torch.nn.MSELoss()

# ----------------------
# 数据加载器
# ----------------------
flu_tr_datasets = {
    cid: FluDataset('data/flu_mapped.csv', city_id=cid,
                    seq_len=seq_len, pred_len=pred_len, mode='train', split_ratio=0.7)
    for cid in city_ids
}

flu_tr_loaders = {
    cid: DataLoader(flu_tr_datasets[cid], batch_size=32, shuffle=True)
    for cid in city_ids
}

flu_te_datasets = {
    cid: FluDataset('data/flu_mapped.csv', city_id=cid,
                    seq_len=seq_len, pred_len=pred_len, mode='test', split_ratio=0.7, 
                    mean=flu_tr_datasets[cid].mean, std=flu_tr_datasets[cid].std)
    for cid in city_ids
}

flu_te_loaders = {
    cid: DataLoader(flu_te_datasets[cid], batch_size=8, shuffle=False, drop_last=True)
    for cid in city_ids
}

migration_tr_dataset = MigrationDataset('data/matrix_mapped.csv',
                                     seq_len=pred_len, pred_len=pred_len, mode='train', split_ratio=0.7)
migration_te_dataset = MigrationDataset('data/matrix_mapped.csv',
                                      seq_len=pred_len, pred_len=pred_len, mode='test', split_ratio=0.7, 
                                      _min=migration_tr_dataset.min, _max=migration_tr_dataset.max)
migration_tr_loader = DataLoader(migration_tr_dataset, batch_size=8, shuffle=True)
migration_te_loader = DataLoader(migration_te_dataset, batch_size=8, shuffle=False, drop_last=True)

# ----------------------
# 联邦训练器初始化
# ----------------------
trainer = FederatedTrainer(local_models, central_model, city_ids, device)

# ----------------------
# 联邦训练主循环
# ----------------------

import time

fed_epoches = 5
local_epoches = 20

for round in range(fed_epoches):
    print(f"\n=== 🌍 Global Round {round} ===")

    round_start = time.time()  # ⏱️ 开始计时

    # Step 0: 全局聚合 -> 分发最新模型权重到所有客户端
    trainer.aggregate_local_models()

    # Step 1: 各地本地模型独立训练
    local_start = time.time()
    trainer.train_local_models(flu_tr_loaders, optimizers, loss_fn, epochs=local_epoches)
    print(f"⏱️ Local training time: {time.time() - local_start:.2f} sec")

    # Step 2: 提取所有城市的时间序列嵌入 [1, T, N, H]
    T = flu_tr_datasets[city_ids[0]].seq_len  # 假设所有城市的序列长度一致
    embed_seqs = []

    for cid in city_ids:
        model = local_models[cid]
        city_embed = model.city_embed.detach().clone()  # [H]
        embed_seqs.append(city_embed)

    embed_tensor = torch.stack(embed_seqs, dim=0)  # [N, H] 这里注意 shape

    # Step 3: 中央模型训练
    central_start = time.time()
    for A_seq, A_target in migration_tr_loader:
        A_seq, A_target = A_seq.to(device), A_target.to(device)
        loss = trainer.train_central_model(
            A_seq, embed_tensor, A_target, central_optimizer, loss_fn
        )
        print(f"📉 Central model loss: {loss:.4f}")
    print(f"⏱️ Central training time: {time.time() - central_start:.2f} sec")

    # Step 4: 更新本地模型中的城市嵌入向量（如果你启用了该机制）
    trainer.update_city_embeddings_from_central()

    print(f"✅ Round {round} finished in {time.time() - round_start:.2f} sec")


# ----------------------
# 模型评估函数（举例：城市0）
# ----------------------
def evaluate_local(city_id):
    model = local_models[city_id]
    model.eval()
    y_true_all = []
    y_pred_all = []
    with torch.no_grad():
        for x, y in flu_te_loaders[city_id]:
            x, y = x.to(device), y.to(device)
            pred = model(x)
            y_true_all.extend(y.cpu().numpy())
            y_pred_all.extend(pred.cpu().numpy())

    y_true_all = np.array(y_true_all)
    y_pred_all = np.array(y_pred_all)

    rmse_score = rmse(y_true_all, y_pred_all)
    mae_score = mae(y_true_all, y_pred_all)

    print(f"[Eval] City {city_id} - MAE: {mae_score:.4f}, RMSE: {rmse_score:.4f}")
    return rmse_score, mae_score

def evaluate_center(model):
    model.eval()
    y_true_all = []
    y_pred_all = []
    with torch.no_grad():
        for x, y in migration_te_loader:
            x, y = x.to(device), y.to(device)
            pred = model(x)
            y_true_all.extend(y.cpu().numpy())
            y_pred_all.extend(pred.cpu().numpy())

    y_true_all = np.array(y_true_all)
    y_pred_all = np.array(y_pred_all)

    rmse_score = rmse(y_true_all, y_pred_all)
    mae_score = mae(y_true_all, y_pred_all)

    print(f"[Eval] Center - MAE: {mae_score:.4f}, RMSE: {rmse_score:.4f}")

for i in range(31):
    evaluate_local(i)
    plot_predictions_vs_actuals_save(i, local_models[i], flu_te_loaders, flu_te_datasets, device)

evaluate_center(central_model)
draw_gif_chord(central_model, migration_te_loader, migration_te_dataset, device)
# ----------------------
# 保存模型
# ----------------------
for cid, model in local_models.items():
    torch.save(model.state_dict(), f"checkpoints/local_model_{cid}.pth")
torch.save(central_model.state_dict(), "checkpoints/central_model.pth")
