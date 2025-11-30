import matplotlib.pyplot as plt
import numpy as np
import os
import torch
import pandas as pd

from matplotlib.animation import FuncAnimation
from matplotlib.patches import PathPatch
from matplotlib.path import Path

city_coords = {
    0: (121.47, 31.23),   # 上海
    1: (102.73, 25.04),   # 昆明（云南）
    2: (111.65, 40.82),   # 呼和浩特（内蒙古）
    3: (116.40, 39.90),   # 北京
    4: (126.55, 43.83),   # 长春（吉林）
    5: (104.06, 30.67),   # 成都（四川）
    6: (117.20, 39.13),   # 天津
    7: (106.27, 38.47),   # 银川（宁夏）
    8: (117.27, 31.86),   # 合肥（安徽）
    9: (117.00, 36.65),   # 济南（山东）
    10: (112.55, 37.87),  # 太原（山西）
    11: (113.27, 23.14),  # 广州（广东）
    12: (108.33, 22.82),  # 南宁（广西）
    13: (87.62, 43.82),   # 乌鲁木齐（新疆）
    14: (118.78, 32.04),  # 南京（江苏）
    15: (115.89, 28.68),  # 南昌（江西）
    16: (114.48, 38.03),  # 石家庄（河北）
    17: (113.65, 34.76),  # 郑州（河南）
    18: (120.19, 30.26),  # 杭州（浙江）
    19: (110.35, 20.02),  # 海口（海南）
    20: (114.30, 30.59),  # 武汉（湖北）
    21: (112.93, 28.23),  # 长沙（湖南）
    22: (103.82, 36.07),  # 兰州（甘肃）
    23: (119.30, 26.08),  # 福州（福建）
    24: (91.13, 29.65),   # 拉萨（西藏）
    25: (106.71, 26.57),  # 贵阳（贵州）
    26: (123.43, 41.80),  # 沈阳（辽宁）
    27: (106.56, 29.57),  # 重庆
    28: (108.95, 34.27),  # 西安（陕西）
    29: (101.78, 36.62),  # 西宁（青海）
    30: (126.63, 45.75),  # 哈尔滨（黑龙江）
}

import os
import torch
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd  # ✅ 新增用于保存 CSV

def plot_predictions_vs_actuals_save(city_id, model, dataloader, dataset, device, save_dir='./plots'):
    model.eval()
    x_hist = []
    y_trues = []
    y_preds = []

    with torch.no_grad():
        for x, y in dataloader[city_id]:
            x = x.to(device)
            pred = model(x)
            x_hist.extend(x.cpu().numpy())
            y_trues.extend(y.cpu().numpy())
            y_preds.extend(pred.cpu().numpy())
            break  # 只处理一个 batch
    
    if not os.path.exists(save_dir):
        os.makedirs(save_dir)

    # 还原标准化
    mean = dataset[city_id].mean[-1]
    std = dataset[city_id].std[-1]
    x_ = x_hist[0][:, -1] * std + mean  # 输入序列最后一个特征
    y_trues_ = y_trues[0]
    y_preds_ = y_preds[0]

    # 绘图数据
    Act = np.concatenate((x_, y_trues_), axis=0)
    Pred = np.concatenate((x_, y_preds_), axis=0)

    # ✅ 保存 CSV 文件
    csv_data = pd.DataFrame({
        'Actual': y_trues_,
        'Predicted': y_preds_,
    })
    csv_path = os.path.join(save_dir, f'city_{city_id}_prediction_data.csv')
    csv_data.to_csv(csv_path, index=False)

    # 绘图
    plt.figure(figsize=(12, 6))
    plt.plot(Act, label='Actual', marker='o')
    plt.plot(Pred, label='Predicted', marker='x')
    plt.title(f'City {city_id} - Predictions vs Actuals')
    plt.xlabel('Sample Index')
    plt.ylabel('Value')
    plt.legend()
    plt.tight_layout()

    # 保存图像
    save_path = os.path.join(save_dir, f'city_{city_id}_prediction_vs_actual.png')
    plt.savefig(save_path)
    plt.close()

    # print(f"Saved plot for city {city_id} at {save_path}")
    # print(f"Saved CSV for city {city_id} at {csv_path}")



def draw_gif_chord(model, dataloader, dataset, device):
    model.eval()

    migration_seq = []
    with torch.no_grad():
        for x, y in dataloader:
            x, y = x.to(device), y.to(device)
            pred = model(x)
            migration_seq.extend(pred.cpu().numpy())
            break  # 只取一个 batch 动图展示

    # 反标准化
    min_ = dataset.min
    max_ = dataset.max
    eps = dataset.eps
    migration_seq = np.array(migration_seq)
    migration_seq = migration_seq * (max_ - min_ + eps) + min_
    migration_seq = migration_seq[0]  # [T, N, N]

    T, N, _ = migration_seq.shape
    angles = np.linspace(0, 2 * np.pi, N, endpoint=False)
    radius = 1.0
    city_coords = np.stack([np.cos(angles), np.sin(angles)], axis=1) * radius

    city_names = {i: name for i, name in enumerate([
        "Shanghai", "Yunnan", "Inner Mongolia", "Beijing", "Jilin", "Sichuan", "Tianjin",
        "Ningxia", "Anhui", "Shandong", "Shanxi", "Guangdong", "Guangxi", "Xinjiang",
        "Jiangsu", "Jiangxi", "Hebei", "Henan", "Zhejiang", "Hainan",
        "Hubei", "Hunan", "Gansu", "Fujian", "Tibet", "Guizhou", "Liaoning",
        "Chongqing", "Shanxi_", "Qinghai", "Heilongjiang"
    ])}

    # 创建保存帧图像的文件夹
    frame_dir = "frames_chord"
    os.makedirs(frame_dir, exist_ok=True)

    fig, ax = plt.subplots(figsize=(16, 16))
    ax.axis('off')

    def update(t):
        ax.clear()
        ax.set_title(f"Top Incoming Migrations per City - Timestep {t}", fontsize=16)
        ax.axis('off')

        rates = migration_seq[t]
        max_rate = rates.max()

        inflow_sources = np.argmax(rates, axis=0)

        # 节点
        for i, (x, y) in enumerate(city_coords):
            ax.plot(x, y, 'o', color='skyblue', markersize=8)
            ax.text(x * 1.1, y * 1.1, city_names[i], fontsize=8, ha='center', va='center')

        # 弦线 + 注释
        for j in range(N):
            i = inflow_sources[j]
            if i == j or rates[i, j] < 1e-6:
                continue

            x1, y1 = city_coords[i]
            x2, y2 = city_coords[j]

            # 曲线中点朝圆心偏移
            mx, my = (x1 + x2) / 2, (y1 + y2) / 2
            direction = np.array([-mx, -my])
            direction = direction / (np.linalg.norm(direction) + 1e-8)
            curvature = 0.2
            mx += direction[0] * curvature
            my += direction[1] * curvature

            alpha = (rates[i, j] / max_rate) * 0.6 + 0.2
            linewidth = (rates[i, j] / max_rate) * 4 + 0.5

            path = Path([[x1, y1], [mx, my], [x2, y2]], [Path.MOVETO, Path.CURVE3, Path.CURVE3])
            patch = PathPatch(path, lw=linewidth, alpha=alpha, color='crimson', zorder=1)
            ax.add_patch(patch)

            # 文本说明
            tx, ty = (x1 + x2) / 2, (y1 + y2) / 2
            ax.text(tx * 0.6, ty * 0.6, f"{city_names[i]} → {city_names[j]} ({rates[i,j]:.2f})",
                    fontsize=6, color='black', ha='center', va='center')

        ax.set_xlim(-1.5, 1.5)
        ax.set_ylim(-1.5, 1.5)

        # 保存每一帧
        frame_path = os.path.join(frame_dir, f"frame_{t:03d}.png")
        fig.savefig(frame_path, dpi=150, bbox_inches='tight')

        # 记录每一帧的迁移流量并保存为 CSV
        migration_data = []
        for i in range(N):
            for j in range(N):
                if rates[i, j] > 1e-6:  # 只记录迁移流量大于0的
                    migration_data.append([city_names[i], city_names[j], rates[i, j]])

        # 将迁移流量保存为 DataFrame
        df = pd.DataFrame(migration_data, columns=["Source", "Target", "Migration Flow"])

        # 保存为 CSV 文件
        csv_dir = "migration_csv"
        os.makedirs(csv_dir, exist_ok=True)
        csv_path = os.path.join(csv_dir, f"migration_frame_t{t:03d}.csv")
        df.to_csv(csv_path, index=False)

        return []

    ani = FuncAnimation(fig, update, frames=range(T), interval=800, blit=False)
    ani.save("migration_chord_top1.gif", writer='pillow', fps=2)
    print("GIF saved as migration_chord_top1.gif")
    print(f"All frames saved in '{frame_dir}'")



