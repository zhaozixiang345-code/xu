import pandas as pd
import torch
from torch.utils.data import Dataset
import numpy as np

class FluDataset(Dataset):
    def __init__(self, csv_file, city_id, seq_len=32, pred_len=12, mode='train', split_ratio=0.8, mean=None, std=None):
        df = pd.read_csv(csv_file)
        df = df[df['city_id'] == city_id].reset_index(drop=True)

        self.seq_len = seq_len
        self.pred_len = pred_len
        self.mode = mode

        self.feature_names = df.drop(['省', '日期', 'city_id'], axis=1).columns.tolist()
        # print(self.feature_names)
        raw_features = df.drop(['省', '日期', 'city_id'], axis=1).values.astype(np.float32)
        self.labels = df['user_rate'].values.astype(np.float32)

        # 划分训练/测试索引区间
        total_len = len(self.labels) - seq_len - pred_len
        split_index = int(total_len * split_ratio)

        if mode == 'train':
            start, end = 0, split_index
        elif mode == 'test':
            start, end = split_index, total_len
        else:
            raise ValueError(f"Unknown mode: {mode}")

        self.indices = list(range(start, end))

        # 训练模式：计算 mean 和 std；测试模式：使用传入的 mean/std
        if mode == 'train':
            full_train_range = range(0, split_index + seq_len + pred_len)
            train_data = raw_features[np.array(full_train_range)]
            self.mean = train_data.mean(axis=0)
            self.std = train_data.std(axis=0) + 1e-8
        else:
            assert mean is not None and std is not None, "测试模式必须提供训练集的 mean 和 std"
            self.mean = mean
            self.std = std

        self.features = (raw_features - self.mean) / self.std

    def __len__(self):
        return len(self.indices)

    def __getitem__(self, idx):
        idx = self.indices[idx]
        x = self.features[idx : idx + self.seq_len]
        y = self.labels[idx + self.seq_len : idx + self.seq_len + self.pred_len]
        return torch.tensor(x, dtype=torch.float32), torch.tensor(y, dtype=torch.float32)

    def denormalize(self, x):
        if isinstance(x, torch.Tensor):
            x = x.cpu().numpy()
        return x * self.std[-1] + self.mean[-1]


if __name__ == '__main__':
    test = FluDataset(csv_file='../data/flu_mapped.csv', city_id=0)
