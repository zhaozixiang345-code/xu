import pandas as pd
import numpy as np
import torch
from torch.utils.data import Dataset

class MigrationDataset(Dataset):
    def __init__(self, csv_file, city_num=31, seq_len=32, pred_len=12, mode='train', split_ratio=0.8, _min=None, _max=None):
        df = pd.read_csv(csv_file)
        df['date'] = pd.to_datetime(df['日期'], format='%Y/%m/%d', errors='coerce')
        df = df.drop(columns=['日期'])
        df = df.dropna(subset=['date'])

        self.seq_len = seq_len
        self.pred_len = pred_len
        self.city_num = city_num
        self.mode = mode

        self.adj_dict = {}
        all_rates = []

        grouped = df.groupby('date')
        for date, group in grouped:
            A = np.zeros((city_num, city_num))
            for _, row in group.iterrows():
                src, dst, rate = int(row['省份id']), int(row['目的地id']), row['迁入率']
                A[src, dst] = rate
                all_rates.append((date, src, dst, rate))
            self.adj_dict[date] = A

        self.dates = sorted(self.adj_dict.keys())

        total_len = len(self.dates) - self.seq_len - self.pred_len
        split_idx = int(total_len * split_ratio)

        if mode == 'train':
            self.start_idx = 0
            self.end_idx = split_idx
            used_dates = set(self.dates[self.start_idx : self.end_idx + self.seq_len + self.pred_len])
            train_rates = [rate for (d, _, _, rate) in all_rates if d in used_dates]
            self.min = np.min(train_rates)
            self.max = np.max(train_rates)
        else:
            self.start_idx = split_idx
            self.end_idx = total_len
            self.min = _min
            self.max = _max
        self.eps = 1e-8
        # 标准化迁入率矩阵
        for date in self.adj_dict:
            self.adj_dict[date] = (self.adj_dict[date] - self.min) / (self.max - self.min + self.eps)

    def __len__(self):
        return self.end_idx - self.start_idx

    def __getitem__(self, idx):
        idx = idx + self.start_idx
        input_matrices = [self.adj_dict[self.dates[i]] for i in range(idx, idx + self.seq_len)]
        output_matrices = [self.adj_dict[self.dates[i]] for i in range(idx + self.seq_len, idx + self.seq_len + self.pred_len)]
        return (
            torch.tensor(np.stack(input_matrices), dtype=torch.float32), 
            torch.tensor(np.stack(output_matrices), dtype=torch.float32)
        )

    def denormalize(self, x):
        if isinstance(x, torch.Tensor):
            x = x.cpu().numpy()
        return x * self.std + self.mean



if __name__ == '__main__':
    test = MigrationDataset(csv_file='../data/matrix_mapped.csv')