import pandas as pd

# 加载数据
flu_df = pd.read_csv('data/flu.csv', encoding='utf-8')
matrix_df = pd.read_csv('data/matrix.csv', encoding='gbk')

print(flu_df.columns)
print(matrix_df.columns)

flu_df = flu_df.drop(columns=['流感', '甲型流感', '乙型流感', '乙流', '发烧', '奥司他韦', '达菲', '咽痛'])
cols = [col for col in flu_df.columns if col != 'user_rate'] + ['user_rate']
flu_df = flu_df[cols]
print(flu_df.columns)

matrix_df['迁入率'] = matrix_df['流向各地人数'] / (matrix_df['人口数（万人）'] / 10)

# 1. 获取所有出现的城市名称（来源城市 & 目的地城市）
flu_cities = set(flu_df['省'])  # 你原始 flu.csv 的城市列叫啥就填啥
matrix_cities = set(matrix_df['省']).union(set(matrix_df['目的地']))
all_cities = sorted(list(flu_cities.union(matrix_cities)))

# 2. 生成编号映射
city2id = {city: idx for idx, city in enumerate(all_cities)}
id2city = {idx: city for city, idx in city2id.items()}

# 3. 替换原始数据中的城市名为编号
flu_df['city_id'] = flu_df['省'].map(city2id)
matrix_df['省份id'] = matrix_df['省'].map(city2id)
matrix_df['目的地id'] = matrix_df['目的地'].map(city2id)

# 4. 保存新文件
flu_df.to_csv('data/flu_mapped.csv', index=False)
matrix_df.to_csv('data/matrix_mapped.csv', index=False)

# 5. 保存映射表（方便调试/可视化用）
pd.DataFrame.from_dict(city2id, orient='index', columns=['city_id']).to_csv('data/city2id.csv')

print("✅ 城市编号映射完成，生成 flu_mapped.csv 和 matrix_mapped.csv")
