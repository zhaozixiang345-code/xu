import torch

class FederatedTrainer:
    def __init__(self, local_models, central_model, city_ids, device='cuda'):
        self.local_models = local_models
        self.central_model = central_model
        self.city_ids = city_ids
        self.device = device

    def train_local_models(self, dataloaders, optimizers, loss_fn, epochs=5):
        for cid in self.city_ids:
            model = self.local_models[cid]
            model.train()
            optimizer = optimizers[cid]
            dataloader = dataloaders[cid]

            print(f"\n🧠 Training Local Model for City {cid}")
            for epoch in range(epochs):
                total_loss = 0.0
                count = 0
                for x, y in dataloader:
                    x, y = x.to(self.device), y.to(self.device)
                    optimizer.zero_grad()
                    pred = model(x)
                    # print("pred : ", pred.shape, " y : ", y.shape)
                    loss = loss_fn(pred, y)
                    loss.backward()
                    optimizer.step()
                    
                    total_loss += loss.item()
                    count += 1
                
                avg_loss = total_loss / count if count > 0 else 0.0
                print(f"  Epoch {epoch+1}/{epochs} | Loss: {avg_loss:.4f}")

    def aggregate_embeddings(self, dataloaders):
        embeddings = {}
        for cid in self.city_ids:
            model = self.local_models[cid]
            model.eval()
            loader = dataloaders[cid]
            all_embeds = []
            with torch.no_grad():
                for x, _ in loader:
                    embed = model.get_hidden(x.to(self.device))
                    all_embeds.append(embed)
            embeddings[cid] = torch.mean(torch.cat(all_embeds), dim=0)
        return torch.stack([embeddings[cid] for cid in self.city_ids])  # (N, hidden)

    def train_central_model(self, A_seq, local_embed_seq, targets, optimizer, loss_fn):
        self.central_model.train()
        A_seq = A_seq.to(self.device)
        local_embed_seq = local_embed_seq.to(self.device)
        targets = targets.to(self.device)
        optimizer.zero_grad()
        preds = self.central_model(A_seq, local_embed_seq)
        loss = loss_fn(preds, targets)
        loss.backward()
        optimizer.step()
        return loss.item()

    def update_city_embeddings_from_central(self):
        with torch.no_grad():
            for cid in self.city_ids:
                emb = self.central_model.node_embeddings[cid].detach().clone()
                self.local_models[cid].city_embed.copy_(emb)

    def get_local_model_states(self):
        return {cid: self.local_models[cid].state_dict() for cid in self.city_ids}

    def aggregate_local_models(self):
        # print("🔄 Aggregating local models into global model (FedAvg)")
        # local_states = [self.local_models[cid].state_dict() for cid in self.city_ids]
        # global_state = {}

        # # 对每个参数做平均
        # for key in local_states[0].keys():
        #     global_state[key] = sum(state[key] for state in local_states) / len(local_states)

        # # 更新每个本地模型为聚合后的全局模型参数
        # for cid in self.city_ids:
        #     self.local_models[cid].load_state_dict(global_state)
        print("🔄 Aggregating only city embeddings into global embedding")

        # 提取所有 local city_embed 参数（假设命名为 city_embed）
        embeddings = [self.local_models[cid].city_embed.data.clone() for cid in self.city_ids]

        # 简单平均
        global_embed = sum(embeddings) / len(embeddings)

        # 更新每个模型的 city_embed 为聚合结果
        for cid in self.city_ids:
            self.local_models[cid].city_embed.data.copy_(global_embed)
