
################################## 1. 包管理和配置 ###################################

required_packages <- c('tidyverse','knitr','lubridate','rEDM','metap',
                       'doParallel','foreach','imputeTS', "kableExtra",
                       'ggplot2', 'ggpubr', 'cowplot', 'patchwork', 
                       'grid', 'gridExtra', 'usmap', 'maps', 'scales', 
                       'ggridges', 'ggforce', 'ggbeeswarm', 'readxl', 'openxlsx')
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if(length(missing_packages) > 0) install.packages(missing_packages, dependencies = TRUE)
lapply(required_packages, require, character.only = TRUE)

tryCatch(stopAllClusters(), error = function(e) NULL)
closeAllConnections()
gc(full = TRUE)

cores <- min(90, parallel::detectCores() - 4)
GLOBAL_SEED <- 42
set.seed(GLOBAL_SEED)
num_sample <- 50
num_surr <- 500
num_surr_daily <- 200
num_sample_daily <- 30
################################## 2. 核心函数 ###################################

fisher_combine <- function(p_values) {
  p_values <- p_values[!is.na(p_values) & p_values > 0 & p_values <= 1]
  if(length(p_values) == 0) return(list(p = 1, statistic = 0, df = 0))
  statistic <- -2 * sum(log(p_values))
  df <- 2 * length(p_values)
  combined_p <- pchisq(statistic, df, lower.tail = FALSE)
  return(list(p = combined_p, statistic = statistic, df = df))
}

logitTransform <- function(p, epsilon = 1e-6) { 
  p_constrained <- pmax(epsilon, pmin(1 - epsilon, p))
  log(p_constrained/(1-p_constrained)) 
}

normFunc <- function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)

get_ccm_library_range <- function(dat, tp_value = 0, E){
  n_rows <- nrow(dat)
  max_lib <- n_rows - abs(tp_value) - E - 5
  lib_out <- matrix(c(E + 2, max_lib), ncol = 2)
  return(lib_out)
}

paper_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(2, "mm"),
    axis.title.x = element_text(size = 12, colour = "black", family = "sans", margin = margin(t = 8)),
    axis.title.y = element_text(size = 12, colour = "black", family = "sans", margin = margin(r = 8)),
    axis.text.x = element_text(size = 10, colour = "black", family = "sans"),
    axis.text.y = element_text(size = 10, colour = "black", family = "sans"),
    plot.title = element_text(size = 14, colour = "black", family = "sans", hjust = 0, margin = margin(b = 12)),
    plot.subtitle = element_text(size = 11, colour = "grey30", family = "sans", hjust = 0, margin = margin(b = 16)),
    legend.position = "bottom",
    legend.title = element_text(size = 11, colour = "black", family = "sans"),
    legend.text = element_text(size = 10, colour = "black", family = "sans"),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.size = unit(4, "mm"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.5),
    strip.text = element_text(size = 10, colour = "black", family = "sans", margin = margin(4, 4, 4, 4)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

paper_colors <- list(
  primary_red = "#E31A1C",
  light_red = "#FB9A99", 
  error_bar = "#999999",
  reference = "#666666",
  violin_fill = "#FFE6E6",
  violin_line = "#E31A1C",
  blue = "#1f77b4",
  blue_light = "#aec7e8",
  orange = "#ff7f0e",
  orange_light = "#ffbb78"
)

PROVINCES_CN <- c("黑龙江", "吉林", "内蒙古", "辽宁", "新疆", "甘肃", "河北", "北京", "天津", "山西", "宁夏", "青海", "山东", "陕西", "河南", "西藏", "江苏", "安徽", "湖北", "四川", "上海", "浙江", "重庆", "湖南", "江西", "贵州", "福建", "云南", "广西", "广东", "海南")
PROVINCES_EN <- c("Heilongjiang", "Jilin", "Inner Mongolia", "Liaoning", "Xinjiang", "Gansu", "Hebei", "Beijing", "Tianjin", "Shanxi", "Ningxia", "Qinghai", "Shandong", "Shaanxi", "Henan", "Xizang", "Jiangsu", "Anhui", "Hubei", "Sichuan", "Shanghai", "Zhejiang", "Chongqing", "Hunan", "Jiangxi", "Guizhou", "Fujian", "Yunnan", "Guangxi", "Guangdong", "Hainan")

# 修正变量顺序（从上到下，与图表一致）
var_order <- c("SO₂", "O₃", "NO₂", "CO", "PM₁₀", "PM₂.₅", 
               "Precipitation", "Wind Speed", "Humidity", 
               "Temperature", "Online Search", "Online Pharm")

prepare_plot_data <- function(data) {
  data %>%
    mutate(
      ST_char = as.character(ST),
      ST_clean = gsub("省|市|自治区|壮族|维吾尔|回族", "", ST_char),
      ST_en = case_when(
        ST_clean %in% PROVINCES_CN ~ PROVINCES_EN[match(ST_clean, PROVINCES_CN)],
        ST_char %in% PROVINCES_EN ~ ST_char,
        TRUE ~ ST_char
      ),
      ST_ordered = factor(ST_en, levels = PROVINCES_EN),
      var_name = case_when(
        as.character(plt) %in% names(var_labels) ~ var_labels[as.character(plt)],
        TRUE ~ as.character(plt)
      ),
      var_ordered = factor(var_name, levels = var_order)
    )
}

save_plot <- function(plot_obj, filename, width = 190, height = 120) {
  width_inch <- width / 25.4
  height_inch <- height / 25.4
  tryCatch({
    ggsave(paste0(filename, ".pdf"), plot_obj, width = width_inch, height = height_inch, dpi = 300)
    ggsave(paste0(filename, ".png"), plot_obj, width = width_inch, height = height_inch, dpi = 600)
    return(TRUE)
  }, error = function(e) return(FALSE))
}

################################## 3. 数据预处理 ###################################

flu_weekly <- read_xlsx("data/flu_weekly.xlsx")
flu_daily <- read_xlsx("data/flu_daily.xlsx")

data_weekly_raw <- flu_weekly %>%
  mutate(
    period_start = as.Date(sapply(strsplit(as.character(period), "~"), `[`, 1)),
    province = `省份`,
    week_number = as.numeric(`周数`),
    ILI_rate_raw = as.numeric(`监测阳性率`),
    drug_order_rate_raw = as.numeric(`美团下单率`),
    search_index = as.numeric(`百度指数`),
    temperature = as.numeric(`气温`),
    humidity = as.numeric(`相对湿度(%)`),
    PM25 = as.numeric(`PM2.5（ug/m3）`),
    PM10 = as.numeric(`PM10（ug/m3）`),
    CO = as.numeric(`CO(mg/m3)`),
    NO2 = as.numeric(`NO2（ug/m3）`),
    O3 = as.numeric(`O3（ug/m3）`),
    SO2 = as.numeric(`SO2（ug/m3）`),
    wind_speed = as.numeric(`风速(m/s)`),
    precipitation = as.numeric(`降水量（mm）`)
  ) %>%
  select(province, period_start, week_number, ILI_rate_raw, drug_order_rate_raw, 
         search_index, temperature, humidity, PM25, PM10, CO, NO2, O3, SO2, 
         wind_speed, precipitation) %>%
  arrange(province, period_start)

data_daily_raw <- flu_daily %>%
  mutate(
    date = as.Date(`日期`),
    province = `省份`,
    drug_order_rate_raw = as.numeric(`美团下单率`),
    search_index = as.numeric(`百度指数`),
    temperature = as.numeric(`气温`),
    humidity = as.numeric(`相对湿度(%)`),
    PM25 = as.numeric(`PM2.5（ug/m3）`),
    PM10 = as.numeric(`PM10（ug/m3）`),
    CO = as.numeric(`CO(mg/m3)`),
    NO2 = as.numeric(`NO2（ug/m3）`),
    O3 = as.numeric(`O3（ug/m3）`),
    SO2 = as.numeric(`SO2（ug/m3）`),
    wind_speed = as.numeric(`风速(m/s)`),
    precipitation = as.numeric(`降水量（mm）`)
  ) %>%
  select(province, date, drug_order_rate_raw, search_index, temperature, humidity, 
         PM25, PM10, CO, NO2, O3, SO2, wind_speed, precipitation) %>%
  arrange(province, date)

df_province_weekly <- data_weekly_raw %>%
  mutate(
    fluP = logitTransform(ILI_rate_raw / 100),
    drug_order_rate = drug_order_rate_raw,
    date = period_start
  ) %>%
  select(province, date, week_number, fluP, drug_order_rate, search_index, 
         temperature, humidity, PM25, PM10, CO, NO2, O3, SO2, wind_speed, precipitation) %>%
  group_by(province) %>%
  mutate(across(c(fluP, drug_order_rate, search_index, temperature, humidity, 
                  PM25, PM10, CO, NO2, O3, SO2, wind_speed, precipitation), normFunc)) %>%
  ungroup() %>%
  arrange(province, date)

df_province_daily <- data_daily_raw %>%
  mutate(drug_order_rate = drug_order_rate_raw) %>%
  select(province, date, drug_order_rate, search_index, temperature, humidity, 
         PM25, PM10, CO, NO2, O3, SO2, wind_speed, precipitation) %>%
  group_by(province) %>%
  mutate(across(c(drug_order_rate, search_index, temperature, humidity, 
                  PM25, PM10, CO, NO2, O3, SO2, wind_speed, precipitation), normFunc)) %>%
  ungroup() %>%
  arrange(province, date)

provinces_weekly <- unique(df_province_weekly$province)
provinces_daily <- unique(df_province_daily$province)
predictor_vars_12 <- c('drug_order_rate', 'search_index', 'temperature', 'humidity', 
                       'wind_speed', 'precipitation', 'PM25', 'PM10', 'CO', 'NO2', 'O3', 'SO2')
environmental_vars_11 <- c('search_index', 'temperature', 'humidity', 'wind_speed', 
                           'precipitation', 'PM25', 'PM10', 'CO', 'NO2', 'O3', 'SO2')
tp_values_weekly <- c(0, -1, -2)
tp_values_daily <- c(0, -1, -3, -7, -14)

var_labels <- c(
  "drug_order_rate" = "Online Pharm",
  "search_index" = "Online Search", 
  "temperature" = "Temperature",
  "humidity" = "Humidity",
  "wind_speed" = "Wind Speed",
  "precipitation" = "Precipitation",
  "PM25" = "PM₂.₅",
  "PM10" = "PM₁₀", 
  "CO" = "CO",
  "NO2" = "NO₂",
  "O3" = "O₃",
  "SO2" = "SO₂"
)

################################## 4. 代理数据参数计算 ###################################
# 4.1 计算最优spar函数
fn_state_spar <- function(states, plts, data, is_daily = FALSE) {
  df_filtered <- data %>% 
    filter(province == states)
  
  if(is_daily) {
    # 日数据：使用一年中的第几天
    x <- as.numeric(format(df_filtered$date, "%j"))
  } else {
    # 周数据：直接使用周数列
    x <- df_filtered$week_number
  }
  
  y <- df_filtered[[plts]]
  
  # 交叉验证找最优spar
  spars <- seq(0, 1.5, by = 0.1)
  ss <- numeric(length(spars))
  
  for(j in seq_along(spars)) {
    res <- numeric(length(x))
    for(i in 1:length(x)) {
      mod <- smooth.spline(x[-i], y[-i], spar = spars[j])
      res[i] <- (predict(mod, x[i])$y - y[i])^2
    }
    ss[j] <- sum(res)
  }
  
  optimal_spar <- spars[which.min(ss)]
  
  return(data.frame(states = states, plt = plts, spar = optimal_spar))
}

# 4.2 计算季节异常标准差
fn_anomaly_PNAS <- function(states, plts, data, spar_data, is_daily = FALSE) {
  spar_val <- spar_data %>% 
    filter(states == !!states & plt == !!plts) %>%
    pull(spar)
  
  df_filtered <- data %>%
    filter(province == states)
  
  if(is_daily) {
    # 日数据：一年中的第几天
    doy <- as.numeric(format(df_filtered$date, "%j"))
    period_offset <- c(-365, 0, 365)
  } else {
    # 周数据：使用周数
    doy <- df_filtered$week_number
    period_offset <- c(-52, 0, 52)
  }
  
  x <- df_filtered[[plts]]
  
  # 环绕数据（前一年、当年、后一年）
  doy_sm <- rep(doy, 3) + rep(period_offset, each = length(doy))
  x_sm <- rep(x, 3)
  
  # 平滑拟合
  xsp <- smooth.spline(doy_sm, x_sm, spar = spar_val[1])
  
  # 获取季节值
  seasonal <- predict(xsp, doy)$y
  anomaly <- x - seasonal
  
  return(data.frame(
    states = states,
    plt = plts,
    spar = spar_val[1],
    sd_PNAS = sd(anomaly)
  ))
}

try(stopCluster(cl), silent = TRUE)  # 尝试停止集群，忽略错误
closeAllConnections()  # 关闭所有连接
gc()  # 垃圾回收

# 4.3 周级参数计算
plist_weekly_spar <- expand.grid(
  states = provinces_weekly,
  plts = c('fluP', predictor_vars_12),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

weekly_spar <- foreach(i = 1:nrow(plist_weekly_spar),
                       .packages = c("tidyverse"),
                       .combine = rbind) %dopar% {
                         fn_state_spar(
                           plist_weekly_spar$states[i],
                           plist_weekly_spar$plts[i],
                           df_province_weekly,
                           FALSE
                         )
                       }

stopCluster(cl)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

sd_data_weekly <- foreach(i = 1:nrow(plist_weekly_spar),
                          .packages = c("tidyverse"),
                          .combine = rbind) %dopar% {
                            fn_anomaly_PNAS(
                              plist_weekly_spar$states[i],
                              plist_weekly_spar$plts[i],
                              df_province_weekly,
                              weekly_spar,
                              FALSE
                            )
                          }

stopCluster(cl)

# 4.4 日级参数计算
plist_daily_spar <- expand.grid(
  states = provinces_daily,
  plts = c('drug_order_rate', environmental_vars_11),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

daily_spar <- foreach(i = 1:nrow(plist_daily_spar),
                      .packages = c("tidyverse"),
                      .combine = rbind) %dopar% {
                        fn_state_spar(
                          plist_daily_spar$states[i],
                          plist_daily_spar$plts[i],
                          df_province_daily,
                          TRUE
                        )
                      }

stopCluster(cl)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

sd_data_daily <- foreach(i = 1:nrow(plist_daily_spar),
                         .packages = c("tidyverse"),
                         .combine = rbind) %dopar% {
                           fn_anomaly_PNAS(
                             plist_daily_spar$states[i],
                             plist_daily_spar$plts[i],
                             df_province_daily,
                             daily_spar,
                             TRUE
                           )
                         }

stopCluster(cl)

message("代理数据参数计算完成")
################################## 5. 最优参数计算 ###################################
# 5.1 计算最优嵌入维度E
fn_E_smapc <- function(data, ST, y, max_E = 6, is_daily = FALSE){
  df <- data %>% filter(province == ST) %>% select(date, all_of(y))
  
  # 设置库大小
  lib <- get_ccm_library_range(df, 0, 3)
  pred <- lib
  
  # 寻找最优E
  E_result <- EmbedDimension(dataFrame = df,
                             lib = lib, pred = pred,
                             columns = y, target = y,
                             maxE = max_E, showPlot = FALSE)
  
  E_result$dis <- y
  E_result$ST <- ST
  return(E_result)
}

# 5.2 周级E值计算
cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

E_smapc_out_weekly <- foreach(province = provinces_weekly,
                              .packages = c("rEDM", "tidyverse"),
                              .combine = rbind) %dopar% {
                                fn_E_smapc(df_province_weekly, province, "fluP", max_E = 6, FALSE)
                              }

stopCluster(cl)

E_smapc_weekly <- E_smapc_out_weekly %>% 
  filter(E %in% 2:6) %>%
  group_by(dis, ST) %>%
  filter(rho == max(rho)) %>%
  as.data.frame()

# 5.3 日级E值计算
cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

E_smapc_out_daily <- foreach(province = provinces_daily,
                             .packages = c("rEDM", "tidyverse"),
                             .combine = rbind) %dopar% {
                               fn_E_smapc(df_province_daily, province, "drug_order_rate", max_E = 8, TRUE)
                             }

stopCluster(cl)

E_smapc_daily <- E_smapc_out_daily %>% 
  filter(E %in% 2:8) %>%
  group_by(dis, ST) %>%
  filter(rho == max(rho)) %>%
  as.data.frame()

# 5.4 计算最优theta参数
fn_theta <- function(data, ST, dis, theta, is_daily = FALSE){
  if(is_daily) {
    E <- E_smapc_daily[E_smapc_daily$ST == ST & E_smapc_daily$dis == dis, 'E'][1]
  } else {
    E <- E_smapc_weekly[E_smapc_weekly$ST == ST & E_smapc_weekly$dis == dis, 'E'][1]
  }
  
  df <- data %>% filter(province == ST) %>% select(date, all_of(dis))
  lib <- get_ccm_library_range(df, 0, E)
  pred <- lib
  
  rho_theta <- PredictNonlinear(dataFrame = df,
                                embedded = FALSE,
                                columns = dis, target = dis,
                                Tp = 1, theta = theta,
                                lib = lib, pred = pred,
                                showPlot = FALSE, E = E)
  
  return(data.frame(rho = rho_theta$rho[1], dis = dis, state = ST, theta = theta, E = E))
}

# 5.5 周级theta计算
plist_theta_weekly <- expand.grid(
  ST = provinces_weekly,
  dis = 'fluP',
  theta = c(0.01, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

theta_out_weekly <- foreach(i = 1:nrow(plist_theta_weekly),
                            .packages = c("rEDM", "tidyverse"),
                            .combine = rbind) %dopar% {
                              fn_theta(df_province_weekly, 
                                       plist_theta_weekly$ST[i], 
                                       plist_theta_weekly$dis[i], 
                                       plist_theta_weekly$theta[i], FALSE)
                            }

stopCluster(cl)

best_theta_weekly <- theta_out_weekly %>%
  group_by(state) %>%
  filter(rho == max(rho)) %>%
  as.data.frame()

# 5.6 日级theta计算
plist_theta_daily <- expand.grid(
  ST = provinces_daily,
  dis = 'drug_order_rate',
  theta = c(0.01, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

theta_out_daily <- foreach(i = 1:nrow(plist_theta_daily),
                           .packages = c("rEDM", "tidyverse"),
                           .combine = rbind) %dopar% {
                             fn_theta(df_province_daily, 
                                      plist_theta_daily$ST[i], 
                                      plist_theta_daily$dis[i], 
                                      plist_theta_daily$theta[i], TRUE)
                           }

stopCluster(cl)

best_theta_daily <- theta_out_daily %>%
  group_by(state) %>%
  filter(rho == max(rho)) %>%
  as.data.frame()

message("最优参数计算完成")

################################## 6. CCM分析核心函数 ###################################
# 6.1 生成代理数据函数
fn_surr_data <- function(data, ST, plts, tp_value, is_daily = FALSE){
  df <- data %>% filter(province == ST) %>% select(date, all_of(plts))
  
  if(is_daily) {
    alpha <- sd_data_daily %>% filter(states == ST & plt == plts) %>% pull(sd_PNAS)
    T_period <- 365.25
    current_num_surr <- num_surr_daily
  } else {
    alpha <- sd_data_weekly %>% filter(states == ST & plt == plts) %>% pull(sd_PNAS)
    T_period <- 52.18
    current_num_surr <- num_surr
  }
  
  vec_x <- df[[plts]]
  
  # 设置种子确保可重复
  set.seed(GLOBAL_SEED)
  
  surr_data <- SurrogateData(vec_x, method = "seasonal",
                             T_period = T_period,
                             num_surr = current_num_surr,
                             alpha = alpha[1]) %>%
    as.data.frame()
  
  df_result <- df %>% select(date)
  df_result$T1 <- vec_x
  for(i in 1:current_num_surr) {
    df_result[[paste0("T", i+1)]] <- surr_data[,i]
  }
  
  return(df_result)
}

# 6.2 CCM核心函数
# 6.2 CCM核心函数（确保返回正确的列名）
fn_ccm <- function(data, ST, x, y, tp_value, direction = "forward", is_daily = FALSE){
  df <- data %>% filter(province == ST) %>% select(date, all_of(y), all_of(x))
  
  # 获取嵌入维度
  if(is_daily) {
    E <- E_smapc_daily[E_smapc_daily$ST == ST & E_smapc_daily$dis == y, 'E'][1]
    current_num_surr <- num_surr_daily
    current_num_sample <- num_sample_daily
  } else {
    E <- E_smapc_weekly[E_smapc_weekly$ST == ST & E_smapc_weekly$dis == y, 'E'][1]
    current_num_surr <- num_surr
    current_num_sample <- num_sample
  }
  
  # 生成代理数据
  surr_data <- fn_surr_data(data, ST, x, 0, is_daily)
  all_data <- df %>% left_join(surr_data, by = "date")
  names(all_data) <- c("date", y, 'T1', paste0("T", 2:(current_num_surr+1)))
  
  # 设置库大小
  libSize <- get_ccm_library_range(all_data, tp_value, E)
  
  rho_surr <- data.frame()  # 初始化为数据框
  
  # CCM分析
  for (i in 1:(current_num_surr+1)) {
    targetCol <- paste("T", i, sep = "")
    
    if(direction == "forward") {
      ccm_out <- CCM(dataFrame = all_data, E = E, Tp = tp_value,
                     columns = y, target = targetCol,
                     libSizes = libSize, random = TRUE,
                     sample = current_num_sample, seed = GLOBAL_SEED)
      col <- paste(y, ":", targetCol, sep = "")
    } else {
      ccm_out <- CCM(dataFrame = all_data, E = E, Tp = tp_value,
                     columns = targetCol, target = y,
                     libSizes = libSize, random = TRUE,
                     sample = current_num_sample, seed = GLOBAL_SEED)
      col <- paste(targetCol, ":", y, sep = "")
    }
    
    dat <- ccm_out %>% select(LibSize, all_of(col))
    names(dat) <- c("lib", "rho")
    
    # 创建结果数据框
    test1 <- data.frame(
      lib = dat$lib,
      rho = dat$rho,
      i = i,
      dis = y,
      plt = x,
      E = E,
      tp_value = tp_value,
      ST = ST,
      direction = direction
    )
    
    rho_surr <- rbind(rho_surr, test1)
  }
  
  return(rho_surr)
}

# 6.3 处理CCM结果函数
process_ccm_results <- function(ccm_out, is_daily = FALSE){
  lib_threshold <- ifelse(is_daily, 100, 50)
  
  # 确保数据框格式正确
  ccm_out <- as.data.frame(ccm_out)
  
  # 分离大小库
  dat_min <- ccm_out %>% filter(lib < lib_threshold)
  dat_max <- ccm_out %>% filter(lib >= lib_threshold)
  
  # 按组排序
  dat_min <- dat_min %>% arrange(ST, tp_value, plt, dis, i)
  dat_max <- dat_max %>% arrange(ST, tp_value, plt, dis, i)
  
  # 确保行数相同
  if(nrow(dat_min) != nrow(dat_max)) {
    return(list(
      ccm_out = ccm_out,
      ccm_processed = data.frame(),
      ccm_p = data.frame()
    ))
  }
  
  # 计算收敛性
  dat_processed <- dat_max
  dat_processed$rho_min <- dat_min$rho
  dat_processed$rho <- dat_processed$rho - dat_processed$rho_min  # Δρ
  
  # 提取原始CCM技能
  ccm_out_raw <- dat_processed %>%
    filter(i == 1) %>%
    select(plt, ST, tp_value, rho)
  
  # 计算p值
  ccm_p <- dat_processed %>%
    group_by(dis, plt, E, ST, tp_value) %>%
    summarise(
      p = {
        real_rho <- rho[i == 1][1]
        surr_rhos <- rho[i != 1]
        if(length(surr_rhos) > 0) {
          1 - ecdf(surr_rhos)(real_rho)
        } else {
          1
        }
      },
      .groups = "drop"
    ) %>%
    left_join(ccm_out_raw, by = c("plt", "ST", "tp_value")) %>%
    rename(rho_raw = rho) %>%
    mutate(
      p = ifelse(p == 0, 0.0005, p),
      p = ifelse(rho_raw <= 0, 1, p)
    )
  
  return(list(
    ccm_out = ccm_out,
    ccm_processed = dat_processed,
    ccm_p = ccm_p
  ))
}

# 6.4 元分析函数
conduct_meta_analysis <- function(ccm_p, tp_values, variables){
  meta_results <- data.frame()
  
  for(tp in tp_values) {
    for(var in variables) {
      df_tp <- ccm_p %>% filter(tp_value == tp & plt == var)
      if(nrow(df_tp) > 0) {
        fisher_result <- fisher_combine(df_tp$p)
        meta_result <- data.frame(
          p = fisher_result$p,
          statistic = fisher_result$statistic,
          df = fisher_result$df,
          tp_value = tp, 
          plt = var,
          n_provinces = nrow(df_tp),
          significant_provinces = sum(df_tp$p < 0.05 & df_tp$rho_raw > 0)
        )
        meta_results <- rbind(meta_results, meta_result)
      }
    }
  }
  
  return(meta_results)
}

# 6.5 S-map效应计算函数
fn_smapc <- function(data, ST, plt, dis, tp_value, is_daily = FALSE){
  # 获取参数
  if(is_daily) {
    E <- E_smapc_daily[E_smapc_daily$ST == ST & E_smapc_daily$dis == dis, "E"][1]
    theta <- best_theta_daily[best_theta_daily$state == ST & best_theta_daily$dis == dis, "theta"][1]  # 改这里
  } else {
    E <- E_smapc_weekly[E_smapc_weekly$ST == ST & E_smapc_weekly$dis == dis, "E"][1]
    theta <- best_theta_weekly[best_theta_weekly$state == ST & best_theta_weekly$dis == dis, "theta"][1]  # 明确指定weekly
  }
  
  # 准备数据
  df <- data %>% 
    filter(province == ST) %>%
    mutate(plt_tp = lag(.data[[plt]], -tp_value)) %>%
    filter(!is.na(plt_tp)) %>%
    select(date, all_of(dis), plt_tp)
  
  M <- nrow(df)
  embed_1 <- Embed(dataFrame = df, E = E, tau = -1, columns = dis)
  
  dataFrame <- cbind(
    df[E:M, 'date'],
    df[E:M, dis],
    embed_1[E:M, 1:(E-1)], 
    df[E:M, 'plt_tp']
  ) %>% as.data.frame()
  
  names(dataFrame) <- c('date', dis, letters[1:(E-1)], plt)
  
  columns <- paste(
    paste(letters[1:(E-1)], collapse = ' '), 
    plt, 
    sep = ' '
  )
  
  lib <- get_ccm_library_range(dataFrame, 0, E)
  pred <- lib
  
  # S-map分析
  smap <- SMap(
    dataFrame = dataFrame,
    embedded = TRUE,
    columns = columns,
    target = dis,
    lib = lib,
    pred = pred,
    theta = theta,
    Tp = 1,
    E = E
  )
  
  smapc_df <- smap$coefficients[c(1, 2+E)]
  names(smapc_df) <- c('date', 'effect')
  
  smapc_df <- smapc_df %>%
    mutate(
      date = lubridate::as_date(date, origin = lubridate::origin),
      dis = dis,
      ST = ST, 
      plt = plt,
      E = E,
      tp_value = tp_value,
      theta = theta
    )
  
  return(smapc_df)
}
message("CCM分析核心函数加载完成")
################################################################################
# 分步骤CCM作图函数 
################################################################################

# Step 8: 环境→流感(周级正向)
create_violin_plots_step8 <- function(ccm_data, tp_values) {
  for(tp_val in tp_values) {
    tp_label <- ifelse(tp_val == 0, "lag_0", paste0("lag_", abs(tp_val), "w"))
    
    lag_data <- ccm_data %>%
      filter(tp_value == tp_val, i == 1, !is.na(rho_raw)) %>%
      prepare_plot_data() %>%
      filter(!is.na(var_ordered))
    
    if(nrow(lag_data) > 0) {
      # 全国汇总图
      p_national <- ggplot(lag_data, aes(x = var_ordered, y = rho_raw)) +
        geom_violin(fill = paper_colors$blue_light, colour = paper_colors$blue, 
                    alpha = 0.7, linewidth = 0.6, trim = FALSE, scale = "width") +
        geom_point(aes(shape = sig), size = 2.5, stroke = 0.5, colour = paper_colors$blue,
                   position = position_jitter(width = 0.25, height = 0, seed = 42)) +
        scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                           labels = c("Non-significant", "Significant")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.4) +
        labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
             title = "Environmental Variables → Influenza Activity",
             subtitle = paste("National summary -", gsub("_", " ", tp_label), "(Weekly)")) +
        coord_flip() + paper_theme
      
      save_plot(p_national, paste0("Step8_National_", tp_label), width = 183, height = 140)
      
      # 省级分面图
      surrogate_data <- ccm_data %>%
        filter(tp_value == tp_val, i != 1) %>%
        group_by(ST, plt) %>%
        summarise(Q0 = quantile(rho, 0, na.rm = TRUE), Q95 = quantile(rho, 0.95, na.rm = TRUE), .groups = "drop")
      
      provincial_data <- lag_data %>%
        left_join(surrogate_data, by = c("ST", "plt")) %>%
        filter(!is.na(var_ordered))
      
      if(nrow(provincial_data) > 0) {
        p_provincial <- ggplot(provincial_data) +
          geom_errorbar(aes(x = fct_rev(var_ordered), ymin = ifelse(is.na(Q0), -0.05, Q0),
                            ymax = ifelse(is.na(Q95), 0.05, Q95)), width = 0, linewidth = 0.4, 
                        colour = paper_colors$error_bar) +
          geom_point(aes(x = fct_rev(var_ordered), y = rho_raw, shape = sig), size = 2.2, 
                     stroke = 0.5, colour = paper_colors$blue) +
          facet_wrap(~ ST_ordered, ncol = 6) +
          scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                             labels = c("Non-significant", "Significant")) +
          scale_x_discrete(labels = function(x) str_wrap(gsub("₂|₁|₀", "", x), width = 8)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.3) +
          labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
               title = "Environmental Variables → Influenza Activity (Provincial)",
               subtitle = paste(gsub("_", " ", tp_label), "- 95% surrogate confidence intervals")) +
          coord_flip() + paper_theme +
          theme(strip.text = element_text(size = 8), axis.text.y = element_text(size = 7), legend.position = "bottom")
        
        save_plot(p_provincial, paste0("Step8_Provincial_", tp_label), width = 330, height = 310)
      }
    }
  }
}

# Step 9: 环境→美团(日级机制)
create_violin_plots_step9 <- function(ccm_data, tp_values) {
  for(tp_val in tp_values) {
    tp_label <- ifelse(tp_val == 0, "lag_0", paste0("lag_", abs(tp_val), "d"))
    
    lag_data <- ccm_data %>%
      filter(tp_value == tp_val, i == 1, !is.na(rho_raw)) %>%
      prepare_plot_data() %>%
      filter(!is.na(var_ordered))
    
    if(nrow(lag_data) > 0) {
      # 全国汇总图
      p_national <- ggplot(lag_data, aes(x = var_ordered, y = rho_raw)) +
        geom_violin(fill = paper_colors$orange_light, colour = paper_colors$orange, 
                    alpha = 0.7, linewidth = 0.6, trim = FALSE, scale = "width") +
        geom_point(aes(shape = sig), size = 2.5, stroke = 0.5, colour = paper_colors$orange,
                   position = position_jitter(width = 0.25, height = 0, seed = 42)) +
        scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                           labels = c("Non-significant", "Significant")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.4) +
        labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
             title = "Environmental Variables → Online Pharm Behavior",
             subtitle = paste("National summary -", gsub("_", " ", tp_label), "(Daily)")) +
        coord_flip() + paper_theme
      
      save_plot(p_national, paste0("Step9_National_", tp_label), width = 183, height = 140)
      
      # 省级分面图
      surrogate_data <- ccm_data %>%
        filter(tp_value == tp_val, i != 1) %>%
        group_by(ST, plt) %>%
        summarise(Q0 = quantile(rho, 0, na.rm = TRUE), Q95 = quantile(rho, 0.95, na.rm = TRUE), .groups = "drop")
      
      provincial_data <- lag_data %>%
        left_join(surrogate_data, by = c("ST", "plt")) %>%
        filter(!is.na(var_ordered))
      
      if(nrow(provincial_data) > 0) {
        p_provincial <- ggplot(provincial_data) +
          geom_errorbar(aes(x = fct_rev(var_ordered), ymin = ifelse(is.na(Q0), -0.05, Q0),
                            ymax = ifelse(is.na(Q95), 0.05, Q95)), width = 0, linewidth = 0.4, 
                        colour = paper_colors$error_bar) +
          geom_point(aes(x = fct_rev(var_ordered), y = rho_raw, shape = sig), size = 2.2, 
                     stroke = 0.5, colour = paper_colors$orange) +
          facet_wrap(~ ST_ordered, ncol = 6) +
          scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                             labels = c("Non-significant", "Significant")) +
          scale_x_discrete(labels = function(x) str_wrap(gsub("₂|₁|₀", "", x), width = 8)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.3) +
          labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
               title = "Environmental Variables → Online Pharm Behavior (Provincial)",
               subtitle = paste(gsub("_", " ", tp_label), "- 95% surrogate confidence intervals")) +
          coord_flip() + paper_theme +
          theme(strip.text = element_text(size = 8), axis.text.y = element_text(size = 7), legend.position = "bottom")
        
        save_plot(p_provincial, paste0("Step9_Provincial_", tp_label), width = 330, height = 310)
      }
    }
  }
}

# Step 11: 流感→环境(周级反向)
create_violin_plots_step11 <- function(ccm_data, tp_values) {
  for(tp_val in tp_values) {
    tp_label <- ifelse(tp_val == 0, "lag_0", paste0("lag_", abs(tp_val), "w"))
    
    lag_data <- ccm_data %>%
      filter(tp_value == tp_val, i == 1, !is.na(rho_raw)) %>%
      mutate(var_name = ifelse(dis %in% names(var_labels), var_labels[dis], as.character(dis))) %>%
      mutate(plt = dis) %>%
      prepare_plot_data() %>%
      filter(!is.na(var_ordered))
    
    if(nrow(lag_data) > 0) {
      # 全国汇总图
      p_national <- ggplot(lag_data, aes(x = var_ordered, y = rho_raw)) +
        geom_violin(fill = paper_colors$light_red, colour = paper_colors$primary_red, 
                    alpha = 0.7, linewidth = 0.6, trim = FALSE, scale = "width") +
        geom_point(aes(shape = sig), size = 2.5, stroke = 0.5, colour = paper_colors$primary_red,
                   position = position_jitter(width = 0.25, height = 0, seed = 42)) +
        scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                           labels = c("Non-significant", "Significant")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.4) +
        labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
             title = "Influenza Activity → Environmental Variables (Reverse)",
             subtitle = paste("National summary -", gsub("_", " ", tp_label), "(Weekly)")) +
        coord_flip() + paper_theme
      
      save_plot(p_national, paste0("Step11_National_", tp_label), width = 183, height = 140)
      
      # 省级分面图
      surrogate_data <- ccm_data %>%
        filter(tp_value == tp_val, i != 1) %>%
        group_by(ST, dis) %>%
        summarise(Q0 = quantile(rho, 0, na.rm = TRUE), Q95 = quantile(rho, 0.95, na.rm = TRUE), .groups = "drop")
      
      provincial_data <- lag_data %>%
        left_join(surrogate_data, by = c("ST", "dis")) %>%
        filter(!is.na(var_ordered))
      
      if(nrow(provincial_data) > 0) {
        p_provincial <- ggplot(provincial_data) +
          geom_errorbar(aes(x = fct_rev(var_ordered), ymin = ifelse(is.na(Q0), -0.05, Q0),
                            ymax = ifelse(is.na(Q95), 0.05, Q95)), width = 0, linewidth = 0.4, 
                        colour = paper_colors$error_bar) +
          geom_point(aes(x = fct_rev(var_ordered), y = rho_raw, shape = sig), size = 2.2, 
                     stroke = 0.5, colour = paper_colors$primary_red) +
          facet_wrap(~ ST_ordered, ncol = 6) +
          scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                             labels = c("Non-significant", "Significant")) +
          scale_x_discrete(labels = function(x) str_wrap(gsub("₂|₁|₀", "", x), width = 8)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.3) +
          labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
               title = "Influenza Activity → Environmental Variables (Reverse, Provincial)",
               subtitle = paste(gsub("_", " ", tp_label), "- 95% surrogate confidence intervals")) +
          coord_flip() + paper_theme +
          theme(strip.text = element_text(size = 8), axis.text.y = element_text(size = 7), legend.position = "bottom")
        
        save_plot(p_provincial, paste0("Step11_Provincial_", tp_label), width = 330, height = 310)
      }
    }
  }
}

# Step 12: 美团→环境(日级反向)
create_violin_plots_step12 <- function(ccm_data, tp_values) {
  for(tp_val in tp_values) {
    tp_label <- ifelse(tp_val == 0, "lag_0", paste0("lag_", abs(tp_val), "d"))
    
    lag_data <- ccm_data %>%
      filter(tp_value == tp_val, i == 1, !is.na(rho_raw)) %>%
      mutate(var_name = ifelse(dis %in% names(var_labels), var_labels[dis], as.character(dis))) %>%
      mutate(plt = dis) %>%
      prepare_plot_data() %>%
      filter(!is.na(var_ordered))
    
    if(nrow(lag_data) > 0) {
      # 全国汇总图
      p_national <- ggplot(lag_data, aes(x = var_ordered, y = rho_raw)) +
        geom_violin(fill = paper_colors$light_red, colour = paper_colors$primary_red, 
                    alpha = 0.7, linewidth = 0.6, trim = FALSE, scale = "width") +
        geom_point(aes(shape = sig), size = 2.5, stroke = 0.5, colour = paper_colors$primary_red,
                   position = position_jitter(width = 0.25, height = 0, seed = 42)) +
        scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                           labels = c("Non-significant", "Significant")) +
        geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.4) +
        labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
             title = "Online Pharm Behavior → Environmental Variables (Reverse)",
             subtitle = paste("National summary -", gsub("_", " ", tp_label), "(Daily)")) +
        coord_flip() + paper_theme
      
      save_plot(p_national, paste0("Step12_National_", tp_label), width = 183, height = 140)
      
      # 省级分面图
      surrogate_data <- ccm_data %>%
        filter(tp_value == tp_val, i != 1) %>%
        group_by(ST, dis) %>%
        summarise(Q0 = quantile(rho, 0, na.rm = TRUE), Q95 = quantile(rho, 0.95, na.rm = TRUE), .groups = "drop")
      
      provincial_data <- lag_data %>%
        left_join(surrogate_data, by = c("ST", "dis")) %>%
        filter(!is.na(var_ordered))
      
      if(nrow(provincial_data) > 0) {
        p_provincial <- ggplot(provincial_data) +
          geom_errorbar(aes(x = fct_rev(var_ordered), ymin = ifelse(is.na(Q0), -0.05, Q0),
                            ymax = ifelse(is.na(Q95), 0.05, Q95)), width = 0, linewidth = 0.4, 
                        colour = paper_colors$error_bar) +
          geom_point(aes(x = fct_rev(var_ordered), y = rho_raw, shape = sig), size = 2.2, 
                     stroke = 0.5, colour = paper_colors$primary_red) +
          facet_wrap(~ ST_ordered, ncol = 6) +
          scale_shape_manual(name = "Surrogate test:", values = c("non_sig" = 1, "sig" = 16),
                             labels = c("Non-significant", "Significant")) +
          scale_x_discrete(labels = function(x) str_wrap(gsub("₂|₁|₀", "", x), width = 8)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = paper_colors$reference, linewidth = 0.3) +
          labs(x = "Variables", y = expression(paste(Delta, rho[CCM])),
               title = "Online Pharm Behavior → Environmental Variables (Reverse, Provincial)",
               subtitle = paste(gsub("_", " ", tp_label), "- 95% surrogate confidence intervals")) +
          coord_flip() + paper_theme +
          theme(strip.text = element_text(size = 8), axis.text.y = element_text(size = 7), legend.position = "bottom")
        
        save_plot(p_provincial, paste0("Step12_Provincial_", tp_label), width = 330, height = 310)
      }
    }
  }
}

# 统一调用接口函数(保持与原代码兼容)
create_violin_plots_fixed <- function(ccm_data, step_name, tp_values, is_daily = FALSE) {
  if(step_name == "Step8") {
    create_violin_plots_step8(ccm_data, tp_values)
  } else if(step_name == "Step9") {
    create_violin_plots_step9(ccm_data, tp_values)
  } else if(step_name == "Step11") {
    create_violin_plots_step11(ccm_data, tp_values)
  } else if(step_name == "Step12") {
    create_violin_plots_step12(ccm_data, tp_values)
  }
}

save_step_complete <- function(step_results, step_meta, step_effects, step_name, step_number) {
  saveRDS(list(
    step_results = step_results,
    step_meta = step_meta,
    step_effects = step_effects
  ), paste0("Step", step_number, "_", step_name, "_Complete.rds"))
  
  wb <- createWorkbook()
  
  if(!is.null(step_meta) && nrow(step_meta) > 0) {
    addWorksheet(wb, "Meta_Analysis")
    writeData(wb, "Meta_Analysis", step_meta)
  }
  
  if(length(step_results) > 0) {
    all_ccm_p <- do.call(rbind, lapply(step_results, function(x) {
      if(!is.null(x$ccm_p)) x$ccm_p else data.frame()
    }))
    
    if(nrow(all_ccm_p) > 0) {
      addWorksheet(wb, "Provincial_Results")
      writeData(wb, "Provincial_Results", all_ccm_p)
    }
  }
  
  if(!is.null(step_effects) && nrow(step_effects) > 0) {
    effects_summary <- step_effects %>%
      group_by(plt, tp_value) %>%
      summarise(
        mean_effect = mean(effect, na.rm = TRUE),
        median_effect = median(effect, na.rm = TRUE),
        sd_effect = sd(effect, na.rm = TRUE),
        provinces_count = n_distinct(ST),
        .groups = "drop"
      )
    
    addWorksheet(wb, "SMap_Effects_Summary")
    writeData(wb, "SMap_Effects_Summary", effects_summary)
    
    addWorksheet(wb, "SMap_Effects_Raw")
    writeData(wb, "SMap_Effects_Raw", step_effects)
  }
  
  saveWorkbook(wb, paste0("Step", step_number, "_", step_name, "_Report.xlsx"), overwrite = TRUE)
}

################################## 7. Step 8: 环境→流感(周级正向) ###################################
# Step 8: 检验12种环境变量对流感的因果作用（周级）
step8_results <- list()
step8_meta <- data.frame()

# 7.1 对每个预测变量进行CCM分析
for(current_var in predictor_vars_12) {
  
  # 准备参数列表
  plist_current <- expand.grid(
    ST = provinces_weekly,
    tp_value = tp_values_weekly,
    stringsAsFactors = FALSE
  )
  
  # 并行计算CCM
  cl <- makeCluster(cores, type = 'PSOCK')
  registerDoParallel(cl)
  
  ccm_out_current <- foreach(
    j = 1:nrow(plist_current),
    .packages = c("rEDM", "tidyverse"),
    .combine = rbind
  ) %dopar% {
    fn_ccm(
      df_province_weekly,
      plist_current$ST[j],
      current_var,  # x: 环境变量
      "fluP",       # y: 流感
      plist_current$tp_value[j],
      "forward",
      FALSE
    )
  }
  
  stopCluster(cl)
  
  # 处理结果
  if(!is.null(ccm_out_current) && nrow(ccm_out_current) > 0) {
    processed_results <- process_ccm_results(ccm_out_current, FALSE)
    step8_results[[current_var]] <- processed_results
  }
  
  gc()
}

# 7.2 进行元分析
for(var in names(step8_results)) {
  if(!is.null(step8_results[[var]]$ccm_p)) {
    meta_current <- conduct_meta_analysis(
      step8_results[[var]]$ccm_p, 
      tp_values_weekly, 
      var
    )
    step8_meta <- rbind(step8_meta, meta_current)
  }
}

# 7.3 计算S-map效应
step8_effects <- data.frame()

if(nrow(step8_meta) > 0) {
  # 准备参数列表
  plist_step8 <- expand.grid(
    ST = provinces_weekly,
    plt = unique(step8_meta$plt),
    tp_value = unique(step8_meta$tp_value),
    stringsAsFactors = FALSE
  )
  
  # 并行计算S-map效应
  cl <- makeCluster(cores, type = 'PSOCK')
  registerDoParallel(cl)
  
  step8_effects <- foreach(
    i = 1:nrow(plist_step8),
    .packages = c("rEDM", "tidyverse", "lubridate"),
    .combine = rbind
  ) %dopar% {
    fn_smapc(
      df_province_weekly,
      plist_step8$ST[i], 
      plist_step8$plt[i], 
      "fluP",
      plist_step8$tp_value[i], 
      FALSE
    )
  }
  
  stopCluster(cl)
}

# 7.4 生成可视化
if(length(step8_results) > 0) {
  # 合并所有CCM结果
  all_ccm_causal_step8 <- data.frame()
  
  for(var in names(step8_results)) {
    if(!is.null(step8_results[[var]]$ccm_processed) && 
       !is.null(step8_results[[var]]$ccm_p)) {
      
      ccm_combined <- step8_results[[var]]$ccm_processed %>%
        left_join(
          step8_results[[var]]$ccm_p, 
          by = c("ST", "plt", "dis", "E", "tp_value")
        ) %>%
        mutate(
          sig = ifelse(p < 0.05 & rho_raw > 0, 'sig', 'non_sig'),
          sig = factor(sig, levels = c("non_sig", "sig"))
        )
      
      all_ccm_causal_step8 <- rbind(all_ccm_causal_step8, ccm_combined)
    }
  }
  
  # 生成图表
  if(nrow(all_ccm_causal_step8) > 0) {
    create_violin_plots_step8(all_ccm_causal_step8, tp_values_weekly)
  }
}

# 7.5 保存结果
save_step_complete(step8_results, step8_meta, step8_effects, "Env_to_Flu_Weekly", 8)

################################## 8. Step 9: 环境→美团(日级机制) ###################################

# Step 9: 检验11种环境因素对美团购药的快速响应机制（日级）

step9_results <- list()
step9_meta <- data.frame()

# 8.1 对每个环境变量和时滞进行CCM分析
for(var in environmental_vars_11) {
  for(current_lag in tp_values_daily) {
    
    # 并行计算各省份CCM
    cl <- makeCluster(cores, type = 'PSOCK')
    registerDoParallel(cl)
    
    batch_results <- foreach(
      province = provinces_daily,
      .packages = c("rEDM", "tidyverse"),
      .combine = rbind
    ) %dopar% {
      fn_ccm(
        df_province_daily, 
        province, 
        var,                # x: 环境变量
        "drug_order_rate",  # y: 美团购药
        current_lag,        # 时滞
        "forward", 
        TRUE               # 日级
      )
    }
    
    stopCluster(cl)
    
    # 处理结果
    if(!is.null(batch_results) && nrow(batch_results) > 0) {
      key <- paste0(var, "_lag", abs(current_lag))
      processed_results <- process_ccm_results(batch_results, TRUE)
      if(length(processed_results) > 0) {
        step9_results[[key]] <- processed_results
      }
    }
    
    gc()
  }
}

# 8.2 进行元分析
for(var in environmental_vars_11) {
  # 收集该变量所有时滞的结果
  var_results <- list()
  
  for(lag in tp_values_daily) {
    key <- paste0(var, "_lag", abs(lag))
    if(key %in% names(step9_results) && !is.null(step9_results[[key]]$ccm_p)) {
      var_results[[as.character(lag)]] <- step9_results[[key]]$ccm_p
    }
  }
  
  # 合并并进行元分析
  if(length(var_results) > 0) {
    combined_p <- do.call(rbind, var_results)
    if(nrow(combined_p) > 0) {
      meta_var <- conduct_meta_analysis(combined_p, tp_values_daily, var)
      step9_meta <- rbind(step9_meta, meta_var)
    }
  }
}

# 8.3 计算S-map效应
step9_effects <- data.frame()

if(nrow(step9_meta) > 0) {
  # 准备参数列表
  plist_step9 <- expand.grid(
    ST = provinces_daily,
    plt = environmental_vars_11,
    tp_value = tp_values_daily,
    stringsAsFactors = FALSE
  )
  
  # 并行计算S-map效应
  cl <- makeCluster(cores, type = 'PSOCK')
  registerDoParallel(cl)
  
  step9_effects <- foreach(
    i = 1:nrow(plist_step9),
    .packages = c("rEDM", "tidyverse", "lubridate"),
    .combine = rbind
  ) %dopar% {
    # 修正：日级使用best_theta_daily
    fn_smapc_daily <- function(data, ST, plt, dis, tp_value) {
      E <- E_smapc_daily[E_smapc_daily$ST == ST & E_smapc_daily$dis == dis, "E"][1]
      theta <- best_theta_daily[best_theta_daily$state == ST & best_theta_daily$dis == dis, "theta"][1]
      
      
      df <- data %>% 
        filter(province == ST) %>%
        mutate(plt_tp = lag(.data[[plt]], -tp_value)) %>%
        filter(!is.na(plt_tp)) %>%
        select(date, all_of(dis), plt_tp)
      
      if(nrow(df) < E + 2) return(data.frame())
      
      M <- nrow(df)
      embed_1 <- Embed(dataFrame = df, E = E, tau = -1, columns = dis)
      
      dataFrame <- cbind(
        df[E:M, 'date'],
        df[E:M, dis],
        embed_1[E:M, 1:(E-1)], 
        df[E:M, 'plt_tp']
      ) %>% as.data.frame()
      
      names(dataFrame) <- c('date', dis, letters[1:(E-1)], plt)
      
      columns <- paste(
        paste(letters[1:(E-1)], collapse = ' '), 
        plt, 
        sep = ' '
      )
      
      lib <- get_ccm_library_range(dataFrame, 0, E)
      pred <- lib
      
      smap <- SMap(
        dataFrame = dataFrame,
        embedded = TRUE,
        columns = columns,
        target = dis,
        lib = lib,
        pred = pred,
        theta = theta,
        Tp = 1,
        E = E
      )
      
      smapc_df <- smap$coefficients[c(1, 2+E)]
      names(smapc_df) <- c('date', 'effect')
      
      smapc_df <- smapc_df %>%
        mutate(
          date = lubridate::as_date(date, origin = lubridate::origin),
          dis = dis,
          ST = ST, 
          plt = plt,
          E = E,
          tp_value = tp_value,
          theta = theta
        )
      
      return(smapc_df)
    }
    
    fn_smapc_daily(
      df_province_daily,
      plist_step9$ST[i], 
      plist_step9$plt[i], 
      "drug_order_rate",
      plist_step9$tp_value[i]
    )
  }
  
  stopCluster(cl)
}

# 8.4 生成可视化
if(length(step9_results) > 0) {
  # 合并所有CCM结果
  all_ccm_causal_step9 <- data.frame()
  
  for(result_name in names(step9_results)) {
    if(!is.null(step9_results[[result_name]]$ccm_processed) && 
       !is.null(step9_results[[result_name]]$ccm_p)) {
      
      ccm_combined <- step9_results[[result_name]]$ccm_processed %>%
        left_join(
          step9_results[[result_name]]$ccm_p, 
          by = c("ST", "plt", "dis", "E", "tp_value")
        ) %>%
        mutate(
          sig = ifelse(p < 0.05 & rho_raw > 0, 'sig', 'non_sig'),
          sig = factor(sig, levels = c("non_sig", "sig"))
        )
      
      all_ccm_causal_step9 <- rbind(all_ccm_causal_step9, ccm_combined)
    }
  }
  
  # 生成图表
  if(nrow(all_ccm_causal_step9) > 0) {
    create_violin_plots_step9(all_ccm_causal_step9, tp_values_daily)
  }
}

# 8.5 保存结果
save_step_complete(step9_results, step9_meta, step9_effects, "Env_to_Meituan_Daily", 9)

################################## 9. Step 10: 反向CCM最优参数计算 ###################################
# 为Step 11-12的反向验证计算环境变量作为目标时的最优参数
try(stopCluster(cl), silent = TRUE)  # 尝试停止集群，忽略错误
closeAllConnections()  # 关闭所有连接
gc()  # 垃圾回收

# 10.1 周级环境变量E值计算
cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

E_env_weekly <- foreach(env_var = predictor_vars_12,
                        .packages = c("rEDM", "tidyverse"),
                        .combine = rbind) %:% 
  foreach(province = provinces_weekly,
          .combine = rbind) %dopar% {
            df <- df_province_weekly %>% 
              filter(province == !!province) %>% 
              select(date, all_of(env_var))
            
            lib <- get_ccm_library_range(df, 0, 3)
            pred <- lib
            
            E_result <- EmbedDimension(dataFrame = df,
                                       lib = lib, pred = pred,
                                       columns = env_var, target = env_var,
                                       maxE = 6, showPlot = FALSE)
            E_result$dis <- env_var
            E_result$ST <- province
            E_result
          }

stopCluster(cl)

E_env_weekly_final <- E_env_weekly %>% 
  filter(E %in% 2:6) %>%
  group_by(dis, ST) %>%
  filter(rho == max(rho)) %>%
  ungroup()

# 10.2 日级环境变量E值计算
cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

E_env_daily <- foreach(env_var = environmental_vars_11,
                       .packages = c("rEDM", "tidyverse"),
                       .combine = rbind) %:% 
  foreach(province = provinces_daily,
          .combine = rbind) %dopar% {
            df <- df_province_daily %>% 
              filter(province == !!province) %>% 
              select(date, all_of(env_var))
            
            lib <- get_ccm_library_range(df, 0, 3)
            pred <- lib
            
            E_result <- EmbedDimension(dataFrame = df,
                                       lib = lib, pred = pred,
                                       columns = env_var, target = env_var,
                                       maxE = 8, showPlot = FALSE)
            E_result$dis <- env_var
            E_result$ST <- province
            E_result
          }

stopCluster(cl)

E_env_daily_final <- E_env_daily %>% 
  filter(E %in% 2:8) %>%
  group_by(dis, ST) %>%
  filter(rho == max(rho)) %>%
  ungroup()

# 10.3 周级环境变量theta计算
plist_theta_env_weekly <- expand.grid(
  ST = provinces_weekly,
  dis = predictor_vars_12,
  theta = c(0.01, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

theta_env_weekly <- foreach(i = 1:nrow(plist_theta_env_weekly),
                            .packages = c("rEDM", "tidyverse"),
                            .combine = rbind) %dopar% {
                              ST <- plist_theta_env_weekly$ST[i]
                              dis <- plist_theta_env_weekly$dis[i]
                              theta <- plist_theta_env_weekly$theta[i]
                              
                              E <- E_env_weekly_final$E[E_env_weekly_final$ST == ST & 
                                                          E_env_weekly_final$dis == dis][1]
                              
                              df <- df_province_weekly %>% 
                                filter(province == ST) %>% 
                                select(date, all_of(dis))
                              
                              lib <- get_ccm_library_range(df, 0, E)
                              pred <- lib
                              
                              rho_theta <- PredictNonlinear(dataFrame = df,
                                                            embedded = FALSE,
                                                            columns = dis, target = dis,
                                                            Tp = 1, theta = theta,
                                                            lib = lib, pred = pred,
                                                            showPlot = FALSE, E = E)
                              
                              data.frame(rho = rho_theta$rho[1], dis = dis, state = ST, theta = theta, E = E)
                            }

stopCluster(cl)

best_theta_env_weekly <- theta_env_weekly %>%
  group_by(state, dis) %>%
  filter(rho == max(rho)) %>%
  ungroup()

# 10.4 日级环境变量theta计算
plist_theta_env_daily <- expand.grid(
  ST = provinces_daily,
  dis = environmental_vars_11,
  theta = c(0.01, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9),
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

theta_env_daily <- foreach(i = 1:nrow(plist_theta_env_daily),
                           .packages = c("rEDM", "tidyverse"),
                           .combine = rbind) %dopar% {
                             ST <- plist_theta_env_daily$ST[i]
                             dis <- plist_theta_env_daily$dis[i]
                             theta <- plist_theta_env_daily$theta[i]
                             
                             E <- E_env_daily_final$E[E_env_daily_final$ST == ST & 
                                                        E_env_daily_final$dis == dis][1]
                             
                             df <- df_province_daily %>% 
                               filter(province == ST) %>% 
                               select(date, all_of(dis))
                             
                             lib <- get_ccm_library_range(df, 0, E)
                             pred <- lib
                             
                             rho_theta <- PredictNonlinear(dataFrame = df,
                                                           embedded = FALSE,
                                                           columns = dis, target = dis,
                                                           Tp = 1, theta = theta,
                                                           lib = lib, pred = pred,
                                                           showPlot = FALSE, E = E)
                             
                             data.frame(rho = rho_theta$rho[1], dis = dis, state = ST, theta = theta, E = E)
                           }

stopCluster(cl)

best_theta_env_daily <- theta_env_daily %>%
  group_by(state, dis) %>%
  filter(rho == max(rho)) %>%
  ungroup()

# 10.5 保存反向参数
saveRDS(list(
  E_env_weekly_final = E_env_weekly_final,
  E_env_daily_final = E_env_daily_final,
  best_theta_env_weekly = best_theta_env_weekly,
  best_theta_env_daily = best_theta_env_daily
), "Step10_Reverse_Parameters.rds")

message("Step 10完成: 反向CCM参数计算完成")

################################## 10. Step 11: 流感→环境(周级反向) ###################################

# 清理所有连接
tryCatch(stopAllClusters(), error = function(e) NULL)
closeAllConnections()
gc(full = TRUE)

################################## Step 11: 流感→环境(周级反向) ###################################
# Step 11: 验证反向因果关系（流感是否影响环境变量）
step11_results <- list()
step11_meta <- data.frame()

# 11.1 对每个预测变量进行反向CCM分析
for(current_var in predictor_vars_12) {
  
  # 准备参数列表
  plist_current <- expand.grid(
    ST = provinces_weekly,
    tp_value = tp_values_weekly,
    stringsAsFactors = FALSE
  )
  
  # 并行计算CCM
  cl <- makeCluster(cores, type = 'PSOCK')
  registerDoParallel(cl)
  
  ccm_out_current <- foreach(
    j = 1:nrow(plist_current),
    .packages = c("rEDM", "tidyverse"),
    .combine = rbind
  ) %dopar% {
    fn_ccm(
      df_province_weekly,
      plist_current$ST[j],
      current_var,  # x: 环境变量
      "fluP",       # y: 流感
      plist_current$tp_value[j],
      "reverse",    # 反向验证
      FALSE
    )
  }
  
  stopCluster(cl)
  
  # 处理结果
  processed_results <- process_ccm_results(ccm_out_current, FALSE)
  step11_results[[current_var]] <- processed_results
  
  gc()
}

# 11.2 进行元分析
for(var in names(step11_results)) {
  meta_current <- conduct_meta_analysis(
    step11_results[[var]]$ccm_p, 
    tp_values_weekly, 
    var
  )
  step11_meta <- rbind(step11_meta, meta_current)
}

# 11.3 计算S-map效应（反向）
step11_effects <- data.frame()

if(nrow(step11_meta) > 0) {
  # 准备参数列表
  plist_step11 <- expand.grid(
    ST = provinces_weekly,
    plt = unique(step11_meta$plt),
    tp_value = unique(step11_meta$tp_value),
    stringsAsFactors = FALSE
  )
  
  # 并行计算S-map效应
  cl <- makeCluster(cores, type = 'PSOCK')
  registerDoParallel(cl)
  
  step11_effects <- foreach(
    i = 1:nrow(plist_step11),
    .packages = c("rEDM", "tidyverse", "lubridate"),
    .combine = rbind
  ) %dopar% {
    ST <- plist_step11$ST[i]
    env_var <- plist_step11$plt[i]  # 环境变量作为目标
    tp_value <- plist_step11$tp_value[i]
    
    # 使用环境变量的最优参数
    E <- E_env_weekly_final$E[E_env_weekly_final$ST == ST & 
                                E_env_weekly_final$dis == env_var][1]
    theta <- best_theta_env_weekly$theta[best_theta_env_weekly$state == ST & 
                                           best_theta_env_weekly$dis == env_var][1]
    
    # 准备数据（反向：fluP影响环境变量）
    df <- df_province_weekly %>% 
      filter(province == ST) %>%
      mutate(flu_tp = lag(fluP, -tp_value)) %>%
      filter(!is.na(flu_tp)) %>%
      select(date, all_of(env_var), flu_tp)
    
    M <- nrow(df)
    embed_1 <- Embed(dataFrame = df, E = E, tau = -1, columns = env_var)
    
    dataFrame <- cbind(
      df[E:M, 'date'],
      df[E:M, env_var],
      embed_1[E:M, 1:(E-1)], 
      df[E:M, 'flu_tp']
    ) %>% as.data.frame()
    
    names(dataFrame) <- c('date', env_var, letters[1:(E-1)], 'fluP')
    
    columns <- paste(paste(letters[1:(E-1)], collapse = ' '), 'fluP', sep = ' ')
    
    lib <- get_ccm_library_range(dataFrame, 0, E)
    pred <- lib
    
    smap <- SMap(
      dataFrame = dataFrame,
      embedded = TRUE,
      columns = columns,
      target = env_var,
      lib = lib,
      pred = pred,
      theta = theta,
      Tp = 1,
      E = E
    )
    
    smapc_df <- smap$coefficients[c(1, 2+E)]
    names(smapc_df) <- c('date', 'effect')
    
    smapc_df %>%
      mutate(
        date = lubridate::as_date(date, origin = lubridate::origin),
        dis = env_var,
        ST = ST, 
        plt = 'fluP',
        E = E,
        tp_value = tp_value,
        theta = theta,
        direction = 'reverse'
      )
  }
  
  stopCluster(cl)
}

# 11.4 生成可视化
# 11.4 生成可视化
if(length(step11_results) > 0) {
  # 合并所有CCM结果
  all_ccm_causal_step11 <- data.frame()
  
  for(var in names(step11_results)) {
    ccm_combined <- step11_results[[var]]$ccm_processed %>%
      left_join(
        step11_results[[var]]$ccm_p, 
        by = c("ST", "plt", "dis", "E", "tp_value")
      ) %>%
      mutate(
        sig = ifelse(p < 0.05 & rho_raw > 0, 'sig', 'non_sig'),
        sig = factor(sig, levels = c("non_sig", "sig")),
        # 关键修改：直接使用plt列（已经包含环境变量名）
        dis = plt  # 将环境变量名复制到dis列，供作图函数使用
      )
    
    all_ccm_causal_step11 <- rbind(all_ccm_causal_step11, ccm_combined)
  }
  
  # 生成图表
  create_violin_plots_step11(all_ccm_causal_step11, tp_values_weekly)
}

# 11.5 保存结果
save_step_complete(step11_results, step11_meta, step11_effects, "Flu_to_Env_Weekly_Reverse", 11)

################################## 11. Step 12: 美团→环境(日级反向) ###################################

# 12.1 日级反向CCM函数
################################## 11. Step 12: 美团→环境(日级反向) ###################################

# 12.1 日级反向CCM函数
fn_ccm_reverse_step12 <- function(data, ST, env_var, tp_value){
  E_env <- E_env_daily_final$E[E_env_daily_final$ST == ST & 
                                 E_env_daily_final$dis == env_var][1]
  
  df <- data %>% 
    filter(province == ST) %>% 
    select(date, drug_order_rate, all_of(env_var))
  
  # 生成代理数据
  surr_data <- fn_surr_data(data, ST, env_var, 0, TRUE)
  all_data <- df %>% left_join(surr_data, by = "date")
  names(all_data) <- c("date", "drug_order_rate", env_var, paste0("T", 1:(num_surr_daily+1)))
  
  libSize <- get_ccm_library_range(all_data, tp_value, E_env)
  
  rho_surr <- data.frame()
  
  for (i in 1:(num_surr_daily+1)) {
    targetCol <- paste0("T", i)
    
    ccm_out <- CCM(dataFrame = all_data, 
                   E = E_env, 
                   Tp = tp_value,
                   columns = targetCol,
                   target = "drug_order_rate",
                   libSizes = libSize, 
                   random = TRUE,
                   sample = num_sample_daily, 
                   seed = GLOBAL_SEED)
    
    col <- paste(targetCol, ":", "drug_order_rate", sep = "")
    dat <- ccm_out %>% select(LibSize, all_of(col))
    names(dat) <- c("lib", "rho")
    
    test1 <- data.frame(
      lib = dat$lib,
      rho = dat$rho,
      i = i,
      dis = env_var,
      plt = "drug_order_rate",
      E = E_env,
      tp_value = tp_value,
      ST = ST,
      direction = "reverse"
    )
    
    rho_surr <- rbind(rho_surr, test1)
  }
  
  return(rho_surr)
}

# 12.2 执行日级反向CCM分析
step12_results <- list()

for(current_env_var in environmental_vars_11) {
  for(current_lag in tp_values_daily) {
    
    cl <- makeCluster(cores, type = 'PSOCK')
    registerDoParallel(cl)
    
    batch_results <- foreach(
      province = provinces_daily,
      .packages = c("rEDM", "tidyverse"),
      .combine = rbind
    ) %dopar% {
      fn_ccm_reverse_step12(df_province_daily, province, current_env_var, current_lag)
    }
    
    stopCluster(cl)
    
    key <- paste0(current_env_var, "_lag", abs(current_lag))
    processed_results <- process_ccm_results(batch_results, TRUE)
    step12_results[[key]] <- processed_results
  }
}

# 12.3 元分析
step12_meta <- data.frame()

for(var in environmental_vars_11) {
  for(tp in tp_values_daily) {
    key <- paste0(var, "_lag", abs(tp))
    df_tp <- step12_results[[key]]$ccm_p
    
    fisher_result <- fisher_combine(df_tp$p)
    
    meta_result <- data.frame(
      p = fisher_result$p,
      statistic = fisher_result$statistic,
      df = fisher_result$df,
      tp_value = tp, 
      plt = var,
      n_provinces = nrow(df_tp),
      significant_provinces = sum(df_tp$p < 0.05 & df_tp$rho_raw > 0)
    )
    
    step12_meta <- rbind(step12_meta, meta_result)
  }
}

# 12.4 计算S-map效应
plist_step12 <- expand.grid(
  ST = provinces_daily,
  plt = environmental_vars_11,
  tp_value = tp_values_daily,
  stringsAsFactors = FALSE
)

cl <- makeCluster(cores, type = 'PSOCK')
registerDoParallel(cl)

step12_effects <- foreach(
  i = 1:nrow(plist_step12),
  .packages = c("rEDM", "tidyverse", "lubridate"),
  .combine = rbind
) %dopar% {
  ST <- plist_step12$ST[i]
  plt <- plist_step12$plt[i]
  tp_value <- plist_step12$tp_value[i]
  
  # 使用日级环境变量的最优参数（修正数据提取方式）
  E <- E_env_daily_final$E[E_env_daily_final$ST == ST & 
                             E_env_daily_final$dis == plt][1]
  theta <- best_theta_env_daily$theta[best_theta_env_daily$state == ST & 
                                        best_theta_env_daily$dis == plt][1]
  
  # 准备数据（反向：drug_order_rate影响环境变量）
  df <- df_province_daily %>% 
    filter(province == ST) %>%
    mutate(drug_tp = lag(drug_order_rate, -tp_value)) %>%
    filter(!is.na(drug_tp)) %>%
    select(date, all_of(plt), drug_tp)
  
  M <- nrow(df)
  embed_1 <- Embed(dataFrame = df, E = E, tau = -1, columns = plt)
  
  dataFrame <- cbind(
    df[E:M, 'date'],
    df[E:M, plt],
    embed_1[E:M, 1:(E-1)], 
    df[E:M, 'drug_tp']
  ) %>% as.data.frame()
  
  names(dataFrame) <- c('date', plt, letters[1:(E-1)], 'drug_order_rate')
  
  columns <- paste(paste(letters[1:(E-1)], collapse = ' '), 'drug_order_rate', sep = ' ')
  
  lib <- get_ccm_library_range(dataFrame, 0, E)
  pred <- lib
  
  smap <- SMap(
    dataFrame = dataFrame,
    embedded = TRUE,
    columns = columns,
    target = plt,
    lib = lib,
    pred = pred,
    theta = theta,
    Tp = 1,
    E = E
  )
  
  smapc_df <- smap$coefficients[c(1, 2+E)]
  names(smapc_df) <- c('date', 'effect')
  
  smapc_df %>%
    mutate(
      date = lubridate::as_date(date, origin = lubridate::origin),
      dis = plt,
      ST = ST, 
      plt = 'drug_order_rate',
      E = E,
      tp_value = tp_value,
      theta = theta,
      direction = 'reverse'
    )
}

stopCluster(cl)

# 12.5 合并结果用于可视化
all_ccm_causal_step12 <- data.frame()

for(result_name in names(step12_results)) {
  ccm_combined <- step12_results[[result_name]]$ccm_processed %>%
    left_join(step12_results[[result_name]]$ccm_p, 
              by = c("ST", "plt", "dis", "E", "tp_value")) %>%
    mutate(
      sig = ifelse(p < 0.05 & rho_raw > 0, 'sig', 'non_sig'),
      sig = factor(sig, levels = c("non_sig", "sig"))
      # 删除 plt = dis 这一行，保持原始数据结构
    )
  
  all_ccm_causal_step12 <- rbind(all_ccm_causal_step12, ccm_combined)
}

# 12.6 生成可视化
create_violin_plots_step12(all_ccm_causal_step12, tp_values_daily)
# 12.7 保存结果
save_step_complete(step12_results, step12_meta, step12_effects, "Meituan_to_Env_Daily_Reverse", 12)

# 12.8 输出统计摘要
cat("\nStep 12: 美团→环境（日级反向验证）完成\n")
cat(sprintf("测试变量数: %d\n", length(unique(step12_meta$plt))))
cat(sprintf("测试时滞数: %d\n", length(unique(step12_meta$tp_value))))
cat(sprintf("显著结果数: %d / %d\n", 
            sum(step12_meta$p < 0.05), 
            nrow(step12_meta)))

# 显示非显著结果（支持单向因果）
non_sig_results <- step12_meta %>% 
  filter(p > 0.05) %>%
  arrange(desc(p))

if(nrow(non_sig_results) > 0) {
  cat("\n反向不显著（支持环境→美团单向因果）:\n")
  for(i in 1:min(5, nrow(non_sig_results))) {
    cat(sprintf("  %s (lag %d天): p = %.3f\n",
                non_sig_results$plt[i],
                abs(non_sig_results$tp_value[i]),
                non_sig_results$p[i]))
  }
}

message("Step 12完成: 美团→环境反向验证完成")
