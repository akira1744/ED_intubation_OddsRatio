rm(list=ls())

if (!require("pacman")) install.packages("pacman")

pacman::p_load(lattice,skimr,broom,broom.mixed,lme4,tidyverse,tidylog)

################################################################################
# サンプルデータ作成
################################################################################

set.seed(123)  # 再現性を保つためのシード

# low_outliner
ptids <- 1:3613
centers <- 1:50
n_ed_intubation <- 304
n_motality <- 305

low_outliner <- tibble(ptid = ptids) %>% 
  mutate(motality = if_else(ptid %in% sample(ptids, n_motality), 1, 0)) %>% 
  mutate(ED_intubation = if_else(ptid %in% sample(ptids, n_ed_intubation), 1, 0)) %>% 
  mutate(center = sample(centers, size = length(ptids), replace = TRUE)) %>% 
  mutate(class='low_outliner') %>% 
  print()

# average_center
ptids <- 3614:(3613+3814)
centers <- 51:(50+137)
n_ed_intubation <- 770
n_motality <- 366

average_center <- tibble(ptid = ptids) %>% 
  mutate(motality = if_else(ptid %in% sample(ptids, n_motality), 1, 0)) %>% 
  mutate(ED_intubation = if_else(ptid %in% sample(ptids, n_ed_intubation), 1, 0)) %>% 
  mutate(center = sample(centers, size = length(ptids), replace = TRUE)) %>% 
  mutate(class='average_center') %>%
  print()

# high_outliner
ptids <- (3613+3814+1):(3613+3814+2240)
centers <- (50+137+1):(50+137+66)
n_ed_intubation <- 898
n_motality <- 216

high_outliner <- tibble(ptid = ptids) %>% 
  mutate(motality = if_else(ptid %in% sample(ptids, n_motality), 1, 0)) %>% 
  mutate(ED_intubation = if_else(ptid %in% sample(ptids, n_ed_intubation), 1, 0)) %>% 
  mutate(center = sample(centers, size = length(ptids), replace = TRUE)) %>% 
  mutate(class='high_outliner') %>% 
  print()

# 結合
df <- rbind(low_outliner,average_center,high_outliner) %>% 
  print()


# HPの分類をfactorに
df <- df %>% 
  mutate(class = factor(class,levels = c('low_outliner','average_center','high_outliner'))) %>% 
  mutate(center=factor(center)) 

# 年齢列を追加
df <- df %>%
  mutate(age = rnorm(n = nrow(df), mean = 33, sd = 10))

# ageをsexのデータを追加
df <- df %>% 
  mutate(male = if_else(ptid %in% sample(df$ptid, 8046),1,0)) %>% 
  mutate(male=factor(male))

################################################################################

# 想定通りのデータになっていることの確認
df

skimr::skim(df)

df %>% 
  group_by(class) %>% 
  summarise(
    patients = n_distinct(ptid),
    centers = n_distinct(center),
    ED_intubation =sum(ED_intubation),
    motality = sum(motality)
  )

################################################################################
# Figure1-Aを作成
################################################################################

# centerごとのED_intubation_rate
agg1 <- df %>% 
  group_by(class,center) %>% 
  summarise(ED_intubation_rate = sum(ED_intubation) / n()) %>% 
  ungroup() %>% 
  print()

# ED_intubation_rateが低い順に並べて,新たに連番をふる
agg1 <- agg1 %>% 
  arrange(ED_intubation_rate) %>% 
  rowid_to_column('rank') %>% 
  print()

# Figure1-Aを作成
ggplot(agg1)+
  aes(x=rank, y=ED_intubation_rate, fill=class)+
  geom_col()+
  scale_y_continuous(labels = scales::percent,breaks = seq(0,0.8,0.1)) +  # y軸をパーセント表記に変換
  scale_fill_manual(values = c("#04B14A", "#DEDEDE", "#BD0300"))+  # 棒の色を指定
  xlab("Trauma Centers") +  
  ylab("ED Intubation (%)")+  
  theme_classic()


################################################################################
# マルチレベルロジスティック回帰
################################################################################

# centerごとにED_intubationを予測するときの切片を変えるようなモデルを作ってみる。
formula <- ED_intubation ~ age + male + (1 | center)
model = lme4::glmer(formula, df, family=binomial)
summary(model)

# 固定効果はOddsが出せる
cc <- confint(model,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(model), cc)
rtab <- exp(ctab)
print(rtab, digits=3)

# ランダム効果は個別の切片がわかるだけ
lme4::ranef(model)$center

# このあとどうやってcenterごとのMOR(median odds ratio)を出すのかを調べる必要がある。
# 該当論文が引用している、以下の論文を読む必要がある。
# Merio J, Chaix B, Ohlsson H, Beckman A, Johnell K, Hjerpe P, et al.A brief conceptual tutorial of multilevel analysis in social epidemiology: using mea­ sures of clustering in multilevel logistic regression to investigate contextual phenomena. JEpidemiol Community Health. 2006;60(4):290-297.
