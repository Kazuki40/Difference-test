library(rstatix)

# read_csv ----------------------------------------------------------------
file_name <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(file_name)) {
  file_name <- "aaa.csv"
}

use_data_anrysis_func <- function(file) {
  data_read_csv <-
    read.csv(file_name, header = TRUE)
  
  return(data.frame(kind = factor(data_read_csv[, 1]), value = data_read_csv[, 2]))
}

use_data_anrysis <- use_data_anrysis_func(file_name)

# boxplot -----------------------------------------------------------------

#("boxplot.jpg")
boxplot(value ~ kind, data = use_data_anrysis, main = "box_plot")
#dev.off()

# group_num ---------------------------------------------------------------

id_label <- unique(use_data_anrysis$kind)

id_label_leng <- length(id_label)
if (id_label_leng == 1) {
  print("error 1")
}


# basic_statistics --------------------------------------------------------
basic <- lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  summary(temp)
})
names(basic) <- id_label
print(basic)

lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  file_name <- paste("histgram_", id_label[i], ".jpg")
  # jpeg(file_name)
  hist(temp, main = id_label[i])
  
  #  dev.off()
})


# Normality test (Shapiro-Wilk test) --------------------------------------
#H0: 正規分布である，H1:正規分布ではない
shap_test <- lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  shapiro.test(temp)
})
names(shap_test) <- id_label

print(shap_test)

normality_tf <- TRUE
for (loop_i in 1:id_label_leng) {
  if (shap_test[[loop_i]]$p.value <= 0.05) {
    normality_tf <- FALSE
  }
}

if (normality_tf == FALSE) {
  print("no_normality")
}

if (id_label_leng == 1) {
  stop("error")
}

# test --------------------------------------------------------------------

#対応のない検定
if (id_label_leng == 2) {
  #H0: 2群の平均値は等しい，H1: 2群の平均値は異なっている
  
  
  #t検定(welchのt検定)
  #等分散を仮定しない2群間の検定
  ans_t_test <-
    t_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      paired = FALSE,
      var.equal = FALSE,
      alternative = "two.sided",
      mu = 0,
      conf.level = 0.95,
      detailed = TRUE
    )
  
  #ウィルコクソンの順位和検定（ =マンホィットニーのU検定）
  #正規性を仮定しない2群間の検定
  ans_will_test <-
    wilcox_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      paired = FALSE,
      exact = TRUE,
      alternative = "two.sided",
      mu = 0,
      conf.level = 0.95,
      detailed = TRUE
    )
  
  #Brunner-Munzelの検定
  #正規性・等分散を仮定しない2群間の検定
  #2群からそれぞれ値を取り出した時，一方が多い確率は1/2
  ans_bru <- brunner.munzel.test(
    use_data_anrysis[use_data_anrysis$kind == id_label[1], 2],
    use_data_anrysis[use_data_anrysis$kind == id_label[2], 2],
    alternative = c("two.sided"),
    alpha = 0.05
  )
  
  #出力
  print(ans_t_test)
  print(summary(ans_t_test))
  print(ans_will_test)
  print(summary(ans_will_test))
  print(ans_bru)
  print(summary(ans_bru))
  
} else{
  #H0: 全ての群の平均値は等しい，H1: 全ての群のうちいずれかの平均値は異なっている
  
  
  #１元配置分散
  ans_aov <-
    aov(
      value ~ kind,
      data = use_data_anrysis,
      projections = FALSE,
      qr = TRUE,
      contrasts = NULL
    )
  
  #クラスカル・ウォーリスのH検定
  ans_kurask <- kruskal.test(value ~ kind, data = use_data_anrysis)
  
  print(ans_aov)
  print(summary(ans_aov))
  print(ans_kurask)
  print(summary(ans_kurask))
  
  
  #多重比較検定
  #補正方法はHolm の方法
  #H0: 抽出した2群の平均値は等しい，H1: 抽出した2群の平均値は異なっている
  
  
  #ｔ検定
  par_ans_t_test <-
    pairwise_t_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      paired = FALSE,
      detailed = TRUE
    )
  #ウィルコクソンの検定(U検定)
  par_ans_will_test <-
    pairwise_wilcox_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      detailed = TRUE
    )
  #出力
  print("多重比較検定")
  print(par_ans_t_test)
  print(summary(par_ans_t_test))
  print(par_ans_will_test)
  print(summary(par_ans_will_test))
  
}
