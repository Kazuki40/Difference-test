loop_i = 0
library(rstatix)
library(parallel)
library(lawstat)
# read_csv ----------------------------------------------------------------
file_name <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(file_name)) {
  file_name <- "aaa.csv"
}

data_read_csv <-
  read.csv(file_name, header = TRUE, fileEncoding = "CP932")

use_data_anrysis <-
  data.frame(kind = factor(data_read_csv[, 1]), value = data_read_csv[, 2])


# boxplot -----------------------------------------------------------------

boxplot(value ~ kind, data = use_data_anrysis, main = "box_plot")

# group_num ---------------------------------------------------------------

id_label <- unique(use_data_anrysis$kind)
id_label_leng <- length(id_label)
if (id_label_leng == 1) {
  print("error 1")
}


# basic_statistics --------------------------------------------------------
cl <- makeCluster(detectCores())
clusterExport(cl, c('id_label_leng', 'use_data_anrysis', 'id_label'))

basic <- parLapply(cl = cl, 1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  summary(temp)
})
stopCluster(cl)
print(basic)

lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  #summary(temp)
  hist(temp, main = id_label[i])
})

# Normality test (Shapiro-Wilk test) --------------------------------------
#H0: ³‹K•ª•z‚Å‚ ‚éCH1:³‹K•ª•z‚Å‚Í‚È‚¢
shap_test <- lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  shapiro.test(temp)
})
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


# test --------------------------------------------------------------------

if (id_label_leng == 2) {
  ans_t_test <-
    t_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      paired = FALSE,
      var.equal = FALSE,
      alternative = "two.sided",
      mu = 0,
      conf.level = 0.95,
      detailed = TRUE
    )
  
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
  
  ans_bru <- brunner.munzel.test(
    use_data_anrysis[use_data_anrysis$kind == id_label[1], 2],
    use_data_anrysis[use_data_anrysis$kind == id_label[2], 2],
    alternative = c("two.sided"),
    alpha = 0.05
  )
  print(ans_t_test)
  print(summary(ans_t_test))
  print(ans_will_test)
  print(summary(ans_will_test))
  print(ans_bru)
  print(summary(ans_bru))
  
} else{
  ans_aov <-
    aov(
      value ~ kind,
      data = use_data_anrysis,
      projections = FALSE,
      qr = TRUE,
      contrasts = NULL
    )
  
  ans_kurask <- kruskal.test(value ~ kind, data = use_data_anrysis)
  
  print(ans_aov)
  print(summary(ans_aov))
  print(ans_kurask)
  print(summary(ans_kurask))
  
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
  
  par_ans_will_test <-
    pairwise_wilcox_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      detailed = TRUE
    )
  
  print(par_ans_t_test)
  print(summary(par_ans_t_test))
  print(par_ans_will_test)
  print(summary(par_ans_will_test))
  
}
