library(rstatix)
library(parallel)
library(lawstat)
# read_csv ----------------------------------------------------------------
file_name <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(file_name)) {
  file_name <- "aaa.csv"
}

use_data_anrysis_func <- function(file) {
  data_read_csv <-
    read.csv(file_name, header = TRUE, fileEncoding = "CP932")
  
  return(data.frame(kind = factor(data_read_csv[, 1]), value = data_read_csv[, 2]))
}

use_data_anrysis <- use_data_anrysis_func(file_name)

# boxplot -----------------------------------------------------------------

jpeg("boxplot.jpg")
boxplot(value ~ kind, data = use_data_anrysis, main = "box_plot")
dev.off()

# group_num ---------------------------------------------------------------

id_label <- unique(use_data_anrysis$kind)
id_label_leng <- length(id_label)
if (id_label_leng == 1) {
  print("error 1")
}


# basic_statistics --------------------------------------------------------
cl <- makeCluster(detectCores())
clusterExport(cl, c('id_label_leng', 'use_data_anrysis', 'id_label'))

#parallelization
basic <- parLapply(cl = cl, 1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  temp_list <- list(temp)
  names(temp_list) <- id_label[i]
  summary(temp_list)
})
stopCluster(cl)
print(basic)

lapply(1:id_label_leng, function(i) {
  temp <- use_data_anrysis[use_data_anrysis$kind == id_label[i], 2]
  file_name <- paste("histgram_", id_label[i], ".jpg")
  jpeg(file_name)
  hist(temp, main = id_label[i])
  
  dev.off()
})


# Normality test (Shapiro-Wilk test) --------------------------------------
#H0: ���K���z�ł���CH1:���K���z�ł͂Ȃ�
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

#�Ή��̂Ȃ�����
if (id_label_leng == 2) {
  #H0: 2�Q�̕��ϒl�͓������CH1: 2�Q�̕��ϒl�͈قȂ��Ă���
  
  
  #t����(welch��t����)
  #�����U�����肵�Ȃ�2�Q�Ԃ̌���
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
  
  #�E�B���R�N�\���̏��ʘa����i =�}���z�B�b�g�j�[��U����j
  #���K�������肵�Ȃ�2�Q�Ԃ̌���
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
  
  #Brunner-Munzel�̌���
  #���K���E�����U�����肵�Ȃ�2�Q�Ԃ̌���
  #2�Q���炻�ꂼ��l�����o�������C����������m����1/2
  ans_bru <- brunner.munzel.test(
    use_data_anrysis[use_data_anrysis$kind == id_label[1], 2],
    use_data_anrysis[use_data_anrysis$kind == id_label[2], 2],
    alternative = c("two.sided"),
    alpha = 0.05
  )
  
  #�o��
  print(ans_t_test)
  print(summary(ans_t_test))
  print(ans_will_test)
  print(summary(ans_will_test))
  print(ans_bru)
  print(summary(ans_bru))
  
} else{
  #H0: �S�Ă̌Q�̕��ϒl�͓������CH1: �S�Ă̌Q�̂��������ꂩ�̕��ϒl�͈قȂ��Ă���
  
  
  #�P���z�u���U
  ans_aov <-
    aov(
      value ~ kind,
      data = use_data_anrysis,
      projections = FALSE,
      qr = TRUE,
      contrasts = NULL
    )
  
  #�N���X�J���E�E�H�[���X��H����
  ans_kurask <- kruskal.test(value ~ kind, data = use_data_anrysis)
  
  print(ans_aov)
  print(summary(ans_aov))
  print(ans_kurask)
  print(summary(ans_kurask))
  
  
  #���d��r����
  #�␳���@��Holm �̕��@
  #H0: ���o����2�Q�̕��ϒl�͓������CH1: ���o����2�Q�̕��ϒl�͈قȂ��Ă���
  
  
  #������
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
  #�E�B���R�N�\���̌���(U����)
  par_ans_will_test <-
    pairwise_wilcox_test(
      data = use_data_anrysis,
      value ~ kind,
      comparisons = NULL,
      ref.group = NULL,
      p.adjust.method = "holm",
      detailed = TRUE
    )
  #�o��
  print("���d��r����")
  print(par_ans_t_test)
  print(summary(par_ans_t_test))
  print(par_ans_will_test)
  print(summary(par_ans_will_test))
  
}