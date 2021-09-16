
################################

# Script for the global meta-analysis of the folowing article (in preparation): 
# Episodic and semantic FOK in aging: Systematic review & meta-analyses

# Audrey Mazancieux 2021

################################


## Import packages  -----------------------------------------------

library(tidyverse)
library(magrittr)
library(reshape2)
library(metafor)
library(broom)
library(esc)


## Import and arrange data  -------------------------

data_all_FOK <-read.csv2("Data_all_FOKs.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# create dataframe for sensitivity measures
sensitivity <- data_all_FOK %>% 
  select(ref = Reference,
         authors = authors,
         exp = exp,
         effect = effect,
         perf_ctrl = attempt.to.control.for.performance.in.design,
         task = Task,
         n_YA = Sample_size_YA,
         n_OA = Sample_Size_OA,
         sensitivity_mean_YA = Sensitivity_mean__YA,
         sensitivity_sdt_YA = Sensitivity_ET_YA,
         sensitivity_mean_OA = Sensitivity_mean_OA,
         sensitivity_sdt_OA = Sensitivity_ET_OA,
         recall_mean_YA = Recall_mean_YA,
         recall_sdt_YA = Recall_SD_YA,
         recall_mean_OA = Recall_mean_OA,
         recall_sdt_OA = Recall_SD_OA,
         recog_mean_YA = Recog_mean_YA,
         recog_sdt_YA = Recog_SD_YA,
         recog_mean_OA = Recog_mean_OA,
         recog_sdt_OA = Reco_SD_OA,
         recog_type = Recog.type)

# exclude Hertzog (2010) - 30min condition
sensitivity %<>% 
  filter(ref != "Hertzog , Dunlosky & Sinclair (2010) - 30min")

  
## Compute effect size for each study -----------------------------------------

eff_size_all_studies <- data.frame()
var_all_studies <- data.frame()

for (refs in 1:nrow(sensitivity)){
  
  effect_size <- esc_mean_sd(grp1m = sensitivity$sensitivity_mean_YA[refs], 
                             grp1sd = sensitivity$sensitivity_sdt_YA[refs], 
                             grp1n = sensitivity$n_YA[refs],
                             grp2m = sensitivity$sensitivity_mean_OA[refs], 
                             grp2sd = sensitivity$sensitivity_sdt_OA[refs], 
                             grp2n = sensitivity$n_YA[refs], 
                             es.type = "g")
  
  g_hedge <- effect_size[1]
  sampling_var <- effect_size[3] 
  eff_size_all_studies %<>% rbind(g_hedge)
  var_all_studies %<>% rbind(sampling_var) 
}

# add references column 
ref = c(sensitivity[1])
authors = c(sensitivity[2])
exp = c(sensitivity[3])
effect = c(sensitivity[4])
task = c(sensitivity[6])
effects_all_studies <- eff_size_all_studies %>% 
  cbind(ref) %>%
  cbind(var_all_studies) %>% 
  cbind(authors) %>% 
  cbind(task) %>% 
  cbind(exp) %>% 
  cbind(effect)


## eFOK meta-analysis --------------------------------------------------------

# create model
eFOK_data <- effects_all_studies %>% 
  filter(task == "Episodic")

eFOK_meta_model <- rma.mv(es, 
                          var, 
                          random = list(~ 1 | effect,
                                        ~ 1 | exp,
                                        ~ 1 | authors),
                          data = eFOK_data)
summary(eFOK_meta_model)

# forest plot
jpeg(file="./figures/forest_eFOK_meta_all.jpeg",
     width=13, height=8, units="in", res=300)
forest(eFOK_meta_model, slab = eFOK_data$ref)
dev.off()

# funnel plot 
jpeg(file="./figures/funnel_eFOK_meta_all.jpeg",
     width=8, height=6, units="in", res=300)
funnel(eFOK_meta_model)
dev.off()

# test of moderator: attempt to control for task performance
epi <- sensitivity %>% filter(task == "Episodic")
perf_ctrl <- ifelse(epi$perf_ctrl == 'no', -0.5, 0.5)
eFOK_data %<>%
  mutate(perf_ctrl = perf_ctrl)

eFOK_reg1_model <- rma.mv(es, 
                          var, 
                          mods = perf_ctrl,
                          random = list(~ 1 | effect,
                                        ~ 1 | exp,
                                        ~ 1 | authors),
                          data = eFOK_data)
summary(eFOK_reg1_model)

# test of moderator: group difference effect size for recall
eff_size_recall <- data.frame()
var_recall <- data.frame()

for (refs in 1:nrow(sensitivity)){
  
  effect_size <- esc_mean_sd(grp1m = sensitivity$recall_mean_YA[refs], 
                             grp1sd = sensitivity$recall_sdt_YA[refs], 
                             grp1n = sensitivity$n_YA[refs],
                             grp2m = sensitivity$recall_mean_OA[refs], 
                             grp2sd = sensitivity$recall_sdt_OA[refs], 
                             grp2n = sensitivity$n_YA[refs], 
                             es.type = "g")
  
  g_hedge <- effect_size[1]
  sampling_var <- effect_size[3] 
  eff_size_recall %<>% rbind(g_hedge)
  var_recall %<>% rbind(sampling_var) 
}

eff_size_recall %<>% 
  cbind(task) %>% 
  filter(task == "Episodic")
eFOK_data %<>% 
  cbind(recall = eff_size_recall$es)

eFOK_reg2_model <- rma.mv(es, 
                          var, 
                          mods = recall,
                          random = list(~ 1 | effect,
                                        ~ 1 | exp,
                                        ~ 1 | authors),
                          data = eFOK_data)
summary(eFOK_reg2_model)

# test of moderator: group difference effect size for recognition
eff_size_recog <- data.frame()
var_recog <- data.frame()

for (refs in 1:nrow(sensitivity)){
  
  effect_size <- esc_mean_sd(grp1m = sensitivity$recog_mean_YA[refs], 
                             grp1sd = sensitivity$recog_sdt_YA[refs], 
                             grp1n = sensitivity$n_YA[refs],
                             grp2m = sensitivity$recog_mean_OA[refs], 
                             grp2sd = sensitivity$recog_sdt_OA[refs], 
                             grp2n = sensitivity$n_YA[refs], 
                             es.type = "g")
  
  g_hedge <- effect_size[1]
  sampling_var <- effect_size[3] 
  eff_size_recog %<>% rbind(g_hedge)
  var_recog %<>% rbind(sampling_var) 
}

eff_size_recog %<>% 
  cbind(task) %>% 
  filter(task == "Episodic")
eFOK_data %<>% 
  cbind(recog = eff_size_recog$es)

eFOK_reg3_model <- rma.mv(es, 
                          var, 
                          mods = recog,
                          random = list(~ 1 | effect,
                                        ~ 1 | exp,
                                        ~ 1 | authors),
                          data = eFOK_data)
summary(eFOK_reg3_model)

# forest plot
jpeg(file="./figures/forest_eFOK_meta_recog_mod.jpeg",
     width=13, height=8, units="in", res=300)
forest(eFOK_reg3_model, slab = eFOK_data$ref)
dev.off()

# test of moderator: type of recognition
recog_type = epi$recog_type
eFOK_data %<>%
  mutate(recog_type = recog_type)

eFOK_reg4_model <- rma.mv(es, 
                          var, 
                          mods = recog_type,
                          random = list(~ 1 | effect,
                                        ~ 1 | exp,
                                        ~ 1 | authors),
                          data = eFOK_data)
summary(eFOK_reg4_model)


## sFOK meta-analysis --------------------------------------------------------

# create model
sFOK_data <- effects_all_studies %>% 
  filter(task == "semantic")
sFOK_meta_model <- rma.mv(es, var, random = ~ 1 | ref, data=sFOK_data)
summary(sFOK_meta_model)

# forest plot
jpeg(file="./figures/forest_sFOK_meta_all.jpeg",
     width=10, height=8, units="in", res=300)
forest(sFOK_meta_model, slab = sFOK_data$ref)
dev.off()

# funnel plot 
jpeg(file="./figures/funnel_sFOK_meta_all.jpeg",
     width=8, height=6, units="in", res=300)
funnel(sFOK_meta_model)
dev.off()  
