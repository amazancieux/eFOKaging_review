
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
         perf_ctrl = attempt.to.control.for.performance.in.design,
         task = Task,
         n_YA = Sample_size_YA,
         n_OA = Sample_Size_OA,
         sensitivity_mean_YA = Sensitivity_mean__YA,
         sensitivity_sdt_YA = Sensitivity_ET_YA,
         sensitivity_mean_OA = Sensitivity_mean_OA,
         sensitivity_sdt_OA = Sensitivity_ET_OA)


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
refs = c(sensitivity[1])
task = c(sensitivity[3])
effects_all_studies <- eff_size_all_studies %>% 
  cbind(var_all_studies) %>% 
effects_all_studies %<>%
  cbind(refs)
effects_all_studies %<>%
  cbind(task)


## eFOK meta-analysis --------------------------------------------------------

# create model
eFOK_data <- effects_all_studies %>% 
  filter(task == "Episodic")
eFOK_meta_model <- rma(es, var, data=eFOK_data)
summary(eFOK_meta_model)

# forest plot
jpeg(file="./figures/forest_eFOK_meta_all.jpeg",
     width=10, height=8, units="in", res=300)
forest(eFOK_meta_model, slab = eFOK_data$ref)
dev.off()

# funnel plot 
jpeg(file="./figures/funnel_eFOK_meta_all.jpeg",
     width=8, height=6, units="in", res=300)
funnel(eFOK_meta_model)
dev.off()


## sFOK meta-analysis --------------------------------------------------------

# create model
sFOK_data <- effects_all_studies %>% 
  filter(task == "semantic")
sFOK_meta_model <- rma(es, var, data=sFOK_data)
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
