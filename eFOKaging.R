
################################

# Script for the sub meta-analysis of the folowing article (in preparation): 
# Episodic and semantic FOK in aging: Systematic review & meta-analyses

# Audrey Mazancieux 2020

################################


## Import packages  -----------------------------------------------

library(tidyverse)
library(magrittr)
library(reshape2)
library(metafor)
library(broom)
library(esc)


## Import and arrange data  -------------------------

MetaDF <-read.csv("Data_for_meta.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# Exclude read/gen expe
MetaDF %<>%
  filter(Paper != "read/gen")

# Rename papers 
article <- unique(MetaDF$Paper)
MetaDF %<>%
  mutate(Paper = case_when(
    Paper == article[1] ~ "Souchay et al. (2007) - Exp1",
    Paper == article[2] ~ "Souchay et al. (2007) - Exp2", 
    Paper == article[3] ~ "Souchay & Isingrini (2012)", 
    Paper == article[4] ~ "Unpublished data",
    Paper == article[5] ~ "Souchay et al (2000)",
    Paper == article[6] ~ "Perrotin et al. (2006)"))
    
# Calculate corrected gamma
MetaDF %<>%
  mutate(A_freq = (0.5+A) / (num_item4gamma+1),
         B_freq = (0.5+B) / (num_item4gamma+1),
         C_freq = (0.5+C) / (num_item4gamma+1),
         D_freq = (0.5+D) / (num_item4gamma+1),
         Gamma = ((A_freq*D_freq - B_freq*C_freq) / (A_freq*D_freq + B_freq*C_freq)),
         Group = ifelse(Group == 1, "Young", "Old"))

# Prepare data for meta-analysis
Meta_mean <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


## Meta1: corrected gamma across OA and YA -------------------------------------------------------------------

# Calculate effet size and their variance
eff_size1 <- data.frame()
var1 <- data.frame()

for (refs in 1:nrow(Meta_pp)){
  
  effect_size1 <- esc_mean_sd(grp1m = Meta_mean$Young[refs], 
                             grp1sd = Meta_sd$Young[refs], 
                             grp1n = Meta_pp$Young[refs],
                             grp2m = Meta_mean$Old[refs], 
                             grp2sd = Meta_sd$Old[refs], 
                             grp2n = Meta_pp$Old[refs], 
                             es.type = "g")
  
  g_hedge <- effect_size1[1]
  sampling_var <- effect_size1[3] 
  eff_size1 %<>% rbind(g_hedge)
  var1 %<>% rbind(sampling_var) 
}

# add references column 
refs = c(Meta_pp[1])
Effect <- eff_size1 %>% 
  cbind(var1)
Effect %<>%
  cbind(refs)

# Create model
ma_model <- rma(es, var, data = Effect)
summary(ma_model)

# Forest plot
jpeg(file="./figures/forest_meta1.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model, slab = Meta_mean$Paper)
dev.off()

# Funnel plot 
jpeg(file="./figures/funnel_meta1.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model)
dev.off()


## Meta2: comparing recall performance across OA and YA -------------------------------------------------------------------

# Prepare data
Meta_mean2 <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Recall", mean)

Meta_sd2 <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Recall", sd)

# Calculate effet size and their variance 
Effect2 <- escalc(n1i = Meta_pp$Young, n2i = Meta_pp$Old, m1i = Meta_mean2$Young, m2i = Meta_mean2$Old, 
                  sd1i = Meta_sd2$Young, sd2i = Meta_sd2$Old, measure = "SMD", 
                  append = TRUE)

# Create model
ma_model2 <- rma(yi, vi, data = Effect2)
summary(ma_model2)

# Forest plot 
jpeg(file="./figures/forest_meta2.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model2, slab = Meta_mean2$Paper)
dev.off()

# Funnel plot 
jpeg(file="./figures/funnel_meta2.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model2)
dev.off()


## Meta3: corrected gamma for 50% of the participants according to recall performance  --------------------------------------

# Calculate median for each group
recall <- MetaDF %>%
  group_by(Paper, Group) %>%
  summarise(median = median(Recall))

# Prepare data for median list 
Meta_pp %<>%
  gather(Group, sample_size, -Paper) 

match <- MetaDF
study <- unique(MetaDF$Paper)
group <- unique(MetaDF$Group)

# Create median list for each study and each group
median_list <- data.frame()
      
for (s in study) {

  for (g in group){
    median <- rep(recall$median[recall$Paper == s & recall$Group == g], 
               Meta_pp$sample_size[Meta_pp$Paper == s & Meta_pp$Group == g])
    median <- data.frame(median)
    median_list %<>% rbind(median) 
  }
}

# Filter pp > median for older adults and pp < median for young
match <- MetaDF %>% 
  select(Paper, Group, Gamma, Recall) %>% 
  cbind(median_list) %>% 
  mutate(filter = case_when(
    Group == 'Old' & Recall >= median ~ 1,
    Group == 'Old' & Recall <= median ~ 0,
    Group == 'Young' & Recall <= median ~ 1,
    Group == 'Young' & Recall >= median ~ 0)) %>% 
  filter(filter == 1)

# Prepare data 
Meta_mean3 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd3 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp3 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


# Calculate effet size and their variance 
Effect3 <- escalc(n1i = Meta_pp3$Young, n2i = Meta_pp3$Old, m1i = Meta_mean3$Young, m2i = Meta_mean3$Old, 
                 sd1i = Meta_sd3$Young, sd2i = Meta_sd3$Old, measure = "SMD", 
                 append = TRUE)

# Create model 
ma_model3 <- rma(yi, vi, data = Effect3)
summary(ma_model3)

# Forest plot 
jpeg(file="./figures/forest_meta3.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model3, slab = Meta_mean3$Paper)
dev.off()

# Funnel plot
jpeg(file="./figures/funnel_meta3.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model3)
dev.off()


# Filter pp < median for older adults and pp > median for young
match2 <- MetaDF %>% 
  select(Paper, Group, Gamma, Recall) %>% 
  cbind(median_list) %>% 
  mutate(filter = case_when(
    Group == 'Old' & Recall >= median ~ 1,
    Group == 'Old' & Recall <= median ~ 0,
    Group == 'Young' & Recall <= median ~ 1,
    Group == 'Young' & Recall >= median ~ 0)) %>% 
  filter(filter == 0)

# Prepare data 
Meta_mean3.2 <- match2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd3.2 <- match2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp3.2 <- match2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


# Calculate effet size and their variance 
Effect3.2 <- escalc(n1i = Meta_pp3.2$Young, n2i = Meta_pp3.2$Old, m1i = Meta_mean3.2$Young, m2i = Meta_mean3.2$Old, 
                  sd1i = Meta_sd3.2$Young, sd2i = Meta_sd3.2$Old, measure = "SMD", 
                  append = TRUE)

# Create model 
ma_model3.2 <- rma(yi, vi, data = Effect3.2)
summary(ma_model3.2)

# Forest plot 
jpeg(file="./figures/forest_meta3.2.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model3.2, slab = Meta_mean3.2$Paper)
dev.off()

# Funnel plot
jpeg(file="./figures/funnel_meta3.2.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model3.2)
dev.off()


## Non-preregistered analyis: corrected gamma for 50% of the participants according to recognition performance  --------------------------------------

# Calculate median for each group
recog <- MetaDF %>%
  mutate(recog = Hits) %>% 
  group_by(Paper, Group) %>%
  summarise(median = median(recog))

# Prepare data for median list 
Meta_pps <- Meta_pp %>% 
  gather(Group, sample_size, -Paper) 

# Create median list for each study and each group
median_recog_list <- data.frame()

for (s in study) {
  
  for (g in group){
    median <- rep(recog$median[recog$Paper == s & recog$Group == g], 
                  Meta_pps$sample_size[Meta_pps$Paper == s & Meta_pps$Group == g])
    median <- data.frame(median)
    median_recog_list %<>% rbind(median) 
  }
}

# Filter pp > median for older adults and pp < median for young
match_recog <- MetaDF %>% 
  select(Paper, Group, Gamma, Hits) %>% 
  arrange(Paper) %>% 
  cbind(median_recog_list) %>% 
  mutate(filter = case_when(
    Group == 'Old' & Hits >= median ~ 1,
    Group == 'Old' & Hits <= median ~ 0,
    Group == 'Young' & Hits <= median ~ 1,
    Group == 'Young' & Hits >= median ~ 0)) %>% 
  filter(filter == 1)

# Prepare data 
Meta_mean4 <- match_recog %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd4 <- match_recog %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp4 <- match_recog %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


# Calculate effet size and their variance 
Effect4 <- escalc(n1i = Meta_pp4$Young, n2i = Meta_pp4$Old, m1i = Meta_mean4$Young, m2i = Meta_mean4$Old, 
                  sd1i = Meta_sd4$Young, sd2i = Meta_sd4$Old, measure = "SMD", 
                  append = TRUE)

# Create model 
ma_model4 <- rma(yi, vi, data = Effect4)
summary(ma_model4)

# Forest plot 
jpeg(file="./figures/forest_meta4.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model4, slab = Meta_mean4$Paper)
dev.off()

# Funnel plot
jpeg(file="./figures/funnel_meta4.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model4)
dev.off()


# Filter pp < median for older adults and pp > median for young
match_recog2 <- MetaDF %>% 
  select(Paper, Group, Gamma, Hits) %>% 
  arrange(Paper) %>% 
  cbind(median_recog_list) %>% 
  mutate(filter = case_when(
    Group == 'Old' & Hits >= median ~ 1,
    Group == 'Old' & Hits <= median ~ 0,
    Group == 'Young' & Hits <= median ~ 1,
    Group == 'Young' & Hits >= median ~ 0)) %>% 
  filter(filter == 0)

# Prepare data 
Meta_mean4.2 <- match_recog2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd4.2 <- match_recog2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp4.2 <- match_recog2 %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


# Calculate effet size and their variance 
Effect4.2 <- escalc(n1i = Meta_pp4.2$Young, n2i = Meta_pp4.2$Old, m1i = Meta_mean4.2$Young, m2i = Meta_mean4.2$Old, 
                    sd1i = Meta_sd4.2$Young, sd2i = Meta_sd4.2$Old, measure = "SMD", 
                    append = TRUE)

# Create model 
ma_model4.2 <- rma(yi, vi, data = Effect4.2)
summary(ma_model4.2)

# Forest plot 
jpeg(file="./figures/forest_meta4.2.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model4.2, slab = Meta_mean4.2$Paper)
dev.off()

# Funnel plot
jpeg(file="./figures/funnel_meta4.2.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model4.2)
dev.off()


## Meta5: effect sizes for models wtih recall as a covariable  --------------------------------------

# crlt_perf <- MetaDF %>%
#   mutate(GroupC = ifelse(Group == "Old", -0.5, 0.5))
# 
# study <- Meta_pp$Paper
# es_value <- data.frame()
# var_value <- data.frame()
# 
# # Compute linear model for each study
# for (s in study){
# 
#   # create a dataframe for this study
#   crlt_perf_s <- crlt_perf %>%
#     filter(Paper == s)
# 
#   # linear model
#   reg <- lm(Gamma ~ GroupC + Recall, crlt_perf_s)
#   reg <- lm(Gamma ~ GroupC * Recall, crlt_perf_s)
# 
#   # linear regression checks
#   qqnorm(residuals(reg))
#   qqline(residuals(reg))
# 
#   data.frame(x = residuals(reg)) %>%
#     ggplot(aes(x = x)) +
#     geom_histogram()
# 
#   # results
#   res <- summary(reg)
#   tidy_res <- tidy(res[[4]])
#   tidy_var = coef(res)[, "Std. Error"]
# 
#   # append dataframe
#   es_value %<>% rbind(tidy_res$x[2])
#   var_value %<>% rbind(tidy_var[2])
# }
# 
# # Arrange dataframe
# data_m5 <- es_value %>% cbind(var_value)
# colnames(data_m5)[1] <- "es"
# colnames(data_m5)[2] <- "se"
# Meta_pp %<>%
#   mutate(N = Old+Young)
# data_m4 %<>%
#   cbind(N = Meta_pp$N) %>%
#   mutate(var = es/sqrt(N))
# 
# # Create model #4
# ma_model5 <- rma(es, var, data = data_m5)
# summary(ma_model5)
# 
# # Forest plot
# jpeg(file="./figures/forest_meta5.jpeg",
#      width=8, height=4.5, units="in", res=300)
# forest(ma_model5, slab = study)
# dev.off()
# 
# # Funnel plot
# jpeg(file="./figures/funnel_meta5.jpeg",
#      width=8, height=6, units="in", res=300)
# funnel(ma_model5)
# dev.off()


## Exploratory analysis  ------------------------------------------------

MetaDF %<>%
  mutate(Hamann = ((A+D) - (B+C)) / num_item4gamma,
         pH = ifelse(A+C == 0, 0.5/num_item4gamma, 
                     ifelse(A == 0, 0.5/(A+C), A/(A+C))),
         pFA = ifelse(B+D == 0, 0.5/num_item4gamma, 
                      ifelse(B == 0, 0.5/(B+D), B/(B+D))),
         d = ifelse(pH == 1 & pFA == 1, qnorm(((A+C)-0.5)/(A+C)) - qnorm(((B+D)-0.5)/(B+D)),
                    ifelse(pH == 1, qnorm(((A+C)-0.5)/(A+C)) - qnorm(pFA),
                           ifelse(pFA == 1, qnorm(pH) - qnorm(((B+D)-0.5)/(B+D)),
                                  qnorm(pH) - qnorm(pFA)))))
                           
# Prepare data for meta-analysis
Meta_mean_hamann <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Hamann", mean)

Meta_sd_hamann <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Hamann", sd)

Meta_mean_d <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "d", mean)

Meta_sd_d <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "d", sd)


# Meta model Hamman
# Calculate effet size and their variance
eff_size_hamann <- data.frame()
var_hamann <- data.frame()

for (refs in 1:nrow(Meta_pp)){
  
  effect_size1 <- esc_mean_sd(grp1m = Meta_mean_hamann$Young[refs], 
                              grp1sd = Meta_sd_hamann$Young[refs], 
                              grp1n = Meta_pp$Young[refs],
                              grp2m = Meta_mean_hamann$Old[refs], 
                              grp2sd = Meta_sd_hamann$Old[refs], 
                              grp2n = Meta_pp$Old[refs], 
                              es.type = "g")
  
  g_hedge <- effect_size1[1]
  sampling_var <- effect_size1[3] 
  eff_size_hamann %<>% rbind(g_hedge)
  var_hamann %<>% rbind(sampling_var) 
}

# add references column 
Effect_hamann <- eff_size_hamann %>% 
  cbind(var1)
Effect_hamann %<>%
  cbind(refs)

# Create model
ma_model_hamann <- rma(es, var, data = Effect_hamann)
summary(ma_model_hamann)

# Forest plot
jpeg(file="./figures/forest_meta_hamann.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model_hamann, slab = Meta_mean_hamann$Paper)
dev.off()

# Funnel plot 
jpeg(file="./figures/funnel_meta_hamann.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model_hamann)
dev.off()


# Meta model d'
# Calculate effet size and their variance
eff_size_d <- data.frame()
var_d <- data.frame()

for (refs in 1:nrow(Meta_pp)){
  
  effect_size1 <- esc_mean_sd(grp1m = Meta_mean_d$Young[refs], 
                              grp1sd = Meta_sd_d$Young[refs], 
                              grp1n = Meta_pp$Young[refs],
                              grp2m = Meta_mean_d$Old[refs], 
                              grp2sd = Meta_sd_d$Old[refs], 
                              grp2n = Meta_pp$Old[refs], 
                              es.type = "g")
  
  g_hedge <- effect_size1[1]
  sampling_var <- effect_size1[3] 
  eff_size_d %<>% rbind(g_hedge)
  var_d %<>% rbind(sampling_var) 
}

# add references column 
Effect_d <- eff_size_d %>% 
  cbind(var1)
Effect_d %<>%
  cbind(refs)

# Create model
ma_model_d <- rma(es, var, data = Effect_d)
summary(ma_model_d)

# Forest plot
jpeg(file="./figures/forest_meta_d.jpeg",
     width=8, height=4.5, units="in", res=300)
forest(ma_model_d, slab = Meta_mean_d$Paper)
dev.off()

# Funnel plot 
jpeg(file="./figures/funnel_meta_d.jpeg",
     width=8, height=6, units="in", res=300)
funnel(ma_model_d)
dev.off()

