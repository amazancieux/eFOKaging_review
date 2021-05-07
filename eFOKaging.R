
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
# Effect <- escalc(n1i = Meta_pp$Young, n2i = Meta_pp$Old, m1i = Meta_mean$Young, m2i = Meta_mean$Old, 
#                  sd1i = Meta_sd$Young, sd2i = Meta_sd$Old, measure = "SMD", 
#                  append = TRUE)


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
Effect2 <- escalc(n1i = Meta_pp2$Young, n2i = Meta_pp2$Old, m1i = Meta_mean2$Young, m2i = Meta_mean2$Old, 
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


## Meta3: corrected gamma when recall difference is decrease  --------------------------------------

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


# Meta-analysis using the reverse dataset 
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


## Meta4: recall as a covriate to control for memory performance --------------------------------------

crlt_perf <- MetaDF %>% 
  mutate(GroupC = ifelse(Group == "Old", -0.5, 0.5))

reg_value <- data.frame()

# Compute linear model for each study
for (s in study){
  
  # create a dataframe for this study 
  crlt_perf_s <- crlt_perf %>% 
    filter(Paper == s)
  
  # linear model
  reg <- lm(Gamma ~ GroupC + Recall, crlt_perf_s)
  
  # linear regression checks 
  qqnorm(residuals(reg))
  qqline(residuals(reg))
  
  data.frame(x = residuals(reg)) %>% 
    ggplot(aes(x = x)) +
    geom_histogram()
  
  # results
  res <- summary(reg)
  res <- tidy(res)
  res <- as.numeric(res[2,])
  
  # append dataframe
  reg_value %<>% rbind(res) 
}

# Arrange dataframe
colnames(reg_value) <- c("name", "estimate", "std", "t_value", "p_value") 
reg_value %<>%
  select(-name)

# Calculate effet size and their variance #4
# Effect4 <- escalc(n1i = Meta_pp2$Young, n2i = Meta_pp2$Old, m1i = Meta_mean3$Young, m2i = Meta_mean3$Old, 
#                   sd1i = Meta_sd3$Young, sd2i = Meta_sd3$Old, measure = "SMD", 
#                   append = TRUE)

# # Create model #4
# ma_model4 <- rma(yi, vi, data = Effect4)
# summary(ma_model4)
# 
# # Forest plot #4
# forest(ma_model4, slab = Meta_mean4$Paper)
# 
# # Funnel plot #4
# funnel(ma_model4)

