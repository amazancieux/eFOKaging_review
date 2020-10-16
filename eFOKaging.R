
################################

# Script for the meta-analysis of the folowing article: 
# Metacognition in the aging brain: are episodic FOK really impaired?

# Audrey Mazancieux 2020

################################


## Packages and plot theme -----------------------------------------------

library(tidyverse)
library(magrittr)
library(reshape2)
library(metafor)


## Import and arrange data  -------------------------

MetaDF <-read.csv("Data_for_meta.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# Exclude read/gen expe
MetaDF %<>%
  filter(Paper != "read/gen")

# Calculate corrected gamma
MetaDF %<>%
  mutate(A_c = ifelse(A == 0, 0.5, A),
         B_c = ifelse(B == 0, 0.5, B),
         C_c = ifelse(C == 0, 0.5, C),
         D_c = ifelse(D == 0, 0.5, D),
         Gamma = ((A_c*D_c - B_c*C_c) / (A_c*D_c + B_c*C_c)),
         Group = ifelse(Group == 1, "Young", "Old"))

# Prepare data for meta-analysis
Meta_mean <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp <- MetaDF %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


## Run first meta-analysis using metafor  -------------------------

# Calculate effet size and their variance
Effect <- escalc(n1i = Meta_pp$Young, n2i = Meta_pp$Old, m1i = Meta_mean$Young, m2i = Meta_mean$Old, 
                 sd1i = Meta_sd$Young, sd2i = Meta_sd$Old, measure = "SMD", 
                 append = TRUE)

# Create model
ma_model <- rma(yi, vi, data = Effect)
summary(ma_model)

# Forest plot
forest(ma_model, slab = Meta_mean$Paper)

# Funnel plot 
funnel(ma_model)


## Match sample according to memory performance  --------------------------------------

# Calculate median for each group
recall <- MetaDF %>%
  group_by(Paper, Group) %>%
  summarise(median = median(Recall))

# Prepare data for median list 
Meta_pp %<>%
  select(-recog_type) %>% 
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

# Prepare data for meta-analysis #2
Meta_mean2 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma", mean)

Meta_sd2 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma", sd)

Meta_pp2 <- match %>% 
  dcast(Paper ~ Group, value.var = "Gamma")


# Calculate effet size and their variance #2
Effect2 <- escalc(n1i = Meta_pp2$Young, n2i = Meta_pp2$Old, m1i = Meta_mean2$Young, m2i = Meta_mean2$Old, 
                 sd1i = Meta_sd2$Young, sd2i = Meta_sd2$Old, measure = "SMD", 
                 append = TRUE)

# Create model #2
ma_model2 <- rma(yi, vi, data = Effect2)
summary(ma_model2)

# Forest plot #2
forest(ma_model2, slab = Meta_mean2$Paper)

# Funnel plot #2 
funnel(ma_model2)



## Use recall as a covriate to control for memory performance --------------------------------------

crlt_perf <- data.frame()

