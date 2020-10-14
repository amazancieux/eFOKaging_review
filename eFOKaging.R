
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
  dcast(Paper + recog_type ~ Group, value.var = "Gamma", mean)

Meta_sd <- MetaDF %>% 
  dcast(Paper + recog_type ~ Group, value.var = "Gamma", sd)

Meta_pp <- MetaDF %>% 
  dcast(Paper + recog_type ~ Group, value.var = "Gamma")


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


## Use recall as a covriate to control for memory performance --------------------------------------

crlt_perf <- data.frame()


## Match sample according to memory performance  --------------------------------------

# Calculate median for each group
# recall <- MetaDF %>% 
#   group_by(Paper, Group) %>% 
#   summarise(median = median(Recall))
# 
# match <- MetaDF
# study <- unique(MetaDF$Paper)
# group <- unique(MetaDF$Group)
# 
# match <- MetaDF %>% 
#   mutate(Pp = seq(1:nrow(MetaDF)) %>% 
#   dcast(Paper + Pp ~ Group, value.var = Gamma)
# 
# 
# if (match$Paper == i & match$Group == g){
#   match %<>%
#     mutate()
# }
# 
# match <- MetaDF %>% 
#   mutate(Median = case_when()
# 
# for (i in study) {
#   
#   for (g in group){
#     
#     match %<>%
#       filter(if (Paper == i & Group == g) Recall <= recall$median[recall$Paper == i & recall$Group == g])
#   }
# }  
# 
# match %<>%
#   mutate(
#   filter(if (Paper == i & Group == g) Recall <= recall$median[recall$Paper == i & recall$Group == g])
#   
# median <- 
#   
# match_perf <- MetaDF %>% 
#   mutate(median = case_when(
    

