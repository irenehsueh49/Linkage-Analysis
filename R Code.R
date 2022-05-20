---
title: "Irene Hsueh's BS 858 Homework 2"
author: "Irene Hsueh"
date: "9/27/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

# Analyzing Sibling Pairs
```{r}
#Reading in CSV File
siblings <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 858 - Statistical Genetics I/Class 2 - Linkage Analysis/Homework 2/homework2_siblings.csv") %>% #Reordering and Renaming Variables
  dplyr::select(family_id = famid, 
                child1_id = id1,
                child2_id = id2,
                ibd_alleles = ibd,
                child1_trait = trait1,
                child2_trait = trait2, 
                child1_adjusted = resid1, 
                child2_adjusted = resid2,
                child1_disease = disease1, 
                child2_disease = disease2) %>%
#Reassign Values to Binary Variables
  mutate_at(c("child1_disease", "child2_disease"), 
            funs(dplyr::recode(., "2"="Affected", .default="Unaffected"))) 
head(siblings, 20)
```



# Goodness of Fit Test
```{r}
siblings_disease <- siblings[siblings$child1_disease=="Affected" & siblings$child2_disease=="Affected",]
head(siblings_disease, 20)

n_ibd <- table(siblings_disease$ibd_alleles)



goodness_of_fit <- chisq.test(n_ibd, p=c(0.25, 0.5, 0.25))
p_value_gt <- (goodness_of_fit$p.value)/2
lod_score_gt <- goodness_of_fit$statistic/(2*log(10))
```



# Haseman-Elston Approach
```{r}
siblings$trait_difference <- (siblings$child1_adjusted - siblings$child2_adjusted)^2
siblings$pi <- siblings$ibd_alleles/2
hea <- lm(trait_difference ~ pi, data=siblings)
summary(hea)

t_statistic_hea <- summary(hea)$coefficients[2,3]
p_value_hea <- pt(t_statistic_hea, hea$df, lower=TRUE)
lod_score_hea <- t_statistic_hea^2 / (2*log(10))

head(siblings, 20)
```



# Haseman-Elston Revisted
```{r}
population_mean <-mean(c(siblings$child1_adjusted, siblings$child2_adjusted))
siblings$mean_adjusted_product <- (siblings$child1_adjusted - population_mean) * (siblings$child2_adjusted - population_mean)
her <- lm(mean_adjusted_product ~ pi, data=siblings)
summary(her)

t_statistic_her <- summary(her)$coefficients[2,3]
p_value_her <- pt(t_statistic_her, her$df, lower=FALSE)
lod_score_her <- t_statistic_her^2 / (2*log(10))

head(siblings, 20)
```










