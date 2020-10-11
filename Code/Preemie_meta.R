######################################################

## Load packages
p = c("magrittr", "extraDistr", "meta", "metafor", "purrr", "dplyr", "ggplot2", "tidyr", "readr")
lapply(p, require, character.only = TRUE)

## Create dataframe of findings

##EP

#Danish
#number of EP births 2015-2019
EPbirths15_19<-13+9+9+13+13
#number of non EP births 2020
5161-1
#number of non EP births 2015-2019
totalBirths15_19<-4841+5331+5294+5398+5154
totalBirths15_19-EPbirths15_19

ep_df = bind_rows(
  # "MSH" =c( "Case" = 5, "Case_total" = 83, "Ctrl" = 3184, "Ctrl_total" = 24447),
  "MSH" =c( "Case" = 5, "Case_total" = 92, "Ctrl" = 3598, "Ctrl_total" = 29145),
  "Hedermann, et. al" = c( "Case" = 1, "Case_total" = 57, "Ctrl" = 5161, "Ctrl_total" = 25961),
  .id = 'Study') %>% 
  mutate(Case_unaffected = Case_total - Case,
         Ctrl_unaffected = Ctrl_total - Ctrl) %>% 
  as.data.frame

##ELBW

#irish
#annual ELBW 15+6+16+20+10+16+12+12+13+17+11+17+13+13+12+10+16+11+15
#ELBW January-April 2001->2019
4+2+5+7+3+4+6+4+6+6+4+6+2+3+4+4+7+3+8
#TotalBirths
totalBirths01_19=1337+1428+1498+1458+1447+1539+1704+1818+1803+1676+1655+1580+1482+1565+1483+1406+1464+1310
totalBirths01_19 - 88

1381+29324
#sinai
61127+213

elbw_df = bind_rows(
  # "MSH" =c( "Case" = 5, "Case_total" = 104, "Ctrl" = 3211, "Ctrl_total" = 26085),
  # "Irish" = c( "Case" = 0, "Case_total" = 88, "Ctrl" = 1381, "Ctrl_total" = 27565),
  "MSH" =c( "Case" = 13, "Case_total" = 241, "Ctrl" = 3590, "Ctrl_total" = 28996),
  "Philip, et. al" = c( "Case" = 3, "Case_total" = 240, "Ctrl" = 1381-3, "Ctrl_total" = 29324-240),
  .id = 'Study') %>% 
  mutate(Case_unaffected = Case_total - Case,
         Ctrl_unaffected = Ctrl_total - Ctrl) %>% 
  as.data.frame

## Confirm it looks right
elbw_df
ep_df

ep_df %>% 
  rename(n_cases_muts = Case_total, n_ctrl_muts = Ctrl_total) %>% 
  rowwise() %>% 
  mutate(
    fisher.p = fisher.test(cbind("Case" = c(Case, n_cases_muts - Case),
                                 "Ctrl" = c(Ctrl, n_ctrl_muts - Ctrl)))$p.value,
    ci_lo = fisher.test(cbind("Case" = c(Case, n_cases_muts - Case),
                              "Ctrl" = c(Ctrl, n_ctrl_muts - Ctrl)))$conf.int[[1]],
    ci_hi = fisher.test(cbind("Case" = c(Case, n_cases_muts - Case),
                              "Ctrl" = c(Ctrl, n_ctrl_muts - Ctrl)))$conf.int[[2]],
    or = (Case/(n_cases_muts-Case))/(Ctrl/(n_ctrl_muts-Ctrl)),
  ) %>% as.data.frame


##############################################
# fixed and random effects meta-analysis in R
##############################################
# Resource on metabin input formats:
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/binary.html

meta_ep = metabin(Case, Case_total, Ctrl, Ctrl_total,
                  data = ep_df,
                  studlab = paste(Study),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ", ## all tau methods give comparable results
                  prediction = FALSE,
                  sm = "OR")
meta_ep$pval.random
meta_ep
obs_coef_ep = meta_ep$TE.random ## observed log(OR) for downstream null hypothesis tests

meta_elbw = metabin(Case, Case_total, Ctrl, Ctrl_total,
                  data = elbw_df,
                  studlab = paste(Study),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ", ## all tau methods give comparable results
                  prediction = FALSE,
                  sm = "OR")

obs_coef_elbw = meta_elbw$TE.random
meta_elbw
meta_elbw$pval.random
meta_elbw$TE.random

## P=0.0147 if hakn = FALSE, P=0.1216 if hakn = TRUE.
## However, the Random effects OR (2.8494) remains the same

## Plotting meta-analysis results
forest(meta_ep, layout = "JAMA")

forest(meta_elbw, layout="JAMA")
