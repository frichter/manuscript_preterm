# Felix Richter
# 11/27/2020
# Running DID and/or RDD analyses for varying
# gestational age cut-offs
######################################################

setwd("/Users/felixrichter/Dropbox/PhD/")
options(stringsAsFactors=FALSE)

# Load packages
p = c("magrittr", "stringi", "purrr", 'broom',
      "dplyr", "ggplot2", "tidyr", "readr")
lapply(p, require, character.only = TRUE)

data_dir = "/Users/felixrichter/Volumes/mscic_vm/identified_data/2020_11_25/"

# test_connection = read_tsv(paste0(data_dir, "did_df_2020_11_25.tsv"))
did_df = read_tsv(paste0(data_dir, "did_df_2020_11_25.tsv"))

##########################################
# DID analysis function
##########################################

run_did = function(analysis_df) {
  # add the DID/interaction term
  analysis_df$did = analysis_df$born2020 * analysis_df$post_cut_off
  # fit = lm(pct_outcome ~ born2020 + post_cut_off + did + refyear + time_bin, analysis_df)
  fit = glm(outcome ~ born2020 + post_cut_off + did, analysis_df, family = 'binomial')
  ##  + refyear + cut_off_diff_col
  # fit = lme4::lmer(pct_outcome ~ born2020 + post_cut_off + did + (1|refyear), analysis_df)
  # fit = nlme::lme(pct_outcome ~ born2020 + post_cut_off + did + (1|refyear), analysis_df)
  # print(anova(fit))
  fit_df = tidy(fit)
  return(fit_df)
}

##########################################
# DID analysis for gestational age
##########################################

run_did_ga = function(ga_cut_off, time_to_cut_off, date_interval_size, cut_off_diff_col_i, did_df) {
  did_df['cut_off_diff_col'] = did_df[cut_off_diff_col_i]
  did_df['refyear'] = did_df[paste0(cut_off_diff_col_i, '_refyear')]
  time_cut_off_bins = seq(-time_to_cut_off, time_to_cut_off, by=date_interval_size)
  analysis_df = did_df %>%
    filter(!(Borough %in% c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))) %>%
    filter(MOTHER_COVID19_PREG_POS == 0) %>%
    mutate(born2020 = ifelse(refyear == 2020, 1, 0)) %>% 
    ## keep only 2017-2020 to as sensitivity analysis for missing GA
    # filter(refyear > 2016) %>%
    # threshold
    filter(!(GA_category %in% c('Not recorded', 'Late term'))) %>% 
    ## prematurity as outcome
    filter((GA_WKS < ga_cut_off) | (GA_category == 'Term')) %>%
    mutate(outcome = as.numeric(GA_WKS < ga_cut_off)) %>%
    ## NICU (question: keep or exclude the GA N/A babies?)
    # mutate(outcome = ifelse(DEPARTMENT == 'NICU', 1, 0)) %>%
    ## censor times closest to cut off
    # filter(abs(cut_off_diff_col) > 0.2) %>% 
    ## only keep babies born within time_to_cut_off window of cut-off
    filter(abs(cut_off_diff_col) < time_to_cut_off) %>% 
    mutate(post_cut_off = as.numeric(cut_off_diff_col > 0))

  ## get percent of births within each date bin: uncomment for 
  # analysis_df %<>%
  #   # bin dates
  #   mutate(time_bin = cut(cut_off_diff_col, time_cut_off_bins, labels=head(time_cut_off_bins, -1)) %>% 
  #            as.character %>% as.numeric) %>% 
  #   # group by date bins, retaining all predictors in groupings
  #   group_by(refyear, time_bin, born2020, post_cut_off) %>% 
  #   summarize(n_births = n(), 
  #             n_outcome = sum(outcome),
  #             # create other variables that could explain variance
  #             pct_outcome = 100*(n_outcome/n_births)) %>% ungroup
  # sensitivity analysis: excluding points closest to cut-off
  # analysis_df %<>% filter(abs(time_bin) > 0.3)
  
  ## get a sample size dataframe
  # sample_size_df = analysis_df %>% 
  #   group_by()
  
  fit_df = run_did(analysis_df)
  fit_df %<>% 
    mutate(ga_cut_off = ga_cut_off, time_to_cut_off = time_to_cut_off,
           cut_off_group = cut_off_diff_col_i,
           time_bin_size = date_interval_size)
  return(fit_df)
}

ga_vec = 37 # c(28, 30, 32, 34, 37)
time_to_cut_off_vec = c(1, 2, 3, 4)
time_bin_sizes = 0.1 # c(0.025, 0.05, 0.1, 0.2, 0.25, 0.5) ## uncomment to test sensitivity to bin sizes
loop_df = expand.grid('GA_cutoff' = ga_vec, 'mo_cutoff' = time_to_cut_off_vec,
                      'time_bin_sizes' = time_bin_sizes)
did_lock = pmap_df(list(loop_df$GA_cutoff, loop_df$mo_cutoff, loop_df$time_bin_sizes),
                   run_did_ga, 'lockdown_diff_mo', did_df)
did_reop = pmap_df(list(loop_df$GA_cutoff, loop_df$mo_cutoff, loop_df$time_bin_sizes),
                   run_did_ga, 'reopening_diff_mo', did_df)

did_final = bind_rows(did_lock, did_reop) %>%
  select(cut_off_group, time_to_cut_off, term, ga_cut_off, estimate, statistic, p.value,
         time_bin_size)

did_final %>% filter(p.value < 0.05) %>% 
  filter(term != '(Intercept)') %>% 
  as.data.frame

did_final %>% filter(term == 'did') %>% 
  # lockdown_diff_mo reopening_diff_mo
  # filter(cut_off_group == 'reopening_diff_mo') %>%
  filter(ga_cut_off == 37, time_bin_size == 0.1)# %>% ##
  # filter(p.value == min(p.value))
  filter(time_to_cut_off < 4) #%>%
  ## p.value estimate
  select(p.value) %>% unlist %>% signif(2) %>% paste(collapse=',')

##########################################
# DID plot
##########################################

clean_plot_df = function(cut_off_diff_col_name, ga_cut_off, time_to_cut_off, date_interval_size, plot_df) {
  print(cut_off_diff_col_name)
  plot_df['cut_off_diff_col'] = plot_df[cut_off_diff_col_name]
  plot_df['refyear'] = plot_df[paste0(cut_off_diff_col_name, '_refyear')]
  time_cut_off_bins = seq(-time_to_cut_off, time_to_cut_off, by=date_interval_size)
  plot_df %<>% 
    filter((Borough %in% c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))) %>%
    filter(MOTHER_COVID19_PREG_POS == 0) %>%
    mutate(born2020 = ifelse(refyear == 2020, 1, 0)) %>% 
    ## sensitivity analysis: keep only 2017-2020
    filter(refyear > 2016) %>%
    ## GA bins
    filter(!(GA_category %in% c('Not recorded', 'Late term'))) %>% 
    filter((GA_WKS < ga_cut_off) | (GA_category == 'Term')) %>%
    mutate(outcome = as.numeric(GA_WKS < ga_cut_off)) %>%
    ## NICU (question: keep or exclude the GA N/A babies?)
    # mutate(outcome = ifelse(DEPARTMENT == 'NICU', 1, 0)) %>%
    ## censor times closest to cut off
    # filter(abs(cut_off_diff_col) > 0.1) %>% 
    ## only keep times within selected month bandwidth
    filter(abs(cut_off_diff_col) < time_to_cut_off) %>% 
    mutate(post_cut_off = as.numeric(cut_off_diff_col > 0)) %>% 
    mutate(cut_off_group = cut_off_diff_col_name)
  plot_df %<>%
    # bin dates
    # mutate(time_bin = cut(cut_off_diff_col, time_cut_off_bins, labels=head(time_cut_off_bins, -1)) %>% 
    #          as.character %>% as.numeric) %>% 
    # group by date bins, retaining all predictors in groupings
    group_by(born2020, post_cut_off, cut_off_group) %>% ## refyear, time_bin, 
    summarize(n_births = n(), 
              n_outcome = sum(outcome),
              pct_outcome = 100*(n_outcome/n_births)) %>% ungroup
  return(plot_df)
}

time_to_cut_off = 3
ga_cut_off = 37
date_interval_size = 0.1
plot_df = map_df(c('reopening_diff_mo', 'lockdown_diff_mo'), clean_plot_df,
                 ga_cut_off, time_to_cut_off, date_interval_size, did_df)

plot_df %<>% 
  mutate(post_cut_off = factor(ifelse(post_cut_off, 'After (+3 mo.)', 'Before (-3 mo.)'), levels=c('Before (-3 mo.)', 'After (+3 mo.)'))) %>% 
  mutate(cut_off_group = ifelse(grepl('lockdown', cut_off_group), 'Lockdown: 3/16/2020', 'Reopening: 6/8/2020'))
# plot_df

### plotting just the regression discontinuity in 2020
p = plot_df %>% 
  filter(refyear == '2020') %>% 
  ggplot(aes(x=time_bin, y=pct_outcome, group = post_cut_off)) + ## BIRTH_YEAR Year
  # geom_point(size = 2) +
  geom_line(size = 0.5, col='grey30') +
  facet_wrap(~cut_off_group, ncol = 1) +
  geom_smooth(method='lm', formula= y~x, se=TRUE, col='red', alpha = 0.25) +
  # scale_color_manual(values = c('grey40', 'red')) +
  xlab('Time from cut-off (months)') +
  ylab('Percent premature') +
  # ylab('Percent Admitted to NICU') +
  theme_classic()
# p
# did_ga37_3mo.png did_ga37_2mo_wide.png
# did_nicu_3mo_2cols.png did_nicu_2mo.png
# ggsave('nicu_projects/manuscript_preterm/Figures/figures_quasi_exp/did_ga37_2mo.png',
#        # p, width = 5, height = 2)
#        p, width = 3, height = 4.5)

### plotting the DiD logistic regression results
p = plot_df %>% 
  mutate(Year = ifelse(born2020, '2020', '2017-2019')) %>% 
  ggplot(aes(x=post_cut_off, y=pct_outcome, col=Year, group=Year)) + ## BIRTH_YEAR Year
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_discrete(expand=c(0.4, 0)) +
  facet_wrap(~cut_off_group, ncol = 2) +
  scale_color_manual(values = c('grey40', 'red')) +
  xlab('') + 
  ylab('Percent premature') +
  # ylab('Percent Admitted to NICU') +
  theme_classic()
p
# did_ga37_3mo.png did_nicu_3mo.png
ggsave('nicu_projects/manuscript_preterm/Figures/figures_quasi_exp_logistic_regression/did_ga37_3mo_wide_2017plus.png',
       p, width = 5.5, height = 2)
       # p, width = 3.5, height = 3.5)

##########################################
# DID permutation test
# Across all GA and month thresholds
##########################################

ga_vec = 37 #c(28, 30, 32, 34, 37)
time_to_cut_off_vec = c(1, 2, 3, 4)
loop_df = expand.grid('GA_cutoff' = ga_vec, 'mo_cutoff' = time_to_cut_off_vec)

# resample gestational age with replacement
PermutateGA = function(perm_i, loop_df, did_df) {
  # print(perm_i)
  if(perm_i %% 5 == 0) {
    print(perm_i)
  }
  did_perm_df = did_df %>% 
    # mutate(GA_WKS = sample(GA_WKS, replace = T))
    # mutate(DEPARTMENT = sample(DEPARTMENT, replace = T))
    ### resample reference year with replacement
    mutate(reopening_diff_mo_refyear = sample(reopening_diff_mo_refyear, replace = T)) %>% 
    mutate(lockdown_diff_mo_refyear = sample(lockdown_diff_mo_refyear, replace = T))
  
  did_lock_perm = map2_df(loop_df$GA_cutoff, loop_df$mo_cutoff, run_did_ga, 0.1, 'lockdown_diff_mo', did_perm_df)
  did_reop_perm = map2_df(loop_df$GA_cutoff, loop_df$mo_cutoff, run_did_ga, 0.1, 'reopening_diff_mo', did_perm_df)
  did_final_perm = bind_rows(did_lock_perm, did_reop_perm) %>% 
    # filter(term == 'did') %>% 
    mutate(perm = perm_i)
  # did_final_perm %>% write_tsv(paste0(sim_dir, 'ga_perm_', perm_i, '.tsv'))
  return(did_final_perm)
}

# resample missing values? No. resample from outside NYC? No
# retain both of these to retain structure in case non-randomly distributed
did_df_clean = did_df %>% 
  filter(MOTHER_COVID19_PREG_POS == 0) %>%
  filter((Borough %in% c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))) %>%
  filter(GA_category != 'Not recorded')

perm_df = map_df(c(1:1e3), PermutateGA, loop_df, did_df_clean)
sim_dir = '/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/perm_results/'
# ga37_perm_allmo.tsv nicu_perm_allmo.tsv
# ga37_perm_allmo_logistricRegression.tsv nicu_perm_allmo_logistricRegression.tsv
# perm_df %>% write_tsv(paste0(sim_dir, 'nicu_perm_allmo_logistricRegression.tsv'))

perm_df = read_tsv(paste0(sim_dir, 'nicu_perm_allmo_logistricRegression.tsv'))

## running in parallele
# library(foreach)
# library(doMC)
# registerDoMC(3)
# final_SV_analysis = foreach(i=1:100,combine=rbind)%dopar%{
#   PermutateGA(i, loop_df, did_df_clean, sim_dir)
# }

p_min_df = did_final %>% filter(term == 'did') %>% 
  filter(ga_cut_off == 37, time_bin_size == 0.1) %>% 
  group_by(cut_off_group) %>% filter(p.value == min(p.value)) %>% 
  ungroup %>% rename(obs_p_min = p.value, obs_stat = statistic) %>%
  select(cut_off_group, obs_p_min, obs_stat)
p_min_df

perm_df %>% 
  # filter(cut_off_group == 'reopening_diff_mo') %>% 
  filter(term == 'did') %>% 
  ## keep only 1-3 months
  filter(time_to_cut_off < 4) %>% 
  # keep if same sign:
  inner_join(p_min_df) %>% 
  # filter(obs_stat*statistic > 0) %>%
  group_by(cut_off_group, perm) %>% filter(p.value == min(p.value)) %>% ungroup %>% 
  filter(p.value <= obs_p_min) %>%
  group_by(cut_off_group) %>% tally

p = perm_df %>% 
  filter(time_to_cut_off < 4) %>% 
  filter(term == 'did') %>% 
  ## reopening_diff_mo, lockdown_diff_mo
  # filter(cut_off_group == 'reopening_diff_mo') %>% 
  inner_join(p_min_df) %>% 
  ## keep if same sign (ie perform a 1-tailed permutation test)
  # filter(obs_stat*statistic > 0) %>%
  group_by(cut_off_group, perm) %>% filter(p.value == min(p.value)) %>% ungroup %>% 
  mutate(log_p = -log10(p.value)) %>%
  mutate(cut_off_group = ifelse(grepl('lockdown', cut_off_group), 'Lockdown: 3/16/2020', 'Reopening: 6/8/2020')) %>% 
  ggplot(aes(x = log_p)) +
  geom_histogram(bins = 45, fill = 'grey40') +
  geom_vline(mapping=aes(xintercept = -log10(obs_p_min)), color = 'red', size = 1) +
  xlab('P-value (-log10)') + ylab('Permutation') +
  facet_wrap(~cut_off_group, ncol = 1) +
  # xlim(0, -log10(0.0000616)) +
  theme_classic()
p
# did_ga37_allmo_perm.png did_nicu_allmo_perm.png
ggsave('nicu_projects/manuscript_preterm/Figures/figures_quasi_exp_logistic_regression/did_nicu_allmo_perm.png',
       p, width = 3, height = 3)

##################################
# Other summary stats
##################################

### how many gestational ages are not recorded by birth year?
did_df %>% 
  filter((Borough %in% c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))) %>% 
  filter(MOTHER_COVID19_PREG_POS == 0) %>% 
  filter(!(GA_category %in% c('Not recorded', 'Late term'))) %>%
  filter((lockdown_diff_mo_refyear == 2020) & (lockdown_diff_mo > 0)) %>% dim
  # filter((reopening_diff_mo_refyear == 2020) & (reopening_diff_mo > 0)) %>% dim
  # group_by(GA_category) %>% tally
  # filter(BIRTH_YEAR <= 2016) %>% 
  # group_by(BIRTH_YEAR > 2016) %>%
  # summarize(n_ga_missing = sum(is.na(GA_WKS)), pct_no_GA = n_ga_missing/n())

p = did_df %>% 
  filter((Borough %in% c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island'))) %>% 
  filter(MOTHER_COVID19_PREG_POS == 0) %>% 
  filter(!(GA_category %in% c('Not recorded', 'Late term'))) %>% 
  filter((lockdown_diff_mo_refyear == 2020) | 
           (reopening_diff_mo_refyear == 2020) ) %>% 
  select(reopening_diff_mo, lockdown_diff_mo) %>% 
  gather(key = 'cut_off_group', value = 'time_to_date') %>% 
  filter(abs(time_to_date) < 3) %>% 
  mutate(cut_off_group = ifelse(grepl('lockdown', cut_off_group), 'Lockdown: 3/16/2020', 'Reopening: 6/8/2020')) %>% 
  ggplot(aes(x = time_to_date)) +
  geom_histogram(bins = 35, fill = 'grey50') +
  # geom_freqpoly(bins = 30, fill = 'grey50') +
  facet_wrap(~cut_off_group) +
  ylab('Births') + xlab('Time from cut-off date (months)') +
  theme_classic()
p
  
