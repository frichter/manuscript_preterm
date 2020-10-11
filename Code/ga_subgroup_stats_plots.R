# Felix Richter
# 10/10/2020
# Plotting and statistics for gestational age and birthweight SUBGROUPS
######################################################


setwd("/Users/felixrichter/Dropbox/PhD/")
options(stringsAsFactors=FALSE)

# Load packages
p = c("magrittr", "stringi", "purrr", 'broom',
      "dplyr", "ggplot2", "tidyr", "readr")
lapply(p, require, character.only = TRUE)


#########################
# Data cleaning/loading
#########################

count_df_ga = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/count_df_GA_subgroups.tsv')

ga_order = c("Extremely premature", "Very premature", "Moderate/late premature", "Term", "Late term")
count_df_for_plot = count_df_ga %>%
  # exclude births after 9/21
  filter(!(lockdown_group %in% c('postcovid', 'prioryears_postcovid'))) %>% 
  # calculate rates per 1000 births
  group_by(lockdown_group, BIRTH_YEAR) %>%
  mutate(Births_Total = sum(Births)) %>% 
  mutate(rate_per_1k = 1000*(Births/Births_Total)) %>% 
  ungroup %>% 
  # label if 2020 vs 2012-2019 and set as factor variable
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019')) %>% 
  mutate(GA_category = factor(GA_category, levels=ga_order)) 

# 3 classes: lockdown (3/16-6/7), reopening (6/8-9/21), precovid (1/1-3/15)
# only plotting analyzing lockdown/reopening because otherwise too messy
# but looked at precovid for internal validation of results

###################
# GA plotting
###################

## boxplots
p = count_df_for_plot %>% 
  ## reopening lockdown precovid
  filter(grepl('reopening', lockdown_group)) %>% 
  filter(GA_category != 'Late term') %>% 
  ggplot(aes(x = GA_category, y= rate_per_1k, color=Year)) +
  geom_boxplot(position = 'identity', size = 0.85) +
  facet_wrap(~GA_category, scales='free', nrow=1) +
  ylab('Birth rate\n(per 1000 live singleton births)') + xlab('') +
  scale_color_manual(values = c('grey30', 'red')) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
p
## GA_reopening.png GA_lockdown.png GA_precovid.png
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/subgoup_figures/GA_lockdown.png', p,
       width = 8, height = 3)

# line plots
p = count_df_for_plot %>% 
  mutate(lockdown_group = gsub('prioryears_', '', lockdown_group) %>% 
           gsub('lockdown', 'Lockdown (3/16-6/7)', .) %>% 
           gsub('reopening', 'Reopening (6/8-9/21)', .)) %>% 
  filter(lockdown_group != 'precovid') %>% 
  rename(`Comparison period` = lockdown_group) %>% 
  ggplot(aes(x = BIRTH_YEAR, y= rate_per_1k, color = `Comparison period`)) +
  geom_line() +
  facet_wrap(~GA_category, scales='free', ncol=1) +
  # geom_hline(aes(yintercept=median_by_yr, color=`Median rate`)) +
  ylab('Birth rate\n(per 1000 live singleton births)') + xlab('Year') +
  scale_color_manual(values = c('dodgerblue', 'orange')) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/subgoup_figures/GA_lineplot_SUBGROUPS.png',
       p, width = 6, height = 10)

########################################
# Calculate chi-squared P-value and 
# percent change compared to baseline
########################################

## overall chi-squared p-value
count_df_for_plot %>%
  filter(grepl('reopening', lockdown_group)) %>% # lockdown reopening precovid
  group_by(GA_category, Year) %>%
  # Births total should actually be total term babies
  summarise(Births = sum(Births)) %>%
  ungroup %>%
  spread(key = Year, value = Births) %>%
  filter(GA_category != 'Late term') %>%
  select(-GA_category) %>%
  chisq.test

## get medians and percent change
count_df_for_plot %>% 
  # filter(BIRTH_YEAR %in% c(2020, 2019)) %>%
  filter(GA_category != 'Late term') %>% 
  filter(grepl('reopening', lockdown_group)) %>% # lockdown reopening precovid
  group_by(GA_category, Year) %>% 
  summarise(Rate_median = median(rate_per_1k)) %>% 
  ungroup %>% 
  spread(key = Year, value = Rate_median) %>% 
  mutate(pct_change = 100*(`2020`/`2012-2019`) - 100)



#####################
# Poisson regression
#####################

# example of using offset: https://rpubs.com/kaz_yos/poisson
ga_vec = c("Extremely premature", "Very premature", "Moderate/late premature", "Term", "Late term")

poisson_regression_ga = function(ga_i, count_df_for_plot) {
  print(ga_i)
  count_df_i = count_df_for_plot %>%
    filter(GA_category == ga_i) %>% 
    mutate(lockdown_group = gsub('prioryears_', '', lockdown_group)) %>%
    filter(BIRTH_YEAR != 2020) %>%
    filter(grepl('lockdown|reopening', lockdown_group))
  # count_df_i
  fit = glm(Births ~ BIRTH_YEAR + lockdown_group,
            offset = log(Births_Total),
            family = poisson(link = "log"),
            data = count_df_i)
  fit_summary = tidy(fit) %>% 
    filter(term != '(Intercept)') %>% 
    mutate(ga_group = ga_i) %>% 
    select(ga_group, everything())
  return(fit_summary)
}

# poisson regressions per year for lockdown and reopening period (run separately for each GA category)
poisson_df = map_df(ga_vec, poisson_regression_ga, count_df_for_plot)

poisson_df

poisson_df %>% filter(term== 'BIRTH_YEAR')
