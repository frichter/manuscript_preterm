# Felix Richter
# 9/18/2020
# - Plotting geographic breakdown, total births, NICU
# admissions and mortality (percent and monthly)
# - Stats for length of stay analysis
######################################################

setwd("/Users/felixrichter/Dropbox/PhD/")
options(stringsAsFactors=FALSE)

# Load packages
p = c("magrittr", "stringi", "purrr", 'broom',
      "dplyr", "ggplot2", "tidyr", "readr")
lapply(p, require, character.only = TRUE)


## plot percent by borough
pct_df_geo = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/pct_df_GEO.tsv')
p = pct_df_geo %>% 
  mutate(Borough = gsub('Outside_NYC', 'Outside NYC', Borough)) %>% 
  mutate(pct_by_borough = count*100) %>% 
  ggplot(aes(x = BIRTH_YEAR, y = pct_by_borough, color = Borough)) +
  geom_line(size = 1) +
  xlab('Birth year') + ylab('Births (%)') +
  theme_classic()
p 
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/original/Borough_rates.png', p,
       width = 4, height = 2)

## Plot rates over time
ct_df_total = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/count_df_yearly.tsv')
p = ct_df_total %>% 
  ggplot(aes(x = factor(BIRTH_YEAR), y = Births, group=1)) +
  geom_line(size = 1) +
  xlab('Birth year\n(Only births between 3/16-9/21)') + ylab('Total births') +
  ylim(3000, 4000) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p 
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/Births_yearly.png', p,
       width = 4, height = 3)

## Plot NICU admissions percent
pct_df_nicu = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/nicu_births.tsv') %>% 
  filter(DEPARTMENT == 'NICU') %>% select(-DEPARTMENT)
pct_df_mortality = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/mortality.tsv') %>% 
  filter(Deceased) %>% select(-Deceased)

plot_df = bind_rows('NICU admissions' = pct_df_nicu,
                    'Mortality' = pct_df_mortality,
                    .id = 'Outcome')

plot_df %>% filter(Outcome == 'Mortality')
p = plot_df %>% 
  mutate(Outcome = factor(Outcome, levels = c('NICU admissions', 'Mortality'))) %>% 
  mutate(births_pct = births_pct*1000) %>% 
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019')) %>% 
  ggplot(aes(x = Outcome, y = births_pct, color = Year)) +
  # geom_line(size = 1) +
  geom_boxplot(position = 'identity') +
  xlab('') +
  ylab('Rate per 1000 live births') +
  scale_color_manual(values = c('grey30', 'red')) +
  facet_wrap(~Outcome, scales = 'free') +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
p 
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/original/nicu_mortality.png',
       p, width = 3.5, height = 2.5)

## Plot monthly rates
monthly_df = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/monthly_mortality_and_nicu_lockdownOnly.tsv')
# add 0s to plot
plot_df = monthly_df %>% spread(key = YEAR_2020, value = births_pct) %>% replace(is.na(.), 0) %>% 
  gather(key = 'Year', value = births_pct, -Outcome, -BIRTH_MONTH)
p = plot_df %>% 
  filter(BIRTH_MONTH <= 10) %>% 
  mutate(Outcome = factor(Outcome, levels = c('NICU admissions', 'Mortality'))) %>% 
  mutate(births_pct = births_pct*1000) %>% 
  ggplot(aes(x = BIRTH_MONTH, y = births_pct, color = Year)) +
  geom_line(size = 1) +
  xlab('Month') +
  ylab('Rate per 1000 live births') +
  scale_color_manual(values = c('grey30', 'red')) +
  facet_wrap(~Outcome, scales = 'free', ncol = 1) +
  theme_classic() #+
  # theme(strip.background = element_blank(), strip.text.x = element_blank())
p 
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/original/monthly_nicu_and_mortality_lockdownOnly.png',
       p, width = 4, height = 3.5)

## length of stay analysis
los_df = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/LOS_days_NICU.tsv')
los_df %<>% mutate(YEAR_2020 = factor(YEAR_2020))
wilcox.test(LOS_days ~ YEAR_2020, los_df)
t.test(LOS_days ~ YEAR_2020, los_df)


