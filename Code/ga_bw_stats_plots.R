# Felix Richter
# 9/30/2020
# Plotting and statistics for gestational age and birthweight
######################################################

setwd("/Users/felixrichter/Dropbox/PhD/")
options(stringsAsFactors=FALSE)

# Load packages
p = c("magrittr", "stringi", "purrr", 'broom',
      "dplyr", "ggplot2", "tidyr", "readr")
lapply(p, require, character.only = TRUE)

###################
# GA analysis
###################

count_df_ga = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/count_df_GA.tsv')

## get counts for Danish meta-anlysis
count_df_ga %>% 
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019')) %>% 
  group_by(Year, GA_category %in% c('Extremely premature')) %>% 
  summarise(Births = sum(Births))

## prepping for plotting
ga_order = c("Extremely premature", "Very premature", "Moderate/late premature", "Term", "Late term")
count_df_for_plot = count_df_ga %>%
  filter(GA_category != 'Not recorded') %>% 
  group_by(BIRTH_YEAR) %>%
  mutate(Births_Total = sum(Births)) %>% 
  mutate(rate_per_1k = 1000*(Births/Births_Total)) %>% 
  ungroup %>% 
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019')) %>% 
  mutate(GA_category = factor(GA_category, levels=ga_order)) 

## get medians and percent change
count_df_for_plot %>% 
  group_by(GA_category, Year) %>% 
  summarise(Rate_median = median(rate_per_1k)) %>% 
  ungroup %>% 
  spread(key = Year, value = Rate_median) %>% 
  mutate(pct_change = 100*(`2020`/`2012-2019`) - 100)

## boxplots
p = count_df_for_plot %>% 
  ggplot(aes(x = GA_category, y= rate_per_1k, color=Year)) +
  geom_boxplot(position = 'identity', size = 0.85) +
  facet_wrap(~GA_category, scales='free', nrow=1) +
  ylab('Birth rate\n(per 1000 live singleton births)') + xlab('') +
  scale_color_manual(values = c('grey30', 'red')) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
p
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/original/GA_rates.png', p,
       width = 10, height = 3)

## Calculate odds ratios and C.I.s
or_df = count_df_for_plot %>% 
  group_by(GA_category, Year) %>%
  # Births total should actually be total term babies
  summarise(Births = sum(Births), BirthsOther = sum(Births_Total) - Births) %>%
  ungroup %>% 
  gather(key = 'Birth_group', value = 'Births', - GA_category, -Year) %>% 
  mutate(comp_group = paste(Birth_group, Year, sep = '_')) %>% 
  select(GA_category, comp_group, Births) %>% 
  spread(key = comp_group, value = Births) %>% 
  rowwise() %>% 
  mutate(
    fisher.p = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                                 "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$p.value,
    ci_lo = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                              "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$conf.int[[1]],
    ci_hi = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                              "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$conf.int[[2]],
    or = (Births_2020/BirthsOther_2020)/(`Births_2012-2019`/`BirthsOther_2012-2019`)) %>% 
  ungroup

or_df
or_df %>% 
  write_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/odds_ratios_GA.txt')

## overall chi-squared p-value
count_df_for_plot %>%
  group_by(GA_category, Year) %>%
  # Births total should actually be total term babies
  summarise(Births = sum(Births)) %>%
  ungroup %>%
  spread(key = Year, value = Births) %>%
  # filter(GA_category != 'Late term') %>%
  select(-GA_category) %>%
  chisq.test


## Plotting the odds ratios
p = or_df %>% 
  ggplot(aes(x = GA_category, y = or, ymax = ci_hi, ymin = ci_lo)) +
  geom_hline(yintercept = 1, color = "blue") +
  geom_pointrange() +
  xlab("GA category") +
  scale_y_continuous(trans = 'log2') +
  theme_classic()
p

###################
# BW analysis
###################

count_df_bw = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/count_df_BW.tsv')

## getting rates for ELBW+VLBW for irish meta-analysis
count_df_bw %>% 
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019')) %>% 
  group_by(Year, BW_category %in% c('VLBW', 'ELBW')) %>% 
  summarise(Births = sum(Births))

count_df_for_plot = count_df_bw %>%
  filter(BW_category != 'Not recorded') %>% 
  group_by(BIRTH_YEAR) %>%
  mutate(Births_Total = sum(Births)) %>% 
  mutate(rate_per_1k = 1000*(Births/Births_Total)) %>% 
  ungroup %>% 
  mutate(Year = ifelse(BIRTH_YEAR == 2020, '2020', '2012-2019'))

## Calculate odds ratios and C.I.s
or_df = count_df_for_plot %>% 
  group_by(BW_category, Year) %>%
  # Births total should actually be total term babies
  summarise(Births = sum(Births), BirthsOther = sum(Births_Total) - Births) %>%
  ungroup %>% 
  gather(key = 'Birth_group', value = 'Births', - BW_category, -Year) %>% 
  mutate(comp_group = paste(Birth_group, Year, sep = '_')) %>% 
  select(BW_category, comp_group, Births) %>% 
  spread(key = comp_group, value = Births) %>% 
  rowwise() %>% 
  mutate(
    fisher.p = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                                 "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$p.value,
    ci_lo = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                              "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$conf.int[[1]],
    ci_hi = fisher.test(cbind("2020" = c(Births_2020, BirthsOther_2020),
                              "2012-2019" = c(`Births_2012-2019`, `BirthsOther_2012-2019`)))$conf.int[[2]],
    or = (Births_2020/BirthsOther_2020)/(`Births_2012-2019`/`BirthsOther_2012-2019`)) %>% 
  ungroup

or_df
## overall chi-squared p-value
count_df_for_plot %>%
  group_by(BW_category, Year) %>%
  # Births total should actually be total term babies
  summarise(Births = sum(Births)) %>%
  ungroup %>%
  spread(key = Year, value = Births) %>%
  # filter(GA_category != 'Late term') %>%
  select(-BW_category) %>%
  chisq.test

########################################
# Plot monthly GA and BW rates
########################################

# monthly_ga.tsv monthly_bw.tsv
monthly_df = read_tsv('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Data/monthly_ga.tsv')
ga_order = c("Extremely premature", "Very premature", "Moderate/late premature", "Term", "Late term")
bw_order = c('ELBW', 'VLBW', 'LBW', 'Normal/High')
p = monthly_df %>% 
  filter(BIRTH_MONTH < 10) %>% 
  mutate(births_pct = births_pct*1000) %>% 
  rename(Year = YEAR_2020) %>% 
  ## if using birthweight
  # mutate(BW_category = BW_category %>% gsub('BW_WNL_OR_HIGH', 'Normal/High', .) %>%
  #          factor(., levels=bw_order)) %>%
  ## if using gestational age
  mutate(GA_category = factor(GA_category, levels=ga_order)) %>%
  ggplot(aes(x = BIRTH_MONTH, y = births_pct, color = Year)) +
  geom_line(size = 1) +
  xlab('Month') +
  ylab('Rate per 1000 live births') +
  scale_color_manual(values = c('grey30', 'red')) +
  # BW_category GA_category
  facet_wrap(~GA_category, scales = 'free', nrow = 1) +
  theme_classic() #+
# theme(strip.background = element_blank(), strip.text.x = element_blank())
p 
# monthly_ga.png monthly_bw.png
ggsave('/Users/felixrichter/Dropbox/PhD/nicu_projects/manuscript_preterm/Figures/original/monthly_ga.png',
       p, width = 10, height = 2)

