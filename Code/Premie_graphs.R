## Shoshana Rosenzweig 9/16/20
##### NICU graphs ####

library(ggplot2)
library(dplyr)
library(car)

#download data
premie_raw1 <- read.table(file = "birth_df_2020_09_01.tsv", header = TRUE, sep="\t")

summary(premie_raw1$lockdown_group)

#exploring data
EP <- premie_raw1
EP <- EP[!(is.na(EP$BW)),]

table(EP$BIRTH_MONTH)

EPtotal <- EP %>%
  group_by(BIRTH_MONTH, lockdown_group, BIRTH_YEAR) %>%
     summarise(count=n())

##total births over time##
EP$BIRTH_MONTH <- as.factor(EP$BIRTH_MONTH)
summary(EPbirths$BIRTH_MONTH)

#get the month and year in one cell
EP$monyear <- paste(EP$BIRTH_MONTH,"-",EP$BIRTH_YEAR, sep = "")

#remove groups not needed
EPbirths <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
EPbirths <- EPbirths[!((EPbirths$lockdown_group == 'reopening')),]
EPbirths <- EPbirths[!((EPbirths$lockdown_group == 'precovid')),]
summary(EPbirths$lockdown_group)
summary(EPbirths$BIRTH_MONTH)

#get counts
EPbirth_count <- EPbirths %>%
  group_by(BIRTH_YEAR) %>%
  count()

p1 <- ggplot(data=EPbirth_count, aes(x=BIRTH_YEAR, y=n, group =1)) +
  geom_line(size = 1.5) + labs(x = "Year", y = "Total Births", 
                     title = "Total births during lockdown period (3/16-8/31)") +
  scale_x_continuous(breaks = EPbirth_count$BIRTH_YEAR, labels = EPbirth_count$BIRTH_YEAR) +
  scale_y_continuous(limits = c(800,1800), n.breaks = 10)

p1 + theme_classic(base_size = 20) +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "Tot_Births.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",
  scale = 1,width = 4,height = 4,units = c("in", "cm", "mm"),dpi = 300, limitsize = TRUE)

##trends in NICU admissions##
summary(EP$DEPARTMENT)
EP <- EP[!(is.na(EP$BW)),]

EPnicu <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
EPnicu <- EPnicu[!((EPnicu$lockdown_group == 'reopening')),]
EPnicu <- EPnicu[!((EPnicu$lockdown_group == 'precovid')),]
summary(EPnicu$lockdown_group)
summary(EPnicu$DEPARTMENT)
EPnicu$BIRTH_YEAR <- as.factor(EPnicu$BIRTH_YEAR)
summary(EPnicu$BIRTH_YEAR)

#get nicu count
EPnicu_count <- EPnicu %>%
  group_by(BIRTH_YEAR, DEPARTMENT) %>%
  count()

#get total count
EP_count <- EPnicu %>%
  group_by(BIRTH_YEAR) %>%
  count()

#merge and get percentage
EPnicu_count <- merge(EPnicu_count, EP_count, by = c("BIRTH_YEAR"), all.x = TRUE)
EPnicu_count$percentage <- (EPnicu_count$n.x/EPnicu_count$n.y)*100

#remove WBN
EPnicu_count <- EPnicu_count[!((EPnicu_count$DEPARTMENT == "WBN")),]

p2 <- ggplot(data=EPnicu_count, aes(x=BIRTH_YEAR, y=percentage, group =1)) +
  geom_line(size = 1.5) + labs(x = "Year", y = "Percentage of Births in NICU", 
                     title = "Percentage of NICU births \n during lockdown period (3/16-8/31)") +
              scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,12), n.breaks = 10)

p2 + theme_classic(base_size = 20) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "NICU_trends.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",
  scale = 1,width = 4,height = 4,units = c("in", "cm", "mm"),dpi = 300, limitsize = TRUE)


##prematurity per 1000 births##
summary(EP$GA_category)
EP <- EP[!(is.na(EP$BW)),]

#remove unwanted groups
EPep <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
EPep <- EPep[!((EPep$lockdown_group == 'reopening')),]
EPep <- EPep[!((EPep$lockdown_group == 'precovid')),]
summary(EPep$lockdown_group)
summary(EPep$GA_category)
EPep$BIRTH_YEAR <- as.factor(EPep$BIRTH_YEAR)
summary(EPep$BIRTH_YEAR)

#get counts
EPep_count <- EPep %>%
  group_by(BIRTH_YEAR, GA_category) %>%
  count()

EPep_count <- EPep_count[EPep_count$GA_category == "Extremely premature",]

EP_count <- EPep %>%
  group_by(BIRTH_YEAR) %>%
  count()

#merge and get percentage
EPep_count <- merge(EPep_count, EP_count, by = c("BIRTH_YEAR"), all.x = TRUE)
EPep_count$percentage <- (EPep_count$n.x/EPep_count$n.y)
EPep_count$per_1000 <- EPep_count$percentage*1000

p3 <- ggplot(data=EPep_count, aes(x=BIRTH_YEAR, y=per_1000, group =1)) +
  geom_line(size = 1.5) + labs(x = "Year", y = "EP rate per 1000 live births", 
                     title = "Rate of EP births per 1000 births \n during lockdown period (3/16-8/31)") +
  scale_y_continuous(limits = c(0,5), n.breaks = 10)

p3 + theme_classic(base_size = 20) + theme(plot.title = element_text(hjust = 0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "Pre-mie_1000.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",
  scale = 1,width = 4,height = 4,units = c("in", "cm", "mm"),dpi = 300, limitsize = TRUE)


##premie by month##
summary(EP$GA_category)
EP <- EP[!(is.na(EP$BW)),]

EPep <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
EPep <- EPep[!((EPep$lockdown_group == 'reopening')),]
EPep <- EPep[!((EPep$lockdown_group == 'precovid')),]
summary(EPep$lockdown_group)
summary(EPep$GA_category)
EPep$BIRTH_YEAR <- as.factor(EPep$BIRTH_YEAR)
summary(EPep$BIRTH_YEAR)

EPep_covid <- EPep[((EPep$lockdown_group == 'lockdown')),]
EPep_precovid <- EPep[!((EPep$lockdown_group == 'lockdown')),]

#covid numbers
EPep_covid_count <- EPep_covid %>%
  group_by(GA_category, BIRTH_MONTH) %>%
  count()

EPep_covid_count <- EPep_covid_count[EPep_covid_count$GA_category == "Extremely premature",]
EPep_covid_count[nrow(EPep_covid_count) + 1,] = list("Extremely premature", as.factor(3), as.double(0))
EPep_covid_count[nrow(EPep_covid_count) + 1,] = list("Extremely premature", as.factor(5), as.double(0))
EPep_covid_count[nrow(EPep_covid_count) + 1,] = list("Extremely premature", as.factor(6), as.double(0))
#get total number
EP_covid_count <- EPep_covid %>%
  group_by(BIRTH_MONTH) %>%
  count()
#merge and get percent
EPep_covid_count <- merge(EPep_covid_count, EP_covid_count, by = c("BIRTH_MONTH"), all.x = TRUE)
EPep_covid_count$percentage <- (EPep_covid_count$n.x/EPep_covid_count$n.y)
EPep_covid_count$per_1000 <- EPep_covid_count$percentage*1000
EPep_covid_count$group <- "covid"
#get average
hline <- mean(EPep_covid_count$per_1000)
EPep_covid_count$hline <- hline

#precovid numbers
EPep_precovid_count <- EPep_precovid %>%
  group_by(GA_category, BIRTH_MONTH) %>%
  count()
EPep_precovid_count <- EPep_precovid_count[EPep_precovid_count$GA_category == "Extremely premature",]

#get total number
EP_precovid_count <- EPep_precovid %>%
  group_by(BIRTH_MONTH) %>%
  count()
#merge and get percent
EPep_precovid_count <- merge(EPep_precovid_count, EP_precovid_count, by = c("BIRTH_MONTH"), all.x = TRUE)
EPep_precovid_count$percentage <- (EPep_precovid_count$n.x/EPep_precovid_count$n.y)
EPep_precovid_count$per_1000 <- EPep_precovid_count$percentage*1000
EPep_precovid_count$group <- "precovid"
#get average
hline <- mean(EPep_precovid_count$per_1000)
EPep_precovid_count$hline <- hline

#merge into one dataframe
EPep_analysis <- rbind(EPep_precovid_count, EPep_covid_count)

p4 <- ggplot(data=EPep_analysis, aes(x=BIRTH_MONTH, y=per_1000, group =group)) +
  geom_line(aes(color=group), size = 1.5) + labs(x = "Month", y = "EP rate per 1000 live births", 
                     title = "Rate of EP births per 1000 births \n during lockdown period (3/16-8/31)") +
  scale_y_continuous(limits = c(0,6), n.breaks = 12) +
  scale_x_discrete(labels = c("March", "April", "May", "June")) +
  scale_color_discrete(name = "Years", breaks = c("covid", "precovid"),labels=c("2020","2012-2019"))  + 
  geom_hline(aes(yintercept=hline, col = group), linetype="dashed", size = 1.5)

p4 + theme_classic(base_size = 20) + theme(plot.title = element_text(hjust = 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  "EP.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",scale = 1,width = 4,height = 4,units = c("in", "cm", "mm"),dpi = 300,
  limitsize = TRUE)

##prematurity per 1000 births##
summary(EP$BW_category)
EP <- EP[!(is.na(EP$BW)),]

ELBW <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
ELBW <- ELBW[!((ELBW$lockdown_group == 'reopening')),]
ELBW <- ELBW[!((ELBW$lockdown_group == 'precovid')),]
summary(ELBW$lockdown_group)
summary(ELBW$BW_category)
ELBW$BIRTH_YEAR <- as.factor(ELBW$BIRTH_YEAR)
summary(ELBW$BIRTH_YEAR)

ELBW_elbw_count <- ELBW %>%
  group_by(BIRTH_YEAR, BW_category) %>%
  count()

ELBW_elbw_count <- ELBW_elbw_count[ELBW_elbw_count$BW_category == "ELBW",]

ELBW_count <- ELBW %>%
  group_by(BIRTH_YEAR) %>%
  count()

ELBW_count <- merge(ELBW_elbw_count, ELBW_count, by = c("BIRTH_YEAR"), all.x = TRUE)
ELBW_count$percentage <- (ELBW_count$n.x/ELBW_count$n.y)
ELBW_count$per_1000 <- ELBW_count$percentage*1000


p5 <- ggplot(data=ELBW_count, aes(x=BIRTH_YEAR, y=per_1000, group =1)) +
  geom_line(size = 1.5) + labs(x = "Year", y = "ELBW rate per 1000 live births", 
                     title = "Rate of ELBW births per 1000 births \n during lockdown period (3/16-8/31)") +
  scale_y_continuous(limits = c(0,6.5), n.breaks = 12)

p5 + theme_classic(base_size = 20) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "ELBW_1000.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",scale = 1,width = NA,height = NA,units = c("in", "cm", "mm"),dpi = 300,
  limitsize = TRUE)

##premie by month##
summary(ELBW$BW_category)
EP <- EP[!(is.na(EP$BW)),]

ELBW <- EP[!((EP$lockdown_group == 'precovid_reopening_period')),]
ELBW <- ELBW[!((ELBW$lockdown_group == 'reopening')),]
ELBW <- ELBW[!((ELBW$lockdown_group == 'precovid')),]
summary(ELBW$lockdown_group)
summary(ELBW$BW_category)
ELBW$BIRTH_MONTH <- as.factor(ELBW$BIRTH_MONTH)
summary(ELBW$BIRTH_YEAR)

ELBW_covid <- ELBW[((ELBW$lockdown_group == 'lockdown')),]
ELBW_precovid <- ELBW[!((ELBW$lockdown_group == 'lockdown')),]

#covid numbers
ELBW_covid_count <- ELBW_covid %>%
  group_by(BW_category, BIRTH_MONTH) %>%
  count()

ELBW_covid_count <- ELBW_covid_count[ELBW_covid_count$BW_category == "ELBW",]
ELBW_covid_count[nrow(ELBW_covid_count) + 1,] = list("ELBW", as.factor(3), as.double(0))
ELBW_covid_count[nrow(ELBW_covid_count) + 1,] = list("ELBW", as.factor(5), as.double(0))
ELBW_covid_count[nrow(ELBW_covid_count) + 1,] = list("ELBW", as.factor(6), as.double(0))
#get total number
ELBW_covid_total <- ELBW_covid %>%
  group_by(BIRTH_MONTH) %>%
  count()
#merge and get percent
ELBW_covid_count <- merge(ELBW_covid_count, ELBW_covid_total, by = c("BIRTH_MONTH"), all.x = TRUE)
ELBW_covid_count$percentage <- (ELBW_covid_count$n.x/ELBW_covid_count$n.y)
ELBW_covid_count$per_1000 <- ELBW_covid_count$percentage*1000
ELBW_covid_count$group <- "covid"
hline <- mean(ELBW_covid_count$per_1000)
ELBW_covid_count$hline <- hline

#precovid numbers
ELBW_precovid_count <- ELBW_precovid %>%
  group_by(BW_category, BIRTH_MONTH) %>%
  count()
ELBW_precovid_count <- ELBW_precovid_count[ELBW_precovid_count$BW_category == "ELBW",]

#get total number
ELBW_precovid_total <- ELBW_precovid %>%
  group_by(BIRTH_MONTH) %>%
  count()
#merge and get percent
ELBW_precovid_count <- merge(ELBW_precovid_count, ELBW_precovid_total, by = c("BIRTH_MONTH"), all.x = TRUE)
ELBW_precovid_count$percentage <- (ELBW_precovid_count$n.x/ELBW_precovid_count$n.y)
ELBW_precovid_count$per_1000 <- ELBW_precovid_count$percentage*1000
ELBW_precovid_count$group <- "precovid"
#get the average
hline <- mean(ELBW_precovid_count$per_1000)
ELBW_precovid_count$hline <- hline

#merge into one dataframe
ELBW_analysis <- rbind(ELBW_precovid_count, ELBW_covid_count)

p6 <- ggplot(data=ELBW_analysis, aes(x=BIRTH_MONTH, y=per_1000, group =group)) +
  geom_line(aes(color=group), size = 1.5) + labs(x = "Month", y = "ELBW rate per 1000 live births", 
                                     title = "Rate of ELBW births per 1000 births \n during lockdown period (3/16-8/31)") +
  scale_y_continuous(limits = c(0,5.5), n.breaks = 12) +
  scale_x_discrete(labels = c("March", "April", "May", "June")) +
  scale_color_discrete(name = "Years", breaks = c("covid", "precovid"),labels=c("2020","2012-2019"))  + 
  geom_hline(aes(yintercept=hline, col = group), linetype="dashed", size = 1.5)

p6 + theme_classic(base_size = 20) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "ELBW.png", plot = last_plot(), device = png(), path = "/Users/shoshanarosenzweig/Documents/Sinai/Research/GlicksbergLab/Pre-mie/",scale = 1,width = NA,height = NA,units = c("in", "cm", "mm"),dpi = 300,
  limitsize = TRUE)
dev.off()
