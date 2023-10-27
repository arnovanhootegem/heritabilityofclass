library(data.table)
library(here)
library(tidyverse)
library(purrr)
library(umx)
library(OpenMx)
library(psych)
library(labelled)
library(dplyr)
library(gtsummary)
library(dtplyr)


setwd("N:/durable/projects/class")

regoccimerge <- readRDS("N:/durable/projects/class/01 - Data/regoccimerge_all.rds")


# Save income and education at age 50 -------------------------------------


regoccimerge <- filter(regoccimerge, foedselsaar > 1940 & foedselsaar < 1981)

regoccimerge$age <- regoccimerge$Year - regoccimerge$foedselsaar
regoccimerge <- filter(regoccimerge, age < 63)
regoccimerge$dev <- regoccimerge$age - 50
regoccimerge$dev <- abs(regoccimerge$dev)

regoccimerge <- lazy_dt(regoccimerge)

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, dev) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(BU50 = first(na.omit(BU))) %>% 
  mutate(Inc50 = first(na.omit(Income))) %>% 
  ungroup()

regoccimerge <- as_tibble(regoccimerge)

# Twin data ---------------------------------------------------------------

twins <- fread("N:/durable/prepared/twin-linked-nodups.csv")
nrow(twins)
names(twins)

twins <- select(twins, w18_0590_lopenr_person, Zygo, tvilling_par_ID, kjoenn)
occ <- select(regoccimerge, w18_0590_lopenr_person, foedselsaar, BU50, Inc50)
occ <- distinct(occ)

trei <- readRDS("N:/durable/projects/class/01 - Data/Trei.rds")
trei <- select(trei, w18_0590_lopenr_person, trei50)

occtwin <- left_join(twins, occ, by = "w18_0590_lopenr_person")
occtwins <- left_join(occtwin, trei, by = "w18_0590_lopenr_person")
occtwins <- distinct(occtwins)

occtwins <- occtwins %>%                                     ## twins_utfall hvis vi ikke skal kaste ut dem som er gift 
  mutate(twinpair = floor(tvilling_par_ID/10)) %>% 
  mutate(twinno = tvilling_par_ID-(twinpair*10)) %>%
  arrange(twinpair) %>%
  group_by(twinpair) %>%
  mutate(ant_tvilling = n()) %>% 
  filter(ant_tvilling==2) %>%  #beholder bare der begge tvillinger er representert
  mutate(sexcomb = case_when(var(kjoenn)==0 & kjoenn==1 ~ "Males",
                             var(kjoenn)==0 & kjoenn==2 ~ "Females",
                             TRUE ~ "Mixed")) %>% 
  filter(sexcomb!="Mixed") %>% 
  select(twinpair, twinno, Zygo, foedselsaar, sexcomb, BU50, Inc50, trei50, ant_tvilling)

occtwins <- filter(occtwins, foedselsaar > 1940 & foedselsaar < 1981)
occtwins$BU50 <- as.numeric(occtwins$BU50)
occtwins <- mutate(occtwins, gender = ifelse(sexcomb == "Females", 1, 0))

occtwins <- drop_na(occtwins)

mz <- filter(occtwins, Zygo ==  1)
dz <- filter(occtwins, Zygo ==  2)

# Descriptives for gender, birth yer, income, education and SIOPS for mz, dz and full population -----------------------------


summary(mz)
sd(mz$gender)
sd(mz$foedselsaar)
sd(mz$BU50, na.rm = TRUE)
sd(mz$Inc50, na.rm = TRUE)
sd(mz$trei50, na.rm = TRUE)

summary(dz)
sd(dz$gender)
sd(dz$foedselsaar)
sd(dz$BU50, na.rm = TRUE)
sd(dz$Inc50, na.rm = TRUE)
sd(dz$trei50, na.rm = TRUE)

rec <- regoccimerge %>% distinct(w18_0590_lopenr_person, .keep_all = TRUE)
rec$BU50 <- as.numeric(rec$BU50)
rec <- mutate(rec, gender = ifelse(kjoenn == 2, 1, 0))
summary(rec)
sd(rec$gender)
sd(rec$foedselsaar)
sd(rec$BU50, na.rm = TRUE)
sd(rec$Inc50, na.rm = TRUE)


regoccimerge <- readRDS("N:/durable/projects/class/01 - Data/Trei.rds")
rec <- regoccimerge %>% distinct(w18_0590_lopenr_person, .keep_all = TRUE)
summary(rec)
sd(rec$trei50, na.rm = TRUE)



#### Correlations and percentage in each class for twins

EGPtwins <- fread("N:/durable/projects/class/01 - Data/EGPtwins_wide.csv")
ORDCtwins <- fread("N:/durable/projects/class/01 - Data/ORDCtwins_wide.csv")
Oeschtwins <- fread("N:/durable/projects/class/01 - Data/Oeschtwins_wide.csv")
Treitwins <- fread("N:/durable/projects/class/01 - Data/Treitwins_wide.csv")

#EGP

EGPtwins <- mutate(EGPtwins, EGP4c1 = ifelse((EGP501 == 1 | EGP501 == 2), 1, NA))
EGPtwins <- mutate(EGPtwins, EGP4c1 = ifelse((EGP501 == 3 | EGP501 == 4), 2, EGP4c1))
EGPtwins <- mutate(EGPtwins, EGP4c1 = ifelse((EGP501 == 8 | EGP501 == 9), 3, EGP4c1))
EGPtwins <- mutate(EGPtwins, EGP4c1 = ifelse((EGP501 == 10 | EGP501 == 11), 4, EGP4c1))
EGPtwins$EGP4c1 <- EGPtwins$EGP4c1 - 1


EGPtwins <- mutate(EGPtwins, EGP4c2 = ifelse((EGP502 == 1 | EGP502 == 2), 1, NA))
EGPtwins <- mutate(EGPtwins, EGP4c2 = ifelse((EGP502 == 3 | EGP502 == 4), 2, EGP4c2))
EGPtwins <- mutate(EGPtwins, EGP4c2 = ifelse((EGP502 == 8 | EGP502 == 9), 3, EGP4c2))
EGPtwins <- mutate(EGPtwins, EGP4c2 = ifelse((EGP502 == 10 | EGP502 == 11), 4, EGP4c2))
EGPtwins$EGP4c2 <- EGPtwins$EGP4c2 - 1

EGPtwins <- mutate(EGPtwins, EGPsame = ifelse(EGP4c1 == EGP4c2, 1, 0))

EGPtwins %>% 
  select(EGPsame, Zygo)  %>% 
  tbl_summary(by = "Zygo")

mzEGP <- filter(EGPtwins, Zygo == 1)
dzEGP <- filter(EGPtwins, Zygo == 2)

cor.test(mzEGP$EGP4c1, mzEGP$EGP4c2, method = 'spearman', data = mzEGP, exact = FALSE)
cor.test(dzEGP$EGP4c1, dzEGP$EGP4c2, method = 'spearman', data = dzEGP, exact = FALSE)

#Oesch

Oeschtwins <- mutate(Oeschtwins, Oesch4c1 = ifelse((Oesch501 == 1 | Oesch501 == 5 | Oesch501 == 9 | Oesch501 == 13), 0, NA))
Oeschtwins <- mutate(Oeschtwins, Oesch4c1 = ifelse((Oesch501 == 6 | Oesch501 == 10 | Oesch501 == 14), 1, Oesch4c1))
Oeschtwins <- mutate(Oeschtwins, Oesch4c1 = ifelse((Oesch501 == 7 | Oesch501 == 11 | Oesch501 == 15), 2, Oesch4c1))
Oeschtwins <- mutate(Oeschtwins, Oesch4c1 = ifelse((Oesch501 == 8 | Oesch501 == 12 | Oesch501 == 16), 3, Oesch4c1))

Oeschtwins <- mutate(Oeschtwins, Oesch4c2 = ifelse((Oesch502 == 1 | Oesch502 == 5 | Oesch502 == 9 | Oesch502 == 13), 0, NA))
Oeschtwins <- mutate(Oeschtwins, Oesch4c2 = ifelse((Oesch502 == 6 | Oesch502 == 10 | Oesch502 == 14), 1, Oesch4c2))
Oeschtwins <- mutate(Oeschtwins, Oesch4c2 = ifelse((Oesch502 == 7 | Oesch502 == 11 | Oesch502 == 15), 2, Oesch4c2))
Oeschtwins <- mutate(Oeschtwins, Oesch4c2 = ifelse((Oesch502 == 8 | Oesch502 == 12 | Oesch502 == 16), 3, Oesch4c2))

Oeschtwins <- mutate(Oeschtwins, Oeschsame = ifelse(Oesch4c1 == Oesch4c2, 1, 0))

Oeschtwins %>% 
  select(Oeschsame, Zygo)  %>% 
  tbl_summary(by = "Zygo")

mzOesch <- filter(Oeschtwins, Zygo == 1)
dzOesch <- filter(Oeschtwins, Zygo == 2)

cor.test(mzOesch$Oesch4c1, mzOesch$Oesch4c2, method = 'spearman', data = mzOesch, exact = FALSE)
cor.test(dzOesch$Oesch4c1, dzOesch$Oesch4c2, method = 'spearman', data = dzOesch, exact = FALSE)

# ORDC 

ORDCtwins <- mutate(ORDCtwins, ORDC4c1 = ifelse((ORDC501 < 4), 0, NA))
ORDCtwins <- mutate(ORDCtwins, ORDC4c1 = ifelse((ORDC501 > 3 & ORDC501 < 7), 1, ORDC4c1))
ORDCtwins <- mutate(ORDCtwins, ORDC4c1 = ifelse((ORDC501 > 6 & ORDC501 < 10), 2, ORDC4c1))
ORDCtwins <- mutate(ORDCtwins, ORDC4c1 = ifelse((ORDC501 > 9), 3, ORDC4c1))

ORDCtwins <- mutate(ORDCtwins, ORDC4c2 = ifelse((ORDC502 < 4), 0, NA))
ORDCtwins <- mutate(ORDCtwins, ORDC4c2 = ifelse((ORDC502 > 3 & ORDC502 < 7), 1, ORDC4c2))
ORDCtwins <- mutate(ORDCtwins, ORDC4c2 = ifelse((ORDC502 > 6 & ORDC502 < 10), 2, ORDC4c2))
ORDCtwins <- mutate(ORDCtwins, ORDC4c2 = ifelse((ORDC502 > 9), 3, ORDC4c2))

ORDCtwins <- mutate(ORDCtwins, ORDCsame = ifelse(ORDC4c1 == ORDC4c2, 1, 0))

ORDCtwins %>% 
  select(ORDCsame, Zygo)  %>% 
  tbl_summary(by = "Zygo")

mzORDC <- filter(ORDCtwins, Zygo == 1)
dzORDC <- filter(ORDCtwins, Zygo == 2)

cor.test(mzORDC$ORDC4c1, mzORDC$ORDC4c2, method = 'spearman', data = mzORDC, exact = FALSE)
cor.test(dzORDC$ORDC4c1, dzORDC$ORDC4c2, method = 'spearman', data = dzORDC, exact = FALSE)

# Treiman

Treitwins <- mutate(Treitwins, Treisame = ifelse(trei501 == trei502, 1, 0))

Treitwins %>% 
  select(Treisame, Zygo)  %>% 
  tbl_summary(by = "Zygo")

mztrei <- filter(Treitwins, Zygo == 1)
dztrei <- filter(Treitwins, Zygo == 2)

cor.test(mztrei$trei501, mztrei$trei502, method = 'pearson', data = mztrei, exact = FALSE)
cor.test(dztrei$trei501, dztrei$trei502, method = 'pearson', data = dztrei, exact = FALSE)
