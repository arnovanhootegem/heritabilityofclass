library(data.table)
library(here)
library(tidyverse)
library(purrr)
library(umx)
library(OpenMx)
library(psych)
library(labelled)
library(dplyr)
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

occtwins <- inner_join(twins, occ, by = "w18_0590_lopenr_person")

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
  select(twinpair, twinno, Zygo, foedselsaar, sexcomb, BU50, Inc50, ant_tvilling)

occtwins <- filter(occtwins, foedselsaar > 1940 & foedselsaar < 1981)
occtwins$BU50 <- as.numeric(occtwins$BU50)
occtwins <- mutate(occtwins, gender = ifelse(sexcomb == "Females", 1, 0))

mz <- filter(occtwins, Zygo ==  1)
dz <- filter(occtwins, Zygo ==  2)


# Descriptives for gender, birth yer, income, education and SIOPS for mz, dz and full population -----------------------------


summary(mz)
sd(mz$gender)
sd(mz$foedselsaar)
sd(mz$BU50, na.rm = TRUE)
sd(mz$Inc50, na.rm = TRUE)

summary(dz)
sd(dz$gender)
sd(dz$foedselsaar)
sd(dz$BU50, na.rm = TRUE)
sd(dz$Inc50, na.rm = TRUE)

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

occtwins <- fread("N:/durable/projects/class/01 - Data/Treitwins.csv")

mz <- filter(occtwins, Zygo ==  1)
dz <- filter(occtwins, Zygo ==  2)
summary(mz)
sd(mz$trei50, na.rm = TRUE)
summary(dz)
sd(dz$trei50, na.rm = TRUE)

