################# TWIN ANALYSES CLASS HERITABILITY ##################


library(data.table)
library(here)
library(tidyverse)
library(purrr)
library(umx)
library(OpenMx)
library(psych)
library(labelled)
library(dplyr)

setwd("N:/durable/projects/class")

regoccimerge <- readRDS("N:/durable/users/arnovh/Class/01 - Data/Classes at age 50.rds")


# Preparing twin data ----------------------------------------------------

twins <- fread("N:/durable/data/registers/twin registry/PDB2601_NTR_2023.csv")

twins$w19_0634_lnr <- twins$SSBLNr

twins <- select(twins, w19_0634_lnr, Zygo, ParNr, TvillingNr)
occ <- select(regoccimerge, w19_0634_lnr, foedselsaar, kjoenn, 
              BU50, Inc50, EGP50, devEGP, ORDC50, devORDC, Oesch50, devOesch, 
              trei50, devTrei, ISEI50, devISEI)
occ <- distinct(occ)

Occtwins <- inner_join(twins, occ, by = "w19_0634_lnr")

Occtwins <- Occtwins %>%                                  
  arrange(ParNr) %>%
  group_by(ParNr) %>%
  mutate(ant_tvilling = n()) %>% 
  filter(ant_tvilling==2) %>%  #beholder bare der begge tvillinger er representert
  mutate(sexcomb = case_when(var(kjoenn)==0 & kjoenn==1 ~ "Males",
                             var(kjoenn)==0 & kjoenn==2 ~ "Females",
                             TRUE ~ "Mixed")) %>% 
  filter(sexcomb!="Mixed") %>% 
  select(ParNr, TvillingNr, Zygo, foedselsaar, sexcomb, EGP50, devEGP, ant_tvilling, BU50, Inc50, EGP50, devEGP, ORDC50, devORDC, Oesch50, devOesch, 
         trei50, devTrei, ISEI50, devISEI)

# Create shorter class schemes

# EGP 7 class

Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 1), 0, NA))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 2), 1, EGP7c))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 3), 2, EGP7c))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 4), 3, EGP7c))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 8), 4, EGP7c))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 9), 5, EGP7c))
Occtwins <- mutate(Occtwins, EGP7c = ifelse((EGP50 == 10 | EGP50 == 11), 6, EGP7c))


# EGP 4 class

Occtwins <- mutate(Occtwins, EGP4c = ifelse((EGP50 == 1 | EGP50 == 2), 0, NA))
Occtwins <- mutate(Occtwins, EGP4c = ifelse((EGP50 == 3 | EGP50 == 4), 1, EGP4c))
Occtwins <- mutate(Occtwins, EGP4c = ifelse((EGP50 == 8 | EGP50 == 9), 2, EGP4c))
Occtwins <- mutate(Occtwins, EGP4c = ifelse((EGP50 == 10 | EGP50 == 11), 3, EGP4c))


# Oesch 4 class

Occtwins <- mutate(Occtwins, Oesch4c = ifelse((Oesch50 == 1 | Oesch50 == 5 | Oesch50 == 9 | Oesch50== 13), 0, NA))
Occtwins <- mutate(Occtwins, Oesch4c = ifelse((Oesch50 == 6 | Oesch50 == 10 | Oesch50 == 14), 1, Oesch4c))
Occtwins <- mutate(Occtwins, Oesch4c = ifelse((Oesch50 == 7 | Oesch50 == 11 | Oesch50 == 15), 2, Oesch4c))
Occtwins <- mutate(Occtwins, Oesch4c = ifelse((Oesch50 == 8 | Oesch50 == 12 | Oesch50 == 16), 3, Oesch4c))

#ORDC 4 class

Occtwins <- mutate(Occtwins, ORDC4c = ifelse((ORDC50 < 4), 0, NA))
Occtwins <- mutate(Occtwins, ORDC4c = ifelse((ORDC50 > 3 & ORDC50 < 7), 1, ORDC4c))
Occtwins <- mutate(Occtwins, ORDC4c = ifelse((ORDC50 > 6 & ORDC50 < 10), 2, ORDC4c))
Occtwins <- mutate(Occtwins, ORDC4c = ifelse((ORDC50 > 9), 3, ORDC4c))

# Create twins wide format

Occtwins_wide <- Occtwins %>%
  group_by(ParNr) %>%
  gather(key=var, value=info, EGP50, devEGP, ant_tvilling, BU50, Inc50, EGP50, devEGP, ORDC50, devORDC, Oesch50, devOesch, 
         trei50, devTrei, ISEI50, devISEI, EGP7c, EGP4c, Oesch4c, ORDC4c) %>%
  mutate(var = str_c(var,TvillingNr)) %>%
  select(-TvillingNr) %>%
  spread(key=var, value=info)

write.csv(Occtwins,"N:/durable/users/arnovh/Class/01 - Data/Twins with class.csv", row.names = FALSE)
write.csv(Occtwins_wide,"N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv", row.names = FALSE)

occtwins <- Occtwins_wide
rm(Occtwins_wide, regoccimerge, occ, twins)
occtwins <- data.frame(occtwins)

# Treiman -----------------------------------------------------------------

# Full sample

twinstrei <- select(occtwins,trei501, trei502, devTrei1, devTrei2, Zygo, sexcomb, foedselsaar)

mztrei <- na.omit(subset(twinstrei, Zygo ==  1))
dztrei <- na.omit(subset(twinstrei, Zygo ==  2))
mztrei <- data.frame(mztrei)
dztrei <- data.frame(dztrei)

m1 <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzData = dztrei, mzData = mztrei, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Assorative mating

m1asort <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzAr = 0.6, dzData = dztrei, mzData = mztrei, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzAr = 0.7, dzData = dztrei, mzData = mztrei, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mztreiman <- filter(mztrei, sexcomb == "Males")
dztreiman <- filter(dztrei, sexcomb == "Males")
mztreiwoman <- filter(mztrei, sexcomb == "Females")
dztreiwoman <- filter(dztrei, sexcomb == "Females")

m1man <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzData = dztreiman, mzData = mztreiman, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzData = dztreiwoman, mzData = mztreiwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mztreiung <- filter(mztrei, foedselsaar > 1960)
dztreiung <- filter(dztrei, foedselsaar > 1960)
mztreigaml <- filter(mztrei, foedselsaar < 1961)
dztreigaml <- filter(dztrei, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzData = dztreiung, mzData = mztreiung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("trei50"), selCov = "devTrei", sep = "", dzData = dztreigaml, mzData = mztreigaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# ISEI -----------------------------------------------------------------

# Full sample

twinsISEI <- select(occtwins,ISEI501, ISEI502, devISEI1, devISEI2, Zygo, sexcomb, foedselsaar)

mzISEI <- na.omit(subset(twinsISEI, Zygo ==  1))
dzISEI <- na.omit(subset(twinsISEI, Zygo ==  2))
mzISEI <- data.frame(mzISEI)
dzISEI <- data.frame(dzISEI)

m1 <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzData = dzISEI, mzData = mzISEI, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Assorative mating

m1asort <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzAr = 0.6, dzData = dzISEI, mzData = mzISEI, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzAr = 0.7, dzData = dzISEI, mzData = mzISEI, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzISEIman <- filter(mzISEI, sexcomb == "Males")
dzISEIman <- filter(dzISEI, sexcomb == "Males")
mzISEIwoman <- filter(mzISEI, sexcomb == "Females")
dzISEIwoman <- filter(dzISEI, sexcomb == "Females")

m1man <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzData = dzISEIman, mzData = mzISEIman, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzData = dzISEIwoman, mzData = mzISEIwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzISEIung <- filter(mzISEI, foedselsaar > 1960)
dzISEIung <- filter(dzISEI, foedselsaar > 1960)
mzISEIgaml <- filter(mzISEI, foedselsaar < 1961)
dzISEIgaml <- filter(dzISEI, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzData = dzISEIung, mzData = mzISEIung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("ISEI50"), selCov = "devISEI", sep = "", dzData = dzISEIgaml, mzData = mzISEIgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Education -----------------------------------------------------------------

# Full sample

twinsBU <- select(occtwins,BU501, BU502, Zygo, sexcomb, foedselsaar)

mzBU <- na.omit(subset(twinsBU, Zygo ==  1))
dzBU <- na.omit(subset(twinsBU, Zygo ==  2))
mzBU <- data.frame(mzBU)
dzBU <- data.frame(dzBU)

m1 <- umxACE(selDVs = c("BU50"), sep = "", dzData = dzBU, mzData = mzBU, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Assorative mating

m1asort <- umxACE(selDVs = c("BU50"), sep = "", dzAr = 0.6, dzData = dzBU, mzData = mzBU, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("BU50"), sep = "", dzAr = 0.7, dzData = dzBU, mzData = mzBU, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzBUman <- filter(mzBU, sexcomb == "Males")
dzBUman <- filter(dzBU, sexcomb == "Males")
mzBUwoman <- filter(mzBU, sexcomb == "Females")
dzBUwoman <- filter(dzBU, sexcomb == "Females")

m1man <- umxACE(selDVs = c("BU50"), sep = "", dzData = dzBUman, mzData = mzBUman, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("BU50"), sep = "", dzData = dzBUwoman, mzData = mzBUwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzBUung <- filter(mzBU, foedselsaar > 1960)
dzBUung <- filter(dzBU, foedselsaar > 1960)
mzBUgaml <- filter(mzBU, foedselsaar < 1961)
dzBUgaml <- filter(dzBU, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("BU50"), sep = "", dzData = dzBUung, mzData = mzBUung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("BU50"), sep = "", dzData = dzBUgaml, mzData = mzBUgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Income  -----------------------------------------------------------------

# Full sample

twinsInc <- select(occtwins,Inc501, Inc502, Zygo, sexcomb, foedselsaar)

mzInc <- na.omit(subset(twinsInc, Zygo ==  1))
dzInc <- na.omit(subset(twinsInc, Zygo ==  2))
mzInc <- data.frame(mzInc)
dzInc <- data.frame(dzInc)

m1 <- umxACE(selDVs = c("Inc50"), sep = "", dzData = dzInc, mzData = mzInc, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Assorative mating

m1asort <- umxACE(selDVs = c("Inc50"), sep = "", dzAr = 0.6, dzData = dzInc, mzData = mzInc, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("Inc50"), sep = "", dzAr = 0.7, dzData = dzInc, mzData = mzInc, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzIncman <- filter(mzInc, sexcomb == "Males")
dzIncman <- filter(dzInc, sexcomb == "Males")
mzIncwoman <- filter(mzInc, sexcomb == "Females")
dzIncwoman <- filter(dzInc, sexcomb == "Females")

m1man <- umxACE(selDVs = c("Inc50"), sep = "", dzData = dzIncman, mzData = mzIncman, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("Inc50"), sep = "", dzData = dzIncwoman, mzData = mzIncwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzIncung <- filter(mzInc, foedselsaar > 1960)
dzIncung <- filter(dzInc, foedselsaar > 1960)
mzIncgaml <- filter(mzInc, foedselsaar < 1961)
dzIncgaml <- filter(dzInc, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("Inc50"), sep = "", dzData = dzIncung, mzData = mzIncung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("Inc50"), sep = "", dzData = dzIncgaml, mzData = mzIncgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)



# EGP ---------------------------------------------------------------------

# Create ordinal

occtwins[, "EGP7c1"] = umxFactor(occtwins[, "EGP7c1"])
occtwins[, "EGP7c2"] = umxFactor(occtwins[, "EGP7c2"])

occtwins[, "EGP4c1"] = umxFactor(occtwins[, "EGP4c1"])
occtwins[, "EGP4c2"] = umxFactor(occtwins[, "EGP4c2"])

twinsEGP <- select(occtwins, EGP7c1, EGP7c2, EGP4c1, EGP4c2, devEGP1, devEGP2, Zygo, sexcomb, foedselsaar)

mzEGP <- na.omit(subset(twinsEGP, Zygo ==  1))
dzEGP <- na.omit(subset(twinsEGP, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Ordinal ACE models: with 4 and 7 classes

m1 <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxSummary(m1)

m1 <- umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
                 run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxSummary(m1)

m1 <- umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
                 run = TRUE, showErrorCodes = TRUE)

# Assortative mating 

m1asort <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzAr = 0.6, dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzAr = 0.6, dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


m1asort2 <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzAr = 0.7, dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzAr = 0.7, dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzEGPman <- filter(mzEGP, sexcomb == "Males")
dzEGPman <- filter(dzEGP, sexcomb == "Males")
mzEGPwoman <- filter(mzEGP, sexcomb == "Females")
dzEGPwoman <- filter(dzEGP, sexcomb == "Females")


m1man <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzData = dzEGPman, mzData = mzEGPman, bound = NULL, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1man <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPman, mzData = mzEGPman, bound = NULL, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzData = dzEGPwoman, mzData = mzEGPwoman, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPwoman, mzData = mzEGPwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzEGPung <- filter(mzEGP, foedselsaar > 1960)
dzEGPung <- filter(dzEGP, foedselsaar > 1960)
mzEGPgaml <- filter(mzEGP, foedselsaar < 1961)
dzEGPgaml <- filter(dzEGP, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzData = dzEGPung, mzData = mzEGPung, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1ung <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPung, mzData = mzEGPung, bound= NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("EGP7c"), selCov = "devEGP", sep = "", dzData = dzEGPgaml, mzData = mzEGPgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPgaml, mzData = mzEGPgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Binary analyses

# Create binary

occtwins <- mutate(occtwins, EGPb11 = ifelse((EGP501 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb21 = ifelse(EGP501 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb31 = ifelse(EGP501 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb41 = ifelse(EGP501 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb51 = ifelse((EGP501 == 5 | EGP501 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb61 = ifelse((EGP501 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb71 = ifelse((EGP501 == 9 | EGP501 == 10 | EGP501 == 11), 1, 0))

occtwins <- mutate(occtwins, EGPb12 = ifelse((EGP502 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb22 = ifelse(EGP502 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb32 = ifelse(EGP502 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb42 = ifelse(EGP502 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb52 = ifelse((EGP502 == 5 | EGP502 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb62 = ifelse((EGP502 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb72 = ifelse((EGP502 == 9 | EGP502 == 10 | EGP502 == 11), 1, 0))


occtwins[, "EGPb11"] = umxFactor(occtwins[, "EGPb11"])
occtwins[, "EGPb21"] = umxFactor(occtwins[, "EGPb21"])
occtwins[, "EGPb31"] = umxFactor(occtwins[, "EGPb31"])
occtwins[, "EGPb41"] = umxFactor(occtwins[, "EGPb41"])
occtwins[, "EGPb51"] = umxFactor(occtwins[, "EGPb51"])
occtwins[, "EGPb61"] = umxFactor(occtwins[, "EGPb61"])
occtwins[, "EGPb71"] = umxFactor(occtwins[, "EGPb71"])

occtwins[, "EGPb12"] = umxFactor(occtwins[, "EGPb12"])
occtwins[, "EGPb22"] = umxFactor(occtwins[, "EGPb22"])
occtwins[, "EGPb32"] = umxFactor(occtwins[, "EGPb32"])
occtwins[, "EGPb42"] = umxFactor(occtwins[, "EGPb42"])
occtwins[, "EGPb52"] = umxFactor(occtwins[, "EGPb52"])
occtwins[, "EGPb62"] = umxFactor(occtwins[, "EGPb62"])
occtwins[, "EGPb72"] = umxFactor(occtwins[, "EGPb72"])

EGPtwins <- select(occtwins, EGPb11:EGPb72, devEGP1, devEGP2, Zygo)

mzEGP <- na.omit(subset(EGPtwins, Zygo ==  1))
dzEGP <- na.omit(subset(EGPtwins, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Binary ACE models

m1 <- umxACE(selDVs = c("EGPb1"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb2"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb3"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb4"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb5"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb6"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb7"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)


# Binary analyses with inactive

occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

occtwins <- mutate(occtwins, EGP501 = ifelse(is.na(EGP501), 12, EGP501))
occtwins <- mutate(occtwins, devEGP1 = ifelse(is.na(devEGP1), 0, devEGP1))
occtwins <- mutate(occtwins, EGP502 = ifelse(is.na(EGP502), 12, EGP502))
occtwins <- mutate(occtwins, devEGP2 = ifelse(is.na(devEGP2), 0, devEGP2))

occtwins <- mutate(occtwins, EGPb11 = ifelse((EGP501 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb21 = ifelse(EGP501 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb31 = ifelse(EGP501 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb41 = ifelse(EGP501 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb51 = ifelse((EGP501 == 5 | EGP501 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb61 = ifelse((EGP501 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb71 = ifelse((EGP501 == 9 | EGP501 == 10 | EGP501 == 11), 1, 0))
occtwins <- mutate(occtwins, EGPb81 = ifelse(EGP501 == 12, 1, 0))

occtwins <- mutate(occtwins, EGPb12 = ifelse((EGP502 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb22 = ifelse(EGP502 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb32 = ifelse(EGP502 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb42 = ifelse(EGP502 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb52 = ifelse((EGP502 == 5 | EGP502 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb62 = ifelse((EGP502 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb72 = ifelse((EGP502 == 9 | EGP502 == 10 | EGP502 == 11), 1, 0))
occtwins <- mutate(occtwins, EGPb82 = ifelse(EGP502 == 12, 1, 0))


occtwins[, "EGPb11"] = umxFactor(occtwins[, "EGPb11"])
occtwins[, "EGPb21"] = umxFactor(occtwins[, "EGPb21"])
occtwins[, "EGPb31"] = umxFactor(occtwins[, "EGPb31"])
occtwins[, "EGPb41"] = umxFactor(occtwins[, "EGPb41"])
occtwins[, "EGPb51"] = umxFactor(occtwins[, "EGPb51"])
occtwins[, "EGPb61"] = umxFactor(occtwins[, "EGPb61"])
occtwins[, "EGPb71"] = umxFactor(occtwins[, "EGPb71"])
occtwins[, "EGPb81"] = umxFactor(occtwins[, "EGPb81"])

occtwins[, "EGPb12"] = umxFactor(occtwins[, "EGPb12"])
occtwins[, "EGPb22"] = umxFactor(occtwins[, "EGPb22"])
occtwins[, "EGPb32"] = umxFactor(occtwins[, "EGPb32"])
occtwins[, "EGPb42"] = umxFactor(occtwins[, "EGPb42"])
occtwins[, "EGPb52"] = umxFactor(occtwins[, "EGPb52"])
occtwins[, "EGPb62"] = umxFactor(occtwins[, "EGPb62"])
occtwins[, "EGPb72"] = umxFactor(occtwins[, "EGPb72"])
occtwins[, "EGPb82"] = umxFactor(occtwins[, "EGPb82"])

EGPtwins <- select(occtwins, EGPb11:EGPb82, devEGP1, devEGP2, Zygo)

mzEGP <- na.omit(subset(EGPtwins, Zygo ==  1))
dzEGP <- na.omit(subset(EGPtwins, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Binary ACE models

m1 <- umxACE(selDVs = c("EGPb1"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb2"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb3"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb4"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb5"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb6"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb7"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb8"), sep = "", dzData = dzEGP, mzData = mzEGP, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)


# Oesch -------------------------------------------------------------------

occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

occtwins[, "Oesch4c1"] = umxFactor(occtwins[, "Oesch4c1"])
occtwins[, "Oesch4c2"] = umxFactor(occtwins[, "Oesch4c2"])

twinsoesch <- select(occtwins,Oesch4c1, Oesch4c2, devOesch1, devOesch2, Zygo, sexcomb, foedselsaar)

mzOesch <- na.omit(subset(twinsoesch, Zygo ==  1))
dzOesch <- na.omit(subset(twinsoesch, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Ordinal models
m1 <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Assortative mating

m1asort <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzAr = 0.6, dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzAr = 0.7, dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzOeschman <- filter(mzOesch, sexcomb == "Males")
dzOeschman <- filter(dzOesch, sexcomb == "Males")
mzOeschwoman <- filter(mzOesch, sexcomb == "Females")
dzOeschwoman <- filter(dzOesch, sexcomb == "Females")


m1man <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschman, mzData = mzOeschman, bound = NULL, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschwoman, mzData = mzOeschwoman, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzOeschung <- filter(mzOesch, foedselsaar > 1960)
dzOeschung <- filter(dzOesch, foedselsaar > 1960)
mzOeschgaml <- filter(mzOesch, foedselsaar < 1961)
dzOeschgaml <- filter(dzOesch, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschung, mzData = mzOeschung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschgaml, mzData = mzOeschgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Binary analyses

occtwins <- mutate(occtwins, Oeschb11 = ifelse((Oesch501 == 1 | Oesch501 == 3 | Oesch501 == 4), 1, 0))
occtwins <- mutate(occtwins, Oeschb21 = ifelse(Oesch501 == 5, 1, 0))
occtwins <- mutate(occtwins, Oeschb31 = ifelse(Oesch501 == 6, 1, 0))
occtwins <- mutate(occtwins, Oeschb41 = ifelse(Oesch501 == 7, 1, 0))
occtwins <- mutate(occtwins, Oeschb51 = ifelse(Oesch501 == 8, 1, 0))
occtwins <- mutate(occtwins, Oeschb61 = ifelse(Oesch501 == 9, 1, 0))
occtwins <- mutate(occtwins, Oeschb71 = ifelse(Oesch501 == 10, 1, 0))
occtwins <- mutate(occtwins, Oeschb81 = ifelse((Oesch501 == 11 | Oesch501 == 12), 1, 0))
occtwins <- mutate(occtwins, Oeschb91 = ifelse(Oesch501 == 13, 1, 0))
occtwins <- mutate(occtwins, Oeschb101 = ifelse(Oesch501 == 14, 1, 0))
occtwins <- mutate(occtwins, Oeschb111 = ifelse(Oesch501 == 15, 1, 0))
occtwins <- mutate(occtwins, Oeschb121 = ifelse(Oesch501 == 16, 1, 0))

occtwins <- mutate(occtwins, Oeschb12 = ifelse((Oesch502 == 1 | Oesch502 == 3  | Oesch502 == 4), 1, 0))
occtwins <- mutate(occtwins, Oeschb22 = ifelse(Oesch502 == 5, 1, 0))
occtwins <- mutate(occtwins, Oeschb32 = ifelse(Oesch502 == 6, 1, 0))
occtwins <- mutate(occtwins, Oeschb42 = ifelse(Oesch502 == 7, 1, 0))
occtwins <- mutate(occtwins, Oeschb52 = ifelse(Oesch502 == 8, 1, 0))
occtwins <- mutate(occtwins, Oeschb62 = ifelse(Oesch502 == 9, 1, 0))
occtwins <- mutate(occtwins, Oeschb72 = ifelse(Oesch502 == 10, 1, 0))
occtwins <- mutate(occtwins, Oeschb82 = ifelse((Oesch502 == 11 | Oesch502 == 12), 1, 0))
occtwins <- mutate(occtwins, Oeschb92 = ifelse(Oesch502 == 13, 1, 0))
occtwins <- mutate(occtwins, Oeschb102 = ifelse(Oesch502 == 14, 1, 0))
occtwins <- mutate(occtwins, Oeschb112 = ifelse(Oesch502 == 15, 1, 0))
occtwins <- mutate(occtwins, Oeschb122 = ifelse(Oesch502 == 16, 1, 0))

occtwins[, "Oeschb11"] = umxFactor(occtwins[, "Oeschb11"])
occtwins[, "Oeschb21"] = umxFactor(occtwins[, "Oeschb21"])
occtwins[, "Oeschb31"] = umxFactor(occtwins[, "Oeschb31"])
occtwins[, "Oeschb41"] = umxFactor(occtwins[, "Oeschb41"])
occtwins[, "Oeschb51"] = umxFactor(occtwins[, "Oeschb51"])
occtwins[, "Oeschb61"] = umxFactor(occtwins[, "Oeschb61"])
occtwins[, "Oeschb71"] = umxFactor(occtwins[, "Oeschb71"])
occtwins[, "Oeschb81"] = umxFactor(occtwins[, "Oeschb81"])
occtwins[, "Oeschb91"] = umxFactor(occtwins[, "Oeschb91"])
occtwins[, "Oeschb101"] = umxFactor(occtwins[, "Oeschb101"])
occtwins[, "Oeschb111"] = umxFactor(occtwins[, "Oeschb111"])
occtwins[, "Oeschb121"] = umxFactor(occtwins[, "Oeschb121"])

occtwins[, "Oeschb12"] = umxFactor(occtwins[, "Oeschb12"])
occtwins[, "Oeschb22"] = umxFactor(occtwins[, "Oeschb22"])
occtwins[, "Oeschb32"] = umxFactor(occtwins[, "Oeschb32"])
occtwins[, "Oeschb42"] = umxFactor(occtwins[, "Oeschb42"])
occtwins[, "Oeschb52"] = umxFactor(occtwins[, "Oeschb52"])
occtwins[, "Oeschb62"] = umxFactor(occtwins[, "Oeschb62"])
occtwins[, "Oeschb72"] = umxFactor(occtwins[, "Oeschb72"])
occtwins[, "Oeschb82"] = umxFactor(occtwins[, "Oeschb82"])
occtwins[, "Oeschb92"] = umxFactor(occtwins[, "Oeschb92"])
occtwins[, "Oeschb102"] = umxFactor(occtwins[, "Oeschb102"])
occtwins[, "Oeschb112"] = umxFactor(occtwins[, "Oeschb112"])
occtwins[, "Oeschb122"] = umxFactor(occtwins[, "Oeschb122"])

Oeschtwins <- select(occtwins, Oeschb11:Oeschb122, devOesch1, devOesch2, Zygo)

mzOesch <- na.omit(subset(Oeschtwins, Zygo ==  1))
dzOesch <- na.omit(subset(Oeschtwins, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Binary models

m1 <- umxACE(selDVs = c("Oeschb1"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb2"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb3"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb4"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb5"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb6"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb7"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb8"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb9"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb10"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb11"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb12"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL,intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)


# Binary analyses with inactive

occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

occtwins <- mutate(occtwins, Oesch501 = ifelse(is.na(Oesch501), 17, Oesch501))
occtwins <- mutate(occtwins, devOesch1 = ifelse(is.na(devOesch1), 0, devOesch1))
occtwins <- mutate(occtwins, Oesch502 = ifelse(is.na(Oesch502), 17, Oesch502))
occtwins <- mutate(occtwins, devOesch2 = ifelse(is.na(devOesch2), 0, devOesch2))

occtwins <- mutate(occtwins, Oeschb11 = ifelse((Oesch501 == 1 | Oesch501 == 3 | Oesch501 == 4), 1, 0))
occtwins <- mutate(occtwins, Oeschb21 = ifelse(Oesch501 == 5, 1, 0))
occtwins <- mutate(occtwins, Oeschb31 = ifelse(Oesch501 == 6, 1, 0))
occtwins <- mutate(occtwins, Oeschb41 = ifelse(Oesch501 == 7, 1, 0))
occtwins <- mutate(occtwins, Oeschb51 = ifelse(Oesch501 == 8, 1, 0))
occtwins <- mutate(occtwins, Oeschb61 = ifelse(Oesch501 == 9, 1, 0))
occtwins <- mutate(occtwins, Oeschb71 = ifelse(Oesch501 == 10, 1, 0))
occtwins <- mutate(occtwins, Oeschb81 = ifelse((Oesch501 == 11 | Oesch501 == 12), 1, 0))
occtwins <- mutate(occtwins, Oeschb91 = ifelse(Oesch501 == 13, 1, 0))
occtwins <- mutate(occtwins, Oeschb101 = ifelse(Oesch501 == 14, 1, 0))
occtwins <- mutate(occtwins, Oeschb111 = ifelse(Oesch501 == 15, 1, 0))
occtwins <- mutate(occtwins, Oeschb121 = ifelse(Oesch501 == 16, 1, 0))
occtwins <- mutate(occtwins, Oeschb131 = ifelse(Oesch501 == 17, 1, 0))

occtwins <- mutate(occtwins, Oeschb12 = ifelse((Oesch502 == 1 | Oesch502 == 3  | Oesch502 == 4), 1, 0))
occtwins <- mutate(occtwins, Oeschb22 = ifelse(Oesch502 == 5, 1, 0))
occtwins <- mutate(occtwins, Oeschb32 = ifelse(Oesch502 == 6, 1, 0))
occtwins <- mutate(occtwins, Oeschb42 = ifelse(Oesch502 == 7, 1, 0))
occtwins <- mutate(occtwins, Oeschb52 = ifelse(Oesch502 == 8, 1, 0))
occtwins <- mutate(occtwins, Oeschb62 = ifelse(Oesch502 == 9, 1, 0))
occtwins <- mutate(occtwins, Oeschb72 = ifelse(Oesch502 == 10, 1, 0))
occtwins <- mutate(occtwins, Oeschb82 = ifelse((Oesch502 == 11 | Oesch502 == 12), 1, 0))
occtwins <- mutate(occtwins, Oeschb92 = ifelse(Oesch502 == 13, 1, 0))
occtwins <- mutate(occtwins, Oeschb102 = ifelse(Oesch502 == 14, 1, 0))
occtwins <- mutate(occtwins, Oeschb112 = ifelse(Oesch502 == 15, 1, 0))
occtwins <- mutate(occtwins, Oeschb122 = ifelse(Oesch502 == 16, 1, 0))
occtwins <- mutate(occtwins, Oeschb132 = ifelse(Oesch502 == 17, 1, 0))


occtwins[, "Oeschb11"] = umxFactor(occtwins[, "Oeschb11"])
occtwins[, "Oeschb21"] = umxFactor(occtwins[, "Oeschb21"])
occtwins[, "Oeschb31"] = umxFactor(occtwins[, "Oeschb31"])
occtwins[, "Oeschb41"] = umxFactor(occtwins[, "Oeschb41"])
occtwins[, "Oeschb51"] = umxFactor(occtwins[, "Oeschb51"])
occtwins[, "Oeschb61"] = umxFactor(occtwins[, "Oeschb61"])
occtwins[, "Oeschb71"] = umxFactor(occtwins[, "Oeschb71"])
occtwins[, "Oeschb81"] = umxFactor(occtwins[, "Oeschb81"])
occtwins[, "Oeschb91"] = umxFactor(occtwins[, "Oeschb91"])
occtwins[, "Oeschb101"] = umxFactor(occtwins[, "Oeschb101"])
occtwins[, "Oeschb111"] = umxFactor(occtwins[, "Oeschb111"])
occtwins[, "Oeschb121"] = umxFactor(occtwins[, "Oeschb121"])
occtwins[, "Oeschb131"] = umxFactor(occtwins[, "Oeschb131"])

occtwins[, "Oeschb12"] = umxFactor(occtwins[, "Oeschb12"])
occtwins[, "Oeschb22"] = umxFactor(occtwins[, "Oeschb22"])
occtwins[, "Oeschb32"] = umxFactor(occtwins[, "Oeschb32"])
occtwins[, "Oeschb42"] = umxFactor(occtwins[, "Oeschb42"])
occtwins[, "Oeschb52"] = umxFactor(occtwins[, "Oeschb52"])
occtwins[, "Oeschb62"] = umxFactor(occtwins[, "Oeschb62"])
occtwins[, "Oeschb72"] = umxFactor(occtwins[, "Oeschb72"])
occtwins[, "Oeschb82"] = umxFactor(occtwins[, "Oeschb82"])
occtwins[, "Oeschb92"] = umxFactor(occtwins[, "Oeschb92"])
occtwins[, "Oeschb102"] = umxFactor(occtwins[, "Oeschb102"])
occtwins[, "Oeschb112"] = umxFactor(occtwins[, "Oeschb112"])
occtwins[, "Oeschb122"] = umxFactor(occtwins[, "Oeschb122"])
occtwins[, "Oeschb132"] = umxFactor(occtwins[, "Oeschb132"])

Oeschtwins <- select(occtwins, Oeschb11:Oeschb132, devOesch1, devOesch2, Zygo)

mzOesch <- na.omit(subset(Oeschtwins, Zygo ==  1))
dzOesch <- na.omit(subset(Oeschtwins, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Binary models

m1 <- umxACE(selDVs = c("Oeschb1"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb2"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb3"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb4"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb5"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb6"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb7"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb8"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb9"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb10"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb11"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb12"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb13"), sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)


# ORDC --------------------------------------------------------------------

occtwins[, "ORDC4c1"] = umxFactor(occtwins[, "ORDC4c1"])
occtwins[, "ORDC4c2"] = umxFactor(occtwins[, "ORDC4c2"])

ORDCtwins <- select(occtwins, ORDC4c1, ORDC4c2, devORDC1, devORDC2, Zygo, foedselsaar, sexcomb)

mzORDC <- na.omit(subset(ORDCtwins, Zygo ==  1))
dzORDC <- na.omit(subset(ORDCtwins, Zygo ==  2))
mzORDC <- data.frame(mzORDC)
dzORDC <- data.frame(dzORDC)

m1 <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Assorative mating

m1asort <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzAr = 0.6, dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1asort2 <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzAr = 0.7, dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1asort2, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender

mzORDCman <- filter(mzORDC, sexcomb == "Males")
dzORDCman <- filter(dzORDC, sexcomb == "Males")
mzORDCwoman <- filter(mzORDC, sexcomb == "Females")
dzORDCwoman <- filter(dzORDC, sexcomb == "Females")

m1man <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzData = dzORDCman, mzData = mzORDCman, bound = NULL, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzData = dzORDCwoman, mzData = mzORDCwoman, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age

mzORDCung <- filter(mzORDC, foedselsaar > 1960)
dzORDCung <- filter(dzORDC, foedselsaar > 1960)
mzORDCgaml <- filter(mzORDC, foedselsaar < 1961)
dzORDCgaml <- filter(dzORDC, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzData = dzORDCung, mzData = mzORDCung, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("ORDC4c"), selCov = "devORDC", sep = "", dzData = dzORDCgaml, mzData = mzORDCgaml, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)



# Binary analyses

#Create dummies

occtwins <- mutate(occtwins, ORDCb11 = ifelse((ORDC501 == 1), 1, 0))
occtwins <- mutate(occtwins, ORDCb21 = ifelse(ORDC501 == 2, 1, 0))
occtwins <- mutate(occtwins, ORDCb31 = ifelse(ORDC501 == 3, 1, 0))
occtwins <- mutate(occtwins, ORDCb41 = ifelse(ORDC501 == 4, 1, 0))
occtwins <- mutate(occtwins, ORDCb51 = ifelse(ORDC501 == 5, 1, 0))
occtwins <- mutate(occtwins, ORDCb61 = ifelse(ORDC501 == 6, 1, 0))
occtwins <- mutate(occtwins, ORDCb71 = ifelse(ORDC501 == 7, 1, 0))
occtwins <- mutate(occtwins, ORDCb81 = ifelse((ORDC501 == 8), 1, 0))
occtwins <- mutate(occtwins, ORDCb91 = ifelse(ORDC501 == 9, 1, 0))
occtwins <- mutate(occtwins, ORDCb101 = ifelse(ORDC501 == 10, 1, 0))
occtwins <- mutate(occtwins, ORDCb111 = ifelse(ORDC501 == 11, 1, 0))
occtwins <- mutate(occtwins, ORDCb121 = ifelse(ORDC501 == 12, 1, 0))

occtwins <- mutate(occtwins, ORDCb12 = ifelse((ORDC502 == 1), 1, 0))
occtwins <- mutate(occtwins, ORDCb22 = ifelse(ORDC502 == 2, 1, 0))
occtwins <- mutate(occtwins, ORDCb32 = ifelse(ORDC502 == 3, 1, 0))
occtwins <- mutate(occtwins, ORDCb42 = ifelse(ORDC502 == 4, 1, 0))
occtwins <- mutate(occtwins, ORDCb52 = ifelse(ORDC502 == 5, 1, 0))
occtwins <- mutate(occtwins, ORDCb62 = ifelse(ORDC502 == 6, 1, 0))
occtwins <- mutate(occtwins, ORDCb72 = ifelse(ORDC502 == 7, 1, 0))
occtwins <- mutate(occtwins, ORDCb82 = ifelse((ORDC502 == 8), 1, 0))
occtwins <- mutate(occtwins, ORDCb92 = ifelse(ORDC502 == 9, 1, 0))
occtwins <- mutate(occtwins, ORDCb102 = ifelse(ORDC502 == 10, 1, 0))
occtwins <- mutate(occtwins, ORDCb112 = ifelse(ORDC502 == 11, 1, 0))
occtwins <- mutate(occtwins, ORDCb122 = ifelse(ORDC502 == 12, 1, 0))

occtwins[, "ORDCb11"] = umxFactor(occtwins[, "ORDCb11"])
occtwins[, "ORDCb21"] = umxFactor(occtwins[, "ORDCb21"])
occtwins[, "ORDCb31"] = umxFactor(occtwins[, "ORDCb31"])
occtwins[, "ORDCb41"] = umxFactor(occtwins[, "ORDCb41"])
occtwins[, "ORDCb51"] = umxFactor(occtwins[, "ORDCb51"])
occtwins[, "ORDCb61"] = umxFactor(occtwins[, "ORDCb61"])
occtwins[, "ORDCb71"] = umxFactor(occtwins[, "ORDCb71"])
occtwins[, "ORDCb81"] = umxFactor(occtwins[, "ORDCb81"])
occtwins[, "ORDCb91"] = umxFactor(occtwins[, "ORDCb91"])
occtwins[, "ORDCb101"] = umxFactor(occtwins[, "ORDCb101"])
occtwins[, "ORDCb111"] = umxFactor(occtwins[, "ORDCb111"])
occtwins[, "ORDCb121"] = umxFactor(occtwins[, "ORDCb121"])

occtwins[, "ORDCb12"] = umxFactor(occtwins[, "ORDCb12"])
occtwins[, "ORDCb22"] = umxFactor(occtwins[, "ORDCb22"])
occtwins[, "ORDCb32"] = umxFactor(occtwins[, "ORDCb32"])
occtwins[, "ORDCb42"] = umxFactor(occtwins[, "ORDCb42"])
occtwins[, "ORDCb52"] = umxFactor(occtwins[, "ORDCb52"])
occtwins[, "ORDCb62"] = umxFactor(occtwins[, "ORDCb62"])
occtwins[, "ORDCb72"] = umxFactor(occtwins[, "ORDCb72"])
occtwins[, "ORDCb82"] = umxFactor(occtwins[, "ORDCb82"])
occtwins[, "ORDCb92"] = umxFactor(occtwins[, "ORDCb92"])
occtwins[, "ORDCb102"] = umxFactor(occtwins[, "ORDCb102"])
occtwins[, "ORDCb112"] = umxFactor(occtwins[, "ORDCb112"])
occtwins[, "ORDCb122"] = umxFactor(occtwins[, "ORDCb122"])

ORDCtwins <- select(occtwins, ORDCb11:ORDCb122, devORDC1, devORDC2, Zygo)

mzORDC <- na.omit(subset(ORDCtwins, Zygo ==  1))
dzORDC <- na.omit(subset(ORDCtwins, Zygo ==  2))
mzORDC <- data.frame(mzORDC)
dzORDC <- data.frame(dzORDC)


# Binary models

m1 <- umxACE(selDVs = c("ORDCb1"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb2"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb3"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb4"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb5"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb6"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb7"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb8"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb9"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb10"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb11"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb12"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

# Binary analyses with inactive ---------------------------------------------------------

#Create dummies

occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

occtwins <- mutate(occtwins, ORDC501 = ifelse(is.na(ORDC501), 13, ORDC501))
occtwins <- mutate(occtwins, devORDC1 = ifelse(is.na(devORDC1), 0, devORDC1))
occtwins <- mutate(occtwins, ORDC502 = ifelse(is.na(ORDC502), 13, ORDC502))
occtwins <- mutate(occtwins, devORDC2 = ifelse(is.na(devORDC2), 0, devORDC2))


occtwins <- mutate(occtwins, ORDCb11 = ifelse((ORDC501 == 1), 1, 0))
occtwins <- mutate(occtwins, ORDCb21 = ifelse(ORDC501 == 2, 1, 0))
occtwins <- mutate(occtwins, ORDCb31 = ifelse(ORDC501 == 3, 1, 0))
occtwins <- mutate(occtwins, ORDCb41 = ifelse(ORDC501 == 4, 1, 0))
occtwins <- mutate(occtwins, ORDCb51 = ifelse(ORDC501 == 5, 1, 0))
occtwins <- mutate(occtwins, ORDCb61 = ifelse(ORDC501 == 6, 1, 0))
occtwins <- mutate(occtwins, ORDCb71 = ifelse(ORDC501 == 7, 1, 0))
occtwins <- mutate(occtwins, ORDCb81 = ifelse((ORDC501 == 8), 1, 0))
occtwins <- mutate(occtwins, ORDCb91 = ifelse(ORDC501 == 9, 1, 0))
occtwins <- mutate(occtwins, ORDCb101 = ifelse(ORDC501 == 10, 1, 0))
occtwins <- mutate(occtwins, ORDCb111 = ifelse(ORDC501 == 11, 1, 0))
occtwins <- mutate(occtwins, ORDCb121 = ifelse(ORDC501 == 12, 1, 0))
occtwins <- mutate(occtwins, ORDCb131 = ifelse(ORDC501 == 13, 1, 0))

occtwins <- mutate(occtwins, ORDCb12 = ifelse((ORDC502 == 1), 1, 0))
occtwins <- mutate(occtwins, ORDCb22 = ifelse(ORDC502 == 2, 1, 0))
occtwins <- mutate(occtwins, ORDCb32 = ifelse(ORDC502 == 3, 1, 0))
occtwins <- mutate(occtwins, ORDCb42 = ifelse(ORDC502 == 4, 1, 0))
occtwins <- mutate(occtwins, ORDCb52 = ifelse(ORDC502 == 5, 1, 0))
occtwins <- mutate(occtwins, ORDCb62 = ifelse(ORDC502 == 6, 1, 0))
occtwins <- mutate(occtwins, ORDCb72 = ifelse(ORDC502 == 7, 1, 0))
occtwins <- mutate(occtwins, ORDCb82 = ifelse((ORDC502 == 8), 1, 0))
occtwins <- mutate(occtwins, ORDCb92 = ifelse(ORDC502 == 9, 1, 0))
occtwins <- mutate(occtwins, ORDCb102 = ifelse(ORDC502 == 10, 1, 0))
occtwins <- mutate(occtwins, ORDCb112 = ifelse(ORDC502 == 11, 1, 0))
occtwins <- mutate(occtwins, ORDCb122 = ifelse(ORDC502 == 12, 1, 0))
occtwins <- mutate(occtwins, ORDCb132 = ifelse(ORDC502 == 13, 1, 0))

occtwins[, "ORDCb11"] = umxFactor(occtwins[, "ORDCb11"])
occtwins[, "ORDCb21"] = umxFactor(occtwins[, "ORDCb21"])
occtwins[, "ORDCb31"] = umxFactor(occtwins[, "ORDCb31"])
occtwins[, "ORDCb41"] = umxFactor(occtwins[, "ORDCb41"])
occtwins[, "ORDCb51"] = umxFactor(occtwins[, "ORDCb51"])
occtwins[, "ORDCb61"] = umxFactor(occtwins[, "ORDCb61"])
occtwins[, "ORDCb71"] = umxFactor(occtwins[, "ORDCb71"])
occtwins[, "ORDCb81"] = umxFactor(occtwins[, "ORDCb81"])
occtwins[, "ORDCb91"] = umxFactor(occtwins[, "ORDCb91"])
occtwins[, "ORDCb101"] = umxFactor(occtwins[, "ORDCb101"])
occtwins[, "ORDCb111"] = umxFactor(occtwins[, "ORDCb111"])
occtwins[, "ORDCb121"] = umxFactor(occtwins[, "ORDCb121"])
occtwins[, "ORDCb131"] = umxFactor(occtwins[, "ORDCb131"])

occtwins[, "ORDCb12"] = umxFactor(occtwins[, "ORDCb12"])
occtwins[, "ORDCb22"] = umxFactor(occtwins[, "ORDCb22"])
occtwins[, "ORDCb32"] = umxFactor(occtwins[, "ORDCb32"])
occtwins[, "ORDCb42"] = umxFactor(occtwins[, "ORDCb42"])
occtwins[, "ORDCb52"] = umxFactor(occtwins[, "ORDCb52"])
occtwins[, "ORDCb62"] = umxFactor(occtwins[, "ORDCb62"])
occtwins[, "ORDCb72"] = umxFactor(occtwins[, "ORDCb72"])
occtwins[, "ORDCb82"] = umxFactor(occtwins[, "ORDCb82"])
occtwins[, "ORDCb92"] = umxFactor(occtwins[, "ORDCb92"])
occtwins[, "ORDCb102"] = umxFactor(occtwins[, "ORDCb102"])
occtwins[, "ORDCb112"] = umxFactor(occtwins[, "ORDCb112"])
occtwins[, "ORDCb122"] = umxFactor(occtwins[, "ORDCb122"])
occtwins[, "ORDCb132"] = umxFactor(occtwins[, "ORDCb132"])

ORDCtwins <- select(occtwins, ORDCb11:ORDCb132, devORDC1, devORDC2, Zygo)

mzORDC <- na.omit(subset(ORDCtwins, Zygo ==  1))
dzORDC <- na.omit(subset(ORDCtwins, Zygo ==  2))
mzORDC <- data.frame(mzORDC)
dzORDC <- data.frame(dzORDC)


# Binary models

m1 <- umxACE(selDVs = c("ORDCb1"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb2"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb3"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb4"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb5"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb6"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb7"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb8"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb9"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb10"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb11"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb12"), selCov = "devORDC", sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("ORDCb13"), sep = "", dzData = dzORDC, mzData = mzORDC, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

