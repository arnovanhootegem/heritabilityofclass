library(data.table)
library(here)
library(tidyverse)
library(purrr)
library(occupar)
library(umx)
library(OpenMx)
library(psych)
library(labelled)
library(dplyr)
library(dtplyr)

setwd("N:/durable/projects/class")

regoccimerge <- readRDS("N:/durable/projects/class/01 - Data/regoccimerge_all.rds")

# Create EGP --------------------------------------------------------------

# Script Occupar package R: www.rdrr.io/github/DiogoFerrari/occupar/scr/R/occupar_occupation.R

regoccimerge <- mutate(regoccimerge, isco88 = ifelse(startsWith(isco88, "00"), NA, isco88))
regoccimerge <- mutate(regoccimerge, isco88 = ifelse(isco88 == "0", NA, isco88))
regoccimerge <- mutate(regoccimerge, isco88 = ifelse(isco88 == "-2-2", NA, isco88))

regoccimerge$EGP <- isco88toEGP(regoccimerge$isco88, n.employees = NULL, self.employed=NULL, n.classes = 11)
table(regoccimerge$EGP, exclude = NULL)
regoccimerge <- mutate(regoccimerge, EGP = ifelse(yrkstat == "2", 6, EGP))
regoccimerge <- mutate(regoccimerge, EGP = ifelse(yrkstat == "2" & ansatte_bedrift > 1, 5, EGP))

regoccimerge$EGPimp <- isco88toEGP(regoccimerge$yrke_imp, n.employees = NULL, self.employed=NULL, n.classes = 11)
regoccimerge$EGPimp <- as.numeric(regoccimerge$EGPimp)

regoccimerge <- mutate(regoccimerge, EGP = ifelse(is.na(EGP), EGPimp, EGP))
table(regoccimerge$EGP, exclude = NULL)

regoccimerge <- select(regoccimerge, -EGPimp)

val_labels(regoccimerge$EGP) <- c("Service class I" = 1,
                                    "Service class II" = 2,
                                    "Routine non-manual, higher grade" = 3,
                                    "Routine non-manual, lower grade" = 4,
                                    "Self-employed with employees" = 5,
                                    "Self-employed with no empoyees" = 6,
                                    "Self-employed Farmers" = 7,
                                    "Manual supervisors/Lower grade technicians" = 8,
                                    "Skilled workers" = 9,
                                    "Unskilled workers" = 10,
                                    "Farm labours" = 11)

# Check missing codes

miss <- filter(regoccimerge, is.na(regoccimerge$EGP))
tab <- miss %>% group_by(Year, isco88) %>% summarize(freq=n())
write.table(tab, "03 - Results/EGPmiss.txt")

# Checking consistency across years

dat <- regoccimerge %>%
  count(Year, EGP) %>%
  group_by(Year) %>%
  mutate(percent = n / sum(n))

dat$EGP <- as.factor(dat$EGP)

ggplot(dat, aes(EGP, percent, group = Year, colour = as.factor(Year))) +
  geom_line() +
  geom_point()

ggsave("03 - Results/EGPyears.pdf", plot = last_plot())

# Checking descriptives

regoccimerge$BU <- as.numeric(regoccimerge$BU)
regoccimerge <- mutate(regoccimerge, kjoenn = ifelse(kjoenn == 2, 1, 0))


tab <- regoccimerge %>% 
  subset(Year == 2010) %>% 
  group_by(EGP) %>% 
  summarise_at(vars(Income, decile, BU, kjoenn), funs(mean(., na.rm = TRUE)))

write.table(tab, "03 - Results/EGPdesc.txt")

# Keeping occupation at age 50

regoccimerge <- select(regoccimerge, -kjoenn, -BU, -decile, -Income)

regoccimerge <- filter(regoccimerge, foedselsaar > 1940 & foedselsaar < 1981)

regoccimerge$age <- regoccimerge$Year - regoccimerge$foedselsaar
regoccimerge <- filter(regoccimerge, age < 63)
regoccimerge$dev <- regoccimerge$age - 50
regoccimerge$dev <- abs(regoccimerge$dev)

regoccimerge <- lazy_dt(regoccimerge)

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, dev) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(EGP50 = first(na.omit(EGP))) %>% 
  ungroup()

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, dev) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(devEGP = case_when(EGP50 == EGP ~ dev)) %>% 
  ungroup()

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, devEGP) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(devEGP = first(na.omit(devEGP))) %>% 
  ungroup()

regoccimerge <- as_tibble(regoccimerge)

saveRDS(regoccimerge, "N:/durable/projects/class/01 - Data/EGP.rds")

# Twin data ---------------------------------------------------------------

twins <- fread("N:/durable/prepared/twin-linked-nodups.csv")
nrow(twins)
names(twins)

twins <- select(twins, w18_0590_lopenr_person, Zygo, tvilling_par_ID, kjoenn)
occ <- select(regoccimerge, w18_0590_lopenr_person, foedselsaar, EGP50, devEGP)
occ <- distinct(occ)

Occtwins <- inner_join(twins, occ, by = "w18_0590_lopenr_person")

Occtwins <- Occtwins %>%                                     ## twins_utfall hvis vi ikke skal kaste ut dem som er gift 
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
  select(twinpair, twinno, Zygo, foedselsaar, sexcomb, EGP50, devEGP, ant_tvilling)

Occtwins_wide <- Occtwins %>%
  group_by(twinpair) %>%
  gather(key=var, value=info, EGP50, devEGP) %>%
  mutate(var = str_c(var,twinno)) %>%
  select(-twinno) %>%
  spread(key=var, value=info)

write.csv(Occtwins,"N:/durable/projects/class/01 - Data/EGPtwins.csv", row.names = FALSE)
write.csv(Occtwins_wide,"N:/durable/projects/class/01 - Data/EGPtwins_wide.csv", row.names = FALSE)

occtwins <- Occtwins_wide
rm(Occtwins_wide, regoccimerge, occ, twins)
occtwins <- as_tibble(occtwins)
occtwins <- data.frame(occtwins)

# General twin analyses ---------------------------------------------------------------

# Create ordinal

occtwins <- mutate(occtwins, EGP4c1 = ifelse((EGP501 == 1 | EGP501 == 2), 1, NA))
occtwins <- mutate(occtwins, EGP4c1 = ifelse((EGP501 == 3 | EGP501 == 4), 2, EGP4c1))
occtwins <- mutate(occtwins, EGP4c1 = ifelse((EGP501 == 8 | EGP501 == 9), 3, EGP4c1))
occtwins <- mutate(occtwins, EGP4c1 = ifelse((EGP501 == 10 | EGP501 == 11), 4, EGP4c1))
occtwins$EGP4c1 <- occtwins$EGP4c1 - 1


occtwins <- mutate(occtwins, EGP4c2 = ifelse((EGP502 == 1 | EGP502 == 2), 1, NA))
occtwins <- mutate(occtwins, EGP4c2 = ifelse((EGP502 == 3 | EGP502 == 4), 2, EGP4c2))
occtwins <- mutate(occtwins, EGP4c2 = ifelse((EGP502 == 8 | EGP502 == 9), 3, EGP4c2))
occtwins <- mutate(occtwins, EGP4c2 = ifelse((EGP502 == 10 | EGP502 == 11), 4, EGP4c2))
occtwins$EGP4c2 <- occtwins$EGP4c2 - 1

occtwins[, "EGP4c1"] = umxFactor(occtwins[, "EGP4c1"])
occtwins[, "EGP4c2"] = umxFactor(occtwins[, "EGP4c2"])



mzEGP <- na.omit(subset(occtwins, Zygo ==  1))
dzEGP <- na.omit(subset(occtwins, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Ordinal ACE models

m1 <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxSummary(m1)

m1 <- umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Models per gender -------------------------------------------------------


mzEGPman <- filter(mzEGP, sexcomb == "Males")
dzEGPman <- filter(dzEGP, sexcomb == "Males")
mzEGPwoman <- filter(mzEGP, sexcomb == "Females")
dzEGPwoman <- filter(dzEGP, sexcomb == "Females")

# Ordinal models

m1 <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPman, mzData = mzEGPman, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPwoman, mzData = mzEGPwoman, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age ----------------------------------------------------------


mzEGPung <- filter(mzEGP, foedselsaar > 1960)
dzEGPung <- filter(dzEGP, foedselsaar > 1960)
mzEGPgaml <- filter(mzEGP, foedselsaar < 1961)
dzEGPgaml <- filter(dzEGP, foedselsaar < 1961)

m1ung <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPung, mzData = mzEGPung, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("EGP4c"), selCov = "devEGP", sep = "", dzData = dzEGPgaml, mzData = mzEGPgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

# Binary analyses -------------------------------------------------------

# Create binary

occtwins <- fread("N:/durable/projects/class/01 - Data/EGPtwins_wide.csv")

occtwins <- mutate(occtwins, EGPb11 = ifelse((EGP501 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb21 = ifelse(EGP501 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb31 = ifelse(EGP501 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb41 = ifelse(EGP501 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb51 = ifelse((EGP501 == 5 | EGP501 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb61 = ifelse((EGP501 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb71 = ifelse(EGP501 == 9, 1, 0))
occtwins <- mutate(occtwins, EGPb81 = ifelse((EGP501 == 10 | EGP501 == 11), 1, 0))

occtwins <- mutate(occtwins, EGPb12 = ifelse((EGP502 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb22 = ifelse(EGP502 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb32 = ifelse(EGP502 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb42 = ifelse(EGP502 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb52 = ifelse((EGP502 == 5 | EGP502 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb62 = ifelse((EGP502 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb72 = ifelse(EGP502 == 9, 1, 0))
occtwins <- mutate(occtwins, EGPb82 = ifelse((EGP502 == 10 | EGP502 == 11), 1, 0))


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

mzEGP <- na.omit(subset(occtwins, Zygo ==  1))
dzEGP <- na.omit(subset(occtwins, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Binary ACE models

m1 <- umxACE(selDVs = c("EGPb1"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb2"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb3"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb4"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb5"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb6"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb7"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb8"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

# Binary analyses with inactive -------------------------------------------------------

occtwins <- fread("N:/durable/projects/class/01 - Data/EGPtwins_wide.csv")

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
occtwins <- mutate(occtwins, EGPb71 = ifelse(EGP501 == 9, 1, 0))
occtwins <- mutate(occtwins, EGPb81 = ifelse((EGP501 == 10 | EGP501 == 11), 1, 0))
occtwins <- mutate(occtwins, EGPb91 = ifelse(EGP501 == 12, 1, 0))

occtwins <- mutate(occtwins, EGPb12 = ifelse((EGP502 == 1), 1, 0))
occtwins <- mutate(occtwins, EGPb22 = ifelse(EGP502 == 2, 1, 0))
occtwins <- mutate(occtwins, EGPb32 = ifelse(EGP502 == 3, 1, 0))
occtwins <- mutate(occtwins, EGPb42 = ifelse(EGP502 == 4, 1, 0))
occtwins <- mutate(occtwins, EGPb52 = ifelse((EGP502 == 5 | EGP502 == 6), 1, 0))
occtwins <- mutate(occtwins, EGPb62 = ifelse((EGP502 == 8), 1, 0))
occtwins <- mutate(occtwins, EGPb72 = ifelse(EGP502 == 9, 1, 0))
occtwins <- mutate(occtwins, EGPb82 = ifelse((EGP502 == 10 | EGP502 == 11), 1, 0))
occtwins <- mutate(occtwins, EGPb92 = ifelse(EGP502 == 12, 1, 0))


occtwins[, "EGPb11"] = umxFactor(occtwins[, "EGPb11"])
occtwins[, "EGPb21"] = umxFactor(occtwins[, "EGPb21"])
occtwins[, "EGPb31"] = umxFactor(occtwins[, "EGPb31"])
occtwins[, "EGPb41"] = umxFactor(occtwins[, "EGPb41"])
occtwins[, "EGPb51"] = umxFactor(occtwins[, "EGPb51"])
occtwins[, "EGPb61"] = umxFactor(occtwins[, "EGPb61"])
occtwins[, "EGPb71"] = umxFactor(occtwins[, "EGPb71"])
occtwins[, "EGPb81"] = umxFactor(occtwins[, "EGPb81"])
occtwins[, "EGPb91"] = umxFactor(occtwins[, "EGPb91"])

occtwins[, "EGPb12"] = umxFactor(occtwins[, "EGPb12"])
occtwins[, "EGPb22"] = umxFactor(occtwins[, "EGPb22"])
occtwins[, "EGPb32"] = umxFactor(occtwins[, "EGPb32"])
occtwins[, "EGPb42"] = umxFactor(occtwins[, "EGPb42"])
occtwins[, "EGPb52"] = umxFactor(occtwins[, "EGPb52"])
occtwins[, "EGPb62"] = umxFactor(occtwins[, "EGPb62"])
occtwins[, "EGPb72"] = umxFactor(occtwins[, "EGPb72"])
occtwins[, "EGPb82"] = umxFactor(occtwins[, "EGPb82"])
occtwins[, "EGPb92"] = umxFactor(occtwins[, "EGPb92"])

mzEGP <- na.omit(subset(occtwins, Zygo ==  1))
dzEGP <- na.omit(subset(occtwins, Zygo ==  2))
mzEGP <- data.frame(mzEGP)
dzEGP <- data.frame(dzEGP)

# Binary ACE models

m1 <- umxACE(selDVs = c("EGPb1"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb2"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb3"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb4"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb5"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb6"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb7"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb8"), selCov = "devEGP", sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
m1 <- umxACE(selDVs = c("EGPb9"), sep = "", dzData = dzEGP, mzData = mzEGP, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
