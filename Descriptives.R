################# DESCRIPTIVES CLASS HERITABILITY ##################

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


setwd("N:/durable/projects/class")

occ <- readRDS("N:/durable/users/arnovh/Class/01 - Data/Classes at age 50.rds")
occ <- select(occ, w19_0634_lnr, foedselsaar, kjoenn, 
              BU50, Inc50, EGP50, ORDC50, Oesch50, 
              trei50, ISEI50)
occ <- distinct(occ)

twins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class.csv")

occ <- mutate(occ, gender = ifelse(kjoenn == 2, 1, 0))
twins <- mutate(twins, gender = ifelse(sexcomb == "Females", 1, 0))

#twins <- drop_na(twins)

mz <- filter(twins, Zygo ==  1)
dz <- filter(twins, Zygo ==  2)

# Descriptives for gender, birth year, income, education and SIOPS for mz, dz and full population -----------------------------

summary(mz)
sd(mz$gender)
sd(mz$foedselsaar)
sd(mz$BU50, na.rm = TRUE)
sd(mz$Inc50, na.rm = TRUE)
sd(mz$trei50, na.rm = TRUE)
sd(mz$ISEI50, na.rm = TRUE)

summary(dz)
sd(dz$gender)
sd(dz$foedselsaar)
sd(dz$BU50, na.rm = TRUE)
sd(dz$Inc50, na.rm = TRUE)
sd(dz$trei50, na.rm = TRUE)
sd(dz$ISEI50, na.rm = TRUE)

summary(occ)
sd(occ$gender)
sd(occ$foedselsaar)
sd(occ$BU50, na.rm = TRUE)
sd(occ$Inc50, na.rm = TRUE)
sd(occ$trei50, na.rm = TRUE)
sd(occ$ISEI50, na.rm = TRUE)


# Correlation between class schemes ---------------------------------------

# Phenotypic

cor <- select(twins, trei50, ISEI50, EGP7c, EGP4c, Oesch4c, ORDC4c, Inc50, BU50)
cor <- na.omit(cor)
correlation_matrix <- cor(cor, method = "spearman")
correlation_matrix <- cor(cor, method = "pearson")


# Genetic

occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

occtwins <- select(occtwins, Zygo, trei501, trei502, ISEI501, ISEI502, EGP7c1, EGP7c2, 
                   Inc501, Inc502, BU501, BU502)

occtwins <- na.omit(occtwins)

occtwins[, "EGP7c1"] = umxFactor(occtwins[, "EGP7c1"])
occtwins[, "EGP7c2"] = umxFactor(occtwins[, "EGP7c2"])

mz <- na.omit(subset(occtwins, Zygo ==  1))
dz <- na.omit(subset(occtwins, Zygo ==  2))
mz <- data.frame(mz)
dz <- data.frame(dz)

m1 <- umxACE(selDVs = c("trei50", "ISEI50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m1, showRg = TRUE)
m2 <- umxACE(selDVs = c("trei50", "EGP7c"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m2, showRg = TRUE)
m3 <- umxACE(selDVs = c("trei50", "Inc50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m3, showRg = TRUE)
m4 <- umxACE(selDVs = c("trei50", "BU50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m4, showRg = TRUE)

m2 <- umxACE(selDVs = c("ISEI50", "EGP7c"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m2, showRg = TRUE)
m3 <- umxACE(selDVs = c("ISEI50", "Inc50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m3, showRg = TRUE)
m4 <- umxACE(selDVs = c("ISEI50", "BU50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m4, showRg = TRUE)

m3 <- umxACE(selDVs = c("EGP7c", "Inc50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m3, showRg = TRUE)
m4 <- umxACE(selDVs = c("EGP7c", "BU50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m4, showRg = TRUE)

m4 <- umxACE(selDVs = c("Inc50", "BU50"), sep = "", dzData = dz, mzData = mz, bound = NULL, intervals = TRUE)
umxSummary(m4, showRg = TRUE)


#  Correlations between twins --------------------


occtwins <- fread("N:/durable/users/arnovh/Class/01 - Data/Twins with class_wide.csv")

mz <- filter(occtwins, Zygo == 1)
dz <- filter(occtwins, Zygo == 2)

cor(mz$EGP7c1, mz$EGP7c2, method = 'spearman', use = "complete.obs")
cor(dz$EGP7c1, dz$EGP7c2, method = 'spearman', use = "complete.obs")

cor(mz$trei501, mz$trei502, method = 'pearson', use = "complete.obs")
cor(dz$trei501, dz$trei502, method = 'pearson', use = "complete.obs")

cor(mz$ISEI501, mz$ISEI502, method = 'pearson', use = "complete.obs")
cor(dz$ISEI501, dz$ISEI502, method = 'pearson', use = "complete.obs")

cor(mz$BU501, mz$BU502, method = 'pearson', use = "complete.obs")
cor(dz$BU501, dz$BU502, method = 'pearson', use = "complete.obs")

cor(mz$Inc501, mz$Inc502, method = 'pearson', use = "complete.obs")
cor(dz$Inc501, dz$Inc502, method = 'pearson', use = "complete.obs")
