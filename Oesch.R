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

# Create Oesch ------------------------------------------------------------

# Script Oesch for ESS 2002-2010: www.people.unil.ch/danieloesch/scripts/

regoccimerge <- mutate(regoccimerge, isco88 = ifelse(startsWith(isco88, "00"), NA, isco88))
regoccimerge <- mutate(regoccimerge, isco88 = ifelse(isco88 == "0", NA, isco88))
regoccimerge <- mutate(regoccimerge, isco88 = ifelse(isco88 == "-2-2", NA, isco88))


regoccimerge$selfem_mainjob <- NA
regoccimerge$selfem_mainjob[regoccimerge$yrkstat == 0 | regoccimerge$yrkstat == 1 | regoccimerge$yrkstat == 3 | regoccimerge$yrkstat == 4] <- 1
regoccimerge$selfem_mainjob[regoccimerge$yrkstat == 2 & regoccimerge$ansatte_bedrift == 1] <- 2
regoccimerge$selfem_mainjob[regoccimerge$yrkstat == 2 & regoccimerge$ansatte_bedrift > 1 & regoccimerge$ansatte_bedrift < 11] <- 3
regoccimerge$selfem_mainjob[regoccimerge$yrkstat == 2 & regoccimerge$ansatte_bedrift > 10] <- 4
val_labels(regoccimerge$selfem_mainjob) <- c("Not self-employed" = 1,
                                             "Self-empl without employees" = 2,
                                             "Self-empl with 1-9 employees" = 3,
                                             "Self-empl with 10 or more" = 4)
table(regoccimerge$selfem_mainjob)

regoccimerge$Oesch <- NA
regoccimerge$Oeschimp <- NA


# Large employers (1)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 4] <- 1

# Self-employed professionals (2)

#regoccimerge$Oesch[(regoccimerge$selfem_mainjob == 2 | regoccimerge$selfem_mainjob == 3) & regoccimerge$isco88 >= 2000 & regoccimerge$isco88 <= 2229] <- 2
#regoccimerge$Oesch[(regoccimerge$selfem_mainjob == 2 | regoccimerge$selfem_mainjob == 3) & regoccimerge$isco88 >= 2300 & regoccimerge$isco88 <= 2470] <- 2

# Small business owners with employees (3)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 3] <- 3

# Small business owners without employees (4)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 2] <- 4

# Technical experts (5)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2100 &  regoccimerge$isco88 <= 2213] <- 5

# Technicians (6)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3100 &  regoccimerge$isco88 <= 3152] <- 6
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3210 &  regoccimerge$isco88 <= 3213] <- 6
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 3434] <- 6

# Skilled manual (7)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 6000 &  regoccimerge$isco88 <= 7442] <- 7
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 8310 &  regoccimerge$isco88 <= 8312] <- 7
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 8324 &  regoccimerge$isco88 <= 8330] <- 7
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 8332 &  regoccimerge$isco88 <= 8340] <- 7

# Low-skilled manual (8)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 8000 &  regoccimerge$isco88 <= 8300] <- 8
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 8320 &  regoccimerge$isco88 <= 8321] <- 8
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 8331] <- 8
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 9153 &  regoccimerge$isco88 <= 9333] <- 8

# Higher-grade managers and administrators (9)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 1000 &  regoccimerge$isco88 <= 1239] <- 9
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2400 &  regoccimerge$isco88 <= 2429] <- 9
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2441] <- 9
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2470] <- 9

# Lower-grade managers and administrators (10)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 1300 &  regoccimerge$isco88 <= 1319] <- 10
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3400 &  regoccimerge$isco88 <= 3433] <- 10
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3440 &  regoccimerge$isco88 <= 3450] <- 10

# Skilled clerks (11)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 4000 &  regoccimerge$isco88 <= 4112] <- 11
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 4114 &  regoccimerge$isco88 <= 4210] <- 11
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 4212 &  regoccimerge$isco88 <= 4222] <- 11

# Unskilled clerks (12)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 4113] <- 12
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 4211] <- 12
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 4223] <- 12

# Socio-cultural professionals (13)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2220 &  regoccimerge$isco88 <= 2229] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2300 &  regoccimerge$isco88 <= 2320] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2340 &  regoccimerge$isco88 <= 2359] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2430 &  regoccimerge$isco88 <= 2440] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2442 &  regoccimerge$isco88 <= 2443] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2445] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2451] <- 13
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2460] <- 13

# Socio-cultural semi-professionals (14)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2230] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2330 &  regoccimerge$isco88 <= 2332] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 2444] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2446 &  regoccimerge$isco88 <= 2450] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 2452 &  regoccimerge$isco88 <= 2455] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 3200] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3220 &  regoccimerge$isco88 <= 3224] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 3226] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3229 &  regoccimerge$isco88 <= 3340] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3460 &  regoccimerge$isco88 <= 3472] <- 14
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 3480] <- 14

# Skilled service (15)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 3225] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3227 &  regoccimerge$isco88 <= 3228] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 3473 &  regoccimerge$isco88 <= 3475] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5000 &  regoccimerge$isco88 <= 5113] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 5122] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5131 &  regoccimerge$isco88 <= 5132] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5140 &  regoccimerge$isco88 <= 5141] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 5143] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5160 &  regoccimerge$isco88 <= 5220] <- 15
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 8323] <- 15

# Low-skilled service (16)

regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5120 &  regoccimerge$isco88 <= 5121] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5123 &  regoccimerge$isco88 <= 5130] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 5133 &  regoccimerge$isco88 <= 5139] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 5142] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 5149] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 5230] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 == 8322] <- 16
regoccimerge$Oesch[regoccimerge$selfem_mainjob == 1 & regoccimerge$isco88 >= 9100 &  regoccimerge$isco88 <= 9152] <- 16


val_labels(regoccimerge$Oesch) <- c("Large employers" = 1,
                                    "Self-employed professionals" = 2,
                                    "Small business owners with employees" = 3,
                                    "Small business owners without employees" = 4,
                                    "Technical experts" = 5,
                                    "Technicians" = 6,
                                    "Skilled manual" = 7,
                                    "Low-skilled manual" = 8,
                                    "Higher-grade managers and administrators" = 9,
                                    "Lower-grade managers and administrators" = 10,
                                    "Skilled clerks" = 11,
                                    "Unskilled clerks" = 12,
                                    "Socio-cultural professionals" = 13,
                                    "Socio-cultural semi-professionals" = 14,
                                    "Skilled service" = 15,
                                    "Low-skilled service" = 16)

table(regoccimerge$Oesch)


# Oesch for imp -----------------------------------------------------------


# Large employers (1)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 4] <- 1

# Self-employed professionals (2)

#regoccimerge$Oeschimp[(regoccimerge$selfem_mainjob == 2 | regoccimerge$selfem_mainjob == 3) & regoccimerge$yrke_imp >= 2000 & regoccimerge$yrke_imp <= 2229] <- 2
#regoccimerge$Oeschimp[(regoccimerge$selfem_mainjob == 2 | regoccimerge$selfem_mainjob == 3) & regoccimerge$yrke_imp >= 2300 & regoccimerge$yrke_imp <= 2470] <- 2

# Small business owners with employees (3)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 3] <- 3

# Small business owners without employees (4)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 2] <- 4

# Technical experts (5)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2100 &  regoccimerge$yrke_imp <= 2213] <- 5

# Technicians (6)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3100 &  regoccimerge$yrke_imp <= 3152] <- 6
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3210 &  regoccimerge$yrke_imp <= 3213] <- 6
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 3434] <- 6

# Skilled manual (7)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 6000 &  regoccimerge$yrke_imp <= 7442] <- 7
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 8310 &  regoccimerge$yrke_imp <= 8312] <- 7
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 8324 &  regoccimerge$yrke_imp <= 8330] <- 7
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 8332 &  regoccimerge$yrke_imp <= 8340] <- 7

# Low-skilled manual (8)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 8000 &  regoccimerge$yrke_imp <= 8300] <- 8
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 8320 &  regoccimerge$yrke_imp <= 8321] <- 8
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 8331] <- 8
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 9153 &  regoccimerge$yrke_imp <= 9333] <- 8

# Higher-grade managers and administrators (9)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 1000 &  regoccimerge$yrke_imp <= 1239] <- 9
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2400 &  regoccimerge$yrke_imp <= 2429] <- 9
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2441] <- 9
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2470] <- 9

# Lower-grade managers and administrators (10)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 1300 &  regoccimerge$yrke_imp <= 1319] <- 10
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3400 &  regoccimerge$yrke_imp <= 3433] <- 10
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3440 &  regoccimerge$yrke_imp <= 3450] <- 10

# Skilled clerks (11)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 4000 &  regoccimerge$yrke_imp <= 4112] <- 11
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 4114 &  regoccimerge$yrke_imp <= 4210] <- 11
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 4212 &  regoccimerge$yrke_imp <= 4222] <- 11

# Unskilled clerks (12)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 4113] <- 12
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 4211] <- 12
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 4223] <- 12

# Socio-cultural professionals (13)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2220 &  regoccimerge$yrke_imp <= 2229] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2300 &  regoccimerge$yrke_imp <= 2320] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2340 &  regoccimerge$yrke_imp <= 2359] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2430 &  regoccimerge$yrke_imp <= 2440] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2442 &  regoccimerge$yrke_imp <= 2443] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2445] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2451] <- 13
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2460] <- 13

# Socio-cultural semi-professionals (14)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2230] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2330 &  regoccimerge$yrke_imp <= 2332] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 2444] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2446 &  regoccimerge$yrke_imp <= 2450] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 2452 &  regoccimerge$yrke_imp <= 2455] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 3200] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3220 &  regoccimerge$yrke_imp <= 3224] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 3226] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3229 &  regoccimerge$yrke_imp <= 3340] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3460 &  regoccimerge$yrke_imp <= 3472] <- 14
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 3480] <- 14

# Skilled service (15)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 3225] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3227 &  regoccimerge$yrke_imp <= 3228] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 3473 &  regoccimerge$yrke_imp <= 3475] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5000 &  regoccimerge$yrke_imp <= 5113] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 5122] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5131 &  regoccimerge$yrke_imp <= 5132] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5140 &  regoccimerge$yrke_imp <= 5141] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 5143] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5160 &  regoccimerge$yrke_imp <= 5220] <- 15
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 8323] <- 15

# Low-skilled service (16)

regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5120 &  regoccimerge$yrke_imp <= 5121] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5123 &  regoccimerge$yrke_imp <= 5130] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 5133 &  regoccimerge$yrke_imp <= 5139] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 5142] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 5149] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 5230] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp == 8322] <- 16
regoccimerge$Oeschimp[regoccimerge$selfem_mainjob == 1 & regoccimerge$yrke_imp >= 9100 &  regoccimerge$yrke_imp <= 9152] <- 16

table(regoccimerge$Oesch)
table(regoccimerge$Oeschimp)

regoccimerge <- mutate(regoccimerge, Oesch = ifelse(is.na(Oesch), Oeschimp, Oesch))

regoccimerge <- select(regoccimerge, -selfem_mainjob, -Oeschimp)

# Missing codes

miss <- filter(regoccimerge, is.na(regoccimerge$Oesch))
tab <- miss %>% group_by(Year, isco88) %>% summarize(freq=n())
write.table(tab, "03 - Results/Oeschmiss.txt")
rm(tab, miss)

# Checking consistency across years

dat <- regoccimerge %>%
  count(Year, Oesch) %>%
  group_by(Year) %>%
  mutate(percent = n / sum(n))

dat$Oesch <- as.factor(dat$Oesch)

ggplot(dat, aes(Oesch, percent, group = Year, colour = as.factor(Year))) +
  geom_line() +
  geom_point()

ggsave("03 - Results/Oeschyears.pdf", plot = last_plot())

# Checking descriptives

regoccimerge$BU <- as.numeric(regoccimerge$BU)
regoccimerge <- mutate(regoccimerge, kjoenn = ifelse(kjoenn == 2, 1, 0))

tab <- regoccimerge %>% 
  subset(Year == 2010) %>% 
  group_by(Oesch) %>% 
  summarise_at(vars(Income, decile, BU, kjoenn), funs(mean(., na.rm = TRUE)))

write.table(tab, "03 - Results/Oeschdesc.txt")

# Keeping occupation at age 50

regoccimerge <- select(regoccimerge, -kjoenn, -BU, -decile, -Income, -yrke_imp)
regoccimerge <- select(regoccimerge, -yrke_imp)

regoccimerge <- filter(regoccimerge, foedselsaar > 1940 & foedselsaar < 1981)

regoccimerge$age <- regoccimerge$Year - regoccimerge$foedselsaar
regoccimerge <- filter(regoccimerge, age < 63)
regoccimerge$dev <- regoccimerge$age - 50
regoccimerge$dev <- abs(regoccimerge$dev)

regoccimerge <- lazy_dt(regoccimerge)

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, dev) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(Oesch50 = first(na.omit(Oesch))) %>% 
  ungroup()

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, dev) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(devOesch = case_when(Oesch50 == Oesch ~ dev)) %>% 
  ungroup()

regoccimerge = regoccimerge %>% 
  arrange(w18_0590_lopenr_person, devOesch) %>% 
  group_by(w18_0590_lopenr_person) %>%
  mutate(devOesch = first(na.omit(devOesch))) %>% 
  ungroup()

regoccimerge <- as_tibble(regoccimerge)

saveRDS(regoccimerge, "N:/durable/projects/class/01 - Data/Oesch.rds")

# Twin data ---------------------------------------------------------------

twins <- fread("N:/durable/prepared/twin-linked-nodups.csv")
nrow(twins)
names(twins)

twins <- select(twins, w18_0590_lopenr_person, Zygo, tvilling_par_ID, kjoenn)
occ <- select(regoccimerge, w18_0590_lopenr_person, foedselsaar, Oesch50, devOesch)
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
  select(twinpair, twinno, Zygo, foedselsaar, sexcomb, Oesch50, devOesch, ant_tvilling)

occtwins_wide <- occtwins %>%
  group_by(twinpair) %>%
  gather(key=var, value=info, Oesch50, devOesch) %>%
  mutate(var = str_c(var,twinno)) %>%
  select(-twinno) %>%
  spread(key=var, value=info)

write.csv(occtwins,"N:/durable/projects/class/01 - Data/Oeschtwins.csv", row.names = FALSE)
write.csv(occtwins_wide,"N:/durable/projects/class/01 - Data/Oeschtwins_wide.csv", row.names = FALSE)

occtwins <- occtwins_wide
rm(occtwins_wide, regoccimerge, occ, twins)
occtwins <- data.frame(occtwins)

# General twin analyses ---------------------------------------------------------------

# Create ordinal

occtwins <- mutate(occtwins, Oesch4c1 = ifelse((Oesch501 == 1 | Oesch501 == 5 | Oesch501 == 9 | Oesch501 == 13), 0, NA))
occtwins <- mutate(occtwins, Oesch4c1 = ifelse((Oesch501 == 6 | Oesch501 == 10 | Oesch501 == 14), 1, Oesch4c1))
occtwins <- mutate(occtwins, Oesch4c1 = ifelse((Oesch501 == 7 | Oesch501 == 11 | Oesch501 == 15), 2, Oesch4c1))
occtwins <- mutate(occtwins, Oesch4c1 = ifelse((Oesch501 == 8 | Oesch501 == 12 | Oesch501 == 16), 3, Oesch4c1))

occtwins <- mutate(occtwins, Oesch4c2 = ifelse((Oesch502 == 1 | Oesch502 == 5 | Oesch502 == 9 | Oesch502 == 13), 0, NA))
occtwins <- mutate(occtwins, Oesch4c2 = ifelse((Oesch502 == 6 | Oesch502 == 10 | Oesch502 == 14), 1, Oesch4c2))
occtwins <- mutate(occtwins, Oesch4c2 = ifelse((Oesch502 == 7 | Oesch502 == 11 | Oesch502 == 15), 2, Oesch4c2))
occtwins <- mutate(occtwins, Oesch4c2 = ifelse((Oesch502 == 8 | Oesch502 == 12 | Oesch502 == 16), 3, Oesch4c2))

occtwins[, "Oesch4c1"] = umxFactor(occtwins[, "Oesch4c1"])
occtwins[, "Oesch4c2"] = umxFactor(occtwins[, "Oesch4c2"])

mzOesch <- na.omit(subset(occtwins, Zygo ==  1))
dzOesch <- na.omit(subset(occtwins, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Ordinal models
m1 <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxSummary(m1)

umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)



# Models per gender -------------------------------------------------------

mzOeschman <- filter(mzOesch, sexcomb == "Males")
dzOeschman <- filter(dzOesch, sexcomb == "Males")
mzOeschwoman <- filter(mzOesch, sexcomb == "Females")
dzOeschwoman <- filter(dzOesch, sexcomb == "Females")

# Ordinal models

m1man <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschman, mzData = mzOeschman, bound = NULL, intervals = TRUE)
umxSummary(m1man)

umxConfint(m1man, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1woman <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschwoman, mzData = mzOeschwoman, bound = NULL, intervals = TRUE)
umxSummary(m1woman)

umxConfint(m1woman, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)


# Models per age ----------------------------------------------------------

mzOeschung <- filter(mzOesch, foedselsaar > 1960)
dzOeschung <- filter(dzOesch, foedselsaar > 1960)
mzOeschgaml <- filter(mzOesch, foedselsaar < 1961)
dzOeschgaml <- filter(dzOesch, foedselsaar < 1961)

# Ordinal models

m1ung <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschung, mzData = mzOeschung, bound = NULL, intervals = TRUE)
umxSummary(m1ung)

umxConfint(m1ung, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)

m1gaml <- umxACE(selDVs = c("Oesch4c"), selCov = "devOesch", sep = "", dzData = dzOeschgaml, mzData = mzOeschgaml, bound = NULL, intervals = TRUE)
umxSummary(m1gaml)

umxConfint(m1gaml, parm = c("top.A_std", "top.C_std", "top.E_std", "top.A", "top.C", "top.E"), 
           run = TRUE, showErrorCodes = TRUE)



# Binary analyses -----------------------------------------------------------

occtwins <- fread("N:/durable/projects/class/01 - Data/Oeschtwins_wide.csv")

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

mzOesch <- na.omit(subset(occtwins, Zygo ==  1))
dzOesch <- na.omit(subset(occtwins, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Binary models

m1 <- umxACE(selDVs = c("Oeschb1"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb2"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb3"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb4"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb5"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb6"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb7"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb8"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb9"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb10"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb11"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb12"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)


# Binary analyses with inactive -------------------------------------------

occtwins <- fread("N:/durable/projects/class/01 - Data/Oeschtwins_wide.csv")

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

mzOesch <- na.omit(subset(occtwins, Zygo ==  1))
dzOesch <- na.omit(subset(occtwins, Zygo ==  2))
mzOesch <- data.frame(mzOesch)
dzOesch <- data.frame(dzOesch)

# Binary models

m1 <- umxACE(selDVs = c("Oeschb1"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb2"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb3"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb4"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb5"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb6"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, bound = NULL, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb7"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb8"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb9"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb10"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb11"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb12"), selCov = "devOesch", sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)

m1 <- umxACE(selDVs = c("Oeschb13"), sep = "", dzData = dzOesch, mzData = mzOesch, intervals = TRUE)
umxConfint(m1, parm = c("top.A_std", "top.C_std", "top.E_std"), 
           run = TRUE, showErrorCodes = TRUE)
