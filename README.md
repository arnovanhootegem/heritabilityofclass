# heritabilityofclass
R scripts for paper on the Heritability of Class by Arno Van Hootegem, Adrian Farner Rogne & Torkild Hovde Lyngstad

# My R Scripts

This repository contains a collection of R scripts I've made for the paper "Heritability of class", written by Arno Van Hootegem, Adrian Farner Rogne and Torkild Hovde Lyngstad. 
It constructs class schemes (EGP, Oesch, ORDC and SIOPS) based on Norwegian registry data, which are then analysed using twin models (metric, ordinal and binary) to estimate the heritability of class.

# Scripts

- `data_preperation.R`: Prepares and merges data that is used for the analysis
- `EGP.R`: Creates the EGP scheme and conducts ordinal as well as binary twin analyses
- `Oesch.R`: Creates the Oesch scheme and conducts ordinal as well as binary twin analyses
- `ORDC.R`: Creates the ORDC scheme and conducts ordinal as well as binary twin analyses
- `Treiman.R`: Creates the SIOPS and conducts metric twin analyses
- `Descriptives.R`: Generates descriptive statistics for the full population, monozygotic and dizygotic twins

# Data

Norwegian privacy regulations limit our ability to share our register data. 
Individual researchers may apply to obtain permissions and subsequently access the data. 
We can provide guidance on how to request access to these data. 

# Feedback

Feel free to reach out if you have suggestions to improve the code or if there are things that are not understandable
