#install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("openxlsx")
install.packages("reshape2")

#require(dplyr)
require(ggplot2)
library(knitr)
library(openxlsx)
library(reshape2)

wd <- "G:/My Drive/2024-01-CIS_663-Dev_Analytics_Apps/msu-663-project"
setwd(wd)

dat_file_name <- "survey_results_public-2023.csv"
dat_raw = read.csv(dat_file_name)
#head(dat_raw)
#summary(dat_raw)

keep_column_names = c(
  "Q120",
  "MainBranch",
  "Age",
  "Employment",
  "RemoteWork",
  "CodingActivities",
  "EdLevel",
  "YearsCode",
  "YearsCodePro",
  "DevType",
  "OrgSize",# Convert scale with ln
  "Country",
  "Currency",
  "CompTotal",
  "LanguageHaveWorkedWith",
  "DatabaseHaveWorkedWith",
  "PlatformHaveWorkedWith",
  "WebframeHaveWorkedWith",
  "MiscTechHaveWorkedWith",
  "ToolsTechHaveWorkedWith",
  "NEWCollabToolsHaveWorkedWith",
  # "OpSysPersonal use",
  # "OpSysProfessional use",
  "OfficeStackAsyncHaveWorkedWith",
  "OfficeStackSyncHaveWorkedWith",
  "AISearchHaveWorkedWith",
  "AIDevHaveWorkedWith",
  "Industry",
  "ConvertedCompYearly"
)
# Only keep columns of interest.
dat_selected = dat_raw[keep_column_names]

# Convert categorical variables to factors.
dat_selected$DevType <- factor(dat_selected$DevType)
dat_selected$Industry <- factor(dat_selected$Industry)
# Convert factor variables to numeric.
#dat_selected$DevType <- as.numeric(dat_selected$DevType)
#dat_selected$Industry <- as.numeric(dat_selected$Industry)

# Only keep rows with compensation values.
dat_sliced = dat_selected[
  is.numeric(dat_selected$ConvertedCompYearly)
  #& !is.na(dat_selected$DevType)
  & !is.na(dat_selected$Industry),
]
#dat_sliced = filter(dat_selected, !is.na(dat_selected$ConvertedCompYearly) & !is.na(dat_selected$DevType))

#cor(dat_sliced$ConvertedCompYearly, dat_sliced$Industry, use="na.or.complete")

#TODO remove outliers

# Plot Compensation by Industry.
#ggplot(data=melt(dat_sliced), aes(x=Industry, y=dat_sliced$ConvertedCompYearly)) + geom_boxplot(aes(fill=Industry))
#ggplot(dat_sliced, aes(x=factor(Industry), y=ConvertedCompYearly)) + geom_boxplot()
#ggplot(dat_sliced, aes(x=Industry, y=ConvertedCompYearly)) + geom_point() + geom_smooth(method="lm") + labs(x="Industry", y="Compensation")
