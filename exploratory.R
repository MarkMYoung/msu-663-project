#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
if(!require(knitr)) install.packages("knitr")# this document
if(!require(rmarkdown)) install.packages("rmarkdown")# this document
if(!require(yaml)) install.packages("yaml")# this document

if(!require(dplyr)) install.packages("dplyr")# mutate
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggpubr)) install.packages("ggpubr")# ggqqplot, ggscatter
if(!require(magrittr)) install.packages("magrittr")# %>% operator
if(!require(moments)) install.packages("moments")# kurtosis, skewness
if(!require(nortest)) install.packages("nortest")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(reshape2)) install.packages("reshape2")
if(!require(sjPlot)) install.packages("sjPlot")
if(!require(stargazer)) install.packages("stargazer")
if(!require(tidyr)) install.packages("tidyr")# separate_longer_delim
if(!require(tidytext)) install.packages("tidytext")# unnest_tokens
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Turn off scientific notation.
#options(scipen=999)
# Turn on scientific notation.
options(scipen=0)

wd <- "G:/My Drive/2024-01-CIS_663-Dev_Analytics_Apps/msu-663-project"
setwd(wd)

dat_file_name <- "stack-overflow-developer-survey-2022.survey_results_public.csv"
dat_raw <- read.csv(dat_file_name)
#head(dat_raw)
#summary(dat_raw)

# keep_column_names_2023 <- c(
#   "Q120",
#   "MainBranch",
#   "Age",
#   "Employment",
#   "RemoteWork",
#   "CodingActivities",
#   "EdLevel",
#   "YearsCode",
#   "YearsCodePro",
#   "DevType",
#   "OrgSize",
#   "Country",
#   "Currency",
#   "CompTotal",
#   "LanguageHaveWorkedWith",
#   "DatabaseHaveWorkedWith",
#   "PlatformHaveWorkedWith",
#   "WebframeHaveWorkedWith",
#   "MiscTechHaveWorkedWith",
#   "ToolsTechHaveWorkedWith",
#   "NEWCollabToolsHaveWorkedWith",
#   "OfficeStackAsyncHaveWorkedWith",
#   "OfficeStackSyncHaveWorkedWith",
#   "AISearchHaveWorkedWith",
#   "AIDevHaveWorkedWith",
#   "Industry",
#   "ConvertedCompYearly"
# )
keep_column_names_2022 <- c(
  "ResponseId",
#  "MainBranch",
  "Employment",
  "RemoteWork",
#  "CodingActivities",
  "EdLevel",
#  "YearsCode",
  "YearsCodePro",
  "DevType",
  "OrgSize",
  "Country",
#  "Currency",
#  "CompTotal",
#  "LanguageHaveWorkedWith",
#  "DatabaseHaveWorkedWith",
#  "PlatformHaveWorkedWith",
#  "WebframeHaveWorkedWith",
#  "MiscTechHaveWorkedWith",
#  "ToolsTechHaveWorkedWith",
#  "NEWCollabToolsHaveWorkedWith",
#  "VersionControlSystem",
#  "OfficeStackAsyncHaveWorkedWith",
#  "OfficeStackSyncHaveWorkedWith",
  "Age",
  "Gender",
# "Trans",
#  "Sexuality",
  "Ethnicity",
  "ConvertedCompYearly"
)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Only keep columns of interest.
dat_selected <- dat_raw[keep_column_names_2022]

# Transform numerical variables.  Capture logarithmic values in case it is a multimodal distribution that needs to be split.
if("ConvertedCompYearly" %in% colnames(dat_selected))
{
  dat_selected$ConvertedCompYearly.n <- as.numeric(as.character(dat_selected$ConvertedCompYearly))
  dat_selected$ConvertedCompYearly.log <- log(dat_selected$ConvertedCompYearly.n)
}

# Transform categorical variables to factors ('.f' suffix for "factor").
if("Country" %in% colnames(dat_selected))
{ dat_selected$Country.f <- factor(dat_selected$Country) }
#if("DevType" %in% colnames(dat_selected))
#{ dat_selected$DevType.f <- factor(dat_selected$DevType) }
if("Employment" %in% colnames(dat_selected))
{ dat_selected$Employment.f <- factor(dat_selected$Employment) }
if("Ethnicity" %in% colnames(dat_selected))
{ dat_selected$Ethnicity.f <- factor(dat_selected$Ethnicity) }

# Mapping all values that are not "Man", "Woman", or NA to "Other".
if("Gender" %in% colnames(dat_selected))
{
  #dat_selected$Gender <- as.character(dat_selected$Gender)
  # Copy variable.
  dat_selected$Gender.d <- dat_selected$Gender
  # Replace all values that are not "Woman", or NA with "Other".
  dat_selected$Gender.d[!dat_selected$Gender %in% c("Woman", NA)] <- "Other"
  # Assign dummy values.
  dat_selected$Gender.d[dat_selected$Gender.d == "Other"] <- 0
  dat_selected$Gender.d[dat_selected$Gender.d == "Woman"] <- 1
  # Explicitly convert.
  dat_selected$Gender.d <- as.integer(dat_selected$Gender.d)
  dat_selected$Gender.d <- factor(
    dat_selected$Gender.d,
    levels=c(0, 1),
    labels=c("Other", "Female")
  )
}
if("RemoteWork" %in% colnames(dat_selected))
{ dat_selected$RemoteWork.f <- factor(dat_selected$RemoteWork) }

# Transform ordinal variables with ordered ('.o' suffix for "ordinal").
if("Age" %in% colnames(dat_selected))
{
  # Explicitly convert.
  dat_selected$Age <- as.character(dat_selected$Age)
  #sum(is.na(dat_selected$Age))
  # Copy column.
  dat_selected$Age.o <- dat_selected$Age
  #sum(is.na(dat_selected$Age.o))
  # Replace "Prefer not to say" with NA.
  prefer_not_to_say <- "Prefer not to say"
  dat_selected$Age.o[dat_selected$Age.o == prefer_not_to_say] <- NA
  # Replace "Under 18 years old" with "17 years or younger"
  dat_selected$Age.o[dat_selected$Age.o == "Under 18 years old"] <- "17 years or younger"
  
  #summary(dat_selected$Age.o)

  #dat_selected$Age.o <- ordered(dat_selected$Age.o, exclude=NULL, 
  #  levels=c(
  #    #"Prefer not to say",
  #    "Under 18 years old",
  #    "18-24 years old",
  #    "25-34 years old",
  #    "35-44 years old",
  #    "45-54 years old",
  #    "55-64 years old",
  #    "65 years or older"
  #  )
  #)
  dat_selected$Age.o <- ordered(dat_selected$Age.o)
}
if("EdLevel" %in% colnames(dat_selected))
{
  # Explicitly convert.  
  dat_selected$EdLevel <- as.character(dat_selected$EdLevel)
  #sum(is.na(dat_selected$EdLevel))
  # Copy column.
  dat_selected$EdLevel.o <- dat_selected$EdLevel
  #sum(is.na(dat_selected$EdLevel.o))
  # Remove Unicode from values.
  dat_selected$EdLevel.o <- gsub("[^[:alnum:]///' .,-\\(\\)]", "", dat_selected$EdLevel.o)
  # Replace lengthy values.
  dat_selected$EdLevel.o[dat_selected$EdLevel.o == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"] <- "Secondary school/High school"
  dat_selected$EdLevel.o[dat_selected$EdLevel.o == "Some college/university study without earning a degree"] <- "Some college/university"
  
  #unique(dat_selected$EdLevel.o)
  dat_selected$EdLevel.o <- ordered(dat_selected$EdLevel, exclude=NULL, levels=c(
    NA,
    "Primary/elementary school",
    #"Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
    "Secondary school/High school",
    #"Some college/university study without earning a degree",
    "Some college/university",
    "Something else",
    "Associate degree (A.A., A.S., etc.)",
    "Bachelors degree (B.A., B.S., B.Eng., etc.)",
    "Masters degree (M.A., M.S., M.Eng., MBA, etc.)",
    "Professional degree (JD, MD, Ph.D, Ed.D, etc.)"
  ))
}
# This function returns the values of a list (like `names` returns the keys).
values <- function(keyValuePairList)
{
  vec <- c()
  for(key in names(keyValuePairList))
  {
    val <- keyValuePairList[[key]]
    vec <- append(vec, val)
  }
  vec
}
if("OrgSize" %in% colnames(dat_selected))
{
  # Explicitly convert.
  #dat_selected$OrgSize <- as.character(dat_selected$OrgSize)
  #sum(is.na(dat_selected$OrgSize))
  # Copy variable.
  dat_selected$OrgSize.log <- dat_selected$OrgSize
  #sum(is.na(dat_selected$OrgSize.log))
  # Remove Unicode from values.
  dat_selected$OrgSize.log <- gsub("[^[:alnum:]///' .,-]", "", dat_selected$OrgSize.log)
  # Assign dummy values.
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "I dont know"] <- NA
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "Just me - I am a freelancer, sole proprietor, etc."] <- log(1)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "2 to 9 employees"] <- log(5)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "10 to 19 employees"] <- log(15)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "20 to 99 employees"] <- log(60)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "100 to 499 employees"] <- log(300)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "500 to 999 employees"] <- log(750)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "1,000 to 4,999 employees"] <- log(3000)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "5,000 to 9,999 employees"] <- log(7500)
  dat_selected$OrgSize.log[dat_selected$OrgSize.log == "10,000 or more employees"] <- log(30000)
  # Explicitly convert.
  dat_selected$OrgSize.log <- as.integer(dat_selected$OrgSize.log)
  
  # Copy to logarithmic variable.
  dat_selected$OrgSize.log <- log(dat_selected$OrgSize.log)
  
  #unique(dat_selected$OrgSize.log)
  dat_selected$OrgSize.log <- ordered(
    dat_selected$OrgSize.log,
    exclude=NULL,
    levels=c(
      0, 1, 5, 15, 60,
      300, 750, 3000, 7500, 30000
    ),
    labels=c(
      "Unknown", "1", "2--9", "10--19", "20--99",
      "100--499", "500--999", "1,000--4,999", "5,000--9,999", "10,000+"
    )
  )
}

# Only 2023 dataset.
if("Industry" %in% colnames(dat_selected))
{ dat_selected$Industry.f <- factor(dat_selected$Industry) }

# Transform factor variables to numeric.
#dat_selected$DevType <- as.numeric(dat_selected$DevType)
#dat_selected$Industry <- as.numeric(dat_selected$Industry)

# Count missing values (2022 dataset has 48% missing).
ConvertedCompYearly.sumNA <- sum(is.na(dat_selected$ConvertedCompYearly.n))
ConvertedCompYearly.mean <- mean(dat_selected$ConvertedCompYearly.n, na.rm=TRUE)
dat_selected$ConvertedCompYearly.imputed <- dat_selected$ConvertedCompYearly.n
dat_selected$ConvertedCompYearly.imputed[is.na(dat_selected$ConvertedCompYearly.imputed)] <- ConvertedCompYearly.mean
#dat_selected$ConvertedCompYearly.completed = dat_selected$ConvertedCompYearly.n
#na.omit(dat_selected$ConvertedCompYearly.completed)
if("YearsCodePro" %in% colnames(dat_selected))
{ dat_selected$YearsCodePro.n <- as.numeric(as.character(dat_selected$YearsCodePro)) }
# Count missing values (2022 dataset has 32% missing).
YearsCodePro.sumNA <- sum(is.na(dat_selected$YearsCodePro.n))
YearsCodePro.mean <- mean(dat_selected$YearsCodePro.n, na.rm=TRUE)
dat_selected$YearsCodePro.imputed <- dat_selected$YearsCodePro.n
dat_selected$YearsCodePro.imputed[is.na(dat_selected$YearsCodePro.imputed)] <- YearsCodePro.mean
#dat_selected$YearsCodePro.completed = dat_selected$YearsCodePro.n
#na.omit(dat_selected$YearsCodePro.completed)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Split composite value DevType for analysis.
# splitDevType <- TRUE
# if(splitDevType)
# {
#   dat_split <- dat_selected %>% separate_longer_delim(c(DevType), delim=";")
#   if("DevType" %in% colnames(dat_split))
#   { dat_split$DevType.f <- factor(dat_split$DevType) }
#   dat_selected <- dat_split
# } else {
#   if("DevType" %in% colnames(dat_selected))
#   { dat_selected$DevType.f <- factor(dat_selected$DevType) }
# }
if("DevType" %in% colnames(dat_selected))
{
  # Split.
  dat_selected <- dat_selected %>% separate_longer_delim(c(DevType), delim=";")
  # Replace.
  dat_selected$DevType[dat_selected$DevType == "Cloud infrastructure engineer"] <- "Cloud infrastructure"
  dat_selected$DevType[dat_selected$DevType == "Data scientist or machine learning specialist"] <- "Data scientist"
  dat_selected$DevType[dat_selected$DevType == "Developer, desktop or enterprise applications"] <- "Developer, applications"
  dat_selected$DevType[dat_selected$DevType == "Developer, embedded applications or devices"] <- "Developer, embedded"
  dat_selected$DevType[dat_selected$DevType == "Developer, game or graphics"] <- "Developer, games/graphics"
  dat_selected$DevType[dat_selected$DevType == "Marketing or sales professional"] <- "Marketing or sales"
  dat_selected$DevType[dat_selected$DevType == "Senior Executive (C-Suite, VP, etc.)"] <- "Senior Executive"
  # Capture the frequency count for ordering and filtering.
  dev_type_cnt <- plyr::count(dat_selected$DevType)
  # Transform.
  dat_selected$DevType.f <- factor(dat_selected$DevType,
    levels=dev_type_cnt$x[order(dev_type_cnt$freq, decreasing=FALSE)]
  )
}
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Only keep rows with Compensation and Industry values.
#sum(is.na(dat_selected$DevType.f))
dat_sliced <- dat_selected[
  !is.na(dat_selected$ConvertedCompYearly.n)
  & !is.na(dat_selected$DevType.f)
  # Only 2022 dataset.
  #& !is.na(dat_selected$Ethnicity.f)
  # Only 2022 dataset.
  & !is.na(dat_selected$Gender.d)
  # Only 2023 dataset.
  #& !is.na(dat_selected$Industry.f)
  & !is.na(dat_selected$OrgSize.log)
  & !is.na(dat_selected$YearsCodePro.n)
  # Be sure to specify an comma at the end to avoid "undefined columns selected" error.
  ,
]
#dat_sliced <- filter(dat_selected, !is.na(dat_selected$ConvertedCompYearly.n) & !is.na(dat_selected$DevType))

# Remove outliers (values not programmatically calculated at this time).
mean <- mean(dat_sliced$ConvertedCompYearly.n)
std_dev <- sd(dat_sliced$ConvertedCompYearly.n)
#dat_sliced <- dat_sliced[dat_sliced$ConvertedCompYearly.n < 3 * std_dev,]
dat_sliced <- dat_sliced[dat_sliced$ConvertedCompYearly.n < 250000,]
#kurtosis(dat_sliced$ConvertedCompYearly.n)
#skewness(dat_sliced$ConvertedCompYearly.n)
#max(dat_sliced$ConvertedCompYearly.n)
#ConvertedCompYearly.imputed.std_dev <- sd(dat_sliced$ConvertedCompYearly.imputed)
#dat_sliced %>% filter(
#  dat_sliced$ConvertedCompYearly.imputed > ConvertedCompYearly.mean + ConvertedCompYearly.imputed.std_dev * 3
#  | dat_sliced$ConvertedCompYearly.imputed < ConvertedCompYearly.mean - ConvertedCompYearly.imputed.std_dev * 3
#)
# dat_sliced <- dat_sliced[
#   !is.na(dat_sliced$ConvertedCompYearly.n)
# 
#   & !is.na(dat_sliced$YearsCodePro.n)
#   & !is.na(dat_sliced$DevType.f)
#   & !is.na(dat_sliced$Gender.d)
#   #& !is.na(dat_sliced$Country.f)
#   & !is.na(dat_sliced$Age.o)
#   #& !is.na(dat_sliced$EdLevel.o)
#   & !is.na(dat_sliced$Employment.f)
#   & !is.na(dat_sliced$RemoteWork.f)
#   & !is.na(dat_sliced$OrgSize.log)
#   # Only 2022 dataset.
#   #& !is.na(dat_sliced$Ethnicity.f)
#   
#   # Only 2023 dataset.
#   #& !is.na(dat_sliced$Industry.f)
# 
#   # Be sure to specify an comma at the end to avoid "undefined columns selected" error.
#   ,
# ]

# Labels
#labels <- c(
#  Age.o="Age Range",
#  ConvertedCompYearly.n="Yearly Comp.",
#  Country.f="Country",
#  DevType.f="Dev. Type",
#  EdLevel.o="Edu. Level",
#  Ethnicity.f="Ethnicity",
#  Gender.d="Gender",
#  OrgSize.log="Org. Size",
#  YearsCodePro.n="Years Coding Pro."
#)
#label(dat_sliced) <- as.list(labels[match(names(dat_sliced), names(labels))])
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Tests.
#sum(is.na(dat_sliced$ConvertedCompYearly.n))
#Figure
f.comp <- hist(
  dat_sliced$ConvertedCompYearly.n,
  main="Compensation Frequency Histogram",
  xlab="Annual Compensation",
  breaks=100,
  # Turn off default x-axis labels.
  xaxt="n"
)
# Customize x-axis labels.
axis(side=1, at=f.comp$breaks,
  labels=paste("$", ceiling(f.comp$breaks / 1000), "k", sep="")
)
ggplot(dat_sliced,
  aes(x=ConvertedCompYearly.n, y=..count..)
) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  labs(title="Compensation Frequency Histogram", x="Annual Compensation", y="Frequency")


#Figure
ggdensity(dat_sliced$ConvertedCompYearly.n,
  main="Yearly Compensation Density Plot",
  xlab="Yearly Compensation"
) +
geom_vline(aes(xintercept=median(dat_sliced$ConvertedCompYearly.n)),
  colour="darkgray", linetype="dashed"
)

#Figure
f.years <- hist(
  dat_sliced$YearsCodePro.n,
  main="Years Code Pro. Frequency Histogram",
  xlab="Years Coding Professionally",
  breaks=50
)
#Figure
ggdensity(dat_sliced$YearsCodePro.n,
  main="Years Code Pro Density Plot",
  xlab="Years Coding Professionally"
) +
geom_vline(aes(xintercept=median(dat_sliced$YearsCodePro.n)),
  colour="darkgray", linetype="dashed"
)

#Figure
f.gender <- barplot(
  table(dat_sliced$Gender.d),
  main="Gender Frequency Bar Chart",
  xlab="Gender"
)

#Figure
f.orgsize <- barplot(
  table(dat_sliced$OrgSize.log),
  main="Org. Size Frequency Bar Chart",
  xlab="Organization Size"
)

#Figure
par(mar=c(5.1, max(4.1, 17/1.8 ), 4.1, 2.1))
f.devtype <- barplot(
  table(dat_sliced$DevType.f),
  main="Dev. Type Frequency Bar Chart",
  xlab="Frequency",
  horiz=TRUE,
  # labels always
  # * 0: always parallel to the axis
  # * 1: always horizontal
  # * 2: always perpendicular to the axis
  # * 3: always vertical
  las=1,
  # Scale axis labels so all will be displayed.
  cex.names=0.7
)

h.comp.imputed <- hist(
  dat_sliced$ConvertedCompYearly.imputed,
  main="Compensation Imputed Histogram",
  xlab="Annual Compensation",
  breaks=50,
  # Turn off x-axis labels.
  xaxt="n"
)
axis(side=1, at=h.comp.imputed$breaks, labels=paste("$", ceiling(h.comp.imputed$breaks / 1000), "k", sep=""))

#Figure
h.comp.log <- hist(
  dat_sliced$ConvertedCompYearly.log,
  main="Compensation Logarithm Histogram",
  xlab="Annual Compensation",
  breaks=100,
  # Turn off x-axis labels.
  xaxt="n"
)
axis(side=1, at=h.comp.log$breaks, labels=paste(ceiling(h.comp.log$breaks), sep=""))
#Figure
ggdensity(dat_sliced$ConvertedCompYearly.log, main="Yearly Compensation Logarithm Density Plot", xlab="Yearly Compensation")

#Figure
ggqqplot(dat_sliced$ConvertedCompYearly.n, main="Yearly Compensation Q-Q Plot", xlab="Theoretical Quantiles")
#ggqqplot(dat_sliced$ConvertedCompYearly.log, main="Yearly Compensation Logarithm Q-Q Plot", xlab="Theoretical Quantiles")
# Shapiro-Wilk's method `shapiro.test` supports sample size 3--5000.
# Kolmogorov-Smirnov test does not support "ties"/duplicate values.
# Anderson-Darling test (supports sample size of 7+).
ad.test(dat_sliced$ConvertedCompYearly.n)
ad.test(dat_sliced$ConvertedCompYearly.log)
# Variance.
#var(dat_sliced$ConvertedCompYearly.n)
# Right-skewed (means, tail to the right).
skewness(dat_sliced$ConvertedCompYearly.n)
# Kurtosis > 3 (leptokurtic) (means, more outliers than normal distribution).
kurtosis(dat_sliced$ConvertedCompYearly.n)

#hist(dat_sliced$ConvertedCompYearly.log, main="Compensation Histogram", xlab="Annual Compensation", breaks=100)
#plot(density(dat_sliced$ConvertedCompYearly.log))

#dat_density <- density(dat_sliced$ConvertedCompYearly.log)
#lines(dat_density, col="darkblue")
#ggplot(data=melt(dat_sliced), aes(y=dat_sliced$ConvertedCompYearly.n, x=Industry)) + geom_boxplot(aes(fill=Industry))
#ggplot(dat_sliced, aes(y=ConvertedCompYearly.n), x=factor(Industry)) + geom_boxplot()
#ggplot(dat_sliced, aes(y=ConvertedCompYearly.n), x=Industry) + geom_point() + geom_smooth(method="lm") + labs(y="Compensation", x="Industry")
#ggscatter(dat_sliced, y="ConvertedCompYearly.n", x="YearsCodePro.n", color="Industry", add="reg.line") #+ stat_regline_equation()
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
# Calculate...
#cor(dat_sliced$ConvertedCompYearly.n, dat_sliced$Industry, use="na.or.complete")
#linear_model <- lm()

# Not using (uppercase) `C`, `contrasts`, and/or `contr.treatment` for "contrast treatments" at this time.
# Use ANCOVA?

head(dat_sliced$DevType.f)
dat_sliced %>%
  tidytext::unnest_tokens(word, DevType, token="regex", pattern=";") %>%
  group_by(word) %>%
  count() %>%
  arrange(desc(n))

linearModelCompensationVersus <- function(data, depVar, indVar)
{
  # Dependent variable to the left of the (binary) '~' operator.
  # Independent variables to the right of the (binary) '*' operator.
  # Control variables to the right of the (binary) '+' operator.
  lm(
    # Dependent variable versus...
    data[, depVar] ~
      # Independent variable.
      data[, indVar] +
      #YearsCodePro.n * # Significant!
      ## Independent variable.
      #DevType.f * # SOME are significant. 
      ##Country.f + # NOT significant.
      ## Independent variable.
      #Gender.d * # Significant. 
      ##Ethnicity.f, # SOME are significant.
      ## Independent variable.
      #Age.o + # NOT significant.
      # Control variables.
      # Employment.f would have to be split to be considered.
      Age.o + EdLevel.o + OrgSize.log,
    # data.frame
    data=data
  )
}

#proc_time <- proc.time()

# There was statistically insignificant (Adj. R^2 0.144, d.f. 1644) upward trend in compensation.
#compensationVersusOrgSize_lm <- linearModelCompensationVersus(dat_sliced, "ConvertedCompYearly.n", "OrgSize.log")
#summary(compensationVersusOrgSize_lm)

#Result?
compensationVersusDevType_lm <- lm(
  # Dependent variable versus (~) independent variable(s).
  ConvertedCompYearly.n ~ DevType.f,
  data=dat_sliced
)
#summary(compensationVersusDevType_lm)
compensationVersusDevType_coeff <- coefficients(compensationVersusDevType_lm)
compensationVersusDevType_intercept <- compensationVersusDevType_coeff[1]
compensationVersusDevType_slope <- compensationVersusDevType_coeff[2]
compensationVersusDevType_report <- capture.output(
  stargazer(compensationVersusDevType_lm, type="html", align=TRUE),
  file="comp_vs_dev_type_lm_report.html"
)
#Result?
compensationVersusGender_lm <- lm(
  # Dependent variable versus (~) independent variable(s).
  ConvertedCompYearly.n ~ Gender.d,
  data=dat_sliced
)
summary(compensationVersusGender_lm)
compensationVersusGender_report <- capture.output(
  stargazer(compensationVersusGender_lm, type="html", align=TRUE),
  file="comp_vs_gender_lm_report.html"
)
#Result?
compensationVersusOrgSize_lm <- lm(
  # Dependent variable versus (~) independent variable(s).
  ConvertedCompYearly.n ~ OrgSize.log,
  data=dat_sliced
)
summary(compensationVersusOrgSize_lm)
#Result?
compensationVersusYears_lm <- lm(
  # Dependent variable versus (~) independent variable(s).
  ConvertedCompYearly.n ~ YearsCodePro.n,
  data=dat_sliced
)
summary(compensationVersusYears_lm)
compensationVersusYears_report <- capture.output(
  stargazer(compensationVersusYears_lm, type="html", align=TRUE),
  file="comp_vs_years_lm_report.html"
)
#Table
html_file_name <- "compensation_regression_models_comparison.html"
sjPlot::tab_model(
  compensationVersusYears_lm,
  compensationVersusGender_lm,
  compensationVersusDevType_lm,
  file=html_file_name
)
htmltools::includeHTML(html_file_name)

#Result
compensationVersusSignificants_lm <- lm(
  # Dependent variable versus...
  ConvertedCompYearly.n ~
    # Independent variables.
    YearsCodePro.n + OrgSize.log + Gender.d + YearsCodePro.n + DevType.f
    #YearsCodePro.n * # Significant!
    ## Independent variable.
    #DevType.f * # SOME are significant. 
    ##Country.f + # NOT significant.
    ## Independent variable.
    #Gender.d * # Significant. 
    ##Ethnicity.f, # SOME are significant.
    ## Independent variable.
    #Age.o + # NOT significant.
    # Control variables.
    # Employment.f would have to be split to be considered.
  #Age.o + EdLevel.o + OrgSize.log
  ,
  # data.frame
  data=dat_sliced
)
summary(compensationVersusSignificants_lm)
compensationVersusOthers_report <- capture.output(
  stargazer(compensationVersusSignificants_lm, type="html", align=TRUE),
  file="comp_vs_others_lm_report.html"
)

#Figure
ggscatter(
  dat_sliced, y="ConvertedCompYearly.n", x="YearsCodePro.n",
  color="black", size=0.75,
  conf.int=TRUE,
  add.params=list(color="darkblue", fill="lightblue", size=0.5),
  #color="group",
  add="reg.line"
) +
  stat_cor(method="pearson", label.x=30, label.y=-10000) +
  labs(title="Yearly Compensation vs. Years Coding Professional\nScatter Plot with Linear Regression Line") +
  scale_y_continuous(name="Yearly Compensation ($USD)", labels=scales::dollar_format())
#Figure
ggscatter(
  dat_sliced, y="ConvertedCompYearly.n", x="YearsCodePro.n",
  color="black", size=0.75,
  conf.int=TRUE,
  add.params=list(color="darkblue", fill="lightblue"),
  #color="group",
  add="loess"
) +
  stat_cor(method="pearson", label.y=-10000, label.x=30) +
  labs(title="Yearly Compensation vs. Years Coding Professional\nScatter Plot with LOESS Regression Line") +
  scale_y_continuous(name="Yearly Compensation ($USD)", labels=scales::dollar_format())
#Figure
ggscatter(
  dat_sliced, y="ConvertedCompYearly.n", x="OrgSize.log",
  color="black", size=0.75,
  conf.int=TRUE,
  add.params=list(color="darkblue", fill="lightblue", size=0.5),
  #color="group",
  add="reg.line"
) +
  stat_cor(method="pearson", label.x=6, label.y=-10000) +
  labs(title="Yearly Compensation vs. Organization Size\nScatter Plot with Linear Regression Line") +
  scale_y_continuous(name="Yearly Compensation ($USD)", labels=scales::dollar_format())
#Figure
# OrgSize.log is not statistically significant, but an upward trend can be seen.
dat_comp_vs_org_df <- dat_sliced[c("ConvertedCompYearly.n", "OrgSize.log")]
#max(dat_comp_vs_org$ConvertedCompYearly.n)
#sum(is.na(dat_comp_vs_org$ConvertedCompYearly.n))
violin <- ggplot(dat_comp_vs_org_df, aes(y=ConvertedCompYearly.n, x=OrgSize.log))
violin +
  geom_violin() + #draw_quantiles=c(0.25, 0.5, 0.75)
  geom_boxplot(width=0.1, aes(middle=mean(ConvertedCompYearly.n))) +
  #geom_smooth(mapping=aes(y=ConvertedCompYearly.n, x=OrgSize.log), color="black", method="lm", linewidth=1, se=T) +
  labs(title="Compensation vs. Org. Size Violin Plot", x="Org. Size (employees)") +
  scale_y_continuous(name="Yearly Compensation ($USD)", labels=scales::dollar_format()) +
  theme(axis.text.x=element_text(angle=30, hjust=1), plot.title=element_text(hjust=0.5))

#run_time <- proc.time() - proc_time

compVsOrgSizeLM <-lm(ConvertedCompYearly.imputed ~ OrgSize, data=dat_sliced)
stargazer(compVsOrgSizeLM, type="text", align=TRUE)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
median(dat_sliced[dat_sliced$Gender.f=="Female",]$ConvertedCompYearly)
median(dat_sliced[dat_sliced$Gender.f=="Other",]$ConvertedCompYearly)