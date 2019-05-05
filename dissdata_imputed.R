#Examining the linear mixed effects
#Called mixed effects because they have both fixed and random effects factors
#fixed effect factor has levels that do not represent a sample from a population
#of possible levels. The levels influence only the mean of y
#random effects factor has levels that represent a sample from a population
#of possible levels. The levels influence only the variance of y. 

#fixed effects factors: has informative factor levels compared to the hypothesis in question
#You are interested in the effect of each level of the predictor. The levels are 
#deliberately arranged by the experimenter. Increasing the sample size does not increase
#the numer of levels of the predictor: Intervention

#random effects factors: has uninformative factor levels compared to the hypothesis in question. 
#You are not interested in the effect of each level of the predictor. Increasing
#the sample size often increases the # of levels of the predictor: group they were in

setwd("~/Desktop/Dissertation")
library(haven)
dissdata <- read_sav("DissData_WORKING.sav")

#Descriptive Data
count(dissdata, subid) #50 participants 
count(dissdata, Intervention)

#Control Variables
summary(dissdata$timeatMara) #in years
summary(dissdata$familyclose) #in hours
summary(dissdata$friendsMara) 
summary(dissdata$staffMara)
summary(dissdata$groupknowwell)
summary(dissdata$groupstranger)
summary(dissdata$techexp_all)

#Visualizing Demographic Data
hist(dissdata$techexp_all) #figure margins too large?
par("mar") 
par(mar=c(1,1,1,1)) #Changed margins (suggestion from Stack Overflow)
hist(dissdata$techexp_all) #Now it works!
hist(dissdata$Age)
hist(dissdata$Sex)
hist(dissdata$Ethnicity)
ggplot(dissdata, aes(x = maritalstatus)) + geom_bar()
dissdata$maritalstatus
       
#Reliabilities
library(psy)
emotionalcap_only <- subset(dissdata, select = c(emotionalcap1:emotionalcap7))
cronbach(emotionalcap_only)

loneliness_only <- subset(dissdata, select = c(ucla_lon1:ucla_lon3))
cronbach(loneliness_only)

vitality_only <- subset(dissdata, select = c(vitality1:vitality7))
cronbach(vitality_only)

depression_only <- subset(dissdata, select = c(depression1:depression15))
cronbach(depression_only)

communaloriet_only <- subset(dissdata, select = c(communaloriet1:communaloriet5))
cronbach(communaloriet_only)

qualcomm_only <- subset(dissdata, select = c(qualcomm1:qualcomm7))
cronbach(qualcomm_only)

#Since I already know the nature of my missing data (all random instances except for three people), I am not going to go into a lot of missing 
#data investigations. Here is the code to do so, however, in case I need to go back. 
library(naniar)
miss_var_summary(dissdata)
library(ggplot2)
gg_miss_var(dissdata)

#Searching for Strange missing Values
dissdata %>%
  miss_scan_count(search = list(" ", "N/A", "."))
dissdata %>%
  replace_with_na(replace = list(grade = c(" ", "N/A", ".")))
dissdata %>%
  miss_scan_count(search = list(" "))

replace_with_na_all() #all variables
dissdata %>%
  replace_with_na_all(condition = ~.x == -99)

replace_with_na_at() # a subset of selected variables
replace_with_na_if() #a subset of variables that fulfill some condition (numeric, character)

#Dealing with missing data: check for NAs
is.na(dissdata)

#Missing data: Are there any NAs?
any(is.na(dissdata))

#How many NAs?
sum(is.na(dissdata))
n_miss(dissdata)

summary(dissdata) #this shows you the NAs at the end of every variable (still not super useful for this big of a dataset)

complete.cases(dissdata)

#boxplot 
boxplot(dissdata$Age)

#histogram
hist(dissdata$Age)

#Understanding structure
class(dissdata)

#View its dimensions
dim(dissdata)

#Filling in missing data 
dissdata %>%
  tidyr::complete(subid, Week)
dissdata %>%
  tidyr::fill(VideoConsent)
dissdata_fill <- dissdata %>%
  tidyr::fill(VideoConsent, Age, Sex, Ethnicity, timeatMara, reasonforMara, familyclose, familytalk, groupknowwell)
dissdata <- dissdata_fill
head(dissdata)

#To get an understanding of what the data look like, important to create scatter plots
#H1: Compared to the control group, residents who engage in the shared IVR intervention 
#with other residents will experience greater personal subjective vitality, which, 
#in turn, will predict better quality communication and social engagement with other residents.
library(ggplot2)
#Vitality + Soc Engage - ORIGINAL
ggplot(OGdissdata_small, aes(x = Vitality, y = SocEngage, color = Intervention)) + 
  geom_point() #same problem here
ggplot(OGdissdata_small, aes(x = Vitality, y = SocEngage, color = Intervention)) + 
  geom_point() + facet_wrap(~Time)

#Vitality + Soc Engage - IMPUTED
ggplot(dissdata_complete, aes(x = Vitality, y = SocEngage, color = Intervention)) + 
  geom_point() 
ggplot(dissdata_complete, aes(x = Vitality, y = SocEngage, color = Intervention)) + 
  geom_point() + facet_wrap(~ Time)

#H2: Compared to the control group, residents who engage in the shared IVR intervention with 
#other residents will experience greater personal subjective vitality, which, in turn, will 
#predict a greater sense of communal orientation
#Vitality + Communal Orientation
ggplot(OGdissdata_small, aes(x = Vitality, y = CommunalOrient, color = Intervention)) + 
  geom_point() #slight positive correlation
ggplot(OGdissdata_small, aes(x = Vitality, y = CommunalOrient, color = Intervention)) + 
  geom_point() + facet_wrap(~Time)

#With completed dataset
ggplot(dissdata_complete, aes(x = Vitality, y = CommunalOrient, color = Intervention)) + 
  geom_point() + scale_x_log10()
ggplot(dissdata_complete, aes(x = Vitality, y = CommunalOrient, color = Intervention)) + 
  geom_point() + scale_x_log10() + facet_wrap(~Time)

#H3: Compared to a control group, residents who engage in shared IVR intervention with other residents
#will experience greater personal subjective vitality, which will, in turn, predict less
#loneliness, depression, and stress
ggplot(dissdata, aes(x = vitality_all, y = logloneliness_avg, color = Intervention)) + geom_point() #this is a weird scale so plot doesn't make sense
ggplot(dissdata, aes(x = vitality_all, y = logloneliness_avg, color = Intervention)) + geom_point() + facet_wrap(~Time)
ggplot(dissdata, aes(x = vitality_all, y = depression_all, color = Intervention)) + geom_point() #strong negative correlation
ggplot(dissdata, aes(x = vitality_all, y = depression_all, color = Intervention)) + geom_point() + facet_wrap(~Time)
ggplot(dissdata, aes(x = vitality_all, y = stress_all, color = Intervention)) + geom_point() #slight positive relationship 
ggplot(dissdata, aes(x = vitality_all, y = stress_all, color = Intervention)) + geom_point() + facet_wrap(~Time)

#With completed dataset
ggplot(dissdata_complete, aes(x = Vitality, y = Loneliness, color = Intervention)) + geom_point() + facet_wrap(~Time)
ggplot(dissdata_completed, aes(x = Vitality, y = Depression, color = Intervention)) + geom_point() + facet_wrap(~Time)
ggplot(dissdata_completed, aes(x = Vitality, y = Stress, color = Intervention)) + geom_point() + scale_x_log10() + facet_wrap(~Time)

#Mean centering predictor variables: establishes a meaningful zero point on scales that otherwise lack such a value.
#Centering by subtracting the mean. This is also known as a random intercept model.
ls(dissdata)
vitality_cen <- scale(dissdata$vitality_all, center = TRUE, scale = FALSE)
head(vitality_cen)

write.csv(dissdata, "dissdata_filledin.csv")
library(haven)
dissdata <- read.csv("dissdata_filledin.csv")
head(dissdata)

#Need to check the ICC before and after imputation.
#If there is no real correlation among observations within a cluster, 
#the cluster means won’t differ.  It’s only when some clusters have generally 
#high values and others have relatively low values that the values within a cluster are correlated.
#It can help you determine whether or not a linear mixed model is even necessary. 
#If you find that the correlation is zero, that means the observations within clusters 
#are no more similar than observations from different clusters.  Go ahead and use a simpler 
#analysis technique.
install.packages("sjstats")
library(sjstats)
as.data.frame(dissdata)
icc(dissdata)
library(multilevel)
icc(dissdata, adjusted = TRUE)

dissdata$groupknowwell <- NULL
dissdata$'groupknowwell#' <- NULL
dissdata$groupstranger <- NULL 
dissdata$`groupstranger#` <- NULL 

#Multiple imputation uses the Expectation Meximization (EM) algorithm 
#This is a common method for obtaining ML estimates with incomplete data that
#has been shown to reduce bias (Peugh & Enders, 2004)
#2 step process where missing values are first imputed and then a covariance matrix
#and mean vector are estimated. This repeats until the difference between the covariance
#matricies from adjacent iterations differs by a trivial amount

#Checking the amount of missing data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(hyp_MissingAnalysis, 2, pMiss)
apply(hyp_MissingAnalysis, 1, pMiss)
md.pattern(dissdata)
library(VIM)
aggr_plot <- aggr(dissdata, col = c('navyblue', 'red'), 
      numbers = TRUE, sortVars = TRUE, labels = names(dissdata), 
      cex.axis = .7, gap = 3, ylab = c("Histogram of Missing Data", "Pattern"))

head(dissdata)
summary(dissdata)
hyp_MissingAnalysis <- cbind("vitality" = dissdata$vitality_all,
                             "socengage" = dissdata$socengage_all,
                             "stress" = dissdata$stress_all,
                             "depression" = dissdata$depression_all, 
                             "lonliness" = dissdata$sqrtloneliness_avg,
                             "commorient" = dissdata$communalorient_all, 
                             "emocap_all" = dissdata$emotionalcap_all)
head(hyp_MissingAnalysis)

apply(hyp_MissingAnalysis, 2, pMiss)
apply(hyp_MissingAnalysis, 1, pMiss)
md.pattern(dissdata)
library(VIM)
aggr_plot <- aggr(hyp_MissingAnalysis, col = c('navyblue', 'red'), 
                  numbers = TRUE, sortVars = TRUE, labels = names(hyp_MissingAnalysis), 
                  cex.axis = .7, gap = 3, ylab = c("Histogram of Missing Data", "Pattern"))
#This plot helps us to understand that there is a lot of missing data
aggr_plot

#Let's look at a box plot now
class(hyp_MissingAnalysis)
hyp_MissingAnalysis <- as.data.frame(as.matrix(hyp_MissingAnalysis))
marginplot(hyp_MissingAnalysis[c(1,2)]) #xlab = "Vitality" + ylab = "Social Engagement"
#^ here we are constrained to plotting only two variables at a time, but nevertheless we can gather some
#interesting insights. The red box plot on the left shows the distribution of social engagement with vitality missing
#while the blue boxplot shows the distribution of the remaining datapoints. 
#Likewise for vitality box plot at the bottom of the graph. If our assumption of MCAR data is correct, then 
#we expect the red and blue box plots to be very similar
marginplot(hyp_MissingAnalysis[c(2,3)]) # x = social engage, y = stress - 
marginplot(hyp_MissingAnalysis[c(3,4)]) # x = stress, y = depression - this one looks ok
marginplot(hyp_MissingAnalysis[c(4,5)]) # x = depression, y = loneliness - I think loneliness is throwing this one off
marginplot(hyp_MissingAnalysis[c(5,6)]) # x = depression, y = communal orientation - these look a little weird but acceptable
marginplot(hyp_MissingAnalysis[c(6,7)]) # x = communal orientation, y = emotional capital - these look good

#Will proceed with imputation
#flipping from long to wide
#Imputing the data - now that all of the data are isolated, made wide, and joined together
#van Buuren S, Groothuis-Oudshoorn K (2011). “mice: Multivariate Imputation by Chained Equations in R.” 
#Journal of Statistical Software, 45(3), 1-67. https://www.jstatsoft.org/v45/i03/.
head(dissdata)
library(dplyr)
library(tidyr)
library(corrr)

#First, need to isolate key vars and make them wide
#Vitality
diss_time <- c(1, 2, 3, 4)
vitality <- round(dissdata$vitality_all, digits = 2)
vitality_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'vitality' = vitality
)
vitality_df <- as.data.frame(vitality_df)
summary(vitality_df)
glimpse(vitality_df)
vitality_wide <- vitality_df %>%
  mutate(Time = paste0('TimeVit_', Time)) %>%
  spread(Time, vitality) %>%
  select(subid, Intervention, TimeVit_1, TimeVit_2, TimeVit_3, TimeVit_4, everything())
str(vitality_wide)

vitality_wide2 <- lapply(vitality_wide[,c(3:6)], as.character)
str(vitality_wide2)
vitality_wide3 <- lapply(vitality_wide2[], as.numeric)
str(vitality_wide3)
vitality_wide2 <- vitality_wide3

vitality_wide$TimeVit_1 <- vitality_wide2$TimeVit_1
vitality_wide$TimeVit_2 <- vitality_wide2$TimeVit_2
vitality_wide$TimeVit_3 <- vitality_wide2$TimeVit_3
vitality_wide$TimeVit_4 <- vitality_wide2$TimeVit_4
str(vitality_wide)
class(vitality_wide)
vitality_wide_df <- as.data.frame(vitality_wide2)
class(vitality_wide_df)


vitality_imp <- mice(vitality_wide_df, m = 1, meth = "pmm")
summary(vitality_imp)

#Diagnostic checking
xyplot(vitality_imp, TimeVit_2 ~ TimeVit_3, pch = 18, cex = 1)
densityplot(vitality_imp)
stripplot(vitality_imp, pch = 20, cex = 1.2)

#Complete
vitality_complete <- complete(vitality_imp)

#Correlating 
vitality_wide_cor <- vitality_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
vitality_wide_cor

vitality_complete$subid <- vitality_wide$subid
vitality_complete$Intervention <- vitality_wide$Intervention
str(vitality_complete)
head(vitality_complete)

#Emotional Cap
diss_time <- c(1, 2, 3, 4)
emotionalcap <- round(dissdata$emotionalcap_all, digits = 2)
emotionalcap_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'emotionalcap' = emotionalcap
)

emotionalcap_df <- as.data.frame(emotionalcap_df)
class(emotionalcap_df)
emotionalcap_wide <- emotionalcap_df %>%
  mutate(Time = paste0('TimeEmoCap_', Time)) %>%
  spread(Time, emotionalcap) %>%
  select(subid, Intervention, TimeEmoCap_1, TimeEmoCap_2, TimeEmoCap_3, TimeEmoCap_4, everything())
head(emotionalcap_wide)

emotionalcap_wide2 <- lapply(emotionalcap_wide[,c(3:6)], as.character)
str(emotionalcap_wide2)
emotionalcap_wide3 <- lapply(emotionalcap_wide2[], as.numeric)
str(emotionalcap_wide3)
emotionalcap_wide2 <- emotionalcap_wide3

emotionalcap_wide$TimeEmoCap_1 <- emotionalcap_wide2$TimeEmoCap_1
emotionalcap_wide$TimeEmoCap_2 <- emotionalcap_wide2$TimeEmoCap_2
emotionalcap_wide$TimeEmoCap_3 <- emotionalcap_wide2$TimeEmoCap_3
emotionalcap_wide$TimeEmoCap_4 <- emotionalcap_wide2$TimeEmoCap_4
str(emotionalcap_wide)
class(emotionalcap_wide)

emotionalcap_wide_df <- as.data.frame(emotionalcap_wide2)
class(emotionalcap_wide_df)

emocap_imp <- mice(emotionalcap_wide_df, m = 1, meth = "pmm")
summary(emocap_imp)

#Diagnostic checking
xyplot(emocap_imp, TimeEmoCap_2 ~ TimeEmoCap_3, pch = 18, cex = 1)
densityplot(emocap_imp)
stripplot(emocap_imp, pch = 20, cex = 1.2)

#Complete
emocap_complete <- complete(emocap_imp)

#Correlating 
emocap_wide_cor <- emocap_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
emocap_wide_cor

emocap_complete$subid <- emotionalcap_wide$subid
emocap_complete$Intervention <- emotionalcap_wide$Intervention
str(emocap_complete)
head(emocap_complete)

#Loneliness
diss_time <- c(1, 2, 3, 4)
loneliness <- round(dissdata$lonelinessall, digits = 2)
loneliness_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'loneliness' = loneliness
)
loneliness_df <- as.data.frame(loneliness_df)

loneliness_wide <- loneliness_df %>%
  mutate(Time = paste0('TimeLon_', Time)) %>%
  spread(Time, loneliness) %>%
  select(subid, Intervention, TimeLon_1, TimeLon_2, TimeLon_3, TimeLon_4, everything())
head(loneliness_wide)

lon_wide2 <- lapply(loneliness_wide[,c(3:6)], as.character)
str(lon_wide2)
lon_wide3 <- lapply(lon_wide2[], as.numeric)
str(lon_wide3)
lon_wide2 <- lon_wide3

loneliness_wide$TimeLon_1 <- lon_wide2$TimeLon_1
loneliness_wide$TimeLon_2 <- lon_wide2$TimeLon_2
loneliness_wide$TimeLon_3 <- lon_wide2$TimeLon_3
loneliness_wide$TimeLon_4 <- lon_wide2$TimeLon_4
str(loneliness_wide)
class(loneliness_wide)

lon_wide_df <- as.data.frame(lon_wide2)
class(lon_wide_df)

lon_imp <- mice(lon_wide_df, m = 1, meth = "pmm")
summary(lon_imp)

#Diagnostic checking
xyplot(lon_imp, TimeLon_2 ~ TimeLon_3, pch = 18, cex = 1)
densityplot(lon_imp)
stripplot(lon_imp, pch = 20, cex = 1.2)

#Complete
lon_complete <- complete(lon_imp)

#Correlating 
lon_wide_cor <- lon_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
lon_wide_cor

lon_complete$subid <- loneliness_wide$subid
lon_complete$Intervention <- loneliness_wide$Intervention
str(lon_complete)
head(lon_complete)

#Depression 
diss_time <- c(1, 2, 3, 4)
class(dissdata$depression_all)
glimpse(dissdata$logdepression)
head(dissdata$logdepression)

depression <- round(dissdata$logdepression, digits = 2)
depression_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'depression' = depression
)
depression_df <- as.data.frame(depression_df)
class(depression_df)
depression_df$Time <- as.numeric(depression_df$Time)
class(depression_df$Time)
depression_wide <- depression_df %>%
  mutate(Time = paste0('TimeDep_', Time)) %>%
  spread(Time, depression) %>%
  select(subid, Intervention, TimeDep_1, TimeDep_2, TimeDep_3, TimeDep_4, everything())
head(depression_wide)

dep_wide2 <- lapply(depression_wide[,c(3:6)], as.character)
str(dep_wide2)
dep_wide3 <- lapply(dep_wide2[], as.numeric)
str(dep_wide3)
dep_wide2 <- dep_wide3

depression_wide$TimeDep_1 <- dep_wide2$TimeDep_1
depression_wide$TimeDep_2 <- dep_wide2$TimeDep_2
depression_wide$TimeDep_3 <- dep_wide2$TimeDep_3
depression_wide$TimeDep_4 <- dep_wide2$TimeDep_4
str(depression_wide)
class(depression_wide)

dep_wide_df <- as.data.frame(dep_wide2)
class(dep_wide_df)

dep_imp <- mice(dep_wide_df, m = 1, meth = "pmm")
summary(dep_imp)

#Diagnostic checking
xyplot(dep_imp, TimeDep_2 ~ TimeDep_3, pch = 18, cex = 1)
densityplot(dep_imp)
stripplot(dep_imp, pch = 20, cex = 1.2)

#Complete
dep_complete <- complete(dep_imp)

#Correlating 
dep_wide_cor <- dep_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
dep_wide_cor

dep_complete$subid <- depression_wide$subid
dep_complete$Intervention <- depression_wide$Intervention
str(dep_complete)
head(dep_complete)

#Communal Orientation
diss_time <- c(1, 2, 3, 4)
class(dissdata$communalorient_all)
summary(dissdata$communalorient_all)
co <- round(dissdata$communalorient_all, digits = 2)
co_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'co' = co
)
co_df <- as.data.frame(co_df)
class(co_df$Time)
co_df$Time <- as.numeric(co_df$Time)
co_wide <- co_df %>%
  mutate(Time = paste0('TimeCO_', Time)) %>%
  spread(Time, co) %>%
  select(subid, Intervention, TimeCO_1, TimeCO_2, TimeCO_3, TimeCO_4, everything())
head(co_wide)

co_wide2 <- lapply(co_wide[,c(3:6)], as.character)
str(co_wide2)
co_wide3 <- lapply(co_wide2[], as.numeric)
str(co_wide3)
co_wide2 <- co_wide3

co_wide$TimeCO_1 <- co_wide2$TimeCO_1
co_wide$TimeCO_2 <- co_wide2$TimeCO_2
co_wide$TimeCO_3 <- co_wide2$TimeCO_3
co_wide$TimeCO_4 <- co_wide2$TimeCO_4
str(co_wide)
class(co_wide)

co_wide_df <- as.data.frame(co_wide2)
class(dep_wide_df)

co_imp <- mice(co_wide_df, m = 1, meth = "pmm")
summary(co_imp)

#Diagnostic checking
xyplot(co_imp, TimeCO_2 ~ TimeCO_3, pch = 18, cex = 1)
densityplot(co_imp)
stripplot(co_imp, pch = 20, cex = 1.2)

#Complete
co_complete <- complete(co_imp)

#Correlating 
co_wide_cor <- co_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
co_wide_cor

co_complete$subid <- co_wide$subid
co_complete$Intervention <- co_wide$Intervention
str(co_complete)
head(co_complete)

#Social Engagement
socengage <- cbind(
  'socengage1' = dissdata$socialactivity1, 
  'socengage2' = dissdata$socialactivity2, 
  'socengage3' = dissdata$socialactivity3, 
  'socengage4' = dissdata$socialactivity4)

summary(socengage)
head(socengage)
socengage_all <- (dissdata$socialactivity1 + dissdata$socialactivity2 + dissdata$socialactivity3 + dissdata$socialactivity4)/4
socengage_all

diss_time <- c(1, 2, 3, 4)
summary(socengage_all)
socengage <- round(socengage_all, digits = 2)
socengage_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'socengage' = socengage_all
)
socengage_df <- as.data.frame(socengage_df)
class(socengage_df$Time)
socengage_df$Time <- as.numeric(socengage_df$Time)
class(socengage_df$Time)
socengage_wide <- socengage_df %>%
  mutate(Time = paste0('TimeSocEngage_', Time)) %>%
  spread(Time, socengage) %>%
  select(subid, Intervention, TimeSocEngage_1, TimeSocEngage_2, TimeSocEngage_3, TimeSocEngage_4, everything())
head(socengage_wide)

socengage_wide2 <- lapply(socengage_wide[,c(3:6)], as.character)
str(socengage_wide2)
socengage_wide3 <- lapply(socengage_wide2[], as.numeric)
str(socengage_wide3)
socengage_wide2 <- socengage_wide3

socengage_wide$TimeSocEngage_1 <- socengage_wide2$TimeSocEngage_1
socengage_wide$TimeSocEngage_2 <- socengage_wide2$TimeSocEngage_2
socengage_wide$TimeSocEngage_3 <- socengage_wide2$TimeSocEngage_3
socengage_wide$TimeSocEngage_4 <- socengage_wide2$TimeSocEngage_4
str(socengage_wide)
class(socengage_wide)

socengage_wide_df <- as.data.frame(socengage_wide2)
class(socengage_wide_df)

socengage_imp <- mice(socengage_wide_df, m = 1, meth = "pmm")
summary(socengage_imp)

#Diagnostic checking
xyplot(socengage_imp, TimeSocEngage_2 ~ TimeSocEngage_3, pch = 18, cex = 1)
densityplot(socengage_imp)
stripplot(socengage_imp, pch = 20, cex = 1.2)

#Complete
socengage_complete <- complete(socengage_imp)
head(socengage_complete)

#Correlating 
socengage_wide_cor <- socengage_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
socengage_wide_cor

socengage_complete$subid <- socengage_wide$subid
socengage_complete$Intervention <- socengage_wide$Intervention
str(socengage_complete)
head(socengage_complete)

#Stress
diss_time <- c(1, 2, 3, 4)
stress <- round(stress_all, digits = 2)
stress_all
stress_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'stress' = stress
)
stress_df <- as.data.frame(stress_df)
class(stress_df$Time)
stress_df$Time <- as.numeric(stress_df$Time)

stress_wide <- stress_df %>%
  mutate(Time = paste0('TimeStress_', Time)) %>%
  spread(Time, stress) %>%
  select(subid, Intervention, TimeStress_1, TimeStress_2, TimeStress_3, TimeStress_4, everything())
head(stress_wide)

stress_wide2 <- lapply(stress_wide[,c(3:6)], as.character)
str(stress_wide2)
stress_wide3 <- lapply(stress_wide2[], as.numeric)
str(stress_wide3)
stress_wide2 <- stress_wide3

stress_wide$TimeStress_1 <- stress_wide2$TimeStress_1
stress_wide$TimeStress_2 <- stress_wide2$TimeStress_2
stress_wide$TimeStress_3 <- stress_wide2$TimeStress_3
stress_wide$TimeStress_4 <- stress_wide2$TimeStress_4
str(stress_wide)
class(stress_wide)

stress_wide_df <- as.data.frame(stress_wide2)
class(stress_wide_df)

stress_imp <- mice(stress_wide_df, m = 1, meth = "pmm")
summary(stress_imp)

#Diagnostic checking
xyplot(stress_imp, TimeStress_2 ~ TimeStress_3, pch = 18, cex = 1)
densityplot(stress_imp)
stripplot(stress_imp, pch = 20, cex = 1.2)

#Complete
stress_complete <- complete(stress_imp)
head(stress_complete)

#Correlating 
stress_wide_cor <- stress_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
stress_wide_cor

stress_complete$subid <- stress_wide$subid
stress_complete$Intervention <- stress_wide$Intervention
str(stress_complete)
head(stress_complete)

#Quality Communication
diss_time <- c(1, 2, 3, 4)
qualcomm <- round(dissdata$qualcomm_all, digits = 2)

qualcomm_df <- cbind(
  'subid' = dissdata$subid,
  'Intervention' = dissdata$Intervention,
  'Time' = diss_time,
  'qualcomm' = qualcomm
)
qualcomm_df <- as.data.frame(qualcomm_df)
class(qualcomm_df$Time)
qualcomm_df$Time <- as.numeric(qualcomm_df$Time)

qualcomm_wide <- qualcomm_df %>%
  mutate(Time = paste0('TimeQualComm_', Time)) %>%
  spread(Time, qualcomm) %>%
  select(subid, Intervention, TimeQualComm_1, TimeQualComm_2, TimeQualComm_3, TimeQualComm_4, everything())
head(qualcomm_wide)

qualcomm_wide2 <- lapply(qualcomm_wide[,c(3:6)], as.character)
str(qualcomm_wide2)
qualcomm_wide3 <- lapply(qualcomm_wide2[], as.numeric)
str(qualcomm_wide3)
qualcomm_wide2 <- qualcomm_wide3

qualcomm_wide$TimeQualComm_1 <- qualcomm_wide2$TimeQualComm_1
qualcomm_wide$TimeQualComm_2 <- qualcomm_wide2$TimeQualComm_2
qualcomm_wide$TimeQualComm_3 <- qualcomm_wide2$TimeQualComm_3
qualcomm_wide$TimeQualComm_4 <- qualcomm_wide2$TimeQualComm_4
str(qualcomm_wide)
class(qualcomm_wide)

qualcomm_wide_df <- as.data.frame(qualcomm_wide2)
class(qualcomm_wide_df)

qualcomm_imp <- mice(qualcomm_wide_df, m = 1, meth = "pmm")
summary(qualcomm_imp)

#Diagnostic checking
xyplot(qualcomm_imp, TimeQualComm_2 ~ TimeQualComm_3, pch = 18, cex = 1)
densityplot(qualcomm_imp)
stripplot(qualcomm_imp, pch = 20, cex = 1.2)

#Complete
qualcomm_complete <- complete(qualcomm_imp)
head(qualcomm_complete)

#Correlating 
qualcomm_wide_cor <- qualcomm_complete %>%
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 2)
qualcomm_wide_cor

qualcomm_complete$subid <- qualcomm_wide$subid
qualcomm_complete$Intervention <- qualcomm_wide$Intervention
str(qualcomm_complete)
head(qualcomm_complete)

#Joining key var data frames together
head(vitality_complete)
head(emocap_complete)
head(emocap_wide)
head(co_complete)
as.data.frame(co_complete)
df_vitemocap <- inner_join(x = vitality_complete, y = emocap_complete, by = c("subid", "Intervention"))
head(df_vitemocap)

df_1_lon <- inner_join(x = df_vitemocap, y = lon_complete, by = c("subid", "Intervention"))
df_1_lon

df_2_dep <- inner_join(x = df_1_lon, y = dep_complete, by = c("subid", "Intervention"))
df_2_dep

df_3_co <- inner_join(x = df_2_dep, y = co_complete, by = c("subid", "Intervention"))
df_3_co

df_4_socengage <- inner_join(x = df_3_co, y = socengage_complete, by = c("subid", "Intervention"))
df_4_socengage

df_5_stress <- inner_join(x = df_4_socengage, y = stress_complete, by = c("subid", "Intervention"))
df_5_stress

df_complete <- df_5_stress
head(df_complete)
#YAY! The data R imputed!
#Correlations over time with wide & imputed data
#Can show how dependent the multiple measurements are and do they change overtime
#Now need to bring them back to long
library(dplyr)
library(tidyr)
head(df_complete)

df_complete2 <- gather(df_complete, key = "Time", value = "Vitality", TimeVit_1:TimeVit_4)
head(df_complete2)
df_complete2$Time <- gsub("TimeVit_1", "1", df_complete2$Time)
df_complete2$Time <- gsub("TimeVit_2", "2", df_complete2$Time)
df_complete2$Time <- gsub("TimeVit_3", "3", df_complete2$Time)
df_complete2$Time <- gsub("TimeVit_4", "4", df_complete2$Time)
head(df_complete2)

vit_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete2$Time,
  'Vitality' = df_complete2$Vitality
)
vit_complete2 <- as.data.frame(vit_complete2)
head(vit_complete2)

df_complete3 <- gather(df_complete, key = "Time", value = "EmoCap", TimeEmoCap_1:TimeEmoCap_4)
df_complete3$Time <- gsub("TimeEmoCap_1", "1", df_complete3$Time)
df_complete3$Time <- gsub("TimeEmoCap_2", "2", df_complete3$Time)
df_complete3$Time <- gsub("TimeEmoCap_3", "3", df_complete3$Time)
df_complete3$Time <- gsub("TimeEmoCap_4", "4", df_complete3$Time)         
head(df_complete3)
class(df_complete$subid)
subid <- as.character(df_complete$subid)
emocap_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete3$Time,
  'EmoCap' = df_complete3$EmoCap
)
head(emocap_complete2)
emocap_complete2 <- as.data.frame(emocap_complete2)
head(emocap_complete2)

df_complete4 <- gather(df_complete, key = "Time", value = "Loneliness", TimeLon_1:TimeLon_4)
df_complete4$Time <- gsub("TimeLon_1", "1", df_complete4$Time)
df_complete4$Time <- gsub("TimeLon_2", "2", df_complete4$Time)
df_complete4$Time <- gsub("TimeLon_3", "3", df_complete4$Time)
df_complete4$Time <- gsub("TimeLon_4", "4", df_complete4$Time)  
head(df_complete4)

lon_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete4$Time,
  'Lon' = df_complete4$Loneliness
)
head(lon_complete2)
lon_complete2 <- as.data.frame(lon_complete2)
head(lon_complete2)

df_complete5 <- gather(df_complete, key = "Time", value = "Depression", TimeDep_1:TimeDep_4)
df_complete5$Time <- gsub("TimeDep_1", "1", df_complete5$Time)
df_complete5$Time <- gsub("TimeDep_2", "2", df_complete5$Time)
df_complete5$Time <- gsub("TimeDep_3", "3", df_complete5$Time)
df_complete5$Time <- gsub("TimeDep_4", "4", df_complete5$Time)

dep_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete5$Time,
  'Depression' = df_complete5$Depression
)
head(dep_complete2)
dep_complete2 <- as.data.frame(dep_complete2)
head(dep_complete2)

df_complete6 <- gather(df_complete, key = "Time", value = "CommunalOrient", TimeCO_1:TimeCO_4)
df_complete6$Time <- gsub("TimeCO_1", "1", df_complete6$Time)
df_complete6$Time <- gsub("TimeCO_2", "2", df_complete6$Time)
df_complete6$Time <- gsub("TimeCO_3", "3", df_complete6$Time)
df_complete6$Time <- gsub("TimeCO_4", "4", df_complete6$Time)  
head(df_complete6)

co_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete6$Time,
  'CO' = df_complete6$CommunalOrient
)
head(co_complete2)
co_complete2 <- as.data.frame(co_complete2)
head(co_complete2)

df_complete7 <- gather(df_complete, key = "Time", value = "SocEngage", TimeSocEngage_1:TimeSocEngage_4)
df_complete7$Time <- gsub("TimeSocEngage_1", "1", df_complete7$Time)
df_complete7$Time <- gsub("TimeSocEngage_2", "2", df_complete7$Time)
df_complete7$Time <- gsub("TimeSocEngage_3", "3", df_complete7$Time)
df_complete7$Time <- gsub("TimeSocEngage_4", "4", df_complete7$Time) 

socengage_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete7$Time,
  'SocEngage' = df_complete7$SocEngage
)
head(socengage_complete2)
socengage_complete2 <- as.data.frame(socengage_complete2)
head(socengage_complete2)

df_complete8 <- gather(df_complete, key = "Time", value = "Stress", TimeStress_1:TimeStress_4)
df_complete8$Time <- gsub("TimeStress_1", "1", df_complete8$Time)
df_complete8$Time <- gsub("TimeStress_2", "2", df_complete8$Time)
df_complete8$Time <- gsub("TimeStress_3", "3", df_complete8$Time)
df_complete8$Time <- gsub("TimeStress_4", "4", df_complete8$Time) 

stress_complete2 <- cbind(
  'subid' = subid,
  'Time' = df_complete8$Time,
  'Stress' = df_complete8$Stress
)
head(stress_complete2)
stress_complete2 <- as.data.frame(stress_complete2)
head(stress_complete2)

dissdata_long <- inner_join(stress_complete2, socengage_complete2, by = c("subid", "Time")) 
head(dissdata_long)
dissdata_long2 <- inner_join(dissdata_long, co_complete2, by = c("subid", "Time"))
head(dissdata_long2)      
dissdata_long3 <- inner_join(dissdata_long2, dep_complete2, by = c("subid", "Time"))
head(dissdata_long3)
dissdata_long4 <- inner_join(dissdata_long3, lon_complete2, by = c("subid", "Time"))
head(dissdata_long4)
dissdata_long5 <- inner_join(dissdata_long4, emocap_complete2, by = c("subid", "Time"))
head(dissdata_long5)
dissdata_long6 <- inner_join(dissdata_long5, vit_complete2, by = c("subid", "Time"))
head(dissdata_long6)
dissdata_long6$Intervention <- df_complete$Intervention
head(dissdata_long6)
summary(dissdata_long6)
str(dissdata_long6)
#Dummy Coding Intervention
dissdata_long6$Intervention <- gsub("I", "1", dissdata_long6$Intervention)
dissdata_long6$Intervention <- gsub("C", "0", dissdata_long6$Intervention)
summary(dissdata_long6$Intervention)

#Reordering intervention
df <- dissdata_long6[, c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)]
head(df)
dissdata_complete <- df
head(dissdata_complete)
str(dissdata_complete)
write.csv(dissdata_complete, "dissdata_complete.csv")

#Changing data type
dissdata_long7 <- lapply(dissdata_long6[,c(3:10)], as.character)
str(dissdata_long7)
summary(dissdata_long7)
dissdata_long7 <- lapply(dissdata_long7[], as.numeric)
str(dissdata_long7)
head(dissdata_long7)
class(dissdata_long7)
summary(dissdata_long7)
dissdata_long8 <- as.data.frame(dissdata_long7)
str(dissdata_long8)
head(dissdata_long8)
summary(dissdata_long8)

dissdata_long8$subid <- dissdata_complete$subid
dissdata_long8$Time <- dissdata_complete$Time
head(dissdata_long8)
dissdata_complete3 <- dissdata_long8[, c(9, 8, 10, 1, 2, 3, 4, 5, 6, 7)]
head(dissdata_complete3)
library(readr)
setwd("~/Desktop/Dissertation")
dissdata_complete <- read_csv("dissdata_complete.csv")
write.csv(dissdata_complete3, "dissdata_complete.csv")
dissdata_complete <- dissdata_complete3

#Inspectigin the distribution of the original and imputed data
head(dissdata_complete)
class(dissdata_complete)

#Using GGplot to look at individual trajectories over time
library(nlme)
library(ggplot2)
ggplot(dissdata_complete, aes(x = Time, y = SocEngage)) + 
  geom_line(aes(group = subid), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw(base_size = 16) +
  xlab("Number of Weeks") + 
  ylab("Social Engagement")

ggplot(dissdata_complete, aes(x = Time, y = CO)) + 
  geom_line(aes(group = subid), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw(base_size = 16) +
  xlab("Number of Weeks") + 
  ylab("Communal Orientation")

ggplot(dissdata_complete, aes(x = Time, y = Depression)) + 
  geom_line(aes(group = subid), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw(base_size = 16) +
  xlab("Number of Weeks") + 
  ylab("Depression")

ggplot(dissdata_complete, aes(x = Time, y = Lon)) + 
  geom_line(aes(group = subid), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw(base_size = 16) +
  xlab("Number of Weeks") + 
  ylab("Loneliness")

ggplot(dissdata_complete, aes(x = Time, y = Stress)) + 
  geom_line(aes(group = subid), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw(base_size = 16) +
  xlab("Number of Weeks") + 
  ylab("Stress")

#Creating a smaller dataset for original data
summary(dissdata$socengage_all)

head(dissdata_complete)
diss_time <- c(1, 2, 3, 4)
OGdissdata_small <- cbind(
  'subid' = dissdata$subid,
  'Time' = diss_time,
  'Intervention' = dissdata$Intervention,
  'Vitality' = dissdata$vitality_all,
  'EmoCap' = dissdata$emotionalcap_all,
  'Loneliness' = dissdata$logloneliness_avg,
  'Depression' = dissdata$depression_all,
  'CommunalOrient' = dissdata$communalorient_all,
  'SocEngage' = socengage_all 
)
OGdissdata_small <- as.data.frame(OGdissdata_small)

#Analysis of the imputed dataset 
#we can use the function with.mids(), a wrapper function that applies
#the complete data model to each of the imputed data sets:
dissdata_complete_imp1 <- as.mids(dissdata_complete)
modelFit1 <- with(dissdata_, lm(socengage_all ~ vitality_all))
modelFit1
#The fit object has class mira and contains the results of five complete-data 
#analyses. These can be pooled as follows:
summary(pool(modelFit1))
summary(dissdata_complete$CO)

#APA Correlation Table
head(dissdata_complete)
vars_only <- cbind(
  'Vitality' = dissdata_complete$Vitality,
  'EmoCap' = dissdata_complete$EmoCap,
  'Loneliness' = dissdata_complete$Lon,
  'Depression' = dissdata_complete$Depression,
  'CommunalOrient' = dissdata_complete$CO,
  'SocEngage' = dissdata_complete$SocEngage,
  'Stress' = dissdata_complete$Stress
)
library(apaTables)
head(dissdata_complete)
as.numeric(vars_only)
as.numeric(vars_only$CommunalOrient)
range(vars_only$CommunalOrient)
apa.cor.table(vars_only, filename="Table1_APA.doc", table.number=1)

library(dplyr)

time1 <- filter(dissdata_complete, Time == 1)
time1
mean(time1, Vitality)


library(nlme)
library(ggplot2)
ggplot(dissdata, aes(x = Week, y = emotionalcap_all)) + geom_line(aes(group = subid), alpha = 0.6) + 
  geom_smooth(se = FALSE, size = 2) + xlab("Weeks") + ylab("Emotional Cap")  

library(readr)
dissdata_complete <- read.csv("dissdata_complete.csv")
head(dissdata_complete)

#Introducing the lmer function (now lme4) which stands for Linear Mixed Effects Regression - used for continuous variables
#Multilevel Mediation Analysis
library(QuantPsyc)
library(mlma)
library(lme4)
str(dissdata_complete)
dissdata_complete$Time <- as.numeric(dissdata_complete$Time)

#adding in control vars to dataset

dissdata_complete
new_dissdata <- dissdata_complete[-c()]

dissdata_complete$timeatMara <- dissdata$timeatMara
dissdata_complete$reasonforMara <- dissdata$reasonforMara
dissdata_complete$familyclose <- dissdata$familyclose
dissdata_complete$familytalk <- dissdata$familytalk

#Mediation with Bootstrapping - Erin Buchanan's YouTube Channel
#Data screening 
#accuracy
summary(dissdata_complete)
dissdata_complete$Vitality



