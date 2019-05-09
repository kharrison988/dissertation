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

#Need to check the ICC before and after imputation.
#If there is no real correlation among observations within a cluster, 
#the cluster means won’t differ.  It’s only when some clusters have generally 
#high values and others have relatively low values that the values within a cluster are correlated.
#It can help you determine whether or not a linear mixed model is even necessary. 
#If you find that the correlation is zero, that means the observations within clusters 
#are no more similar than observations from different clusters.  Go ahead and use a simpler 
#analysis technique.

library(dplyr)
library(tidyr)
library(corrr)
library(mice)

setwd("~/Desktop/Dissertation")
print(dissdata)


load_from_spss <- function() {
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
  library(tidyr)
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
  head(dissdata)
  
  write.csv(dissdata, "dissdata_filledin.csv")
}

missing_dataAnalysis <- function() {
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
  
}

old_code <- function() {
  #Imputing the data - now that all of the data are isolated, made wide, and joined together
  #van Buuren S, Groothuis-Oudshoorn K (2011). “mice: Multivariate Imputation by Chained Equations in R.” 
  #Journal of Statistical Software, 45(3), 1-67. https://www.jstatsoft.org/v45/i03/.
  #Multiple imputation uses the Expectation Meximization (EM) algorithm 
  #This is a common method for obtaining ML estimates with incomplete data that
  #has been shown to reduce bias (Peugh & Enders, 2004)
  #2 step process where missing values are first imputed and then a covariance matrix
  #and mean vector are estimated. This repeats until the difference between the covariance
  #matricies from adjacent iterations differs by a trivial amount

  
  #Correlations over time with wide & imputed data
  #Can show how dependent the multiple measurements are and do they change overtime
  #Correlating 
  vitality_wide_cor <- vitality_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  vitality_wide_cor
  
  #Correlating 
  emocap_wide_cor <- emocap_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  emocap_wide_cor
  

  #Correlating 
  lon_wide_cor <- lon_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  lon_wide_cor

  
  #Correlating 
  dep_wide_cor <- dep_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  dep_wide_cor
  
  
  #Correlating 
  co_wide_cor <- co_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  co_wide_cor
  
  
  #Correlating 
  socengage_wide_cor <- socengage_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  socengage_wide_cor
  
  #Correlating 
  stress_wide_cor <- stress_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  stress_wide_cor
  
  #Correlating 
  qualcomm_wide_cor <- qualcomm_complete %>%
    correlate() %>%
    shave(upper = FALSE) %>%
    fashion(decimals = 2)
  qualcomm_wide_cor
  
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

  
  #Mediation with Bootstrapping - Erin Buchanan's YouTube Channel
  #Data screening 
  #accuracy
  summary(dissdata_complete)
  dissdata_complete$Vitality
  
  setwd("~/Desktop/Dissertation")
  dissdata_complete <- read.csv("dissdata_complete.csv")
  head(dissdata_complete)
}

#find_average <- function(input_df, col_name) {
 # find_average_col <- paste0(col_name, '1')/ 
#}


#Will proceed with imputation
#flipping from long to wide
make_wide <- function(input_df, col_name) {
  week_prefix <- paste0('week_', col_name, '_')
  
  wide_df <- input_df %>%
    mutate(week = paste0(week_prefix, week)) %>%
    spread(week, col_name) %>%
    select(sub_id,
           intervention,
           paste0(week_prefix, '1'),
           paste0(week_prefix, '2'),
           paste0(week_prefix, '3'),
           paste0(week_prefix, '4'),
           everything())
  
  return(wide_df)
}

make_long <- function(input_df, col_name) {
  week_prefix <- paste0('week_', col_name, '_')
  
  long_df <- gather(input_df,
                    key = 'week',
                    value = value_name,
                    paste0(week_prefix, '1'):paste0(week_prefix, '4'))
  names(long_df)[names(long_df) == 'value_name'] <- col_name
  
  long_df$week <- gsub(paste0(week_prefix, '1'), '1', long_df$week)
  long_df$week <- gsub(paste0(week_prefix, '2'), '2', long_df$week)
  long_df$week <- gsub(paste0(week_prefix, '3'), '3', long_df$week)
  long_df$week <- gsub(paste0(week_prefix, '4'), '4', long_df$week)
  
  long_df <- long_df %>% arrange(sub_id)
  
  return(long_df)
}

load_columns <- function(src_df, dest_df, src_col_name, dest_col_name, limit) {
  for (i in 1:limit) {
    src <- paste(src_col_name, i, sep='')
    dest <- paste(dest_col_name, '_', i, sep='')
    
    dest_df[dest] <- src_df[src]
  }
  
  return(dest_df)
}

impute <- function(input_df) {
  # Only get the weeks to impute
  imputed_df <- mice(input_df[,c(3, 4, 5, 6)], m = 1, meth = "pmm")
  
  #Diagnostic checking
  summary(imputed_df)
  # FIXME: Parameterize this
  #print(xyplot(imputed_df, week_comm_orientation_all_2 ~ week_comm_orientation_all_3, pch = 18, cex = 1))
  print(densityplot(imputed_df))
  print(stripplot(imputed_df, pch = 20, cex = 1.2))
 
  imputed_df <- complete(imputed_df)
 
  # Add back the keys that were removed for imputation 
  imputed_df$sub_id <- input_df$sub_id
  imputed_df$intervention <- input_df$intervention
  imputed_df <- imputed_df[, c(5, 6, 1, 2, 3, 4)]
  
  return(imputed_df)
}

#isolating T2-4 
take_out_baseline <- function(input_df, col_name) {
  df_without_base <- data.frame(input_df)
  var_start_index <- 4
  df_without_base_rows <- as.data.frame.list(input_df[, c(var_start_index:ncol(input_df))])
  df_without_base[paste0(col_name, "_avg234")] <- rowMeans(df_without_base_rows)
 
  
  return(df_without_base)
}

#isolating T1
baseline_only <- function(input_df, col_name) {
  df_baseline <- data.frame(input_df)
  var_index <- 3
  df_baseline_rows <- as.data.frame.list(input_df[,3])
  df_baseline[paste0(col_name, "_avgbase")] <- rowMeans(df_baseline_rows)
  
  return(df_baseline)
}

#Adding isolated variables
#adding_isolated_vars <- function(input_df, col_name) {
 # isolated_vars <- data.frame(input_df)
  #var_index_isolated <- 7
  #df_baseline_rows <- as.data.frame.list(input_df[,7])
  #input_df$col_name <- isolated_vars[paste0(input_df,"_avg235"$col_name,"avg234")]
 
  #return(isolated_vars) 
#}

relationship_df <- {}
tech_experience_df <- {}
social_activity_df <- {}
social_activity_all_df <- {}
#comm_orientation_df <- {}
comm_orientation_all_df <- {}
rec_loneliness_avg_df <- {}
loneliness_all_df <- {}
log_depression_df <- {}
stress_df <- {}
stress_all_df <- {}
quality_comm_df <- {}
vitality_df <- {}
control_df <- {}

load_data <- function() {
  dissdata <- read.csv("dissdata_filledin.csv")

  keys_df <- data.frame(
    'week' = dissdata$Week,
    'sub_id' = dissdata$subid,
    'intervention' = dissdata$Intervention
  )
  
  var_start_index <- ncol(keys_df) + 1
  
  relationship_df <<- data.frame(keys_df)
  relationship_df$marital_status <<- dissdata$maritalstatus
  relationship_df$family_talk <<- dissdata$familytalk
  relationship_df$family_close <<- dissdata$familyclose
  relationship_df$friends_maravilla <<- dissdata$friendsMara
  relationship_df$staff_maravilla <<- dissdata$staffMara
  relationship_df$relationship_status <<- dissdata$relationstatus
  relationship_df$romantic_partner_live <<- dissdata$rompartlive
  relationship_df$romantic_partner_care <<- dissdata$rompartcare
  relationship_df$group_know_well <<- dissdata$groupknowwell
  relationship_df$group_stranger <<- dissdata$groupstranger
  
  # socialactivity aka socialengagement
  social_activity_df <<- data.frame(keys_df)
  social_activity_df <<- load_columns(dissdata, social_activity_df, 'socialactivity', 'social_activity', 4)
  social_activity_all_df <<- data.frame(keys_df)
  social_activity_rows <- as.data.frame.list(social_activity_df[, c(var_start_index:ncol(social_activity_df))])
  social_activity_all_df$social_activity_all <<- rowMeans(social_activity_rows)
  
  # communalorientation
  comm_orientation_all_df <<- data.frame(keys_df)
  comm_orientation_all_df$comm_orientation_all <<- dissdata$communalorient_all
  # TODO: Find mean of this
  #comm_orientation_df <<- load_columns(dissdata, comm_orientation_df, 'communaloriet', 'comm_orientation', 5)
  
  # recloneliness_avg
  rec_loneliness_avg_df <<- data.frame(keys_df)
  rec_loneliness_avg_df$rec_loneliness_avg <<- dissdata$recloneliness_avg

  log_loneliness_avg_df <<- data.frame(keys_df)
  log_loneliness_avg_df$log_loneliness_avg <<- dissdata$logloneliness_avg
  
  loneliness_all_df <<- data.frame(keys_df)
  loneliness_all_df$loneliness_all <<- dissdata$lonelinessall
  
  # logdepression
  log_depression_df <<- data.frame(keys_df)
  log_depression_df$log_depression <<- dissdata$logdepression
  
  # stress
  stress_df <<- data.frame(keys_df)
  stress_df <<- load_columns(dissdata, stress_df, 'stress', 'stress', 4)
  stress_all_df <<- data.frame(keys_df)
  stress_rows <- as.data.frame.list(stress_df[, c(var_start_index:ncol(stress_df))])
  stress_all_df$stress_all <<- rowMeans(stress_rows)
  
  print(head(stress_all_df))
  
  # vitality
  vitality_df <<- data.frame(keys_df)
  vitality_df$vitality <<- dissdata$vitality_all
  # TODO: Find mean of this
  #vitality_df <<- load_columns(dissdata, vitality_df, 'vitality', 'vitality', 7)
  
  tech_experience_df <<- data.frame(keys_df)
  tech_experience_df <<- load_columns(dissdata, tech_experience_df, 'techexp', 'tech_experience', 5)
  
  # qualcomm
  quality_comm_df <<- data.frame(keys_df)
  quality_comm_df <<- load_columns(dissdata, quality_comm_df, 'qualcomm', 'quality_comm', 7)
  
  control_df <<- data.frame(keys_df)
  control_df$time_at_mara <<- dissdata$timeatMara
  control_df$reasons_for_mara <<- dissdata$reasonforMara
  control_df$family_close <<- dissdata$familyclose
  control_df$family_talk <<- dissdata$familytalk
}

load_data()

vitality_df_wide <- make_wide(vitality_df, 'vitality')
vitality_df_imputed <- impute(vitality_df_wide)
vitality_avg234 <- take_out_baseline(vitality_df_imputed, 'vitality')
head(vitality_avg234)
vitality_baseline <- baseline_only(vitality_df_imputed, 'vitality')
vitality_df <- make_long(vitality_df_imputed, 'vitality')

log_depression_df_wide <- make_wide(log_depression_df, 'log_depression')
log_depression_df_imputed <- impute(log_depression_df_wide)
depression_avg234 <- take_out_baseline(log_depression_df_imputed, 'log_depression')
depression_baseline <- baseline_only(log_depression_df_imputed, 'log_depression')
log_depression_df <- make_long(log_depression_df_imputed, 'log_depression')

rec_loneliness_avg_df_wide <- make_wide(rec_loneliness_avg_df, 'rec_loneliness_avg')
rec_loneliness_avg_df_imputed <- impute(rec_loneliness_avg_df_wide)
rec_loneliness_avg234 <- take_out_baseline(rec_loneliness_avg_df_imputed, 'loneliness')
rec_loneliness_baseline <- baseline_only(rec_loneliness_avg_df_imputed, 'rec_loneliness_avg')
rec_loneliness_avg_df <- make_long(rec_loneliness_avg_df_imputed, 'rec_loneliness_avg')

log_loneliness_avg_df_wide <- make_wide(log_loneliness_avg_df, 'log_loneliness_avg')
log_loneliness_avg_df_imputed <- impute(log_loneliness_avg_df_wide)
log_loneliness_avg234 <- take_out_baseline(log_loneliness_avg_df_imputed, 'log_loneliness_avg')
log_loneliness_baseline <- baseline_only(log_loneliness_avg_df_imputed, 'log_loneliness_avg')
log_loneliness_avg_df <- make_long(log_loneliness_avg_df_imputed, 'log_loneliness_avg')

loneliness_all_df_wide <- make_wide(loneliness_all_df, 'loneliness_all')
loneliness_all_df_imputed <- impute(loneliness_all_df_wide)
loneliness_avg_avg234 <- take_out_baseline(loneliness_all_df_imputed, 'loneliness_all')
loneliness_baseline <- baseline_only(loneliness_all_df_imputed, 'loneliness_all')
loneliness_all_df <- make_long(loneliness_all_df_imputed, 'loneliness_all')

comm_orientation_all_df_wide <- make_wide(comm_orientation_all_df, 'comm_orientation_all')
comm_orientation_all_df_imputed <- impute(comm_orientation_all_df_wide)
comm_orientation_avg234 <- take_out_baseline(comm_orientation_all_df_imputed, 'comm_orientation_all')
comm_orientation_baseline <- baseline_only(comm_orientation_all_df_imputed, 'comm_orientation_all')
comm_orientation_all_df <- make_long(comm_orientation_all_df_imputed, 'comm_orientation_all')

stress_all_df_wide <- make_wide(stress_all_df, 'stress_all')
stress_all_df_imputed <- impute(stress_all_df_wide)
stress_avg234 <- take_out_baseline(stress_all_df_imputed, 'stress_all')
stress_baseline <- baseline_only(stress_all_df_imputed, 'stress_all')
stress_all_df <- make_long(stress_all_df_imputed, 'stress_all')

social_activity_all_df_wide <- make_wide(social_activity_all_df, 'social_activity_all')
social_activity_all_df_imputed <- impute(social_activity_all_df_wide)
social_activity_avg234 <- take_out_baseline(social_activity_all_df_imputed, 'social_activity_all')
social_activity_baseline <- baseline_only(social_activity_all_df_imputed, 'social_activity_all')
social_activity_all_df <- make_long(social_activity_all_df_imputed, 'social_activity_all')

joined_df <- inner_join(x = vitality_df, y = log_depression_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = rec_loneliness_avg_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = log_loneliness_avg_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = loneliness_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = comm_orientation_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = stress_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = social_activity_all_df, by = c('sub_id', 'intervention', 'week'))


joined_df_avgs_vitality <- full_join(x = joined_df, y = vitality_avg234, by = c('sub_id', 'intervention'))
head(joined_df_avgs_vitality)
joined_df_avgs_depression <- full_join(x = joined_df, y = depression_avg234, by = c('sub_id', 'intervention'))
joined_df_avgs_rec_loneliness <- full_join(x = joined_df_with_avgs, y = rec_loneliness_avg234, by = c('sub_id', 'intervention'))
head(joined_df_avgs_rec_loneliness)
joined_df_avgs_log_loneliness <- full_join(x = joined_df_with_avgs, y = log_loneliness_avg234, by = c('sub_id', 'intervention'))
joined_df_avgs_loneliness <- full_join(x = joined_df_with_avgs, y = loneliness_avg_avg234, by = c('sub_id', 'intervention'))
joined_df_avgs_comm_orientation <- full_join(x = joined_df_with_avgs, y = comm_orientation_avg234, by = c('sub_id','intervention'))
joined_df_avgs_stress <- full_join(x = joined_df_with_avgs, y = stress_avg234, by = c('sub_id','intervention'))
joined_df_avgs_social_activity <- full_join(x = joined_df_with_avgs, y = social_activity_avg234, by = c('sub_id','intervention'))
head(joined_df_avgs_social_activity)

joined_df$vitality_avg234 <- joined_df_avgs_vitality$vitality_avg234
joined_df$depression_avg234 <- joined_df_avgs_depression$log_depression_avg234
joined_df$rec_loneliness_avg234 <- joined_df_avgs_rec_loneliness$loneliness_avg234
joined_df$comm_orientation_avg234 <- joined_df_avgs_comm_orientation$comm_orientation_all_avg234
joined_df$stress_avg234 <- joined_df_avgs_stress$stress_all_avg234
joined_df$social_activity_avg234 <- joined_df_avgs_social_activity$social_activity_all_avg234
head(joined_df)

joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = vitality_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = depression_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = rec_loneliness_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = log_loneliness_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = loneliness+avg_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = comm_orientation_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = stress_baseline, by = c('sub_id', 'intervention'))
joined_df_with_baseline <- full_join(x = joined_df_with_avgs, y = social_activity_baseline, by = c('sub_id', 'intervention'))
head(joined_df_with_baseline)

joined_df$vitality_baseline <- joined_df_with_baseline$vitality_avgbase
joined_df$depression_baseline <- joined_df_with_baseline$log_depression_avgbase
joined_df$rec_loneliness_baseline <- joined_df_with_baseline$rec_loneliness_avg_avgbase
joined_df$comm_orientation_baseline <- joined_df_with_baseline$comm_orientation_all_avgbase
joined_df$stress_baseline <- joined_df_with_baseline$stress_all_avgbase
joined_df$social_activity_baseline <- joined_df_with_baseline$social_activity_all_avgbase
head(joined_df)
  
write.csv(joined_df, 'dissdata_complete.csv')

#Removing people with only one time point
cleaned_df <- subset(joined_df, sub_id != 'A4' & sub_id != 'B5' & sub_id != 'F1')

#Dummy Coding Intervention
cleaned_df$intervention <- gsub('I', '1', cleaned_df$intervention)
cleaned_df$intervention <- gsub('C', '0', cleaned_df$intervention)

head(cleaned_df)

cleaned_control_df <- subset(control_df, week == 1)
cleaned_control_df <- within(cleaned_control_df, rm('week'))
head(cleaned_control_df)

#adding in control vars to dataset
my_df <- inner_join(x = cleaned_control_df, y = cleaned_df, by = c('sub_id', 'intervention'))

#Fitting the unconditional models, with no predictors besides the time variable, Week, 
#which is an important first step when exploring the data and gives insight into the data to be explored
library(lme4)
#first, need to subtract 1 from week so that time starts at zero
class(cleaned_df$week)
cleaned_df$week <- as.numeric(cleaned_df$week)
cleaned_df <- mutate(cleaned_df, week = week - 1) 

#It is good practice to standardize your explanatory variables before proceeding so that they have a 
#mean of zero and sd of 1. It ensures that the estimated coefficients are all on the same scale, making
#easier to compare effect sizes. 
#scale centers the data (the comlumn mean is subtracted from the values in the column) and then scale it
#(the centered column values are divided by the column's sd). 
cleaned_df$comm_orientation_avg234 <- scale(cleaned_df$comm_orientation_avg234)

colMeans(cleaned_df$rec_loneliness_avg234)

library(lme4)
packageVersion("lme4")
library(nlme)
install.packages("numDeriv")
library(numDeriv)
install.packages("RCurl")
library(RCurl)
install.packages("reshape")
library(reshape)
install.packages("plyr")
library(plyr)
library(RColorBrewer)
library(ggplot2)
install.packages("lmerTest")
library(lmerTest)

#communal orientation
library(nlme)
head(cleaned_df)
model1_co <- lme(comm_orientation_all ~ 1, random = ~1 |sub_id, data = cleaned_df, method = "ML")
summary(model1_co)
#this tells us we can reject the null 
intervals(model1_co)
#Calculate ICC
(1.016626^2) / ((1.016626^2) + (0.7802982^2)) 

#Unconditional Growth Model
#Begin by plotting data to see individual slopes
library(lattice)
xyplot(comm_orientation_all ~ week | sub_id, data = cleaned_df, type = c("p", "r"))

#Now run unconditional growth models
#unconditional growth model (mod2) time as a fixed slope
mod2_co <- lme(comm_orientation_all~week, random = ~1|sub_id, data = cleaned_df, method = "ML")
summary(mod2_co)
intervals(mod2_co)
(1.019348^2) / ((1.019348^2) + (0.7659641^2)) 
#now run the unconditional growth model with time as a random slope
ctrl <- lmeControl(opt = 'optim')
mod3_co <- lme(comm_orientation_all~week, random = ~week|sub_id, data = cleaned_df, method = "ML", control = ctrl)
summary(mod3_co)
intervals(mod3_co)
(1.1498172^2) / ((1.1498172^2) + (0.7548625^2))
#run deviant stats to compare the models 
#first, rerun the first model (unconditional)
model1_co <- lme(comm_orientation_all ~ 1, random = ~1 |sub_id, data = cleaned_df, method = "ML")
summary(model1_co)
intervals(model1_co)

#Compare unconditional means model (mod1) to unconditional growth model with a fixed slope (mod2)
(results <-anova(model1_co, mod2_co))
results$'p-value'

#Compare unconditional means model (mod1) to unconditional growth model with a random slope (mod2)
(results <-anova(model1_co, mod3_co))
results$'p-value'

#Starting to build the conditional growth model (full model)
#including all the predictors
mod4_co <- lme(comm_orientation_all ~ week + intervention, random = ~1 | sub_id, data = cleaned_df, method = "ML")
summary(mod4_co)
intervals(mod4_co)
interaction.plot(cleaned_df$week, cleaned_df$intervention)


#generating model predicted values are helpful
data_agg <- cleaned_df %>%
  mutate(pred_values = predict(random_int_social_activity))
library(ggplot2)
ggplot(data_agg, aes(x = week, y = pred_values)) + 
  geom_line(aes(group = sub_id), alpha = 0.6) +
  theme_bw(base_size = 16) + #changes the default theme
  xlab("Weeks") +
  ylab("social engagement")

#using nlme - unconditional model
#Model formulation
#Level 1 Yij Level 2 β0j =β0j+Rij=γ00+U0j(1)(2)(3)
#with,
#U0j∼(0, τ200),(4)
#and
#Rij∼(0, σ2)(5)
#To fit this model we run

library(nlme)

lme(social_activity_all ~ 1, random = ~1 | sub_id, data = cleaned_df)
#Unconditional growth model
lme(social_activity_all ~ week, random = ~week | sub_id, data = cleaned_df)


random_intercept_comm_orientation_all <- lmer(comm_orientation_all ~ 1 + week + intervention +
                                                (1|sub_id), data = my_df)
summary(random_intercept_comm_orientation_all)

random_intercept_stress_all <- lmer(stress_all ~ 1 + week + (1|sub_id), 
                                    data = my_df)
summary(random_intercept_stress_all)

random_intercept_loneliness_all <- lmer(rec_loneliness_avg ~ 1 + week + 
                                          (1|sub_id), data = my_df)
summary(random_intercept_loneliness_all)

random_intercept_depression_all <- lmer(log_depression ~ 1 + week + (1|sub_id), 
                                        data = my_df)
summary(random_intercept_depression_all)

random_intercept_loneliness_all <- lmer(rec_loneliness_avg ~ 1 + week + (1|sub_id), 
                                        data = my_df)
summary(random_intercept_loneliness_all)

devtools::install_github("doomlab/MeMoBootR")
library(MeMoBootR)
head(my_df)
#H1
saved1 = mediation1(y = 'social_activity_all', #DV 
                    x = 'intervention', #IV
                    m = 'comm_orientation_all', 
                    cvs = c('week', 'time_at_mara', 'family_close', 'family_talk'), #Any covariates
                    df = my_df, #dataframe
                    with_out = T, #not required but can change 
                    nboot = 1000, #number of bootstraps
                    conf_level = .95) #CI width
summary(saved1)
#outlier information is in the DF
View(saved$datascreening$fulldata)
#additivity
saved$datascreening$correl
#linearity
saved$datascreening$linearity
#normality
saved$datascreening$normality
#homogs
saved$datascreening$homogen
####view the analysis###
summary(saved$model1) #c path
summary(saved$model2) #a path
summary(saved$model3) #b path

saved2 = mediation1(y = 'vitality', #DV 
                   x = 'intervention', #IV
                   m = 'comm_orientation_all', 
                   cvs = c('week'), #Any covariates
                   df = cleaned_df, #dataframe
                   with_out = T, #not required but can change 
                   nboot = 1000, #number of bootstraps
                   conf_level = .95) #CI width
summary(saved2)
####view the analysis###
summary(saved2$model1) #c path
summary(saved2$model2) #a path
summary(saved2$model3) #b path

saved3 = mediation1(y = 'log_depression', #DV 
                    x = 'intervention', #IV
                    m = 'comm_orientation_all', 
                    cvs = c('week'), #Any covariates
                    df = cleaned_df, #dataframe
                    with_out = T, #not required but can change 
                    nboot = 1000, #number of bootstraps
                    conf_level = .95) #CI width
summary(saved3)
####view the analysis###
summary(saved3$model1) #c path
summary(saved3$model2) #a path
summary(saved3$model3) #b path

saved4 = mediation1(y = 'stress_avg234', #DV 
                    x = 'intervention', #IV
                    m = 'comm_orientation_avg234', 
                    cvs = c('comm_orientation_avg234'), #Any covariates
                    df = cleaned_df, #dataframe
                    with_out = T, #not required but can change 
                    nboot = 1000, #number of bootstraps
                    conf_level = .95) #CI width
summary(saved4)
####view the analysis###
summary(saved4$model1) #c path
summary(saved4$model2) #a path
summary(saved4$model3) #b path

saved5 = mediation1(y = 'rec_loneliness_avg', #DV 
                    x = 'intervention', #IV
                    m = 'comm_orientation_all', 
                    cvs = c('week'), #Any covariates
                    df = cleaned_df, #dataframe
                    with_out = T, #not required but can change 
                    nboot = 1000, #number of bootstraps
                    conf_level = .95) #CI width
summary(saved2)
####view the analysis###
summary(saved5$model1) #c path
summary(saved5$model2) #a path
summary(saved5$model3) #b path
