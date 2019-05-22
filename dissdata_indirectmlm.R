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

library(dplyr)
library(tidyr)
library(corrr)
library(mice)
library(stringr)
library(lme4)
library(ggplot2)
library(boot)
library(sjPlot)
library(DescTools)
library(psy)
library(apaTables)
library(haven)

setwd("~/Desktop/Dissertation")
dissdata_social <- read_sav("dissdata_socialactivity.sav")
  
#Filling in missing data (this is for variables that were only measured once but will not change)
  library(tidyr)
  dissdata %>%
    tidyr::complete(subid, Week)
  dissdata %>%
    tidyr::fill(VideoConsent)
  dissdata_fill <- dissdata %>%
    tidyr::fill(VideoConsent, Age, Sex, Ethnicity, timeatMara, 
                reasonforMara, familyclose, familytalk, groupknowwell)
  dissdata <- dissdata_fill
  head(dissdata)
  
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
  imputed_df <- mice(input_df[,c(3, 4, 5, 6)], m = 1, seed = 5, meth = "pmm")
  
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

relationship_df <- {}
tech_experience_df <- {}
social_activity_df <- {}
social_activity_all_df <- {}
comm_orientation_all_df <- {}
rec_loneliness_avg_df <- {}
loneliness_all_df <- {}
log_depression_df <- {}
stress_df <- {}
stress_all_df <- {}
quality_comm_df <- {}
vitality_df <- {}
control_df <- {}
emo_cap_df <- {}
community_df <-{}
ux_df <- {}

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
  relationship_df$relationship_status <<- dissdata$relationstatus
  relationship_df$romantic_partner_live <<- dissdata$rompartlive
  relationship_df$romantic_partner_care <<- dissdata$rompartcare
  
  # socialactivity aka socialengagement
  social_activity_df <<- data.frame(keys_df)
  social_activity_df <<- load_columns(dissdata, social_activity_df, 'socialactivity', 'social_activity', 4)
  social_activity_all_df <<- data.frame(keys_df)
  social_activity_rows <- as.data.frame.list(social_activity_df[, c(var_start_index:ncol(social_activity_df))])
  social_activity_all_df$social_activity_all <<- rowMeans(social_activity_rows)
  head(social_activity_all_df)
  
  ux_df <<- data.frame(keys_df)
  ux_df$ux_all <<- dissdata$ux_all
  
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
  
  # vitality
  vitality_df <<- data.frame(keys_df)
  vitality_df$vitality <<- dissdata$vitality_all
  
  # qualcomm
  quality_comm_df <<- data.frame(keys_df)
  quality_comm_df <<- load_columns(dissdata, quality_comm_df, 'qualcomm', 'quality_comm', 7)
  
  #emotionl cap 
  emo_cap_df <<-data.frame(keys_df)
  emo_cap_df$emo_cap_all <<- dissdata$emotionalcap_all
  
  #community
  community_df <<-data.frame(keys_df)
  community_df$community_all <<- dissdata$senseofcomm_all
  
  control_df <<- data.frame(keys_df)
  control_df$time_at_mara <<- dissdata$timeatMara
  control_df$reasons_for_mara <<- dissdata$reasonforMara
  control_df$family_close <<- dissdata$familyclose
  control_df$family_talk <<- dissdata$familytalk
  control_df$friends <<- dissdata$friendsMara
  control_df$staff <<- dissdata$staffMara
  control_df$tech_experience <<- dissdata$techexp_all
  control_df$group_know <<- dissdata$groupknowwell
  control_df$group_know2 <<- dissdata$`groupknowwell#`
  control_df$group_stranger <<- dissdata$groupstranger
  control_df$group_stranger2 <<- dissdata$`groupstranger#
  control_df$marital_status <<- dissdata$maritalstatus
  control_df$relationship_status <<- dissdata$relationstatus
  control_df$romantic_partner_live <<- dissdata$rompartlive
  control_df$romantic_partner_care <<- dissdata$rompartcare
  control_df$ux_all <<- dissdata$ux_all`
}

load_data()

emo_cap_df_wide <- make_wide(emo_cap_df, 'emo_cap_all')
emo_cap_df_imputed <- impute(emo_cap_df_wide)
emo_cap_df <- make_long(emo_cap_df_imputed, 'emo_cap_all')

community_df_wide <- make_wide(community_df, 'community_all')
community_df <- make_long(community_df_wide, 'community_all')

vitality_df_wide <- make_wide(vitality_df, 'vitality')
vitality_df_imputed <- impute(vitality_df_wide)
vitality_df <- make_long(vitality_df_imputed, 'vitality')

log_depression_df_wide <- make_wide(log_depression_df, 'log_depression')
log_depression_df_imputed <- impute(log_depression_df_wide)
log_depression_df <- make_long(log_depression_df_imputed, 'log_depression')

rec_loneliness_avg_df_wide <- make_wide(rec_loneliness_avg_df, 'rec_loneliness_avg')
rec_loneliness_avg_df_imputed <- impute(rec_loneliness_avg_df_wide)
rec_loneliness_avg_df <- make_long(rec_loneliness_avg_df_imputed, 'rec_loneliness_avg')

log_loneliness_avg_df_wide <- make_wide(log_loneliness_avg_df, 'log_loneliness_avg')
log_loneliness_avg_df_imputed <- impute(log_loneliness_avg_df_wide)
log_loneliness_avg_df <- make_long(log_loneliness_avg_df_imputed, 'log_loneliness_avg')

loneliness_all_df_wide <- make_wide(loneliness_all_df, 'loneliness_all')
loneliness_all_df_imputed <- impute(loneliness_all_df_wide)
loneliness_all_df <- make_long(loneliness_all_df_imputed, 'loneliness_all')

comm_orientation_all_df_wide <- make_wide(comm_orientation_all_df, 'comm_orientation_all')
comm_orientation_all_df_imputed <- impute(comm_orientation_all_df_wide)
comm_orientation_all_df <- make_long(comm_orientation_all_df_imputed, 'comm_orientation_all')

stress_all_df_wide <- make_wide(stress_all_df, 'stress_all')
stress_all_df_imputed <- impute(stress_all_df_wide)
stress_all_df <- make_long(stress_all_df_imputed, 'stress_all')

social_activity_all_df_wide <- make_wide(social_activity_all_df, 'social_activity_all')
social_activity_all_df_imputed <- impute(social_activity_all_df_wide)
social_activity_all_df <- make_long(social_activity_all_df_imputed, 'social_activity_all')

joined_df <- inner_join(x = vitality_df, y = log_depression_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = rec_loneliness_avg_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = log_loneliness_avg_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = loneliness_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = comm_orientation_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = stress_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = social_activity_all_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = emo_cap_df, by = c('sub_id', 'intervention', 'week'))
joined_df <- inner_join(x = joined_df, y = community_df, by = c('sub_id', 'intervention', 'week'))
head(joined_df)

#Removing people with only one time point
cleaned_df <- subset(joined_df, sub_id != 'A4' & sub_id != 'B5' & sub_id != 'F1')
str(cleaned_df)

#Dummy Coding Intervention
cleaned_df$intervention <- gsub('I', '1', cleaned_df$intervention)
cleaned_df$intervention <- gsub('C', '0', cleaned_df$intervention)

cleaned_control_df <- subset(control_df, week == 1)
cleaned_control_df <- within(cleaned_control_df, rm('week'))
head(cleaned_control_df)

cleaned_relationship_df <- subset(relationship_df, week == 1)
cleaned_relationship_df <- within(cleaned_relationship_df, rm('week'))
head(cleaned_relationship_df)

cleaned_control_df$intervention <- gsub('I', '1', cleaned_control_df$intervention)
cleaned_control_df$intervention <- gsub('C', '0', cleaned_control_df$intervention)

cleaned_relationship_df$intervention <- gsub('I', '1', cleaned_control_df$intervention)
cleaned_relationship_df$intervention <- gsub('C', '0', cleaned_control_df$intervention)

#adding in control vars to dataset
my_df <- inner_join(x = cleaned_control_df, y = cleaned_df, by = c('sub_id', 'intervention'))
my_df <- inner_join(my_df, y = cleaned_relationship_df, by = c('sub_id', 'intervention'))

apa.cor.table(my_df, filename="Table1_APA.doc", table.number=2)
corr_table <-cbind(
  'Vitality' = my_df$vitality,
  'Stress' = my_df$stress_all_win,
  'Depression' = my_df$log_depression,
  'Loneliness' = my_df$rec_loneliness_avg_win,
  'Social Activity' = my_df$social_activity_all_win,
  'Communal Orienation' = my_df$comm_orientation_all_win
)
apa.cor.table(corr_table, filename="Table2_APA.doc", table.number=2)

social_activity_only <- subset(dissdata, select = c(socialactivity1:socialactivity4))
cronbach(social_activity_only)
mean(my_df$social_activity_only)
sd(my_df$social_activity_only)

stress_only <- subset(dissdata, select = c(stress1:stress4))
cronbach(stress_only)
mean(my_df$stress_only)
sd(my_df$stress_only)

vitality_t1 <- my_df %>%
  filter(week == 1) %>%
  summarize(meanVit = mean(vitality))

CO_t1 <- my_df %>%
  filter(week == 1) %>%
  summarize(meanCO = mean(comm_orientation_all))
CO_t1

SA_t1 <- my_df %>%
  filter(week == 1) %>%
  summarize(meanSA = mean(social_activity_all))
SA_t1

stress_t1 <- my_df %>%
  filter(week == 1) %>%
  summarize(meanStress = mean(stress_all))
stress_t1

depression_t1 <- my_df %>%
  filter(week == 1) %>%
  summarize(meanStress = mean(stress_all))
stress_t1

#Time must start at zero
cleaned_df$week <- as.numeric(cleaned_df$week)
cleaned_df <- mutate(cleaned_df, week = week - 1) 

#Percentatge of values missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dissdata,2,pMiss)
apply(dissdata,1,pMiss)

#WINSORIZING - outliers 
my_df$rec_loneliness_avg_win <- Winsorize(my_df$rec_loneliness_avg)
my_df$comm_orientation_all_win <- Winsorize(my_df$comm_orientation_all)
my_df$stress_all_win <- Winsorize(my_df$stress_all)
my_df$social_activity_all_win <- Winsorize(my_df$social_activity_all)

#Filling Control Vars - again variables that were unlikely to change over 4 weeks
my_df <- fill(my_df, tech_experience, friends, staff, 
              group_stranger, community_all, relationship_status,
              romantic_partner_live, romantic_partner_care)

##### Indirect Effects MLM####
write.csv(my_df, "dissdata_ready_for_bootstrap.csv")
my_df <- read.csv("dissdata_ready_for_bootstrap.csv")

######communal orientation####
interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$comm_orientation_all, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Communal Orientation', 
                 col = c('blue2', 'red2')
                 )
                 
intervention2 = as.numeric(my_df$intervention2)
Plot.co<-ggplot(data=my_df, aes(x=week, y=comm_orientation_all_centered, group=intervention2))+
  geom_line(size=2, aes(color=intervention2))+
  ylim(0,4)+
  ylab("Communal Orientation")+
  xlab("Week")+
  ggtitle("Communal Orientation Over 4 Week \nVirtual Reality Intervention")
Plot.co

####vitality#####
output_vit <- boot(data = my_df,
                   statistic = indirect.mlm,
                   R = 5000,
                   x = 'intervention',
                   y = 'vitality',
                   mediator = 'comm_orientation_all_win',
                   group.id = 'sub_id',
                   covariates = c('week', 
                                  'week:intervention',
                                  'group_know', 
                                  'family_close',
                                  'time_at_mara', 
                                  'group_stranger',
                                  'family_talk',
                                  'romantic_partner_care',
                                  'marital_status'),
                   strata = my_df$sub_id,
                   uncentered.x = F,
                   between.m = T)

indirect.mlm.summary(output_vit)

plot(output_vit)

my_df$intervention2 <- factor(x = my_df$intervention, labels = c("TV", "VR"))

interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$vitality, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Vitality', 
                 col = c('blue2', 'red2')
)

#####stress####
output_stress <- boot(data = my_df,
                   statistic = indirect.mlm,
                   R = 5000,
                   x = 'intervention',
                   y = 'stress_all_win',
                   mediator = 'comm_orientation_all_win',
                   group.id = 'sub_id',
                   covariates = c('week', 
                                  'week:intervention',
                                  'group_know', 
                                  'family_close',
                                  'time_at_mara', 
                                  'group_stranger',
                                  'family_talk',
                                  'romantic_partner_care',
                                  'marital_status'),
                   strata = my_df$sub_id,
                   uncentered.x = F,
                   between.m = T)

indirect.mlm.summary(output_stress)

plot(output_stress)

interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$stress_all_win, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Stress', 
                 col = c('blue2', 'red2')
)

####depression#####
output_dep <- boot(data = my_df,
                   statistic = indirect.mlm,
                   R = 5000,
                   x = 'intervention',
                   y = 'log_depression',
                   mediator = 'comm_orientation_all_win',
                   group.id = 'sub_id',
                   covariates = c('week', 
                                  'week:intervention',
                                  'group_know', 
                                  'family_close',
                                  'time_at_mara', 
                                  'group_stranger',
                                  'family_talk',
                                  'romantic_partner_care',
                                  'marital_status'),
                   strata = my_df$sub_id,
                   uncentered.x = F,
                   between.m = T)

indirect.mlm.summary(output_stress)
plot(output_dep)

my_df$intervention2 <- factor(x = my_df$intervention, labels = c("TV", "VR"))

interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$log_depression, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Depression', 
                 col = c('blue2', 'red2')
)

####loneliness#####
output_lon <- boot(data = my_df,
                  statistic = indirect.mlm,
                  R = 5000,
                  x = 'intervention',
                  y = 'social_activity_all_win',
                  mediator = 'comm_orientation_all_win',
                  group.id = 'sub_id',
                  covariates = c('week', 
                                 'week:intervention',
                                 'group_know', 
                                 'family_close',
                                 'time_at_mara', 
                                 'group_stranger',
                                 'family_talk',
                                 'romantic_partner_care',
                                 'marital_status'),
                  strata = my_df$sub_id,
                  uncentered.x = F,
                  between.m = T)

indirect.mlm.summary(output_lon)
plot(output_lon)

my_df$intervention2 <- factor(x = my_df$intervention, labels = c("TV", "VR"))

interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$rec_loneliness_avg_win, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Loneliness', 
                 col = c('blue2', 'red2')
)
####social activity#####
class(my_df['comm_orientation_all_centered'][1,])
my_df$social_activity_all_win
output_sa <- boot(data = my_df,
                   statistic = indirect.mlm,
                   R = 5000,
                   x = 'intervention',
                   y = 'social_activity_all_win',
                   mediator = 'comm_orientation_all_win',
                   group.id = 'sub_id',
                  covariates = c('week', 
                                 'week:intervention',
                                 'group_know', 
                                 'family_close',
                                 'time_at_mara', 
                                 'group_stranger',
                                 'family_talk',
                                 'romantic_partner_care',
                                 'marital_status'),
                   strata = my_df$sub_id,
                   uncentered.x = F,
                   between.m = T)

indirect.mlm.summary(output_sa)

plot(output_sa)

interaction.plot(x.factor = my_df$week, 
                 trace.factor = my_df$intervention2, 
                 response = my_df$social_activity_all_win, 
                 fun = mean,
                 type = 'l',
                 trace.label = 'Intervention', 
                 xlab = 'Week', 
                 ylab = 'Social Activity', 
                 col = c('blue2', 'red2')
)