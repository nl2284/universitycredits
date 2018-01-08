library(dplyr)
library(ggplot2)
library(stringr)


units<-readRDS(file ="enrolled_active_units.RDS")

# Data exploration: the distribution of number of enrolled students, 
###and total units are highly skewed to the right. This means linear regression may not be a good model
units %>% ggplot(aes(x=enrolled))+geom_histogram()
units%>%ggplot(aes(x=totalUnits))+geom_histogram()
###a log transformation might normalize the data
units%>%ggplot(aes(x=log(totalUnits)))+geom_histogram()





###The five programs were excluded in the analysis due to too few observations 
###and too few students, including "DSW-", 
###"DSW 3 Semester AS", "DWS-4 Semester", "DSW-8 Semester", "DSW Modified 8 Semester"


programs<-c("DSW-6 Semester", "MSN-8 Semester PT",
            "MSN-5 Semester FT", "MSW-",
            "MSW-8 Semester", "MSW-Modified 8 Semester",
            "MSW-6 Semester","MSW-4 Semester",
            "MSW-5 Semester AS","MSW-3 Semester AS","MSW-Part Time")

## the relationship between the number of active students and total 
####units follow a linear relationship. Based on the graph, there are three clusters
###of programs that are close in the active students and total units regression slopes
units %>% filter(curriculum_name %in% programs) %>%
  ggplot(aes(x=active, 
             y=totalUnits, 
             color=curriculum_name))+
  geom_point()+
  geom_smooth(method="lm")


###The rest of the 11 programs are recoded to "low", "medium", "high"
###in terms of required units per student 

units_subset=units%>%filter(curriculum_name %in% programs) %>%
  mutate(curr_cluster=ifelse(curriculum_name %in% c(
    "DSW-6 Semester",
    "MSN-8 Semester PT", 
    "MSW-8 Semester",
    "MSW-5 Semester AS",
    "MSW-Modified 8 Semester",
    "MSW-Part Time"), "low", ifelse(
      curriculum_name %in% c(
        "MSN-5 Semester FT",
        "MSW-",
        "MSW-6 Semester",
        "MSW-3 Semester AS"
      ), "medium", "high"
    )),
    curr_clusterlow=ifelse(curr_cluster=="low", 1, 0),
    curr_clustermedium=ifelse(curr_cluster=="medium", 1,0)) %>%
  select(1, 19:21, 2:18) ##rearrange the columns

###I am not sure if cross validation is needed to avoid overfitting. I get 70% of 
###the data as training dataset and 30% of the data as testing dataset
#set.seed(123)
#train_ind <- sample(seq_len(nrow(units_subset)), size = floor(0.70*nrow(units_subset)))
#units_subset_train=units_subset[train_ind,]
#units_subset_test=units_subset[-train_ind,]


###The following three regression models shows total units can be very well 
####predicted by the number of active students. It is good to know but not important.The problem is to
###predict number of active students and total registered units with other variables.
### Number of enrolled students in previous semesters may be good predictors

#library(nlme)
#models<-lmList(totalUnits~active | curr_cluster,units_subset, na.action=na.exclude)
#lapply(models, summary)


#Method 1: without variable transformation
### use enrollment from previous years to predict total units
###The following analyses are not ideal since number of enrolled students are highly skewed, violate 
##assumptions for regression.


####Predict total units with enrollment data, the overall model seems good with R square of 0.9647, but 
###residual plot shows a fanning shape, and many predicted values are <0 which does not make sense.
overallmodel<-
  lm(data=filter(units_subset, semester.code<201801), 
     formula=log(totalUnits)~enrolled+enroll_sem_.1+enroll_sem_.2
     +enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(overallmodel)

###test some assumptions of linear regression
mean(overallmodel$residuals)
plot(overallmodel)


###Model 1 add curr_cluster variable to the model
model1=lm(data=filter(units_subset, semester.code<201801), formula=totalUnits~curr_cluster+enrolled+enroll_sem_.1+ enroll_sem_.2+
                    enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(model1)
mean(model1$residuals)
plot(model1)

##remove enroll_sem_.2
model2=lm(data=filter(units_subset_train, semester.code<201801), formula=totalUnits~curr_cluster+enrolled+enroll_sem_.1+
            enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(model2)


###Model 3: add polynomial to the model, removing enroll_sem_.5
model3=lm(data=filter(units_subset, semester.code<201801), formula=totalUnits~
            curr_cluster+
            enrolled+
            enroll_sem_.1+I(enroll_sem_.1^2)+
            enroll_sem_.2+I(enroll_sem_.2^2)+
            enroll_sem_.3+
            enroll_sem_.4)
summary(model3)
mean(model3$residuals)
plot(model3)

