library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)


units<-readRDS(file ="enrolled_active_units.RDS")

# the distribution of number of enrolled students is highly skewed to the right, with many 0s
units %>% ggplot(aes(x=enrolled))+geom_histogram()

###The five programs were excluded in the analysis due to too few observations 
###and too few students, including "DSW-", 
###"DSW 3 Semester AS", "DWS-4 Semester", "DSW-8 Semester", "DSW Modified 8 Semester"

units%>%
  group_by(curriculum_name)%>%
  summarize(numsem=n(), 
            averageactive=mean(active),
            averageenroll=mean(enrolled),
            averageunits=mean(totalUnits))

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
                                                          ))) %>%
  select(1, 19, 2:18) ##rearrange the columns






###The following three regression models shows total units can be very well 
####predicted by the number of active students. It is good to know but not important.The problem is to
###predict number of active students and total registered units with other variables.
### Number of enrolled students in previous semesters may be good predictors

#library(nlme)
#models<-lmList(totalUnits~active | curr_cluster,units_subset, na.action=na.exclude)
#lapply(models, summary)



#*The following analyses are not ideal since number of enrolled students are highly skewed, violate 
##assumptions for regression.


####Predict total units with enrollment data
overallmodel<-
  lm(data=units_subset, formula=totalUnits~enrolled)
summary(overallmodel)

###the model is not good, residuals not well behaved
units%>%mutate(predicted=582.567+33.365*enrolled) %>%
  ggplot(aes(y=predicted, x=totalUnits))+geom_point()

###seperate analysis by three curriculum clusters
cluster1Model<-lm(data=filter(units_subset, curr_cluster=="low"), 
                    formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
                      enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(cluster1Model)


cluster2Model<-lm(data=filter(units_subset, curr_cluster=="medium"), 
                  formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
                    enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(cluster2Model)


cluster3Model<-lm(data=filter(units_subset, curr_cluster=="high"), 
                  formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
                    enroll_sem_.3)
summary(cluster3Model)


###furthur seperate by program

linearmodel1<-
  lm(data=filter(units_subset, curriculum_name=="DSW-6 Semester"), 
     formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5)
summary(linearmodel1)

linearmodel2<-
  lm(data=filter(units_subset, curriculum_name=="MSN-8 Semester PT"), 
     formula=totalUnits~enrolled+enroll_sem_.1+enroll_sem_.5)
summary(linearmodel2)

linearmodel3<-
  lm(data=filter(units_subset, curriculum_name=="MSN-5 Semester FT"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4, na.action =na.exclude)
summary(linearmodel3)

linearmodel4<-
  lm(data=filter(units_subset, curriculum_name=="MSW-8 Semester"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel4)

###Not good prediction
linearmodel5<-
  lm(data=filter(units_subset, curriculum_name=="MSW-"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel5)

###Not significant, 
linearmodel6<-
  lm(data=filter(units_subset, curriculum_name=="MSW-Modified 8 Semester"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel6)


linearmodel7<-
  lm(data=filter(units_subset, curriculum_name=="MSW-6 Semester"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel7)

linearmodel8<-
  lm(data=filter(units_subset, curriculum_name=="MSW-4 Semester"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel8)

linearmodel9<-
  lm(data=filter(units_subset, curriculum_name=="MSW-5 Semester AS"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel9)

###not significant
linearmodel10<-
  lm(data=filter(units_subset, curriculum_name=="MSW-3 Semester AS"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel10)

linearmodel11<-
  lm(data=filter(units_subset, curriculum_name=="MSW-Part Time"), formula=totalUnits~enrolled+enroll_sem_.1+ enroll_sem_.2+
       enroll_sem_.3+enroll_sem_.4+enroll_sem_.5, na.action =na.exclude)
summary(linearmodel11)




###get the total number of enrolled students in previous N semesters, and use it to predict number of 
###active students and total registered units of the current semester. It is possible that this variable 
###will be relatively normal since it's a sum.

                                    ##Code to be developed##

