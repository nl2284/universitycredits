library(dplyr)
library(ggplot2)


students<-readRDS("summary_student2.rds")


###Step 1: to determine how many recent semesters to exclude from analysis, 
###we need to exclude the students who are still working on their degree. 
###Explanation: If for 99% of the case, the distance between two registered semesters are less than 3, 
### then if a student take a leave for more than 3 semesters without finishing the program,
###we can assume the student dropped out. 
###Based on the above logic, we can determine the cutoff last enrolled semester to be 201603. 
###For majority of students who last enrolled during or before 201603, 
###they have finished their program requirement, units_earned>=units_required.
###If a student was last registered during or before 201603, has not finished the program,
###3 semseters have passed(201701, 201702, 201703), we assume they have dropped out. 
###For students whose last enrolled semester is in 2017 and haven't finish a program, 
###they are likely to continue registering for classes. So we should exclude these cases.


###1.1 The following code find out the distribution of distance between two registered semesters of students
students<-students %>%
  group_by(student.id)%>%
  mutate(lagSem=lag(whichSem, 1),
         diff=abs(whichSem-lagSem))
###The 95 percentile of diff is 2, 99 percentile of diff is 3 semesters,
###we can assume if a student did not register for classes for 3 semesters (before completing the program)
####the student is very likely to drop out.

###Based on the graph and summary, majority of the distances is 1, 
###meaning in most cases, students registered for courses without taking a leave.
ggplot(students, aes(x=diff))+
  geom_histogram()
summary(students2$diff) 
quantile(students2$diff, c(0.95, 0.99), na.rm=TRUE)


###Step 2: Some programs's length information is missing, this step get this information by looking
###at the distribution

###2.1. Select 11 programs, need to check with Jenna if some programs need to be excluded
###
programs<-c("DSW-6 Semester", "MSN-8 Semester PT",
            "MSN-5 Semester FT", "MSW-",
            "MSW-8 Semester", "MSW-Modified 8 Semester",
            "MSW-6 Semester","MSW-4 Semester",
            "MSW-5 Semester AS","MSW-3 Semester AS","MSW-Part Time")

###2.2. the "MSW-Part Time" has no length information
####check out the median, 90 percentile, and 95 percentile of the number of semesters students took.
####the 95 percentile is 9, so I determine the length to be 9 semesters. 

num_sem_parttime<-students%>%filter(curriculum_name=="MSW-Part Time") %>%
  group_by(student.id) %>%
  summarise(num_sem=n()) 
quantile(num_sem_parttime$num_sem, c(0.5, 0.90, 0.95))

###2.3.the "MSW-" has no length information
####check out the median, 90 percentile, and 95 percentile of the number of semesters students took.
####the 95 percentile is 7, so I determine the length to be 7 semesters. 

num_sem_MSW<-students%>%filter(curriculum_name=="MSW-") %>%
  group_by(student.id) %>%
  summarise(num_sem=n()) 
quantile(num_sem_MSW$num_sem, c(0.5, 0.90, 0.95))


###Step 3: creat a data frame with a grain size of per curriculum program per student, 
###one row is one student. Including variables that can determine DROPOUT or might predict dropout likelihood.
###Note: some of the programs are relatively new, so there are not enough students, 
####includingMSN-5 Semester FT (3 students), MSN-8 Semester PT(4 students)

sum_students=students %>% 
  ###It is possible that if students are enrolled in 2017, 
  ####they may be in the progress of finishing a program. If students last enrolled in 2016 and 
  ### didn't enroll in 2017 (three semesters), they either finished the program or dropped out.
  ###so we can only analyze the cases where last.enrolled.code<=201603
  filter(curriculum_name %in% programs,last.enrolled.code<=201603)%>%
  ###add length informaiton
  mutate(program.length.code=ifelse(curriculum_name=="MSW-Part Time",
                9, ifelse(curriculum_name=="MSW-Modified 8 Semester", 8, 
                          ifelse(curriculum_name=="MSW-", 7, program.length.code))), 
         ###creat a new variable to indicate if a student ever received scholarship
         scholarship=ifelse(scholarship.names !="", 1, 0)) %>%
  group_by(curriculum_name, student.id)%>%
  summarize(virtual_campus=first(virtual_campus), ## if the program is virtual or on campus
            program.length.code=mean(program.length.code), ##program length, numeric, same across all rows, use mean function
            gender=first(gender),
            age=mean(age), 
            ethnicity=first(ethnicity),
            zip=first(zip),
            ###determine the distance between last enrolled semester of the student to the first enrolled semseter
            whichSem=max(whichSem), 
            first.enrolled.code=mean(first.enrolled.code),
            last.enrolled.code=mean(last.enrolled.code),
            mean_units=mean(registered.units),
            min_units=min(registered.units),
            max_units=max(registered.units),
            units.earned=mean(units.earned),
            required_units=mean(required_units),
            gpa=mean(gpa),
            last.semester.gpa=mean(last.semester.gpa),
            scholarshipYN=ifelse(sum(scholarship)!=0, 1, 0)) 

###Step 4: determine the rule of dropping out:
###last enrolled during or before 201603, units earned<required_units

sum_students=sum_students%>%
  mutate(dropout=ifelse(units.earned<required_units, 1, 0)) 

write.csv(x = sum_students, file="sum_students.csv")

###Step 5: get the dropout rate for each program
sum_students %>%
  group_by(curriculum_name) %>%
  summarize(dropout_rate=mean(dropout))

###Step 6: This analysis shows for students who have completed program requirement, 
###how many semesters more students need to complete a program. The 95 percentile is 4, meaning for 95% of 
##the time, students who finish their programs need 4 extra semesters or less. 
sum_students2=sum_students %>% filter(units.earned>=required_units)%>%
  mutate(sem_more=whichSem-program.length.code)
quantile(sum_students2$sem_more, c(0.95, 0.99), na.rm=TRUE)

##The graph shows majority of students do not need extra semesters.
sum_students2 %>% filter(units.earned>=required_units)%>%
  mutate(sem_more=whichSem-program.length.code) %>%
  ggplot(aes(x=sem_more))+
  geom_histogram()


###Step 7. This analysis during which semesters students are most likely to drop out
sum_students_dropout<-sum_students %>%
  filter(units.earned<required_units)

ggplot(sum_students_dropout, aes(x=whichSem))+geom_histogram()
summary(sum_students_dropout$whichSem)
###The result show 25% students who drop out after the 1st semester, 
###25% students drop out after the 2nd semester, 25% students who drop out after 3 semester
quantile(sum_students_dropout$whichSem, c(0.25, 0.50, 0.75, 0.90, 0.95), na.rm=TRUE)









  










  