####Overall Notes: This script creates three data frames needed for furthur analysis. 
###The three data frames were created and saved: "summary_student2.RDS", 
####"enrolled_active_units.RDS", "enrolled_active_units_subset.RDS". 
###You may load the data frames and do analysis in a different R script

library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

###Step 1: Reshape the raw data file to create a data frame at the grain size of 
### per student per semester

credit<-read.csv("SFDC Student Course Final Cleaned.csv")

summary_student<-credit%>% 
  #1.1. exclude course records that are not in the major programs
  filter(grepl('SOWK|NURS', x=as.character(course.number)),
         post.code != "272", ###exclude doctoral program
         registered==TRUE)  %>% ###exclude dropped course records
###1.2. create new variables          
  mutate(current_year=as.numeric(substr(semester.code, start=1, stop=4)),
         current_sem=as.numeric(substr(semester.code, start=5, stop=6)),
         first_enrolled_year=as.numeric(substr(first.enrolled.code, start=1, stop = 4)),
         first_enrolled_sem=as.numeric(substr(first.enrolled.code, start=5, stop = 6)),
         last_enrolled_year=as.numeric(substr(last.enrolled.code, start=1, stop = 4)),
         last_enrolled_sem=as.numeric(substr(last.enrolled.code, start=5, stop = 6)),
         program.length.code=as.numeric(substr(program.length, start=1, stop=1)),
         num_sem_finished=(last_enrolled_year-first_enrolled_year)*3+
           (last_enrolled_sem-first_enrolled_sem)+1, ###compute how many semeters the a student has gone through
         sem_passed=(current_year-first_enrolled_year)*3+
           (current_sem-first_enrolled_sem)+1,  ####compute the current semester of the course for the student
         curriculum.full=as.factor(ifelse(post.code=="1411", "Virtual MSW", 
                                          ifelse(post.code=="1632","Virtual MSN",
                                                 ifelse(post.code=="1645", "Virtual DSW", "Campus MSW")))),
         ###Since the program delivered Virtually and on Campus have the same requirement, 
         ###I created a simplied code, by pasting the code with program length, I created curriculum name variable 
         curriculum.simplified=as.factor(ifelse(post.code=="1411", "MSW", 
                                                ifelse(post.code=="1632","MSN",
                                                       ifelse(post.code=="1645", "DSW", "MSW")))),
         curriculum_name=as.factor(paste(curriculum.simplified, program.length, sep="-")),
         virtual_campus=ifelse(post.code %in% c("1411", "1632", "1654"), "Virtual", "Campus")) %>%
  
  ###1.3: The following code create a data frame, showing total number of registered units per student per semester
  group_by(curriculum_name,student.id,semester.code, current_year, current_sem) %>%
  summarize(program.length.code=mean(program.length.code), ##keep the program length variable
            virtual_campus=first(virtual_campus), ##keep the virtual_campus variable
            whichSem=mean(sem_passed), ### which semester a student was in 
            registered.units=sum(units), ## regsitered units per student per semester
            gender=first(gender),
            ethnicity=first(ethnicity),
            age=mean(age),
            zip=first(billing.zip.postal.code),
            first.enrolled.code=first(first.enrolled.code),
            last.enrolled.code=first(last.enrolled.code),
            first_enrolled_year=mean(first_enrolled_year),
            first_enrolled_sem=mean(first_enrolled_sem),
            last_enrolled_year=mean(last_enrolled_year),
            last_enrolled_sem=mean(last_enrolled_sem),
            num_sem_finished=mean(num_sem_finished),
            #admission.term=first(admissions.term.code),
           # cohort.accepted=first(cohort.accepted),
           # cohort.current=first(cohort.current),
            #msn.cohort.accepted=first(msn.cohort.accepted),
          #  msn.cohort.current=first(msn.cohort.current),
           # dsw.cohort.accepted=first(dsw.cohort.accepted),
          #  dsw.cohort.current=first(dsw.cohort.current),
          #  program.length=first(program.length),
          #  applicant.response=first(applicant.response),
            gpa=mean(gpa),
            last.semester.gpa=mean(last.semester.gpa),
            units.attempted=mean(units.attempted),
            units.earned=mean(units.earned),
            scholarship.names=first(scholarship.names)
  ) %>%
  arrange(curriculum_name, student.id, semester.code) 


###1.4: The following code append curriculum required units for graduation
###column to the summary_student data frame created above.

###1.4.1. create a dataframe of required units for each program, the information comes from Curriculum.xlsx
curriculum_required_units<-data.frame(levels(summary_student$curriculum_name),
                                      c(60,35, rep(60, 4), rep(49,2),60, 35, 60, 35,rep(60, 4) ))
colnames(curriculum_required_units)<-c("curriculum_name", "required_units")


### 1.4.2. merge two dataframes by curriculum name. The summary-student2 data frame will
###be furthur summarized to create dataframes 
####at the grain size of per semester per curriculum program
summary_student2<-left_join(summary_student, 
                            curriculum_required_units, 
                            by="curriculum_name")

### 1.5 save the data file for later use
saveRDS(summary_student2, file = "summary_student2.rds")


####Step 2: The following code create the data frame at the grain size 
####of per curriculum per semester

###2.1 get the number of newly enrolled students for each semester
enrolledStudents<-summary_student2 %>% filter(whichSem==1) %>%
  group_by(curriculum_name, semester.code) %>%
  summarize(enrolled=n_distinct(student.id))

###2.2. get the number of total registered units for each semester
totalRegisteredUnits<-summary_student2 %>%
  group_by(curriculum_name, semester.code, current_year, current_sem) %>%
  summarize(totalUnits=sum(registered.units))

###2.3.get the number of active students (who are registering for classes) 
###for each semster (including newly enrolled students)
activeStudents<-summary_student2 %>%
  group_by(curriculum_name,semester.code) %>%
  summarize(active=n_distinct(student.id))

###2.4. join three data frames
enrolled_active<-full_join(activeStudents, enrolledStudents,
                           by=c("curriculum_name", "semester.code")) %>%
  arrange(semester.code)

enrolled_active_units<-full_join(enrolled_active, totalRegisteredUnits,
                                 by=c("curriculum_name","semester.code"))

###2.5. create the curriculum length column and merge it with data frame created above.
####This step may be unnecessary.Will delete this step if the program length numbers are used later
###in the analysis
curriculum_length<-data.frame(levels(enrolled_active_units$curriculum_name),
                              c(8, 3, 4, 6, 8, 8, 5, 8, 8, 3, 4, 5, 6, 8, 8, 15 ))
colnames(curriculum_length)<-c("curriculum_name", "length")

enrolled_active_units<-left_join(enrolled_active_units,
                                 curriculum_length, by="curriculum_name") %>% 
  ###convert NAs to 0, because NA here means there is no active or no enrolled student
  mutate(active=ifelse(is.na(active), 0, active), 
         enrolled=ifelse(is.na(enrolled),0, enrolled))
  

### Step 3: The following code create multiple columns showing 
###total enrolled students in previous semesters. 
###We will use number of enrolled students in previous semesters to predict
###total active students in the current semester and total registered units in the current
#### semester

###3.1. create a function to produce a certain number of columns showing 
####previous N semesters enrollment. 

n_sem_enrollment<-function(df, n){
  ###take the enrolled column from the active_active_units data frame
  enrolled<-df$enrolled
  num_obs=nrow(df)
  n_sems<-data.frame()
  
  for(i in 1:num_obs){
    for(j in 1:n){
      if(i<=j){n_sems[i, j]=NA
      }else{
        n_sems[i, j]=enrolled[i-j]
      }
    }
  }
  colnames(n_sems)=paste("enroll_sem_", 1:n)
  df<-data.frame(df, n_sems)
}

###3.2. split the dataframe by program, calculate n-sem enrollment
df_ls=split(enrolled_active_units, f = enrolled_active_units$curriculum_name, drop=FALSE)

###3.3. here append 10 years of enrollment data to each observation for each program
enrollment_ls<-lapply(df_ls, n_sem_enrollment,10)
enrolled_active_units2<-bind_rows(enrollment_ls)


