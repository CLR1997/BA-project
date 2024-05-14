rm(list=ls())
library(tidyverse) 
library(readspss) 
library(tableone)
library(ggiraph)
library(cowplot)
library(patchwork)
library(nlme)
library(lme4)
library(car)
library(lattice)
library(effects)
library(sjPlot)
library(lmerTest)
library(sandwich)
data.file <- read.sav("BARCS_Data_used_for_paper.sav")
data.sem1 <- data.file %>% filter(Semester == 1)
data.sem2 <- data.file %>% filter(Semester == 2)
data.sem3 <- data.file %>% filter(Semester == 3)
data.sem4 <- data.file %>% filter(Semester == 4)

data.SEM1 <- data.sem1 %>% mutate(LOG_Avg_Drinks_current =  LOG_Avg_Drinks_SEM1, 
                                  LOG_Avg_MJ_current = LOG_Avg_MJ_SEM1,
                                  Cluster_current = Cluster_SEM1,
                                  Log_B30_current = Log_B30_Sem1)  
data.SEM2 <- data.sem2 %>% mutate(LOG_Avg_Drinks_current =  LOG_Avg_Drinks_SEM2, 
                                  LOG_Avg_MJ_current = LOG_Avg_MJ_SEM2,
                                  Cluster_current = Cluster_SEM2,
                                  Log_B30_current = Log_B30_Sem2)  
data.SEM3 <- data.sem3 %>% mutate(LOG_Avg_Drinks_current =  LOG_Avg_Drinks_SEM3, 
                                  LOG_Avg_MJ_current = LOG_Avg_MJ_SEM3,
                                  Cluster_current = Cluster_SEM3,
                                  Log_B30_current = Log_B30_Sem3) 
data.SEM4 <- data.sem4 %>% mutate(LOG_Avg_Drinks_current =  LOG_Avg_Drinks_SEM4, 
                                  LOG_Avg_MJ_current = LOG_Avg_MJ_SEM4,
                                  Cluster_current = Cluster_SEM4,
                                  Log_B30_current = Log_B30_Sem4) 

data.file.long <- rbind(data.SEM1, data.SEM2, data.SEM3, data.SEM4)

data.file.long <- data.file.long %>%
  mutate(Avg_Drinks_SEM1 = 10^(LOG_Avg_Drinks_SEM1) -1,
         Avg_Drinks_SEM2 = 10^(LOG_Avg_Drinks_SEM2) -1,
         Avg_Drinks_SEM3 = 10^(LOG_Avg_Drinks_SEM3) -1,
         Avg_Drinks_SEM4 = 10^(LOG_Avg_Drinks_SEM4) -1,
         Avg_Drinks_current = 10^(LOG_Avg_Drinks_current) -1,
         Avg_MJ_SEM1 = 10^(LOG_Avg_MJ_SEM1) -1,
         Avg_MJ_SEM2 = 10^(LOG_Avg_MJ_SEM2) -1,
         Avg_MJ_SEM3 = 10^(LOG_Avg_MJ_SEM3) -1,
         Avg_MJ_SEM4 = 10^(LOG_Avg_MJ_SEM4) -1,
         Avg_MJ_current = 10^(LOG_Avg_MJ_current) -1,
         B30_SEM1 = 10^(Log_B30_Sem1) -1,
         B30_SEM2 = 10^(Log_B30_Sem2) -1,
         B30_SEM3 = 10^(Log_B30_Sem3) -1,
         B30_SEM4 = 10^(Log_B30_Sem4) -1,
         B30_current = 10^(Log_B30_current) -1
  )

### needs some work
data.file.long <- data.file.long %>%
  group_by(BARCS_ID) %>%
  mutate(diff_Avg_Drinks_current = Avg_Drinks_current - lag(Avg_Drinks_current, 1),
         diff_Avg_MJ_current = Avg_MJ_current - lag(Avg_MJ_current, 1),
         diff_B30_current = B30_current - lag(B30_current, 1),
         diff_LOG_Avg_Drinks_current = LOG_Avg_Drinks_current - lag(LOG_Avg_Drinks_current, 1),
         diff_LOG_Avg_MJ_current = LOG_Avg_MJ_current - lag(LOG_Avg_MJ_current, 1),
         diff_GPA = GPA - lag(GPA, 1),
         diff_LOG_B30_current = Log_B30_current - lag(Log_B30_current, 1),
         transition_current = as.numeric(Cluster_current) - lag(as.numeric(Cluster_current), 1)
  )

data.file.long <- data.file.long %>%
  ungroup() %>%
  group_by(Semester) %>%
  mutate(mean_Avg_Drinks = Avg_Drinks_current - mean(Avg_Drinks_current),
         std_Avg_Drinks = mean_Avg_Drinks / sd(Avg_Drinks_current),
         mean_LOG_Avg_Drinks = LOG_Avg_Drinks_current - mean(LOG_Avg_Drinks_current),
         std_LOG_Avg_Drinks = mean_LOG_Avg_Drinks / sd(LOG_Avg_Drinks_current),
         mean_Avg_MJ = Avg_MJ_current - mean(Avg_MJ_current, na.rm = TRUE),
         std_Avg_MJ = mean_Avg_MJ / sd(Avg_MJ_current, na.rm = TRUE),
         mean_LOG_Avg_MJ = LOG_Avg_MJ_current - mean(LOG_Avg_MJ_current, na.rm = TRUE),
         std_LOG_Avg_MJ = mean_LOG_Avg_MJ / sd(LOG_Avg_MJ_current, na.rm = TRUE),
         mean_B30 = B30_current - mean(B30_current, na.rm = TRUE),
         std_B30 = mean_B30 / sd(B30_current, na.rm = TRUE),
         mean_LOG_B30 = Log_B30_current - mean(Log_B30_current, na.rm = TRUE),
         std_LOG_B30 = mean_LOG_B30 / sd(Log_B30_current, na.rm = TRUE)#,
         # mean_GPA = GPA - mean(GPA),
         # std_GPA = mean_GPA / sd(GPA) there are NA's in the data that were wrongly assigned the value 0
  )

data.file.long <- data.file.long %>%
  relocate(BARCS_ID, Semester, GPA, diff_GPA, Avg_Drinks_current, mean_Avg_Drinks, std_Avg_Drinks, diff_Avg_Drinks_current, 
           LOG_Avg_Drinks_current, mean_LOG_Avg_Drinks, std_LOG_Avg_Drinks, diff_LOG_Avg_Drinks_current, Avg_MJ_current, 
           mean_Avg_MJ, std_Avg_MJ, diff_Avg_MJ_current, LOG_Avg_MJ_current, mean_LOG_Avg_MJ, std_LOG_Avg_MJ, 
           diff_LOG_Avg_MJ_current)

## Cluster and student id need to be factors 
data.file.long <- data.file.long %>% mutate(Cluster_current = as.factor(Cluster_current),
                                            BARCS_ID = as.factor(BARCS_ID))


## Data selection required to identify if GPA = 0 and SAT = 0 are NAs
ind.gpa0 <- data.file.long %>% filter(GPA == 0) %>% select(BARCS_ID, Semester, GPA)
ind.gpa0.vector <- unique(ind.gpa0$BARCS_ID)
ind.gpa0 <- data.file.long %>% filter(BARCS_ID %in% ind.gpa0.vector) %>% group_by(BARCS_ID, Semester)
ind.sat0 <- data.file.long %>% ungroup() %>% filter(SATTotal == 0) %>% select(BARCS_ID, Semester, SATTotal, GPA)


### imputing NA for false classification of SAT / GPA = 0 
data.file.long <- data.file.long %>% mutate(GPA = replace(GPA, GPA == 0, NA),
                                            SATTotal = replace(SATTotal, SATTotal == 0, NA),
                                            SATMath = replace(SATMath, SATMath == 0, NA),
                                            SATVerbal = replace(SATVerbal, SATVerbal == 0, NA),
                                            SATWriting = replace(SATWriting, SATWriting == 0, NA),
                                            mean_GPA = GPA - mean(GPA, na.rm = TRUE),
                                            std_GPA = mean_GPA / sd(GPA, na.rm = TRUE))


### create variable that counts the amount of NAs for each individual student 

data.file.long <- data.file.long %>% group_by(BARCS_ID) %>% 
  mutate(sum.GPAna = sum(is.na(GPA)))

### include average GPA for each student 
data.file.long <- data.file.long %>% group_by(BARCS_ID) %>% mutate(average_GPA = mean(GPA))
#data.file.long <- data.file.long %>% arrange(sum.GPAna, average_GPA)

data.file.long <- data.file.long %>% 
  mutate(Sex = case_match(Sex, 1 ~ "male", 2 ~ "female", .default = NA),
         Cluster_current = case_match(Cluster_current, '1' ~ "1st.cluster", '2' ~ "2nd.cluster",
                                     '3' ~ "3rd.cluster", .default = NA),
         Cluster_SEM1 = case_match(Cluster_SEM1, '1' ~ "1st.cluster", '2' ~ "2nd.cluster",
                                   '3' ~ "3rd.cluster", .default = NA),
         Fager4_binary = case_match(Fager4_binary, 1 ~ "smoker", 0 ~ "non smoker", .default = NA),
         FH_binary = case_match(FH_binary, 0 ~ "negative", 1 ~ "positive", .default = NA))


## splitting the data into the different subsections of missing GPA data  (4 NAs means there is no GPA data -> dropped)
subset.0nagpas <- data.file.long %>% filter(sum.GPAna == 0) %>% arrange(average_GPA)
subset.1nagpas <- data.file.long %>% filter(sum.GPAna == 1) %>% arrange(average_GPA)
subset.2nagpas <- data.file.long %>% filter(sum.GPAna == 2) %>% arrange(average_GPA)
subset.3nagpas <- data.file.long %>% filter(sum.GPAna == 3) %>% arrange(average_GPA)






### Group transition compares only the first semester to the last semester!!!!!!!!!




