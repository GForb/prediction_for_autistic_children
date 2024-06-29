# Code adapted from code written by Caitlin Williams, reproducing Vaso Totsika's SPSS code.

# applies method described for identifying interlectual disability described in https://pubmed.ncbi.nlm.nih.gov/31206633/, described in supporting information here:

# https://srcd.onlinelibrary.wiley.com/doi/10.1111/cdev.13273


# 04/04/2023

# Load R packages

library(tidyverse)
library(haven)
library(psych)

# Load variables from individual data files
mcs_data <- here::here(raw_data, "MCS")
folder_s1 <- here::here(mcs_data, "Sweep 1/UKDA-4683-stata/stata/stata13")
folder_s2 <- here::here(mcs_data, "Sweep 2/UKDA-5350-stata/stata/stata13")
folder_s3 <- here::here(mcs_data, "Sweep 3/UKDA-5795-stata/stata/stata13")
folder_s4 <- here::here(mcs_data, "Sweep 4/UKDA-6411-stata/stata/stata13")
folder_s5 <- here::here(mcs_data, "Sweep 5/UKDA-7464-stata/stata/stata13")


MCS1 <- haven::read_stata(here(folder_s1, 'mcs1_parent_cm_interview.dta')) %>% select(MCSID,ARESP00,ACNUM00) %>% filter(ARESP00 == 1)
MCS2 <- haven::read_stata(here(folder_s2, 'mcs2_cm_cognitive_assessment.dta')) %>% select(MCSID,BCNUM00,BDBASR00,BDBAST00,BDSRCN00)
MCS3 <- haven::read_stata(here(folder_s3, 'mcs3_cm_cognitive_assessment.dta')) %>% select(MCSID,CCNUM00,CCWARN00,CCCSCO00,CCPSCO00,CCNSCO00,CCPCTSCORE,CCNVTSCORE,CCPSTSCORE)

MCS4cog <- haven::read_stata(here(folder_s4, 'mcs4_cm_cognitive_assessment.dta')) %>% select(MCSID,DCNUM00,DCWARN00,DCWRSC00,DCWRSD00,DCTOTS00,DCPCTS00,DCMTOTSCOR,DCMATHS7SA)
MCS4parent <- haven::read_stata(here(folder_s4, 'mcs4_parent_cm_interview.dta')) %>% select(MCSID,DRESP00,DCNUM00,DPCSEN00,DPSENS00,DPRASN0A,DPRASN0B,DPRASN0C,DPRASN0D,DPRASN0E, 
                                                                                                                                     DPRASN0F,DPRASN0G,DPRASN0H,DPRASN0I,DPRASN0J,DPRASN0K,DPRASN0L,DPRASN0M, 
                                                                                                                                     DPRASN0N,DPRASN0O,DPRASN0P,DPRASN0Q,DPRASX0A,DPRASX0B,DPRASX0C,DPRASX0D,
                                                                                                                                     DPRASX0E,DPRASX0F,DPRASX0G,DPRASX0H,DPRASX0I,DPRASX0J,DPRASX0K,DPRASX0L,   
                                                                                                                                     DPRASX0M,DPRASX0N,DPRASX0O,DPRASX0P,DPRASX0Q,DPRASX0R,DPRASX0S,DPRASX0T,
                                                                                                                                     DPRASX0U,DPRASX0V,DPRASX0W,DPRASX0X,DPRASX0Y,DPRASX0Z,DPRASX1A,DPRASX1B,
                                                                                                                                     DPRASX1C,DPRASX1D,DPRSEN0A,DPRSEN0B,DPRSEN0C,DPRSEN0D,DPRSEN0E,DPRSEN0F,
                                                                                                                                     DPRSEN0G,DPRSEN0H,DPRSEN0I,DPRSEN0J,DPRSEN0K,DPRSEN0L,DPRSEN0M,DPRSNX0A,
                                                                                                                                     DPRSNX0B,DPRSNX0C,DPRSNX0D,DPRSNX0E,DPRSNX0F,DPRSNX0G,DPRSNX0H,DPRSNX0I, 
                                                                                                                                     DPRSNX0J,DPRSNX0K,DPRSNX0L,DPRSNX0M,DPRSNX0N,DPRSNX0O,DPRSNX0P,DPRSNX0Q,
                                                                                                                                     DPRSNX0R,DPRSNX0S,DPRSNX0T,DPRSNX0U,DPRSNX0V,DPRSNX0W,DPRSNX0X,DPRSNX0Y,
                                                                                                                                     DPRSNX0Z,DPRSNX1A,DPRSNX1B,DPRSNX1C,DPRSNX1D) %>% filter(DRESP00 == 1)
MCS4teach <- haven::read_stata(here(folder_s4, 'mcs4_cm_teacher_survey.dta')) %>% select(MCSID,DCNUM00,DQ2162,DQ2163,DQ2164,DQ2165,DQ2167,DQ2328,DQ2329,DQ2331)

MCS5cog <- haven::read_stata(here(folder_s5,'mcs5_cm_cognitive_assessment.dta')) %>% select(MCSID,ECNUM00,ECASSX0R)
MCS5cmD <- haven::read_stata(here(folder_s5, 'mcs5_cm_derived.dta')) %>% select(MCSID,ECNUM00,EVSRAW,EVSTSCO)
MCS5teach <- haven::read_stata(here(folder_s5,'/mcs5_cm_teacher_survey.dta')) %>% select(MCSID,ECNUM00,EQ13B,EQ13R,EQ2A,EQ2B,EQ2C,EQ2D,EQ11,EQ12)

# Merge separate databases from same wave together e.g., MCS4cog, MCS4parent, MCS4teach become one MCS4 database

MCS4parentcog <- merge(MCS4cog, MCS4parent, by= c("MCSID","DCNUM00"), all= TRUE)
MCS4 <- merge(MCS4parentcog, MCS4teach, by= c("MCSID","DCNUM00"), all= TRUE)

MCS5cmDcog <- merge(MCS5cog, MCS5cmD, by= c("MCSID","ECNUM00"), all= TRUE)
MCS5 <- merge(MCS5cmDcog, MCS5teach, by= c("MCSID","ECNUM00"), all= TRUE)

# Keeping only main interview responses as partner does not answer questions regarding SEN etc

#===================
# MCS 1 (n=18,764)
#===================
# Variables used from dataset:
#     MCSID    =   MCS Research ID - Anonymised Family/Household Identifier
#     ARESP00  =   Response in survey: Whether respondent (of ELIG) participated in interview
#     ACNUM00  =   Cohort Member number within an MCS family

# Rename CNUM00 for each MCS wave so we can group by this variable

MCS1 <- MCS1 %>% rename(CNUM00 = ACNUM00)
MCS2 <- MCS2 %>% rename(CNUM00 = BCNUM00)
MCS3 <- MCS3 %>% rename(CNUM00 = CCNUM00)
MCS4 <- MCS4 %>% rename(CNUM00 = DCNUM00)
MCS5 <- MCS5 %>% rename(CNUM00 = ECNUM00)

#===================
# MCS 2 (n=15,778)
#===================
# Variables used from dataset:
#     MCSID     =   MCS Research ID - Anonymised Family/Household Identifier
#     BCNUM00   =   Cohort Member number within an MCS family
#     BDBASR00  =   DV BAS Naming Vocabulary - Raw Score
#     BDSRCN00  =   DV Bracken: School Readiness Comp Normativ Classificatn
#     BDBAST00  =   DV BAS Naming Vocabulary - T-scores

MCS2_r <- MCS2 %>% filter(CNUM00==1)
# Focusing on 1 child per family, or CM 1. n=15,579, 199 CM2/3 removed

MCS1_2 <- merge(MCS1, MCS2_r, by= c("MCSID","CNUM00"), all= TRUE)
# n=19,466
# n=702 entered at MCS2

#===================
# MCS 3 (n=15,431)
#===================
# Variables used from dataset:
#     MCSID       =   MCS Research ID - Anonymised Family/Household Identifier
#     CCNUM00     =   Cohort Member number within an MCS family
#     CCWARN00    =   COG: Interviewer: Do not administer is child has learn
#     CCCSCO00    =   COG: Total Score for Pattern Construction (BAS)
#     CCPSCO00    =   COG: Picture Similarity Total raw score (BAS)
#     CCNSCO00    =   COG: Total score for Naming Vocabulary test (BAS)
#     CCPCTSCORE  =   COG: Pattern Construction T-score
#     CCNVTSCORE  =   COG: Naming Vocabulary T-score
#     CCPSTSCORE  =   COG: Pattern Construction T-score

# Factor analyses - ID

df1<-MCS3[,c("MCSID","CNUM00","CCCSCO00", "CCPSCO00", "CCNSCO00")] |> 
  mutate(CCCSCO00 = na_if(CCCSCO00, -1),
         CCPSCO00 = na_if(CCPSCO00, -1),
         CCNSCO00 = na_if(CCNSCO00, -1))
test_dat1 <- df1[complete.cases(df1), ]

pca1 <- psych::principal(test_dat1[,-c(1:2)], nfactors = 1, residuals = FALSE, rotate="none", n.obs= length(MCS3[,1]), covar=FALSE,
                 scores=TRUE,missing=FALSE,oblique.scores=FALSE,method="regression")
scores1 <- pca1$scores

# Dichotomise factor scores

c_ID <- cut(pca1$scores, breaks = c(-Inf, -2, +Inf), labels = c("1", "0"))
c_borderlineID <- cut(pca1$scores, breaks = c(-Inf, -1, +Inf), labels = c("1", "0"))
temp1 <- data.frame(MCSID = test_dat1$MCSID,CNUM00=test_dat1$CNUM00, c_ID=c_ID,c_borderlineID=c_borderlineID)

MCS3<-MCS3 %>% left_join(temp1, by = c("MCSID","CNUM00"))

table(MCS3$c_ID)
table(MCS3$c_borderlineID)
#     c_ID            =   MCS3 ID status raw scores (1= ID [n=519], 0= Non ID [n=14,910])
#     c_borderlineID  =   MCS3 borderline ID status raw scores (1= borderline ID (1 SD<Mean and below) [n=1,906], 0= non-ID [n=13,523])

# Factor analyses - ID (Standardised scores)

df2<-MCS3[,c("MCSID","CNUM00","CCPCTSCORE", "CCNVTSCORE", "CCPSTSCORE")]
test_dat2 <- df2[complete.cases(df2), ]
pca2 <- psych::principal(test_dat2[,-c(1:2)], nfactors = 1, residuals = FALSE, rotate="none", n.obs= length(MCS3[,1]), covar=FALSE,
                  scores=TRUE,missing=FALSE,oblique.scores=FALSE,method="regression")
scores2 <- pca2$scores

# Dichotomise factor scores

cz_ID <- cut(pca2$scores, breaks = c(-Inf, -2, +Inf), labels = c("1", "0"))
cz_borderlineID <- cut(pca2$scores, breaks = c(-Inf, -1, +Inf), labels = c("1", "0"))
temp2 <- data.frame(MCSID = test_dat2$MCSID,CNUM00=test_dat2$CNUM00, cz_ID=cz_ID,cz_borderlineID=cz_borderlineID)

MCS3<-MCS3 %>% left_join(temp2, by = c("MCSID","CNUM00"))

table(MCS3$cz_ID)
table(MCS3$cz_borderlineID)
#     cz_ID            =   MCS3 ID status standardised scores (1= ID [n=555], 0= Non ID [n=14,815])
#     cz_borderlineID  =   MCS3 borderline ID status standardised scores (1= borderline ID (1 SD<Mean and below) [n=1,884], 0= non-ID [n=13,486])

# Crosstabs

with(MCS3,xtabs(~c_ID + cz_ID))
with(MCS3,xtabs(~c_borderlineID + cz_borderlineID))
# The difference in ID classification between raw scores and standardised scores is 144 children. 555 age 5 have ID using standardised scores

# Remove duplicates

MCS3_r <- MCS3 %>% filter(CNUM00==1)
# n=15,236, 195 CM2/3 removed

# Merge with MCS1 and MCS2

MCS1_2_3 <- merge(MCS1_2, MCS3_r, by= c("MCSID","CNUM00"), all= TRUE)
# n=19,469

#===================
# MCS 4 (n=14,013)
#===================
# Variables used from dataset:
#     MCSID       =   MCS Research ID - Anonymised Family/Household Identifier
#     DCNUM00     =   Cohort Member number within an MCS family
#     DCWARN00    =   S4 CM INTERVIEWER: DO NOT ADMINISTER IF LEARNING DIFFICULTY ETC
#     DCWRSC00    =   S4 CM Total score for Word Reading test
#     DCWRSD00    =   S4 CM Word Reading Standard Score
#     DCTOTS00    =   S4 CM Pattern Construction Total Raw Score
#     DCPCTS00    =   S4 CM Pattern Construction age-based T-Scores
#     DCMTOTSCOR  =   S4 CM Maths Test Score (Total Raw Score)
#     DCMATHS7SA  =   S4 CM Maths 7 Standardised Age Score based on standardisation in 2004
#     DRESP00     =   Response in survey: Whether respondent (of ELIG) participated in interview
#     DPCSEN00    =   Has CMs school told you CM has special needs
#     DPSENS00    =   Does CM have a statement of special needs
#     RASN        =   What are the reasons for CM s additional support
#     RASX        =   What are the reason for CM s additional support
#     RSEN        =   CM special education reason
#     RSNX        =   What are the reasons for CM s special education (coded)
#     DQ2162      =   S4 TS Reading (in English)
#     DQ2163      =   S4 TS Reading in Welsh/Gaelic/Irish
#     DQ2164      =   S4 TS Writing (in English)
#     DQ2165      =   S4 TS Writing in Welsh/Gaelic/Irish
#     DQ2167      =   S4 TS Maths and numeracy
#     DQ2328      =   S4 TS Has this child EVER been recognised as having Special Ed. Needs
#     DQ2329      =   S4 TS Does this child have a full statment of SEN
#     DQ2331      =   S4 TS Learning difficulties (including dyspraxia / dyscalculia)

# Vaso only looked at RASN variables, we've included RASX, RSEN and RSNX as we did with autism code (https://osf.io/trhx3/). 
# If learning disabilities was given as a reason for a child receiving additional support or special education, SENDLD will be coded as 1

with(MCS4,xtabs(~DPCSEN00 + DPSENS00))

MCS4 <- MCS4 %>% mutate(SENDLD = if_else(DPRASN0A==2|DPRASN0B==2|DPRASN0C==2|DPRASN0D==2|DPRASN0E==2|DPRASN0F==2|DPRASN0G==2|DPRASN0H==2|DPRASN0I==2|DPRASN0J==2|DPRASN0K==2|
                                         DPRASN0L==2|DPRASN0M==2|DPRASN0N==2|DPRASN0O==2|DPRASN0P==2|DPRASN0Q==2|DPRASX0A==2|DPRASX0B==2|DPRASX0C==2|DPRASX0D==2|DPRASX0E==2|
                                         DPRASX0F==2|DPRASX0G==2|DPRASX0H==2|DPRASX0I==2|DPRASX0J==2|DPRASX0K==2|DPRASX0L==2|DPRASX0M==2|DPRASX0N==2|DPRASX0O==2|DPRASX0P==2|
                                         DPRASX0Q==2|DPRASX0R==2|DPRASX0S==2|DPRASX0T==2|DPRASX0U==2|DPRASX0V==2|DPRASX0W==2|DPRASX0X==2|DPRASX0Y==2|DPRASX0Z==2|DPRASX1A==2|
                                         DPRASX1B==2|DPRASX1C==2|DPRASX1D==2|DPRSEN0A==2|DPRSEN0B==2|DPRSEN0C==2|DPRSEN0D==2|DPRSEN0E==2|DPRSEN0F==2|DPRSEN0G==2|DPRSEN0H==2|
                                         DPRSEN0I==2|DPRSEN0J==2|DPRSEN0K==2|DPRSEN0L==2|DPRSEN0M==2|DPRSNX0A==2|DPRSNX0B==2|DPRSNX0C==2|DPRSNX0D==2|DPRSNX0E==2|DPRSNX0F==2|
                                         DPRSNX0G==2|DPRSNX0H==2|DPRSNX0I==2|DPRSNX0J==2|DPRSNX0K==2|DPRSNX0L==2|DPRSNX0M==2|DPRSNX0N==2|DPRSNX0O==2|DPRSNX0P==2|DPRSNX0Q==2|
                                         DPRSNX0R==2|DPRSNX0S==2|DPRSNX0T==2|DPRSNX0U==2|DPRSNX0V==2|DPRSNX0W==2|DPRSNX0X==2|DPRSNX0Y==2|DPRSNX0Z==2|DPRSNX1A==2|DPRSNX1B==2|
                                         DPRSNX1C==2|DPRSNX1D==2,1,0)) 

table(MCS4$SENDLD)
#     SENDLD     =   Whether LD reason for SEN (1= LD/ID [n=344], 0= No LD)

# Factor analyses - ID

df3<-MCS4[,c("MCSID","CNUM00","DCWRSD00", "DCPCTS00", "DCMTOTSCOR")] |> 
  mutate(DCWRSD00 = na_if(DCWRSD00, -1),
         DCPCTS00 = na_if(DCPCTS00, -1),
         DCMTOTSCOR = na_if(DCMTOTSCOR, -1))
test_dat3 <- df3[complete.cases(df3), ]
pca3 <- psych::principal(test_dat3[,-c(1:2)], nfactors = 1, residuals = FALSE, rotate="none", n.obs= length(MCS4[,1]), covar=FALSE,
                  scores=TRUE,missing=FALSE,oblique.scores=FALSE,method="regression")
scores3 <- pca3$scores

# Dichotomise factor scores

d_ID <- cut(pca3$scores, breaks = c(-Inf, -2, +Inf), labels = c("1", "0"))
d_borderlineID <- cut(pca3$scores, breaks = c(-Inf, -1, +Inf), labels = c("1", "0"))
temp3 <- data.frame(MCSID = test_dat3$MCSID,CNUM00=test_dat3$CNUM00, d_ID=d_ID,d_borderlineID=d_borderlineID)

MCS4<-MCS4 %>% left_join(temp3, by = c("MCSID","CNUM00"))

table(MCS4$d_ID)
table(MCS4$d_borderlineID)
#     d_ID            =   MCS4 ID status raw scores (1= ID [n=448], 0= Non ID [n=12,972])
#     d_borderlineID  =   MCS4 borderline ID status raw scores (1= borderline ID (1 SD<Mean and below) [n=2,124], 0= non-ID [n=11,296])

# Factor analyses - ID (Standardised scores)

df4<-MCS4[,c("MCSID","CNUM00","DCWRSD00", "DCPCTS00", "DCMATHS7SA")] |> 
  mutate(DCWRSD00 = na_if(DCWRSD00, -1),
         DCPCTS00 = na_if(DCPCTS00, -1),
         DCMATHS7SA = na_if(DCMATHS7SA, -1))
test_dat4 <- df4[complete.cases(df4), ]
pca4 <- psych::principal(test_dat4[,-c(1:2)], nfactors = 1, residuals = FALSE, rotate="none", n.obs= length(MCS4[,1]), covar=FALSE,
                  scores=TRUE,missing=FALSE,oblique.scores=FALSE,method="regression")
scores4 <- pca4$scores

# Dichotomise factor scores (Standardised scores)

dz_ID <- cut(pca4$scores, breaks = c(-Inf, -2, +Inf), labels = c("1", "0"))
dz_borderlineID <- cut(pca4$scores, breaks = c(-Inf, -1, +Inf), labels = c("1", "0"))
temp4 <- data.frame(MCSID = test_dat4$MCSID,CNUM00=test_dat4$CNUM00, dz_ID=dz_ID,dz_borderlineID=dz_borderlineID)

MCS4<-MCS4 %>% left_join(temp4, by = c("MCSID","CNUM00"))

table(MCS4$dz_ID)
table(MCS4$dz_borderlineID)
#     dz_ID            =   MCS4 ID status standardised scores (1= ID [n=354], 0= Non ID [n=13,066])
#     dz_borderlineID  =   MCS4 borderline ID status standardised scores (1= borderline ID (1 SD<Mean and below) [n=2,232], 0= non-ID [n=11,188])

# Crosstabs

with(MCS4,xtabs(~d_ID + dz_ID))
with(MCS4,xtabs(~d_borderlineID + dz_borderlineID))
# The difference in ID classification between raw scores and standardised scores is 148 children. 354 children have ID using standardised scores

# Remove duplicates
MCS4_r <- MCS4 %>% filter(CNUM00==1)
# n=13,847, 166 CM2/3 removed

# merge with MCS1, MCS2 and MCS3

MCS1_2_3_4 <- merge(MCS1_2_3, MCS4_r, by= c("MCSID","CNUM00"), all= TRUE)
# n=19,470

#===================
# MCS 5 (n=13,447)
#===================
# Variables used from dataset:
#     MCSID       =   MCS Research ID - Anonymised Family/Household Identifier
#     ECNUM00     =   Cohort Member number within an MCS family
#     ECASSX0R    =   IWR: Circumstances of cogntv assessment MC (A) [CM has learning difficulties]
#     EVSRAW      =   S5 DV Verbal Sims raw score
#     EVSTSCO     =   S5 DV Verbal Sims standard score
#     EQ13B       =   S5 TS Learning difficulties reason for CM's SEN
#     EQ13R       =   S5 TS Downs Syndrome: reason for CM's SEN
#     EQ2A        =   S5 TS English
#     EQ2B        =   S5 TS Welsh
#     EQ2C        =   S5 TS Maths
#     EQ2D        =   S5 TS Science
#     EQ11        =   S5 TS Does CM have Special Educational Needs (SEN)
#     EQ212       =   S5 TS Does CM have a full statement of SEN

table(MCS5$EQ13B) 
# n=533

table(MCS5$EQ13R)
# n=3 

# Remove duplicates

MCS5_r <- MCS5 %>% filter(CNUM00==1)
# n=13,279, 168 CM2/3 removed

# merge with MCS1, MCS2, MCS3 and MCS4

MCS1_2_3_4_5 <- merge(MCS1_2_3_4, MCS5_r, by= c("MCSID","CNUM00"), all= TRUE)
# n=19,471

#===========================
# MCS 1_2_3_4_5 (n=19,471)
#===========================
#===============================================================================
# ID USING COGNITIVE ASSESSMENTS
#===============================================================================

table(MCS1_2_3_4_5$c_ID)
# n=505 ID, n=14,730 no ID

table(MCS1_2_3_4_5$cz_ID)
# n=543 ID, n=14,633 no ID

table(MCS1_2_3_4_5$d_ID)
# n=442 ID, n=12,820 no ID

table(MCS1_2_3_4_5$dz_ID)
# n=350 ID, n=12,912 no ID

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~d_ID + c_ID))
with(MCS1_2_3_4_5,xtabs(~dz_ID + cz_ID))

# create ID variable anchored at 7 with added info from other surveys

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDcd = d_ID)

table(MCS1_2_3_4_5$IDcd)
# n=442

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>%mutate(d_ID=fct_na_value_to_level(d_ID,level = "-1"),
                       c_ID=fct_na_value_to_level(c_ID,level = "-1"),
                       dz_ID=fct_na_value_to_level(dz_ID,level = "-1"),
                       cz_ID=fct_na_value_to_level(cz_ID,level = "-1"))

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDcd = case_when(d_ID==-1 & c_ID==1~"1",d_ID==-1 & c_ID==0~"0",TRUE ~ as.character(d_ID)),IDcd =factor(IDcd, levels=c("-1","0","1")))

table(MCS1_2_3_4_5$IDcd)
# Number of children at MCS4 with ID [n=442] add 244 [n=686]

# Repeat process for standardised scores

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDcd = dz_ID)

table(MCS1_2_3_4_5$zIDcd)
# n=350

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDcd = case_when(dz_ID==-1 & cz_ID==1~"1",dz_ID==-1 & cz_ID==0~"0",TRUE ~ as.character(dz_ID)),zIDcd =factor(zIDcd, levels=c("-1","0","1")))

table(MCS1_2_3_4_5$zIDcd)
# Number of children at MCS4 with zID [n=350] add 271 [n=621]

with(MCS1_2_3_4_5,xtabs(~d_ID + c_ID))
with(MCS1_2_3_4_5,xtabs(~dz_ID + cz_ID))

################################################################################

# Frequency table

table(MCS1_2_3_4_5$BDSRCN00)
# 305 children classified as very delayed on the Bracken. Very delayed is 2 SDs and below standardised norms. 

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~BDSRCN00 + IDcd))
# There are 29 children with a very delayed score among those who had not had a ID classification yet
# In total there are 1,347 children with an entry for Bracken who were missing from MCS4 and MCS3. 
# Of those, 241 (143, 90, 8) did not have a valid score because of not completion or not being able to calculate a score.
# 49 were the very delayed ones

################################################################################

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDbcd = IDcd)

table(MCS1_2_3_4_5$IDbcd)
# n=686

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDbcd = case_when(IDcd==-1 & BDSRCN00==1~"1",IDcd==-1 & BDSRCN00 %in% c(2:5) ~"0",TRUE ~ as.character(IDcd)),IDbcd =factor(IDbcd, levels=c("-1","0","1")))

table(MCS1_2_3_4_5$IDbcd)
# Number of children at MCS3 and MCS4 with ID [n=686] add 29 [n=715]
# We are left with 2,539 children with an unknown classification

# Crosstabs

with(MCS1_2_3_4_5,xtabs(~BDSRCN00 + zIDcd))

# Repeat process for standardised scores

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDbcd = zIDcd)

table(MCS1_2_3_4_5$zIDbcd)
# n=621

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDbcd = case_when(zIDcd==-1 & BDSRCN00==1~"1",IDcd==-1 & BDSRCN00 %in% c(2:5) ~"0",TRUE ~ as.character(zIDcd)),zIDbcd =factor(zIDbcd, levels=c("-1","0","1")))

table(MCS1_2_3_4_5$zIDbcd)
# Number of children at MCS3 and MCS4 with zID [n=621] add 29 [n=650]
# We are left with 2,551 children with an unknown classification

#===============================================================================
# WHO DID NOT DO COGNITIVE ASSESSMENTS
#===============================================================================

# Frequency table

table(MCS1_2_3_4_5$BDSRCN00)
# test not carried out n=822

table(MCS1_2_3_4_5$CCWARN00, useNA= ("ifany"))
# Where this variable is non applicable, the test was not administered because of ID n=7

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~CCWARN00 + CCCSCO00))
with(MCS1_2_3_4_5,xtabs(~DCMATHS7SA + DCWARN00))
with(MCS1_2_3_4_5,xtabs(~EVSRAW + ECASSX0R))
# There is no single variable that consistently identifies between files who had been excluded from cognitive assessment on the basis of LD or the other predetermined reasons
# Use unclassified sole criterion to look at additional parent or teacher report 

#===============================================================================
# PARENTAL OR TEACHER REPORT OF PROBLEMS AT MCS4
#===============================================================================

with(MCS1_2_3_4_5,xtabs(~IDbcd + DPCSEN00))
with(MCS1_2_3_4_5,xtabs(~IDbcd + DPSENS00))
with(MCS1_2_3_4_5,xtabs(~IDbcd + SENDLD))
with(MCS1_2_3_4_5,xtabs(~IDbcd + DQ2328))

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(LDparent_4 = case_when(DPCSEN00==1 & SENDLD==1~2,
                                                               DPCSEN00==1 & is.na(SENDLD)~1, 
                                                               DPCSEN00==2 & is.na(SENDLD)~0,
                                                               is.na(DPCSEN00) & SENDLD==1~1,
                                                               is.na(DPCSEN00) & is.na(SENDLD)~0))

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(LDparent_4st = case_when(is.na(DPSENS00) & is.na(SENDLD)~0,
                                                                 DPSENS00==2 & is.na(SENDLD)~0,
                                                                 DPSENS00==97 & is.na(SENDLD)~0,
                                                                 DPSENS00==1 & is.na(SENDLD)~1,
                                                                 
                                                                 is.na(DPSENS00) & SENDLD==1~1,
                                                                 DPSENS00==2 & SENDLD==1~1,
                                                                 DPSENS00==97 & SENDLD==1~1,
                                                                 DPSENS00==1 & SENDLD==1~2,
                                                                 
                                                                 is.na(DPSENS00) & SENDLD==0~0,
                                                                 DPSENS00==2 & SENDLD==0~0,
                                                                 DPSENS00==97 & SENDLD==0~0,
                                                                 DPSENS00==1 & SENDLD==0~1))

table(MCS1_2_3_4_5$LDparent_4)
table(MCS1_2_3_4_5$LDparent_4st)
#     LDparent_4    =   MCS4 For how many children parent said they have SEN and LD is the reason for additional support in class? [n=340]
#     LDparent_4st  =   MCS4 For how many children parent say they have a statement of SEN and LD is the reason for additional support in class? [n=148]
# How many of those children had so far been unclassified?

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~IDbcd + LDparent_4))
with(MCS1_2_3_4_5,xtabs(~IDbcd + LDparent_4st))
# 3 and 1 of these children have been unclassified so far

with(MCS1_2_3_4_5,xtabs(~DQ2331 + SENDLD))

with(MCS1_2_3_4_5,xtabs(~DPCSEN00 + DQ2328))

# How does teacher think child performs in relation to peers?
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(mcs4teachclass = case_when((DQ2162==5|DQ2163==5) & (DQ2164==5|DQ2165==5) & DQ2167==5 ~1,
                                                                   (DQ2162==4|DQ2163==4) & (DQ2164==4|DQ2165==4) & DQ2167==4 ~0,
                                                                   (DQ2162==3|DQ2163==3) & (DQ2164==3|DQ2165==3) & DQ2167==3 ~0,
                                                                   (DQ2162==2|DQ2163==2) & (DQ2164==2|DQ2165==2) & DQ2167==2 ~0,
                                                                   (DQ2162==1|DQ2163==1) & (DQ2164==1|DQ2165==1) & DQ2167==1 ~0,.default=-1))

table(MCS1_2_3_4_5$mcs4teachclass)
#     mcs4teachclass  =   MCS4 child well below average reading and writing and maths-teacher (1= Well below average, 0= Not delayed) n=256

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~DPSENS00 + mcs4teachclass))
# 85 children with statement parent report and well below average performance in class
with(MCS1_2_3_4_5,xtabs(~DPCSEN00 + mcs4teachclass))
# 141 kids parent-report of special needs and well below average performance in class
with(MCS1_2_3_4_5,xtabs(~DQ2328 + mcs4teachclass))
# 240 with teacher-reported SEN
with(MCS1_2_3_4_5,xtabs(~DQ2329 + mcs4teachclass))
# 87 with teacher-report statement
with(MCS1_2_3_4_5,xtabs(~LDparent_4 + mcs4teachclass))
# 77 child where LDparent_4 is 2 (ie child has statement and additional support for LD) that have been rated well below average by teacher across all 3

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDadd = case_when(DPCSEN00==1 & DQ2328==1 & mcs4teachclass==1~1, DPCSEN00==2 | DQ2328==2 | mcs4teachclass==0~0,.default=-1))

table(MCS1_2_3_4_5$IDadd) 
#     IDadd     =   Additional ID cases from MCS 4 parental report AND Teacher report of SEN AND teacher well-below average in reading and writing and maths (1= ID, 0= Non ID) n=136

with(MCS1_2_3_4_5,xtabs(~IDbcd + IDadd))
# 2 children with unclassified ID status who fulfill the triple parent/teacher report criterion of ID

MCS1_2_3_4_5$IDadd <- dplyr::na_if(MCS1_2_3_4_5$IDadd,-1)

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDbcdextra = IDbcd)
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(IDbcdextra = case_when(IDbcd==-1 & IDadd==1~1,IDbcd==-1 & IDadd==0~0,TRUE~as.numeric(as.character(IDbcd))))

table(MCS1_2_3_4_5$IDbcdextra) 
#     IDbcdextra =   ID class age7,5,3 & par/teach report @7 (1= ID, 0= Non ID) n=2 [n=717]

with(MCS1_2_3_4_5,xtabs(~zIDbcd + IDadd))
# 2 children with unclassified ID status who fulfill the triple parent/teacher report criterion of ID

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDbcdextra = zIDbcd)
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(zIDbcdextra = case_when(zIDbcd==-1 & IDadd==1~1,zIDbcd==-1 & IDadd==0~0,TRUE~as.numeric(as.character(zIDbcd))))

table(MCS1_2_3_4_5$zIDbcdextra) 
#     zIDbcdextra =   zID class age7,5,3 & par/teach report @7 (1= ID, 0= Non ID) [n=652]

#===================================================#
# VALIDATION USING MCS5 NAMING VOCABULARY SCORES
#===================================================#

describe(MCS1_2_3_4_5$EVSTSCO)
# According to the Connelly report on cognitive tests, this sub scale should have been standardised to have a mean of 100 and a SD of 15
# However the descriptive suggest a mean of 57.38 and SD of 13.24

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(NVlow = case_when(EVSTSCO<=20 ~1, EVSTSCO>=21 ~0,TRUE~as.numeric(as.character(EVSTSCO))))
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(NVhigh = case_when(EVSTSCO<=49 ~0, EVSTSCO>=50 ~1,TRUE~as.numeric(as.character(EVSTSCO))))

table(MCS1_2_3_4_5$NVlow)
table(MCS1_2_3_4_5$NVhigh)
#     NVlow  =   3 SDs below population mean on Namving Vocabulary age 11 (1= 3SDs below or lower, 0= Above) [n= 329]
#     NVhigh  =   At or above population mean Naming Vocabulary age 11 (1= Mean or higher, 0= Below mean) [n= 11,104]

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~NVlow + IDbcdextra))
with(MCS1_2_3_4_5,xtabs(~NVlow + zIDbcdextra))
with(MCS1_2_3_4_5,xtabs(~NVhigh + IDbcdextra))
with(MCS1_2_3_4_5,xtabs(~NVhigh + zIDbcdextra))

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(recIDbcdextra = IDbcdextra)
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(reczIDbcdextra = zIDbcdextra)

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(recIDbcdextra = case_when(IDbcdextra==0 & NVlow==1 ~1, IDbcdextra==1 & NVhigh==1 ~0,TRUE~as.numeric(as.character(IDbcdextra))))
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(reczIDbcdextra = case_when(zIDbcdextra==0 & NVlow==1 ~1, zIDbcdextra==1 & NVhigh==1 ~0,TRUE~as.numeric(as.character(zIDbcdextra))))

table(MCS1_2_3_4_5$recIDbcdextra)
table(MCS1_2_3_4_5$reczIDbcdextra)
#     recIDbcdextra  =   ID reclassified using Naming Vocabulary at 11 (1= 3SDs below or lower, 0= Above) n= 747
#     reczIDbcdextra =  ID (standardised) reclassified using Naming Vocabulary at 11 (1= 3SDs below or lower, 0= Above) n= 736

#===================================================# 
# Additional work to identify what is happening with the 147 cases who had been identified as having ID so far, but then scored at mean or above with the naming vocabulary
MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(oddcase147 = case_when(zIDbcdextra==1 & NVhigh==1 ~1,.default=-1))

table(MCS1_2_3_4_5$oddcase147)
#     oddcase147  =   147 cases scoring ID till age 11, then NV score at mean or above [n= 147]

test <- MCS1_2_3_4_5 %>% filter(.,oddcase147==1) 

table(test$EVSTSCO)
# 48% have a score close to the mean of 50, ie between 51 and 55. Rest 52% have scores ranging from 56 to 80. 28% of 147 children have a score of 2SDs higher than the mean. 
# What stage did they enter the ID definition?

#===================================================#

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~oddcase147 + dz_ID))
# 101 of the 147 were present in MCS4 classification
with(MCS1_2_3_4_5,xtabs(~oddcase147 + zIDcd))
# 144 of the 147 were present in the MCS4-3 classification
with(MCS1_2_3_4_5,xtabs(~oddcase147 + zIDbcd))
# 146 of 127 present in the MCS4-3-2 classification
with(MCS1_2_3_4_5,xtabs(~oddcase147 + zIDbcdextra))
# 147 all in at this stage

# Are there additional parent or teacher reports about these children at age 11?
# There dont seem to be any additional parent questions around SEN, but the teacher file at age 11 has some relevant questions which are all in
# The only 2 from the teacher file which have not been brought in are the EQ13R (down syndrome reason for SEN) and EQ13T -developmental delay reason for SEN

#===================================================# 
# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ2A))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ2B))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ2C))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ2D))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ11))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ12))
with(MCS1_2_3_4_5,xtabs(~oddcase147 + EQ13B))

with(MCS1_2_3_4_5,xtabs(~zIDbcdextra + EQ11))

#===================================================#

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(age11ID = if_else((NVlow==1 & (EQ2A==5|EQ2B==5) & (EQ2B==5 & EQ2C==5))==TRUE,1, NA))
table(MCS1_2_3_4_5$age11ID)
# Only 2 children

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(age11ID = if_else((NVlow==1 & EQ11==1)==TRUE,1, NA))
table(MCS1_2_3_4_5$age11ID) 
# 56 children

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~age11ID + zIDbcdextra))

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(case20 = case_when(age11ID==1 & zIDbcdextra==0 ~1))
table(MCS1_2_3_4_5$case20)
# 1 child

#===================================================#

MCS1_2_3_4_5 <- MCS1_2_3_4_5 %>% mutate(age11nonID = if_else((NVhigh==1 & (EQ2A < 4 | EQ2B < 4) & EQ2C < 4 & EQ2D < 4)==TRUE,1,NA))
table(MCS1_2_3_4_5$age11nonID) # n=5,017

# Cross tabulations

with(MCS1_2_3_4_5,xtabs(~zIDbcdextra + age11nonID))

#===================================================#

table(MCS1_2_3_4_5$zIDbcdextra) 
#     zIDbcdextra =   zID class age7,5,3 & par/teach report @7 (1= ID, 0= Non ID) [n=652]
# This is the final variable we are using to classify children with ID from the MCS sample
ID_data <- MCS1_2_3_4_5 |> 
  tibble() |> 
  mutate(base_ld = na_if(zIDbcdextra, -1),
         ID = paste0(MCSID, "_", CNUM00)) |> 
  select(ID, base_ld) 

saveRDS(ID_data, file = here(mcs_data, "mcs_ID"))