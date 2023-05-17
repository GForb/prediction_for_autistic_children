# title: Caitlin Williams Autism Identification MCS

# Loading in necessary packages
library(haven)
library(tidyverse)
library(lavaan)
library(data.table)

# Read MCS SPSS data into R, insert file location for each MCS sweep 'parent_cm_interview' into brackets
MCS3 <- haven::read_stata(file.path(raw_data, "MCS/Sweep 3/UKDA-5795-stata/stata/stata13/mcs3_parent_cm_interview.dta"))
MCS4 <- haven::read_stata(file.path(raw_data, "MCS/Sweep 4/UKDA-6411-stata/stata/stata13/mcs4_parent_cm_interview.dta"))
MCS5 <- haven::read_stata(file.path(raw_data, "MCS/Sweep 5/UKDA-7464-stata/stata/stata13/mcs5_parent_cm_interview.dta"))
MCS6 <- haven::read_stata(file.path(raw_data, "MCS/Sweep 6/UKDA-8156-stata/stata/stata13/mcs6_parent_cm_interview.dta"))

# Creating new dataframes for each MCS sweep with relevant variables
MCS3AutismV <- MCS3 %>% select(MCSID, CCNUM00, CRESP00, CPAUTS00)
MCS4AutismV <- MCS4 %>% select(MCSID, DCNUM00, DRESP00, DPAUTS00, DPRASN0A, DPRASN0B, DPRASN0C, 
                               DPRASN0D, DPRASN0E, DPRASN0F, DPRASN0G, DPRASN0H, DPRASN0I,
                               DPRASN0J, DPRASN0K, DPRASN0L, DPRASN0M, DPRASN0N,
                               DPRASN0O, DPRASN0P, DPRASN0Q, DPRASX0A, DPRASX0B,
                               DPRASX0C, DPRASX0D, DPRASX0E, DPRASX0F, DPRASX0G,   
                               DPRASX0H, DPRASX0I, DPRASX0J, DPRASX0K, DPRASX0L,   
                               DPRASX0M, DPRASX0N, DPRASX0O, DPRASX0P, DPRASX0Q,  
                               DPRASX0R, DPRASX0S, DPRASX0T, DPRASX0U, DPRASX0V,
                               DPRASX0W, DPRASX0X, DPRASX0Y, DPRASX0Z, DPRASX1A,   
                               DPRASX1B, DPRASX1C, DPRASX1D, DPRSEN0A, DPRSEN0B,
                               DPRSEN0C, DPRSEN0D, DPRSEN0E, DPRSEN0F, DPRSEN0G, 
                               DPRSEN0H, DPRSEN0I, DPRSEN0J, DPRSEN0K, DPRSEN0L,   
                               DPRSEN0M, DPRSNX0A, DPRSNX0B, DPRSNX0C, DPRSNX0D,
                               DPRSNX0E, DPRSNX0F, DPRSNX0G, DPRSNX0H, DPRSNX0I, 
                               DPRSNX0J, DPRSNX0K, DPRSNX0L, DPRSNX0M, DPRSNX0N, 
                               DPRSNX0O, DPRSNX0P, DPRSNX0Q, DPRSNX0R, DPRSNX0S,   
                               DPRSNX0T, DPRSNX0U, DPRSNX0V, DPRSNX0W, DPRSNX0X,
                               DPRSNX0Y, DPRSNX0Z, DPRSNX1A, DPRSNX1B, DPRSNX1C,
                               DPRSNX1D)
MCS5AutismV <- MCS5 %>% select(MCSID, ECNUM00, ERESP00, EPAUTS00, EPRASN0D, EPRASX0D, EPRSEN0D, EPRSEX0D)
MCS6AutismV <- MCS6 %>% select(MCSID, FCNUM00, FRESP00, FPAUTS00, FPRASN0D, FPRSEN0D)

# Removing partner interview responses from each sweep, they do not provide information on autism identification variables 
# and have the same ID as main respondents

MCS3AutismMainR <- MCS3AutismV %>%
  filter(CRESP00 == 1)

MCS4AutismMainR <- MCS4AutismV %>%
  filter(DRESP00 == 1)

MCS5AutismMainR <- MCS5AutismV %>%
  filter(ERESP00 == 1)

MCS6AutismMainR <- MCS6AutismV %>%
  filter(FRESP00 == 1)

# Renaming the CNUM00 variable in each dataframe, as this is needed for the merge

MCS3AutismMainR <- MCS3AutismMainR %>% rename(CNUM00 = CCNUM00)
MCS4AutismMainR <- MCS4AutismMainR %>% rename(CNUM00 = DCNUM00)
MCS5AutismMainR <- MCS5AutismMainR %>% rename(CNUM00 = ECNUM00)
MCS6AutismMainR <- MCS6AutismMainR %>% rename(CNUM00 = FCNUM00)

# Merging the dataframes to make one Autism Identification dataframe

AutismMainR_MCS_3_4 <- merge(MCS3AutismMainR, MCS4AutismMainR, by= c("MCSID","CNUM00"), all= TRUE)
AutismMainR_MCS_5_6 <- merge(MCS5AutismMainR, MCS6AutismMainR, by= c("MCSID","CNUM00"), all= TRUE)

Autism_MCS_All <- merge(AutismMainR_MCS_3_4, AutismMainR_MCS_5_6, by= c("MCSID","CNUM00"),all=TRUE)

#=============================================#
# Autism identification Variable
#=============================================#
Autism_MCS_All<- Autism_MCS_All %>%
  mutate(Autism_Total= case_when(
    # Whether doc diagnosed autism
    # MCS3
    CPAUTS00 == 1 |
      # MCS4
      DPAUTS00 == 1 |
      # MCS5
      EPAUTS00 == 1 |
      # MCS6
      FPAUTS00 == 1 |
      # Reasons for CMs additional support
      # MCS4
      DPRASN0A == 4 | DPRASN0B == 4 | DPRASN0C == 4 | DPRASN0D == 4 | 
      DPRASN0E == 4 | DPRASN0F == 4 | DPRASN0G == 4 | DPRASN0H == 4 | 
      DPRASN0I == 4 | DPRASN0J == 4 | DPRASN0K == 4 | DPRASN0L == 4 |
      DPRASN0M == 4 | DPRASN0N == 4 | DPRASN0O == 4 | DPRASN0P == 4 | 
      DPRASN0Q == 4 | DPRASX0A == 4 | DPRASX0B == 4 | DPRASX0C == 4 | 
      DPRASX0D == 4 | DPRASX0E == 4 | DPRASX0F == 4 | DPRASX0G == 4 | 
      DPRASX0H == 4 | DPRASX0I == 4 | DPRASX0J == 4 | DPRASX0K == 4 | 
      DPRASX0L == 4 | DPRASX0M == 4 | DPRASX0N == 4 | DPRASX0O == 4 | 
      DPRASX0P == 4 | DPRASX0Q == 4 | DPRASX0R == 4 | DPRASX0S == 4 | 
      DPRASX0T == 4 | DPRASX0U == 4 | DPRASX0V == 4 | DPRASX0W == 4 | 
      DPRASX0X == 4 | DPRASX0Y == 4 | DPRASX0Z == 4 | DPRASX1A == 4 | 
      DPRASX1B == 4 | DPRASX1C == 4 | DPRASX1D == 4 |
      # MSC5
      EPRASN0D == 1 | EPRASX0D == 1 |
      # MCS6
      FPRASN0D == 1 |
      # Reasons for CMs special education needs
      # MCS4
      DPRSEN0A == 4 | DPRSEN0B == 4 | DPRSEN0C == 4 | DPRSEN0D == 4 | 
      DPRSEN0E == 4 | DPRSEN0F == 4 | DPRSEN0G == 4 | DPRSEN0H == 4 | 
      DPRSEN0I == 4 | DPRSEN0J == 4 | DPRSEN0K == 4 | DPRSEN0L == 4 | 
      DPRSEN0M == 4 | DPRSNX0A == 4 | DPRSNX0B == 4 | DPRSNX0C == 4 | 
      DPRSNX0D == 4 | DPRSNX0E == 4 | DPRSNX0F == 4 | DPRSNX0G == 4 | 
      DPRSNX0H == 4 | DPRSNX0I == 4 | DPRSNX0J == 4 | DPRSNX0K == 4 | 
      DPRSNX0L == 4 | DPRSNX0M == 4 | DPRSNX0N == 4 | DPRSNX0O == 4 | 
      DPRSNX0P == 4 | DPRSNX0Q == 4 | DPRSNX0R == 4 | DPRSNX0S == 4 | 
      DPRSNX0T == 4 | DPRSNX0U == 4 | DPRSNX0V == 4 | DPRSNX0W == 4 | 
      DPRSNX0X == 4 | DPRSNX0Y == 4 | DPRSNX0Z == 4 | DPRSNX1A == 4 | 
      DPRSNX1B == 4 | DPRSNX1C == 4 | DPRSNX1D == 4 |
      # MSC5
      EPRSEN0D == 1 | EPRSEX0D == 1 |
      # MCS6
      FPRSEN0D == 1
    ~ 1))

# There are 643 cohort children in the MCS3-MCS6 that have been identified as autistic
count(Autism_MCS_All, Autism_Total)

# Checking numbers for individual waves
# There are 133 cohort children across MCS3 that have been identified as autistic
count(Autism_MCS_All, CPAUTS00 == 1)

# There are 225 cohort children across MCS4 that have been identified as autistic
count(Autism_MCS_All, DPAUTS00 == 1 | DPRASN0A == 4 | DPRASN0B == 4 | DPRASN0C == 4 | DPRASN0D == 4 | DPRASN0E == 4 | DPRASN0F == 4 | DPRASN0G == 4 | DPRASN0H == 4 | 
        DPRASN0I == 4 | DPRASN0J == 4 | DPRASN0K == 4 | DPRASN0L == 4 | DPRASN0M == 4 | DPRASN0N == 4 | DPRASN0O == 4 | DPRASN0P == 4 | DPRASN0Q == 4 | 
        DPRASX0A == 4 | DPRASX0B == 4 | DPRASX0C == 4 | DPRASX0D == 4 | DPRASX0E == 4 | DPRASX0F == 4 | DPRASX0G == 4 | DPRASX0H == 4 | DPRASX0I == 4 | 
        DPRASX0J == 4 | DPRASX0K == 4 | DPRASX0L == 4 | DPRASX0M == 4 | DPRASX0N == 4 | DPRASX0O == 4 | DPRASX0P == 4 | DPRASX0Q == 4 | DPRASX0R == 4 | 
        DPRASX0S == 4 | DPRASX0T == 4 | DPRASX0U == 4 | DPRASX0V == 4 | DPRASX0W == 4 | DPRASX0X == 4 | DPRASX0Y == 4 | DPRASX0Z == 4 | DPRASX1A == 4 | 
        DPRASX1B == 4 | DPRASX1C == 4 | DPRASX1D == 4 | DPRSEN0A == 4 | DPRSEN0B == 4 | DPRSEN0C == 4 | DPRSEN0D == 4 | DPRSEN0E == 4 | DPRSEN0F == 4 | 
        DPRSEN0G == 4 | DPRSEN0H == 4 | DPRSEN0I == 4 | DPRSEN0J == 4 | DPRSEN0K == 4 | DPRSEN0L == 4 | DPRSEN0M == 4 | DPRSNX0A == 4 | DPRSNX0B == 4 | 
        DPRSNX0C == 4 | DPRSNX0D == 4 | DPRSNX0E == 4 | DPRSNX0F == 4 | DPRSNX0G == 4 | DPRSNX0H == 4 | DPRSNX0I == 4 | DPRSNX0J == 4 | DPRSNX0K == 4 | 
        DPRSNX0L == 4 | DPRSNX0M == 4 | DPRSNX0N == 4 | DPRSNX0O == 4 | DPRSNX0P == 4 | DPRSNX0Q == 4 | DPRSNX0R == 4 | DPRSNX0S == 4 | DPRSNX0T == 4 | 
        DPRSNX0U == 4 | DPRSNX0V == 4 | DPRSNX0W == 4 | DPRSNX0X == 4 | DPRSNX0Y == 4 | DPRSNX0Z == 4 | DPRSNX1A == 4 | DPRSNX1B == 4 | DPRSNX1C == 4 | 
        DPRSNX1D == 4)

# There are 351 cohort children across MCS5 that have been identified as autistic
count(Autism_MCS_All, EPAUTS00 == 1 | EPRASN0D == 1 | EPRASX0D == 1 | EPRSEN0D == 1 | EPRSEX0D == 1)

# There are 390 cohort children across MCS6 that have been identified as autistic
count(Autism_MCS_All, FPAUTS00 == 1 | FPRASN0D == 1 | FPRSEN0D == 1)