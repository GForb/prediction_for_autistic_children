# Notes on datasets

## ALSPAC

Maternal education data does not include whether went to university so will record as missing.
Using data from earlier waves (age 12 wave) adds minimal data due to missing outcome data. 

## GUI 

### Autism status

#### Wave 1: 
Variable MMJ22a to MMJ22h - select from a list of neurodevelopmental diseases, MMJ22c is autism 
MMJ23: was it diagnosed by a professional? There are 6 people who report autism and not diagnosed by professional. 
Require both yes for someone to be autistic

#### Wave 2: 
pc2e15d - ASD - yes/no
pc2e16d - Was your child diagnosed with ASD by a medical professional. 
There are 6 people who report autism and not diagnosed by professional. 
Require both yes for someone to be autistic

#### Wave 3: 
pc3d3d -  Does YP have autism spectrum disorders (e.g. autism, aspergers syndrome)

### Wave 4 - no mention of autism

Speach and language difficulty data is available alongside ASD question in wave 2

## LSAC
Data import on k cohort only so far.
Neighbourhood deprevation is measured using something called SEIFA - Index of Relative Socio-economic Advantage and Disadvantage - 2016 - SA2 - Deciles - National


### Autism Status
Autism asked about in waves 4,5,6,7. Some people have value of -9. we do not know what this means. Have coded as missing.

## Howlin Rutter
We treat the data as two waves and use the following variables from the original data:

#### Wave 1: Childhood
- critage - Age
- sex - Sex
- cpiq = performance iq - note critiq, labelled best estimate iq is identical to this variabl
- cviq = verbal iq

#### Wave 2: Adulthood
- testage - Age
- bestfina - performance IQ
- viqest


## ELENA
IQ comes from a veriety of sources. These are outlined in a separate word document. I have created an IQ mapping to map onto subscales of the WISC IV, as I can then use data from pathwyas to impute an IQ variable. 

For some participants ELENA uses the WISC V so the mapping is not perfect. Fluid reasoning is not in the WISC-IV, but is a subset of perceptual reasoning  reasoning. I am treating this as perceptual reasoning. For simplicity, I am treating nonverabl IQ as full scale iq as the numbers with meaures of nonverbal IQ are small and any bias introduced will be minimal.



## EDX
accounting numbers:
196 individuals
192 Vineland any age
124 vineland over age 12

IQ: From wide data, IQ is taken to be fsiq if available and the mean of nviq, viq otherwise. If mean of viq and nviq are used standard_iq is set to 0. Model based impuation is not used as fsiq is missing below a certain cutoff - impuation would lead to out of range predictions ie. extrapolation. If fsiq is missing and nviq and viq are missing, IQ is set to missing.

## MCS
For maternal education I am using maternal education in wave 1. Additional qualifications may have been earnt but due to time constraints I have not merged. I've used the hhgrid from wave 3 to identify mothers because this is the first place I can find person sex for the house hold member. I only included natural mothers as including other mother like relationships led to duplicates (eg. step mothers).
For maternal mental health I subract 6 as raw scores range from 6 to 36, I require range 0 to 30

## Pathways
Baseline data on the VABS is incomplete in the analysis population. This is due to people having missing domains. No person in the analysis population is missing all three of the domains.

## SSC
There are some problems with the total scores - check them. I sent an eamil to SSC querring a data point because it had an entry error.
Wave 2 data is DSM 5 scores. These differ. Not clear what wave 1 data is, probably DSM 4 as names the affective/depressive problems score 
Onle standard scores for the ABC are available.


## TEDS
Data is received in two stages. First the SRS data and ADOS data was sent to me. The raw ADOS data has not been retained. This is processed by the script process_TEDS_ados_raw. This is merged with the rest of the SRS data in the script process_TEDS_SRS_raw. The processed SRS data is then sent to TEDS to be added to the rest of the data. 

Family IDs over 100000 are not TEDS participants, but were included in the SRS study.

Notes on data checks: SDQ totals may not be reliable as scores as high as 41 exist. Re-calcualte if used.

There are two sources for SDQ - the main TEDS dta and SRS data

Variables that end with 2 often refer to the twin rather than the proband.

There is no data on visual impariment, only hearing impariment. Hearing impariment is taken from age 7.

Not all SDQ items were collected at age 16. The emotional and peer problems subscales were not collected. I'm therefore using the SRS data as final assessment for TEDS. This leads to a lot of people not included. If I include age 16 data I have many more participants but different participants for different sub scales.

I could use different subsets for different sub-domains but that will create confusion with results. 


I have picked one twin per family where more than one eligible twin. Where one twin has a pre baseline diagnosis I pick that twin. Otherwise pick is random. 

### Variables measuring cognative function
lrvtota1 lcg1 lgktota1 lpctot1 lrvtota1 lvctota1

### Financial difficulties and subjective poverty
ipincome ipfinchange 

### Selecting waves
For outcome either wave age 13 or wave age 16 can be used - I've defaulted to wave age 16 as that is what the SAP says but shorter follow up could be had with wave age 13
For baseline, wave age 9, 12, or 13 could be used. I've defaulted to 12 and excluded 9 as taking age 9 as baseline includes a small number of additional participants with very long follow up.

## TOGO1
There is very little IQ data so this is not used. No participants included in the analysis data have IQ recorded at wave 2. 
No participants have visual or hearing impairments. See email from Margo Dewitte <Margo.Dewitte@UGent.be> 10/04/2024.

## TOGO2
Scoring for the DSM-IV domains is based on this doc: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=79a5a9a1ba583ad53871dd8688ccbe564c2c60db
The last two characters of the column names indicate which scale it may be from but it is not perfect.
There are also DSM 5 scoring instructions which differ slighltly (espeically for anxiety)
The questions numbers past q56 are +7. So question 103 in the CBCL is 110 in the data. THis is because question 56 on somatic problems has 7 parts, wchih are each given there own number in the togo dataset.
Not all items were collected as the other problems subscale was not collected. I have prorated and can include in MI sensitivity analysis.
