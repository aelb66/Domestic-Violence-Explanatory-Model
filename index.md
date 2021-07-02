# How I created an explanatory model of domestic violence in NSW LGAs using R.

### :question: What is explanatory analysis? 
We are trying to explain why and how a phenomenon is the way it is. The key output for this type of analysis are insights.

For differences between explanatory, exploratory and descriptive research click [here](https://www.theanalysisfactor.com/differences-in-model-building-explanatory-and-predictive-models/). To view differences between explanatory modelling vs predictive modelling click [here](https://www.theanalysisfactor.com/differences-in-model-building-explanatory-and-predictive-models/). 

### Background to the project
Domestic violence (DV) is recognised as a major Australian public health crisis with one in three homicide and sexual assault reports family and DV related (1). Research suggests that those at a higher risk of experiencing DV are young women (i.e., 15-24 years old), those living in rural/remotes areas; as well as areas with a higher proportion of Indigenous residents, culturally and linguistically diverse (CALD) residents and those with higher unemployment rates (2,3,4). Although the research generalises to the Australian community, it would be interesting to see if this is reflected in NSW. Therefore, I hypothesize that socio-demographic and socioeconomic factors are important in explaining DV rates in NSW local government areas (LGAs). 


### Domestic violence in Australia
It is well known that gender plays a role in DV with on average, one Australian woman each week murdered by her current or former partner (5). Furthermore, the economic cost of domestic violence against women and children in Australia alone is estimated at $21.7 *billion* per year (6). As females are more prone to domestic violence related assault compared to males, I theorise that higher female population rates may increase DV occurrences. Young Australian women are less likely to comprehend the breadth and magnitude of DV-related behaviour, and experience higher reported occurrences of DV compared to older Australian women (7,8). Therefore, I theorise that higher rates of rate of young persons aged between 15-24 years old may increase DV occurrences.

Although there is limited data available, women living in rural areas experience higher risk of DV due to smaller communities, greater societal control and gender norms than metropolitan counterparts (9). Indigenous women are *45 times* more likely to experience DV compared to non-Indigenous women and *35 times* more likely to be hospitalised from DV than non-Indigenous women (10,11). 

As Indigenous people are strongly prone to domestic violence related assault, I theorise that higher Indigenous population rates may increase DV occurrences. In addition to Indigenous women, CALD women, including immigrants and refugees experience higher risk of DV due to dowry disputes, gender norms and patriarchal cultural norms compared to non-CALD women (12,13).As CALD people are prone to domestic violence related assault, I theorise that higher CALD population rates may increase DV occurrences. 

Lastly, research suggests that higher male unemployment lowers the risk of DV against women (14). Opposingly, literature also found that higher male unemployment increases DV against women (3), therefore it will be interesting to clarify this contention. Unemployed males may feel that their contribution in their relationship is reduced, with the power shifting toward the other partner who has financial means to sustain both parties. Therefore, I theorise that higher male unemployment population rates may decrease DV occurrences. 

Although DV prevention campaigns, helplines and programs are widely advertised in Australia, only 50% of DV occurrences are reported to the police, leading to a gross under-estimation of DV data and further caution in interpreting the results of the analysis.

## Data :page_facing_up:
Data includes [reported criminal incidents per month by NSW LGA from 1995 to 2020](https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Offence.aspx) (i.e., offence data) and contains 8,122 cases with 312 variables (15). Additionally, data from the [2016 Census represent sociodemographic and socioeconomic characteristics of persons by NSW LGA](https://datapacks.censusdata.abs.gov.au/datapacks/) (i.e., ABS data) (16). This contains 110 files and an average of 132 cases with 150 variables. The data can be retrieved from my github repository as well as the embedded links. 

As I am explaining DV occurrences across NSW LGAs, the dependent variable will be *Domestic violence related reported assault per 100,000 population*. This is retrieved from the offence data. 

Based on the literature, the independent variables will be:

-	*Young age population rate* as percentage of each LGA’s persons aged between 15-24 years old.

-	*Female population rate* as percentage of each LGA’s female population.

-	*ATSI population rate* as percentage of each LGA’s persons who identify as Aboriginal and/or Torres Strait Islander (ATSI).

-	*CALD populaton rate* as percentage of each LGA’s persons who was born outside of Australia and/or speaks a language other than English at home.

-	*Male unemployment population rate* as percentage of each LGA’s males who are unemployed but looking for work.

These were retrieved from the ABS data. 

## 1. Libraries :books:
Below are the libraries I used in this project. Its useful to comment everything so when you get back to viewing the code a month, a year later, you don't get lost in your code. Especially useful if you have terrible memory like me.
```r
library(knitr) #tables
library(readr) #reading csv
library(dplyr) #data manipulations
library(tidyr) #data manipulation
library(scales) #scaling
library(ggplot2) #plotting
library(ggpubr) #arranging plots
library(psych) #descriptive stats
library(arm) #regression
library(rgdal) #static maps
library(stringr) #data tidying
```

## 2. Loading Data
```r
#data provided
offence.dat <- read_csv("Data/RCI_offencebymonth1.csv") 

#2016 Census data on characteristcs of persons by LGA
char2016.dat <- read_csv("Data/2016 Census GCP Local Government Areas for NSW/2016Census_G01_NSW_LGA.csv")

#2016 census data on age by LGA
age2016.dat <- read_csv("Data/2016 Census GCP Local Government Areas for NSW/2016Census_G03_NSW_LGA.csv")

#2016 census data on A/TSI status by LGA
atsi2016.dat <- read_csv("Data/2016 Census GCP Local Government Areas for NSW/2016Census_G07_NSW_LGA.csv")

#2016 census data on male unemployment rate by LGA
unemp2016.dat <- read_csv("Data/2016 Census GCP Local Government Areas for NSW/2016Census_G40_NSW_LGA.csv")
```

## 3. Method
As the outcome variable is aggregated count data, binary logistic regression and multiple linear regression may not be suitable. Furthermore, the model is to explain the rate of DV occurrences in NSW LGAs. Therefore, Poisson regression will be used and interpreted as every one unit increase in predictor *x*, the expected number of domestic violence related reported assaults per 100,000 changes by a factor of e^{beta_x}, when all other variables in the model are held at zero. I know, this interpretation sounds whack - but perhaps this site explains it better than I could also have a look at this [Poisson regression example](https://stats.idre.ucla.edu/r/dae/poisson-regression/), hopefully it makes more sense?

After confirming the Poisson distribution assumption of Poisson regression, I will then examine associations between sociodemographic and socioeconomic variables and DV occurrence through association plots.

Model selection involves creating a baseline model involving the intercept and the only *young age pop. rate* variable and comparing this to other models which subsequently include the addition of other variables of interest per model. 

As the model is intended for explanation, it will be assessed using McFadden’s quasi-*R²* (17) as well as the regression coefficient standard errors with respective *p*-values. 

## 4. Pre-processing 
Datasets will be cleaned, merged, then variables of interest transformed for ease of interpretation. As most people will say, this takes the most amount of your time. For me, around 70-80% of the project is just pre-processing the data :astonished:.

### Transforming the offence data
First I kept a copy of the original file and examined the data. A cropped snippet of the output is shown. 
```r
#keeping original dataset as precaution
offence.datC <- offence.dat 

#examining data
table(offence.datC$LGA) 
offence.datC$LGA <- as.factor(offence.datC$LGA)
```
![image](https://user-images.githubusercontent.com/75398560/124224423-f76a5000-db48-11eb-8a3c-fe65c6ed773e.png)

#### Deleting variables
I deleted some variables in the datasets that weren't found in one or the other dataset, or were irrelevant to the data. Resulted in 125 NSW LGAs in total. I also turned the LGA variable(column) in offence data as a factor from a numeric variable, which it isn't. 
```r
#removing 'in custody' level from LGA variable
#From cross-referencing: Census 2016 LGA data does not contain 5 LGAs in the offence dataset so delete them
offence.datC <- offence.datC[offence.datC$LGA!="In Custody",]
offence.datC <- offence.datC[offence.datC$LGA!="Bayside",] 
offence.datC <- offence.datC[offence.datC$LGA!="Cootamundra-Gundagai",] 
offence.datC <- offence.datC[offence.datC$LGA!="Dubbo Regional",] 
offence.datC <- offence.datC[offence.datC$LGA!="Lord Howe Island",] 
offence.datC <- offence.datC[offence.datC$LGA!="Unincorporated Far West",] 
offence.datC$LGA <- factor(offence.datC$LGA)
```

#### Handling missing values
There are two columns; one is the column we are interested in _Subcategory_ and there is a somewhat duplicated column called _Offence category_. Our variable of interest has many missing values, we can see from looking at the data that we can fill in those missing values from the _Offence category_ variable. 
```r
#if theres NAs in subcategory variable, replace with data from offence category variable, else keep as same
offence.datC$Subcategory <- ifelse(is.na(offence.datC$Subcategory), offence.datC$`Offence category`, offence.datC$Subcategory)
```

#### Standardising text data
I cleaned up the text by removing characters irrelevant to data like astrices and white space. This is done through `stringr` from the `tidyverse` package. 
```r
#remove astrices in subcateogry variable and white space
offence.datC$Subcategory <- stringr::str_replace(offence.datC$Subcategory, "\\*", "")
offence.datC$Subcategory <- stringr::str_trim(offence.datC$Subcategory)
```

#### Extracting the dependent variable and aggregating counts
If you inspect the data you can see there are multiple crime subcategories. Since we are only interested in domestic violence we will only keep this level of the subcategory variable in analysis. As I am not looking at time series data I chose to aggregate the average counts of DV. You can choose to just look at a particular year or month (cross-sectional data) though you will lose A LOT of data :grimacing:. 
```r
#keeping only DV related assault in Subcategory as its the variable of interest
offence.datC <- offence.datC[offence.datC$Subcategory == "Domestic violence related assault",]

#aggregate avg counts of DV related offences per LGA between 1995-2020. As we are not looking at time series.
offence.datC$avg.dv.count <- apply(offence.datC[,4:311], 1, mean)
```

Data so far:
![image](https://user-images.githubusercontent.com/75398560/124226282-3352e480-db4c-11eb-9e9c-a7aac1b3dbff.png)


### Merging relevant datasets from ABS data to offence data
`merge` function from `tidyverse` can only take 2 arguments so I had to repeat merges. This can be tedious if you have several datasets and I'm sure there is a better way to merge data together :sweat_smile:. Do your research!
```r
#merge1
LGA.dat <- merge(
  offence.datC %>%
    dplyr::rename(AvgDVcount = avg.dv.count) %>%
    dplyr::select(LGA, AvgDVcount),
  char2016.dat %>%
    dplyr::rename(TotFem = Tot_P_F, #total females counted in 2016 census per NSWLGA
                  TotPpl = Tot_P_P, #total people counted in 2016 census per NSW LGA
                  BornAus = Birthplace_Australia_P, #count of people who were born in Australia in 2016 census per NSW LGA
                  BornOther = Birthplace_Elsewhere_P, #count of people who were born in Australia in 2016 census per NSW LGA
                  LangEng = Lang_spoken_home_Eng_only_P, #count of people who only spoke English at home in 2016 census per NSW LGA
                  LangOther = Lang_spoken_home_Oth_Lang_P) %>% #count of people who only another langauge at home in 2016 census per NSW LGA
    dplyr::select(LGA, TotFem, TotPpl, BornAus, BornOther, LangEng, LangOther),
  by = "LGA")

#merge2
LGA.dat <- merge(
  LGA.dat %>%
    dplyr::select(LGA, AvgDVcount, TotFem, TotPpl, BornAus, BornOther, LangEng, LangOther),
  age2016.dat %>%
    dplyr::rename(TotAgeYoung = Total_15_24_yr, #total people aged 15 to 24 years in 2016 census per LGA NSW
                  TotAge = Total_Total) %>% #total people reporting ages between 0-99 years in 2016 census per LGA NSW
    dplyr::select(LGA, TotAgeYoung, TotAge),
  by = "LGA")

#merge3
LGA.dat <- merge(
  LGA.dat %>%
    dplyr::select(LGA, AvgDVcount, TotFem, TotPpl, BornAus, BornOther, LangEng, LangOther,
                  TotAgeYoung, TotAge),
  atsi2016.dat %>%
    dplyr::rename(TotIndig = Tot_Indigenous_P, #total people who identified as Indigenous in 2016 census per LGA NSW
                  TotNonIndig = Tot_Non_Indigenous_P) %>% #total people who identified as non-Indigenous in 2016 census per LGA NSW
    dplyr::select(LGA, TotIndig, TotNonIndig),
  by = "LGA")

#merge4
LGA.dat <- merge(
  LGA.dat %>%
    dplyr::select(LGA, AvgDVcount, TotFem, TotPpl, BornAus, BornOther, LangEng, LangOther, TotAgeYoung,
                  TotAge, TotIndig, TotNonIndig),
  unemp2016.dat %>%
    dplyr::rename(MUnempRate = Percent_Unem_loyment_M) %>% #total number of males who are unemployed and looking for work in 2016 census per LGA NSW
    dplyr::select(LGA, MUnempRate),
  by = "LGA")
```

Data so far:
![image](https://user-images.githubusercontent.com/75398560/124228219-38656300-db4f-11eb-9c19-dbeb109ccfb9.png)

### Creating variables for analysis
So I have six variables of interest that I'm only interested in. These variables were selected using theory (journal articles). As I am not looking at count data, as this is hard to interpret I am creating the following variables:

- _DVrateper100k_ = rate of domestic violence related assault per 100,000 population. This is the dependent (outcome) variable.

Independent variables:
- _FemRate_ = percentage of each LGA’s females.
- _CALDrate_ =  percentage of each LGA’s persons who was born elsewhere to Australia and/ or speaks a language other than English at home.  
- _YoungAgeRate_ = as percentage of each LGA’s persons aged between 15-24 years old.
- _ATSIrate_ = percentage of each LGA’s persons who identify as Aboriginal and/or Torres Strait.
- _MUnempRate_ = percentage of each LGA’s males who are unemployed.
```r
#creating variables for analysis and cleaning dataset to variables only of interest
LGA.dat2 <- LGA.dat %>%
  group_by(LGA) %>%
  mutate(DVrateper100k = ((AvgDVcount/TotPpl)*100000)) %>%
  mutate(FemRate = (TotFem/TotPpl)) %>%
  mutate(CALDrate = ((BornOther + LangOther)/(BornAus + LangEng + BornOther + LangOther))) %>%
  mutate(YoungAgeRate = (TotAgeYoung/TotAge)) %>%
  mutate(ATSIrate = (TotIndig/(TotIndig + TotNonIndig))) %>%
  mutate(MUnempRate = (MUnempRate/100)) %>%
  dplyr::select(LGA, DVrateper100k, FemRate, CALDrate, YoungAgeRate, ATSIrate, MUnempRate)
```

Data so far:
![image](https://user-images.githubusercontent.com/75398560/124228746-f8eb4680-db4f-11eb-8afa-35c0cfb3ecf4.png)


## 5. Descriptive Analysis


_NOTE:_ I'm not perfect and neither is my code :stuck_out_tongue_winking_eye:. I'm learning new/more efficient ways to code all the time, so if you find a better way of doing things then go for that! I'm just putting this code and project out there for those interested in the data science field and to show you what I love doing :blush:.
