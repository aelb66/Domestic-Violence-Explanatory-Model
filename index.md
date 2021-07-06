# How I created an explanatory model of domestic violence in NSW LGAs using R.

### :question: What is explanatory analysis? 
We are trying to explain why and how a phenomenon is the way it is. The key output for this type of analysis are insights. For differences between explanatory, exploratory and descriptive research click [here](https://www.theanalysisfactor.com/differences-in-model-building-explanatory-and-predictive-models/). To view differences between explanatory modelling vs predictive modelling click [here](https://www.theanalysisfactor.com/differences-in-model-building-explanatory-and-predictive-models/). 

### Background to the project
Domestic violence (DV) is recognised as a major Australian public health crisis with one in three homicide and sexual assault reports family and DV related (1). Research suggests that those at a higher risk of experiencing DV are young women (i.e., 15-24 years old), those living in rural/remotes areas; as well as areas with a higher proportion of Indigenous residents, culturally and linguistically diverse (CALD) residents and those with higher unemployment rates (2,3,4). Although the research generalises to the Australian community, it would be interesting to see if this is reflected in NSW. Therefore, I hypothesize that socio-demographic and socioeconomic factors are important in explaining DV rates in NSW local government areas (LGAs). 

### Domestic violence in Australia
It is well known that gender plays a role in DV with on average, one Australian woman each week murdered by her current or former partner (5). Furthermore, the economic cost of domestic violence against women and children in Australia alone is estimated at $21.7 *billion* per year (6). As females are more prone to domestic violence related assault compared to males, I theorise that higher female population rates may increase DV occurrences. Young Australian women are less likely to comprehend the breadth and magnitude of DV-related behaviour, and experience higher reported occurrences of DV compared to older Australian women (7,8). Therefore, I theorise that higher rates of rate of young persons aged between 15-24 years old may increase DV occurrences.

Although there is limited data available, women living in rural areas experience higher risk of DV due to smaller communities, greater societal control and gender norms than metropolitan counterparts (9). Indigenous women are *45 times* more likely to experience DV compared to non-Indigenous women and *35 times* more likely to be hospitalised from DV than non-Indigenous women (10,11). 

As Indigenous people are strongly prone to domestic violence related assault, I theorise that higher Indigenous population rates may increase DV occurrences. In addition to Indigenous women, CALD women, including immigrants and refugees experience higher risk of DV due to dowry disputes, gender norms and patriarchal cultural norms compared to non-CALD women (12,13). As CALD people are prone to domestic violence related assault, I theorise that higher CALD population rates may increase DV occurrences. 

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
library(data.table) #tables
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
As the outcome variable is aggregated count data, binary logistic regression and multiple linear regression may not be suitable. Furthermore, the model is to explain the rate of DV occurrences in NSW LGAs. Therefore, Poisson regression will be used and interpreted as every one unit increase in predictor *x*, the expected number of domestic violence related reported assaults per 100,000 changes by a factor of e^{beta_x}, when all other variables in the model are held at zero. I know, this interpretation sounds whack - but perhaps this site explains it better than I could, also have a look at this [Poisson regression example](https://stats.idre.ucla.edu/r/dae/poisson-regression/), hopefully it makes more sense?

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
Looking at the data (cropped): 
![image](https://user-images.githubusercontent.com/75398560/124451614-eb8cc100-ddc8-11eb-9964-65ebdb48282d.png)

### Deleting variables
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

### Handling missing values
There are two columns; one is the column we are interested in _Subcategory_ and there is a somewhat duplicated column called _Offence category_. Our variable of interest has many missing values, we can see from looking at the data that we can fill in those missing values from the _Offence category_ variable. 
```r
#if theres NAs in subcategory variable, replace with data from offence category variable, else keep as same
offence.datC$Subcategory <- ifelse(is.na(offence.datC$Subcategory), offence.datC$`Offence category`, offence.datC$Subcategory)
```

### Standardising text data
I cleaned up the text by removing characters irrelevant to data like astrices and white space. This is done through `stringr` from the `tidyverse` package. 
```r
#remove astrices in subcateogry variable and white space
offence.datC$Subcategory <- stringr::str_replace(offence.datC$Subcategory, "\\*", "")
offence.datC$Subcategory <- stringr::str_trim(offence.datC$Subcategory)
```

### Extracting the dependent variable and aggregating counts
If you inspect the data you can see there are multiple crime subcategories. Since we are only interested in domestic violence we will only keep this level of the subcategory variable in analysis. As I am not looking at time series data I chose to aggregate the average counts of DV. You can choose to just look at a particular year or month (cross-sectional data) though you will lose A LOT of data :grimacing:. 
```r
#keeping only DV related assault in Subcategory as its the variable of interest
offence.datC <- offence.datC[offence.datC$Subcategory == "Domestic violence related assault",]

#aggregate avg counts of DV related offences per LGA between 1995-2020. As we are not looking at time series.
offence.datC$avg.dv.count <- apply(offence.datC[,4:311], 1, mean)
```

Data so far:
![image](https://user-images.githubusercontent.com/75398560/124451386-ad8f9d00-ddc8-11eb-85e0-6a11f19200c2.png)


### Merging relevant datasets from ABS data to offence data
`merge` function from `tidyverse` can only take 2 arguments so I had to repeat merges 4 times. This can be tedious if you have several datasets and I'm sure there is a better way to merge data together :sweat_smile:. Do your research!

Sample code of two merges below.
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
```

Data so far:
![image](https://user-images.githubusercontent.com/75398560/124451252-8b961a80-ddc8-11eb-96ac-f991b829a736.png)

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
![image](https://user-images.githubusercontent.com/75398560/124451016-5689c800-ddc8-11eb-9093-bf9e2ce488f5.png)

## 5. Descriptive Analysis
### Poisson regression assumption testing
For Poisson regression to occur, the assumption that the outcome variable follows a Poisson distribution must be met. Below graph shows that the DV occurrences take on positive values within an interval of space and follows a Poisson distribution.

Investigating descriptive statistics from the below table (made from `setattr` function from the`data.table` library and `kable` function from `knitr` library) shows that most variances are quite small, suggesting that most variable data are clustered together. 

The mean DV occurrence is approximately 44 people per 100,000 individuals with a standard deviation of 66.81, indicating a larger variation, and an abnormal distribution also confirmed in the graph. The average rate of females per NSW LGA is 50% with a sd of 0.01. Additionally, the average rate of CALD identified people per NSW LGA is 14% with a standard deviation of 0.16. The average rate of young people aged 15-24 years per NSW LGA is 11% with a standard deviation of 0.02. Furthermore, the average rate of ATSI identified people per NSW LGA is 7%, which may seem quite low compared to the other groups, however it is larger than the reported 3.3%  of the total estimated Australian population that Indigenous people represent (16). Though, this may allude to signs of [overrepresentation of Indigenous Australians](https://www.bocsar.nsw.gov.au/Pages/bocsar_pages/Aboriginal-over-representation.aspx) in crime statistics as mentioned heavily in papers. Lastly, average rate of male unemployment per NSW LGA is 6% with a standard deviation of 0.02.
```r
ggplot(LGA.dat2, aes(x=DVrateper100k)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(DVrateper100k)), color="purple",
             linetype="dashed", size = 1.25)+
  labs(x="DV related reported assault per 100k", y = "Frequency")+
  theme_classic() +
  theme(strip.text.x = element_text(size=9),
        axis.title.x = element_text(size=9),
        axis.text.x = element_text(size=8),
        axis.title.y = element_text (size=9),
        axis.text.y = element_text(size=8))

```
![image](https://user-images.githubusercontent.com/75398560/124446547-df523500-ddc3-11eb-8363-7a88acf1fee3.png)

```r
DescTable <- LGA.dat2 %>% 
  dplyr::select(DVrateper100k, FemRate, CALDrate, YoungAgeRate, ATSIrate, MUnempRate) %>% 
  psych::describe() %>%
  base::as.data.frame() %>%  
  dplyr::select(mean, sd, median, min, max)
  
(setattr(DescTable, "row.names", c("LGA", "DV related reported assault per 100k","Female rate","CALD rate","Young age rate",
 "ATSI rate","Male unemployment rate")))
 
DescTable <- round(DescTable,2)
knitr::kable(DescTable,caption = "Descriptive statistics", booktabs=T)
```
![image](https://user-images.githubusercontent.com/75398560/124450536-e1b68e00-ddc7-11eb-8742-453a667a384e.png)

_(The above table format only shows up when you knit the file - the output straight from R Studio looks basic af so don't be worried!)_

### Scatterplots between DV and each IV
For the female population rate plot, there shows slight non-linear associations to DV occurrence. There is a slow growth from approximately 80 DV occurrences per 100,000 to 140 DV occurrences per 100,000 when shifting toward 45% female population, though this may be attributed to outlier LGAs. This then plateaus and decreases in DV occurrence when female population increases.

Both the CALD population rate plot and the young age population plot showed no obvious signs of a relationship with DV occurrence per 100,000. The ATSI population rate however, shows an quite a strong increase in DV occurrence per 100,000 when ATSI population rate increases. Similarly, the male unemployment rate plot shows a very gradual increase in DV occurrences with an increase in male unemployment rate, however it then rises dramatically towards the end. This steep rise may be attributed to the outlier LGA. 

Below shows sample code of one of the many graphs I made - you can see the full code at my github. Link [here](https://github.com/aelb66/Domestic-Violence-Explanatory-Model).
```r
#association between LGA-level DV-related reported offences and LGA-level rate of male unemployment
MUnempRategg <- ggplot(LGA.dat2,
       aes(y=DVrateper100k, x=MUnempRate)) +
  geom_jitter(aes(colour = LGA)) +
  geom_smooth(method = loess, se = FALSE, fullrang = TRUE, colour = "purple") +
  labs(x = "Male unemployment population rate (%)", y=" ") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="light grey", size=0.2),
        panel.grid.minor.y = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=9),
        strip.background = element_blank(),
        axis.title.x = element_text(size=9),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))

#displays graphs in grid form
descGrid <- ggarrange(Femgg,Caldgg,YoungAgeRategg,ATSIrategg,MUnempRategg, ncol=2, nrow=3)

#adds annotations
annotate_figure(descGrid,
                left = text_grob("DV related reported assault (per 100,000)", rot=90,
                                 size = 10))
```
![image](https://user-images.githubusercontent.com/75398560/124450224-900e0380-ddc7-11eb-94fb-52e2a25fb1cb.png)

## 6. Poisson Regression
### Regression preparation
I scaled the independent variables using the `scale` function. Some nice short explanations about scaling in R are found [here](https://stackoverflow.com/questions/20256028/understanding-scale-in-r).
```r
LGA.dat.scaled <-  LGA.dat2 %>%
  dplyr::select(LGA, DVrateper100k)

LGA.dat.scaled$z.FemRate = scale(LGA.dat2$FemRate)
LGA.dat.scaled$z.CALDrate = scale(LGA.dat2$CALDrate)
LGA.dat.scaled$z.YoungAgeRate = scale(LGA.dat2$YoungAgeRate)
LGA.dat.scaled$z.ATSIrate = scale(LGA.dat2$ATSIrate)
LGA.dat.scaled$z.MUnempRate = scale(LGA.dat2$MUnempRate)

#scaled data
head(LGA.dat.scaled )
```
![image](https://user-images.githubusercontent.com/75398560/124560419-20a81a80-de80-11eb-8ce4-eac4e2d6308e.png)

### Poisson regression base model
The *Base Model* is used as a reference point and consisted of only the young age variable. Poisson regression was created using `glm` function with parameter `family=poisson`. This model resulted in being statistically significant with the log odds coefficient estimate being more than two standard errors from zero. The *Base Model* shows that 0.2% of the variability in DV occurrence per 100,000 can be explained by its relationship to the young age rate. Which is very low, like less than 1% low. So clearly, theres definitely more variables that can explain DV occurrence. 
```r
DV.base <- glm(DVrateper100k ~ z.YoungAgeRate,
                         family=poisson(link = log), 
                         data=LGA.dat.scaled)
display(DV.base)

#quasi R^2 = 0.2%
1-(DV.base$deviance/DV.base$null)
```
![image](https://user-images.githubusercontent.com/75398560/124562391-51894f00-de82-11eb-8b9a-43c85dea850b.png)


### Poisson regression candidate model
From all four candidate models tested against the *Base Model*, it can clearly be seen that with each added variable, the model fit increases, with *Model 4* showing the best fit in th below table. 

All variables in this model except for young age were statistically significant with the log odds coefficient estimate being more than two standard errors from zero. 

From the model it can be seen that every one percent increase in the rate of females, the expected number of DV related reported assault per 100,000 decreases by a factor of e^{-0.10} = 0.9048, when all other variables in the model are held at zero. Similarly, every one percent increase in rate of CALD persons, the expected number of DV related reported assault per 100,000 decreases by a factor of e^{-0.05} = 0.9512, when all other variables in the model are held at zero. 

Moreover, every one percent increase in rate of male unemployment, the expected number of DV related reported assault per 100,000 decreases by a factor of e^{-0.12} = 0.8869, when all other variables in the model are held at zero. Lastly, every one percent increase in rate of ATSI persons, the expected number of DV related reported assault per 100,000 increases by a factor of e^{0.53} = 1.6989, when all other variables in the model are held at zero. 

*Model 4* shows that 88.81% of the variability in DV occurrence per 100,000 can be explained by its relationship to female population rate, CALD population rate, male unemployment rate and ATSI population rate. This suggests a good model fit in explaining DV occurrence in NSW LGAs.

Sample code is shown below.
```r
###young age rate + female rate
DV.m1 <- glm(DVrateper100k ~ z.YoungAgeRate + z.FemRate,
                         family=poisson(link = log), 
                         data=LGA.dat.scaled)
display(DV.m1)

#quasi R^2 = 20.16%
1-(DV.m1$deviance/DV.m1$null)

###young age rate + female rate + CALD rate + male unemployment rate + ATSI rate
DV.m4 <- glm(DVrateper100k ~ z.YoungAgeRate + z.FemRate + z.CALDrate + z.MUnempRate + z.ATSIrate,
                         family=poisson(link = log), 
                         data=LGA.dat.scaled)

display(DV.m4)

#quasi R^2 = 81.88%
1-(DV.m4$deviance/DV.m4$null)
```
```r
options(knitr.kable.NA = "")
knitr::kable(regrStats2, caption = "Poisson Regression Output for Five Models", 
             booktabs=TRUE)
```
![image](https://user-images.githubusercontent.com/75398560/124563508-6e725200-de83-11eb-967f-cd80aadf10bb.png)

## 7. Concluding remarks
Overall, the investigated socioeconomic and socio-demographic factors, except for young age, are important in explaining DV rates in NSW LGAs. Results from the model show interesting and opposing views to the literature and theory, where an increase in female population rate and CALD population rate may suggest a decrease in DV occurrence per 100,000. This may be attributed to the quality of the aggregated data itself and is unlikely to represent the behaviours of individuals. Male unemployment rate followed theory and some literature suggesting that an increase in male unemployment rate may decrease DV occurrence per 100,000. 

Furthermore it can be seen that ATSI identified persons had the strongest association to DV occurrence in the model, with results also aligning with theory and literature, where an increase in ATSI population rate may suggest an increase in DV occurrence per 100,000. Though again it is important to highlight the over-representation of indigenous Australians in crime-related statistics and the strong bias this may have on the results. 

Further significant limitations to the study is the strong underreporting bias on DV occurrence data, and the aggregate nature of the datasets make inferences about individual behaviour difficult. In light of these limitations, it would be interesting to see future studies focusing on [1] replicating this study with individual-level statistics and [2] investigating further the role of ATSI people in DV occurrence.

_**NOTE:**_ I'm not perfect and neither is my code :stuck_out_tongue_winking_eye:. I'm learning new/more efficient ways to code all the time, so if you find a better way of doing things then go for that! I'm just putting this code and project out there for those interested in the data science field and to show you what I love doing :blush:.

## 8. References
1.	Australian Bureau of Statistics. Recorded crime: victims, Australia [Internet]. Canberra: Australian Bureau of Statistics; 2019 [cited 22 Jan 2021]. 5 p. ABS Cat. No.: 4510.0. Available from: https://www.abs.gov.au/statistics/people/crime-and-justice/recorded-crime-victims-australia/2019#articles 

2.	Phillips J, Vandenbroek P. Domestic, family and sexual violence in Australia: an overview of the issues [Internet]. Canberra: Department of Parlimentary Services; 2014 [cited 22 Jan 2021]. 25 p. Available from: https://parlinfo.aph.gov.au/parlInfo/download/library/prspub/3447585/upload_binary/3447585.pdf;fileType=application/pdf

3.	People J. Trends and patterns in domestic violence assaults [Internet]. NSW: NSW Bureau of Crime Statistics and Research; 2005 [cited 22 Jan 2021]. 16 p. no. 89. Available from: https://www.bocsar.nsw.gov.au/Publications/CJB/cjb89.pdf

4.	Carrington K, Phillips J. Domestic violence in Australia: an overview of the issues [Internet]. Canberra: Parliament of Australia; 2006 [cited 22 Jan 2021] 30 p. Available from: https://www.aph.gov.au/about_parliament/parliamentary_departments/parliamentary_library/publications_archive/archive/domviolence

5.	Bricknell S. Homicide in Australia 2016-17 [Internet]. Canberra: Australian Institute of Criminology; 2020 [cited 23 Jan 2021]. Statistical Report no. 22. Available from: https://www.aic.gov.au/publications/sr/sr22

6.	PricewaterhouseCoopers Australia. The economic case for preventing violence against women [Internet]. Victoria: PricewaterhouseCoopers Australia; 2015 [cited 8 February 2021]. 16p. Available from: https://www.pwc.com.au/pdf/a-high-price-to-pay.pdf 

7.	Symons H. The effect of family violence on sexual victimization among young women. J Fam Violence [Internet]. 2016 [cited 24 Jan 2021];31(6):759–769. Available from: https://doi.org/10.1007/s10896-016-9803-5

8.	Australian Bureau of Statistics. Personal Safety, Australia [Internet]. Canberra: Australian Bureau of Statistics; 2016 [cited 23 Jan 2021]. 15 p. ABS Cat. No.:4906.0.55.003. Available from: https://www.abs.gov.au/statistics/people/crime-and-justice/personal-safety-australia/latest-release

9.	Crisp B. Domestic violence in rural Australia. Soc Work Educ [Internet]. 2011 [cited 23 Jan 2021];30(1):115–117. Available from: https://doi.org/10.1080/02615479.2010.506047

10.	Adams H, Hunter Y. Surviving justice: family violence, sexual assault and child sexual assault in remote Aboriginal communities in NSW. Indig Law B [Internet]. 2007 [cited 24 Jan 2021]; 7(1):26–28. Available from: http://www5.austlii.edu.au/au/journals/IndigLawB/2007/64.html 

11.	Australian Institute of Health and Welfare, Al-Yaman F, Van Doeland M. Family violence among Aboriginal and Torres Strait Islander peoples [Internet].Canberra: Australian Institute of Health and Welfare; 2006 [cited 24 Jan 2021]. Available from: https://www.aihw.gov.au/reports/indigenous-australians/family-violence-indigenous-peoples/contents/executive-summary 

12.	O’Connor M. Dowry-related domestic violence and complex posttraumatic stress disorder: a case report. Aust Psychiatry [Internet]. 2017 [cited 23 Jan 2021];25(4):351–353. Available from: https://doi.org/10.1177/1039856217700464

13.	Murray W. Between “here” and “there”: family violence against immigrant and refugee women in urban and rural Southern Australia. Gender, Place, Cul: J Fem Geo [Internet]. 2019 [cited 28 Jan 2021];26(1):91–110. Available from: https://doi.org/10.1080/0966369X.2018.1553862

14.	Anderberg R. Unemployment and domestic violence: theory and evidence.  Econ J Lond [Internet]. 2016 [cited 28 Jan 2021];126(597):1947–1979. https://doi.org/10.1111/ecoj.12246  

15.	Data from NSW Bureau of Crime Statistics and Research. Available from: https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Offence.aspx 

16.	Data from Australian Bureau of Statistics. Available from: https://datapacks.censusdata.abs.gov.au/datapacks/ 

17.	Shmueli G. To explain or to predict? Stat Sci [Internet]. 2010 [cited 28 Jan 2021];25(3): 289-310. Available from http://www.jstor.org/stable/41058949 

18.	Shircore M, Douglas H, Morwood V. Domestic and family violence and police negligence. Syd Law Rev [Internet]. 2017 [cited 28 Jan 2021];39(4):539–567. Available from: http://classic.austlii.edu.au/au/journals/SydLawRw/2017/22.html
