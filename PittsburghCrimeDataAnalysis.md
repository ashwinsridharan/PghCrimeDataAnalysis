---
title: "Pittsburgh Crime Data Analysis"
author: "Author: Ashwin Sridharan"
date: 
output: 
  html_document:
    keep_md: true
    toc: true
    toc_depth: 2
    theme: paper
    highlight: tango
---



## **INTRODUCTION** 

### This project is about analyzing the Pittsburgh Crime data which includes a number of demographic variables along with arrest information. Our main goal of the project is about discovering interesting correlations and draw conclusions & suggest recommendations regarding race, gender, location and other crime specifics. We also plan to focus into Juvenile crimes that are a real threat to the society right now.

In this project we have used a data set called "Pittsburgh crime stats" that includes the Pittsburgh arrest records data retrieved from the Western PA Regional Data Center http://www.wprdc.org/ 


## **LOADING THE DATASET & PRELIMINARY ANALYSIS**

### Let's first Load the full `Pittsburgh_crime_stats.csv` data set for preparation & exploratory data analysis (EDA). 



```r
Pittsburgh_crime_stats <- read.csv("~/R hws/Pittsburgh_crime_stats.csv")
```

#### Let's have a look at how many rows and columns are present in the data set.
#### Using the inline coding technique, it looks like we have **15** Columns and **37219** Rows in our data set.

#### Let us now run the command `str()` to view the structure of the data set.

```r
str(Pittsburgh_crime_stats)
```

```
## 'data.frame':	37219 obs. of  15 variables:
##  $ PK                   : int  1975272 1974456 1974466 1974550 1974596 1974556 1974628 1974607 1974643 1974647 ...
##  $ AGE                  : int  42 31 63 25 25 45 29 21 17 14 ...
##  $ GENDER               : Factor w/ 3 levels "F","M","U": 1 2 1 1 2 2 2 1 2 2 ...
##  $ RACE                 : Factor w/ 7 levels "A","B","H","I",..: 2 7 2 7 2 7 7 2 2 2 ...
##  $ ARREST_TIME          : Factor w/ 31658 levels "1998-03-11T11:30:00",..: 481 87 89 112 132 281 304 297 577 459 ...
##  $ ARREST_LOCATION      : Factor w/ 9310 levels "10 Block 35th ST Pittsburgh, PA 15201",..: 3489 3397 5106 6600 5212 4116 2070 4116 1007 9308 ...
##  $ OFFENSES             : Factor w/ 10515 levels "","028 Curfew Violation / 9093 Indirect Criminal Contempt",..: 6525 9 6525 8068 2970 6525 1951 2700 9592 4757 ...
##  $ INCIDENT_LOCATION    : Factor w/ 10079 levels " ,  ","0800 Block Suismon ST Pittsburgh, PA 15212",..: 3984 3877 5763 7290 5864 5555 2405 5229 8434 10077 ...
##  $ INCIDENT_NEIGHBORHOOD: Factor w/ 99 levels "","Allegheny Center",..: 11 65 98 28 25 40 15 74 31 1 ...
##  $ INCIDENT_ZONE        : Factor w/ 8 levels "","1","2","3",..: 6 8 6 2 6 5 7 5 6 6 ...
##  $ INCIDENTTRACT        : int  804 5599 2811 2304 2814 1517 1919 1410 1115 NA ...
##  $ COUNCIL_DISTRICT     : int  8 NA 9 1 2 5 4 NA NA NA ...
##  $ PUBLIC_WORKS_DIVISION: int  2 NA 2 1 5 3 5 NA NA NA ...
##  $ Lat                  : num  -79.9 -80.1 -79.9 -80 -80.1 ...
##  $ Long                 : num  40.5 40.4 40.5 40.5 40.4 ...
```
#### We note that in the 15 Columns, there are primarily 3 data types:int, Factor & num in the data set structure. 
#### The main demographic information comes from the AGE, GENDER & RACE columns while Arrest information can be obtained using the ARREST_TIME, ARREST_LOCATION, OFFENSES, INCIDENT_LOCATION, INCIDENT_NEIGHBORHOOD, INCIDENT_ZONE columns.
#### From the structure, we also get to see that there are NA's in some columns that we need to be careful about going forward.


```r
sum(!complete.cases(Pittsburgh_crime_stats))
```

```
## [1] 5003
```
Note: We have about 1/7th of the data that has some kind of missing values. 

#### Now, let us find out the summary statistic for this data set.

```r
summary(Pittsburgh_crime_stats)
```

```
##        PK               AGE         GENDER    RACE     
##  Min.   :1974435   Min.   :  0.00   F: 9966   A:  163  
##  1st Qu.:1986333   1st Qu.: 24.00   M:27215   B:22203  
##  Median :1998129   Median : 31.00   U:   38   H:  340  
##  Mean   :1998130   Mean   : 33.31             I:   10  
##  3rd Qu.:2009947   3rd Qu.: 41.00             O:  429  
##  Max.   :2021860   Max.   :999.00             U:  273  
##                    NA's   :295                W:13801  
##               ARREST_TIME                                    ARREST_LOCATION 
##  2017-03-19T08:20:00:   17   600 Block 1st AV Pittsburgh, PA 15219   : 3249  
##  2017-08-08T21:45:00:   16   900 Block 2nd AV Pittsburgh, PA 15219   : 2152  
##  2018-04-03T18:36:00:   14   600 Block 1ST AV PITTSBURGH, PA 15219   : 1225  
##  2018-10-26T14:44:00:   13   900 Block 2ND AV Pittsburgh, PA 15219   :  660  
##  2019-10-23T09:25:00:   13   900 Block Second AV Pittsburgh, PA 15219:  644  
##  2016-10-25T18:20:00:   12   Zone 2                                  :  630  
##  (Other)            :37134   (Other)                                 :28659  
##                                                                                        OFFENSES    
##  2701 Simple Assault.                                                                      : 2487  
##  9501 Bench Warrant                                                                        : 1894  
##  9015 Failure To Appear/Arrest on Attachment Order                                         : 1777  
##  13(a)(31) Marijuana: Possession Small Amount                                              :  916  
##  13(a)(32) Paraphernalia - Use or Possession                                               :  816  
##  13(a)(16) Possession of Controlled Substance / 13(a)(32) Paraphernalia - Use or Possession:  649  
##  (Other)                                                                                   :28680  
##                                INCIDENT_LOCATION
##  Zone 1                                 :  308  
##  Zone 3                                 :  263  
##  Zone 5                                 :  229  
##  Zone 2                                 :  225  
##  6200 Block Penn AV Pittsburgh, PA 15206:  206  
##  300 Block Cedar AV Pittsburgh, PA 15212:  197  
##  (Other)                                :35791  
##                INCIDENT_NEIGHBORHOOD INCIDENT_ZONE  INCIDENTTRACT 
##  Central Business District: 2203     1      :8504   Min.   : 103  
##  South Side Flats         : 1983     3      :7582   1st Qu.:1113  
##                           : 1402     5      :7314   Median :1702  
##  Carrick                  : 1379     2      :5952   Mean   :1657  
##  East Allegheny           : 1278     4      :3875   3rd Qu.:2304  
##  Homewood South           : 1174     6      :3675   Max.   :9822  
##  (Other)                  :27800     (Other): 317   NA's   :1380  
##  COUNCIL_DISTRICT PUBLIC_WORKS_DIVISION      Lat              Long      
##  Min.   :1.000    Min.   :0.000         Min.   :-80.49   Min.   : 0.00  
##  1st Qu.:2.000    1st Qu.:2.000         1st Qu.:-80.00   1st Qu.:40.42  
##  Median :5.000    Median :3.000         Median :-79.98   Median :40.45  
##  Mean   :4.767    Mean   :2.954         Mean   :-73.52   Mean   :37.18  
##  3rd Qu.:7.000    3rd Qu.:5.000         3rd Qu.:-79.92   3rd Qu.:40.46  
##  Max.   :9.000    Max.   :6.000         Max.   :  0.00   Max.   :41.23  
##  NA's   :4735     NA's   :4735          NA's   :1320     NA's   :1320
```

#### --> We find that "600 Block 1st AV Pittsburgh, PA 15219" is the most crime prone area while "2701 Simple Assault" was the most common type of incident. 
#### --> We also get to know that Age has irregular data (outliers) with the minimum & maximum age being 0 and 999, which sounds practically impossible.
#### --> Women representation is low as they only form a third of number of men committing crimes.
#### --> Blacks and Whites have commited more crimes than any other Race.

#### --> While the summary also reveals a lot of other statistical information that is relevant to us, let's visualze them in the later sections to get a better view & understanding of the data.


## **DATA PREPARATION & CLEANSING**


#### We can see that some data cleansing is required before we proceed with our analysis. 
#### For instance, **ARREST_TIME** is in the Date-Time format and exists as a Factor. Therefore, we need convert them to the Date type and then extract the Arrest dates, Day of the week and the Hour of arrest for our analysis, as minutes and seconds don't add much value to us.


```r
# Converting Date from factor to date
Pittsburgh_crime_stats$ARREST_DATE <- ymd_hms(Pittsburgh_crime_stats$ARREST_TIME)

# Extracting arrest hour from date
Pittsburgh_crime_stats$ARREST_HOUR <- substring(Pittsburgh_crime_stats$ARREST_DATE,12,13)

# Removing arrest time from date
Pittsburgh_crime_stats$ARREST_DATE <- as.Date(Pittsburgh_crime_stats$ARREST_DATE, format="%m/%d/%Y")

#Getting day of weeek from date
Pittsburgh_crime_stats$ARREST_DAY <- weekdays(as.Date(Pittsburgh_crime_stats$ARREST_DATE))
```

#### We have now added the **ARREST_DATE, ARREST_DAY and ARREST_HOUR** columns to our dataset, which will be useful in our analysis. 

#### After the addition of the columns the data set looks like this now. 


```r
head(Pittsburgh_crime_stats)
```

```
##        PK AGE GENDER RACE         ARREST_TIME
## 1 1975272  42      F    B 2016-08-24T12:20:00
## 2 1974456  31      M    W 2016-08-03T14:55:00
## 3 1974466  63      F    B 2016-08-03T16:45:00
## 4 1974550  25      F    W 2016-08-05T02:36:00
## 5 1974596  25      M    B 2016-08-06T02:00:00
## 6 1974556  45      M    W 2016-08-15T13:30:00
##                                    ARREST_LOCATION
## 1        4700 Block Centre AV Pittsburgh, PA 15213
## 2 4200 Block Steubenville PKE Pittsburgh, PA 15205
## 3       900 Block Freeport RD Fox Chapel, PA 15238
## 4      Foreland ST & Cedar AV Pittsburgh, PA 15212
## 5        900 Block Woodlow ST Pittsburgh, PA 15205
## 6            600 Block 1st AV Pittsburgh, PA 15219
##                                                                                                                                                OFFENSES
## 1                                                                                                                                    3929 Retail Theft.
## 2                                                                                                          13(a)(16) Possession of Controlled Substance
## 3                                                                                                                                    3929 Retail Theft.
## 4                                                                                                    5503 Disorderly Conduct. / 5505 Public Drunkenness
## 5 2702 Aggravated Assault. / 2705 Recklessy Endangering Another Person. / 3925 Receiving Stolen Property. / 4304(a)(1) Endangering Welfare of Children.
## 6                                                                                                                                    3929 Retail Theft.
##                                  INCIDENT_LOCATION INCIDENT_NEIGHBORHOOD
## 1        4700 Block Centre AV Pittsburgh, PA 15213            Bloomfield
## 2 4200 Block Steubenville PKE Pittsburgh, PA 15205          Outside City
## 3       900 Block Freeport RD Fox Chapel, PA 15238              Westwood
## 4      Foreland ST & Cedar AV Pittsburgh, PA 15212        East Allegheny
## 5        900 Block Woodlow ST Pittsburgh, PA 15205       Crafton Heights
## 6      800 Block Hazelwood AV Pittsburgh, PA 15217            Greenfield
##   INCIDENT_ZONE INCIDENTTRACT COUNCIL_DISTRICT PUBLIC_WORKS_DIVISION       Lat
## 1             5           804                8                     2 -79.94928
## 2           OSC          5599               NA                    NA -80.08802
## 3             5          2811                9                     2 -79.89180
## 4             1          2304                1                     1 -80.00194
## 5             5          2814                2                     5 -80.05220
## 6             4          1517                5                     3 -79.92924
##       Long ARREST_DATE ARREST_HOUR ARREST_DAY
## 1 40.45255  2016-08-24          12  Wednesday
## 2 40.44014  2016-08-03          14  Wednesday
## 3 40.48662  2016-08-03          16  Wednesday
## 4 40.45408  2016-08-05          02     Friday
## 5 40.44590  2016-08-06          02   Saturday
## 6 40.41970  2016-08-15          13     Monday
```

#### We also plan not use the columns INCIDENTTRACT, COUNCIL_DISTRICT & PUBLIC_WORKS_DIVISON as these columns have a lot of NA's in them and we are not really sure how they will be useful to our analysis at this point in time.





## **EXPLORATORY DATA ANALYSIS**


### Which age group have commited most crime in the city of Pittburgh?

#### Let us first analyze the AGE column to undestand how the values are distributed.

```r
boxplot(Pittsburgh_crime_stats$AGE,main="Box Plot for Age", ylab="Criminals Age")
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### Box plot clearly indicates the **outliers** and the data point with a 999 years of Age. 
#### Now, let us try to figure to out the distribution of Ages among the criminals.


```r
barplot(table(Pittsburgh_crime_stats$AGE),main="Bar graph for Age",xlab="Criminals Age", ylab = "Frequency" )
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### The bar graph gets very busy from the age 14 and reaches 30 where it hits the peak, and slowly starts to fall off from there.

#### Let's have a clearer look using a histogram to understand the different age groups that are committing most crimes.


```r
hist(Pittsburgh_crime_stats$AGE, main="Histogram for Age", xlab="Criminals Age", 
     border="black", col="red",xlim=c(0,100),las=1, breaks=50)
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### Histogram clearly indicates that the age group of **20 to 40** is the most common among the criminals. 
#### We also note that we have a significant amount of people less than 20, about 5000 of them, who have commited crimes. These might include a lot of **juveniles (Age<18)** most probably, which is a cause of concern for any society. We will have a deeper analysis about this particular age group in the later sections of this report.



### Which gender has commited the most number of crimes in the city?

```r
barplot(table(Pittsburgh_crime_stats$GENDER),main = "Gender Distribution",xlab = "Gender", ylab="Number of Crimes")
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### It is quite evident that **Men** commit the maximum number of crimes when compared to any other gender. Women form almost a third of men commiting crimes, while representation from Others is quite low.

### People from which race have commited most number of crimes in the city of Pittburgh?

```r
barplot(table(Pittsburgh_crime_stats$RACE),main = "Race Distribution",xlab = "Race", ylab="Number of Crimes")
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### **Blacks and Whites** seem to form the majority of offenders. Blacks lead the race by a good margin when compared to the Whites. Other races seem to be under represented in our data, which might not be a bad sign for Pittsburgh after all.


### Which zones have seen the most number of incidents in the city of Pittburgh?


```r
crime.zone <- Pittsburgh_crime_stats %>%
  na.omit()   %>%
  group_by(INCIDENT_ZONE) %>% 
  count() %>% 
  arrange(-n) 
  
plot.crimezone <- ggplot(crime.zone, aes(x ="", y =n, fill=INCIDENT_ZONE )) + labs(x = NULL, y = NULL, fill = NULL, title = "Zonal Distribution of Incidents") +
  geom_bar(stat="identity", width=1,color="white") +coord_polar("y", start=0) +theme_void()
 plot.crimezone
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Reference for Pie Chart: https://www.r-bloggers.com/how-to-make-a-pie-chart-in-r/

#### From the pie chart, we can notice that the crime incidents are more **widespread**. Zones 1,2,3 and 5 seem to get a bit busy when compared to Zones 4 & 6. Thus, we can safely conlcude that the crimes were **not targeted** at a particular zone or location within Pittsburgh.

### Which neighborhoods have seen the most number of crimes in the city of Pittburgh?

#### Let's quickly create a table to see the different neighborhoods and how unsafe they've been with respect to the number of incidents.

```r
crime.loc <- sort(table(Pittsburgh_crime_stats$INCIDENT_NEIGHBORHOOD),decreasing = TRUE) 
crime.loc
```

```
## 
##   Central Business District            South Side Flats 
##                        2203                        1983 
##                                                 Carrick 
##                        1402                        1379 
##              East Allegheny              Homewood South 
##                        1278                        1174 
##              Homewood North                East Liberty 
##                        1059                        1023 
##          Marshall-Shadeland            Mount Washington 
##                         874                         859 
##                       Bluff                   Knoxville 
##                         810                         774 
##                   Hazelwood                   Beechview 
##                         723                         720 
##                   Brookline    Lincoln-Lemington-Belmar 
##                         718                         710 
##            Allegheny Center                    Sheraden 
##                         680                         670 
##                 Perry South            Brighton Heights 
##                         652                         626 
##                  East Hills                 Middle Hill 
##                         593                         580 
##                  Bloomfield                   Allentown 
##                         572                         556 
##                   Shadyside                    Fineview 
##                         555                         510 
##          Central North Side                 North Shore 
##                         504                         484 
##            Crawford-Roberts           South Side Slopes 
##                         478                         468 
##                     Larimer         Squirrel Hill South 
##                         461                         458 
##       Spring Hill-City View               Homewood West 
##                         437                         433 
##                 Perry North                    Garfield 
##                         412                         403 
##           Northview Heights                     Elliott 
##                         391                         359 
##           Bedford Dwellings                 Beltzhoover 
##                         352                         319 
##             Central Oakland             Crafton Heights 
##                         315                         304 
##               Highland Park                  Greenfield 
##                         293                         286 
##       Central Lawrenceville              Strip District 
##                         282                         282 
##                  Manchester                     Chateau 
##                         279                         272 
##                   Troy Hill               North Oakland 
##                         264                         230 
##                  Upper Hill                Point Breeze 
##                         220                         187 
##                West Oakland                Outside City 
##                         183                         180 
##               South Oakland                   Arlington 
##                         177                         172 
##                   Overbrook                 South Shore 
##                         169                         162 
##         Squirrel Hill North        California-Kirkbride 
##                         159                         158 
##                    West End         Lower Lawrenceville 
##                         158                         156 
##               Spring Garden                  Banksville 
##                         154                         144 
##             Terrace Village          Point Breeze North 
##                         143                         140 
##                     Bon Air             Stanton Heights 
##                         131                         121 
##                    Westwood                 Morningside 
##                         120                         115 
##         Upper Lawrenceville                  Friendship 
##                         115                         113 
##                      Esplen            Duquesne Heights 
##                         107                         103 
##               Lincoln Place           Arlington Heights 
##                         103                         102 
##                 Polish Hill                Mount Oliver 
##                          99                          90 
## Golden Triangle/Civic Arena              Allegheny West 
##                          83                          76 
##                  Glen Hazel                   Fairywood 
##                          70                          65 
##                        Hays                     Windgap 
##                          62                          60 
##                 Summer Hill                     Oakwood 
##                          46                          45 
##              Outside County               Outside State 
##                          39                          39 
##                   St. Clair               East Carnegie 
##                          32                          29 
##           Central Northside              Chartiers City 
##                          23                          23 
##               New Homestead              Swisshelm Park 
##                          23                          20 
##               Regent Square                   Ridgemont 
##                          19                          15 
##             Mt. Oliver Boro      Troy Hill-Herrs Island 
##                          12                           6 
##     Mt. Oliver Neighborhood 
##                           2
```

```r
barplot(crime.loc,main = "Distribution of crimes in Neighborhoods ",xlab = "Neighborhoods", ylab="Number of Crimes", las=0, cex.names= 0.6) 
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#### It's evident from the table and bar graph that Central Business District, South Side Flats, Carrick, East Allegheny &  Homewood South form the top 5 dangerous neighborhoods.
####  The safest ones were Regent Square, Ridgemont, Mt. Oliver Boro, Troy Hill-Herrs Island & Mt. Oliver Neighborhood with low crime rates.


### Which are the incidents that have been the most common?

```r
crime.incidents <- Pittsburgh_crime_stats %>% 
  na.omit() %>%
  group_by(OFFENSES) %>%
    count() %>% 
  arrange(-n) 

crime.incidents
```

```
## # A tibble: 9,062 x 2
## # Groups:   OFFENSES [9,062]
##    OFFENSES                                                                    n
##    <fct>                                                                   <int>
##  1 2701 Simple Assault.                                                     2205
##  2 9501 Bench Warrant                                                       1696
##  3 9015 Failure To Appear/Arrest on Attachment Order                        1585
##  4 13(a)(31) Marijuana: Possession Small Amount                              805
##  5 13(a)(32) Paraphernalia - Use or Possession                               740
##  6 13(a)(16) Possession of Controlled Substance / 13(a)(32) Paraphernalia~   582
##  7 2701(a)(1) Simple Assault - Intent., Know., Reckless.Cause Bod. Injury    557
##  8 13(a)(16) Possession of Controlled Substance                              491
##  9 3929 Retail Theft.                                                        400
## 10 2702 Aggravated Assault.                                                  396
## # ... with 9,052 more rows
```

#### Pittsburgh's most common type of crimes were **2701 Simple Assualt, 9501 Bench Warrant and 9015 Failure to Appear.** But, Bench Warrant & Failure to Appear cannot be categorized as "incident crimes" that is of interest to us. Leaving them aside, we could see **Marijuana, Paraphernalia  and Possesion of Controlled Subtances** in the list of popular crimes in Pittsburgh, indicating the possibility of a dangerous drug culture. 

### How have these Crimes changed over time?

```r
Crime.time <- Pittsburgh_crime_stats %>%
  count(ARREST_DATE)

plot.time <- ggplot(Crime.time, aes(x = ARREST_DATE, y = n)) +
  geom_smooth(color = "red") 
 plot.time
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

#### We can notice spikes in the 2017-2020 range suggesting that crimes have increased in the recent past.

### How has simple assault offences changed over time ?

```r
common_incidents <- Pittsburgh_crime_stats %>%
  filter(grepl("Simple Assault.", OFFENSES) == TRUE) %>%
  count(ARREST_DATE)

ggplot(data = common_incidents, aes(x = ARREST_DATE, y = n)) +
  geom_smooth()
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Reference for grepl:- https://awakeningdatascientist.wordpress.com/2015/07/20/r-of-the-day-grep-and-grepl/

#### There has been an increase in simple assualts starting from 2015.

### How has Marijuana offences changed over time ?

```r
mari_incidents <- Pittsburgh_crime_stats %>%
  filter(grepl("Marijuana", OFFENSES) == TRUE) %>%
  count(ARREST_DATE)

ggplot(data = mari_incidents, aes(x = ARREST_DATE, y = n)) +
  geom_smooth()
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Marijuana related incidents have been on the rise steadily.


## **GRAPHICAL DATA ANALYSIS**

### Heat Map of number of crimes in Pittsburgh based on time of the day and day of the week

```r
df_crimetime <-  Pittsburgh_crime_stats %>%
  mutate(ARREST_DAY = factor(ARREST_DAY,
                          levels =  c("Saturday", "Friday", "Thursday", "Wednesday",
                                      "Tuesday", "Monday", "Sunday")))
df_crimetime <- df_crimetime %>%
  na.omit %>%
  group_by(ARREST_DAY, ARREST_HOUR) %>%
  summarize(count = n())

crime.heat <- ggplot(df_crimetime, aes(x = ARREST_HOUR, y = ARREST_DAY, fill = count)) +
  geom_tile()+ labs(x = "Hour of Crime (Local Time)", y = "Day of the Week of Crime", 
                    title = "Number of Crimes by Time of Incident") + 
  scale_fill_gradient(low = "white", high = "red")

crime.heat
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

#### This heat map does a great job is telling us about the timing of the crimes in Pittsburgh.
#### Early hours on the weekend (00.30 and 02.30) seems to be a popular time for the incidents. 
#### On the Weekdays, we can notice a good bit of criminal activity from 15.30 to 20.30. Also, Thursday 17.00 hrs seems to be the most popular time for crimes in Pittsburgh in the weekdays.

### Heat Map of number of crimes in Pittsburgh based on time of the day and day of the week for incident location

```r
df_offense_location <-  Pittsburgh_crime_stats %>%
  mutate(ARREST_DAY = factor(ARREST_DAY,
                          levels =  c("Saturday", "Friday", "Thursday", "Wednesday",
                                      "Tuesday", "Monday", "Sunday")))

df_offense_location <- df_offense_location %>%
  na.omit %>%
  group_by(INCIDENT_ZONE,ARREST_DAY,ARREST_HOUR ) %>% 
  summarize(count = n())

crimelocation.heat <- ggplot(df_offense_location, aes(x = ARREST_HOUR, y = ARREST_DAY, fill = count)) +
  geom_tile()+ labs(x = "Hour of Crime (Local Time)", y = "Day of the Week of Crime", 
                    title = "Number of Crimes by Time of Incident per incident zone") + 
  scale_fill_gradient(low = "white", high = "red")+ 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
facet_wrap(~ INCIDENT_ZONE, nrow = 5)

crimelocation.heat
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

#### Zone 1,2 & 3 have good bit of crime activity on the weekdays between 12.00 to 20.00. 
#### Zone 3 is particularly active on the weekends especially around 00.30 and 02.30. This clearly suggests that better patrolling in these zones on weekday evenings and the late nights on weekends around Zone 3 areas could help curb some of these incidents in the future.


### Heat Map of number of crimes in Pittsburgh based on time of the day and day of the week for Gender

```r
df_offense_gender <-  Pittsburgh_crime_stats %>%
  mutate(ARREST_DAY = factor(ARREST_DAY,
                          levels =  c("Saturday", "Friday", "Thursday", "Wednesday",
                                      "Tuesday", "Monday", "Sunday")))

df_offense_gender <- df_offense_gender %>%
  na.omit %>%
  group_by(GENDER,ARREST_DAY,ARREST_HOUR ) %>% 
  summarize(count = n())

crimegender.heat <- ggplot(df_offense_gender, aes(x = ARREST_HOUR, y = ARREST_DAY, fill = count)) +
  geom_tile()+ labs(x = "Hour of Crime (Local Time)", y = "Day of Week of Crime", 
                    title = "Number of crimes by Time of incident per Gender") + 
  scale_fill_gradient(low = "white", high = "red")+ 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
facet_wrap(~ GENDER, nrow = 5)

crimegender.heat
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### Men & Women are active in commiting crimes on the weekends especially around 00.30 and 02.30 as well as on the weekdays between 12.00 to 20.00. Safe hours include the early mornings from 4.00 to 8.00, where crime activity is the lowest.

### Heat Map of number of crimes in Pittsburgh based on time of the day and day of the week for Race

```r
df_offense_race <-  Pittsburgh_crime_stats %>%
  mutate(ARREST_DAY = factor(ARREST_DAY,
                          levels =  c("Saturday", "Friday", "Thursday", "Wednesday",
                                      "Tuesday", "Monday", "Sunday")))

df_offense_race <- df_offense_race %>%
  na.omit %>%
  group_by(RACE,ARREST_DAY,ARREST_HOUR ) %>% 
  summarize(count = n())

crimerace.heat <- ggplot(df_offense_race, aes(x = ARREST_HOUR, y = ARREST_DAY, fill = count)) +
  geom_tile()+ labs(x = "Hour of Crime (Local Time)", y = "Day of Week of Crime", 
                    title = "Number of Crime by Time of Incident per Race") + 
  scale_fill_gradient(low = "white", high = "red")+ 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
facet_wrap(~ RACE, nrow = 5)

crimerace.heat
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#### Again, Blacks & Whites are active in commiting crimes on the weekends especially around 00.30 and 02.30 as well as on the weekdays between 12.00 to 20.00. Safe hours include the early mornings from 4.00 to 8.00, where crime activity is the lowest.



## **JUVENILE CRIME ANALYSIS - In-depth Look**

#### Juveniles are teenagers commiting crimes. By law, a person is treated as a Juvenile if he/she happens to be younger than 18 years of age at the time of the incident. Juvenile crimes are increasingly becoming a problem globally. Let us now dive into the juvenile criminal cases of Pittsburgh.

### Number of Juvenile Crimes

```r
juv.crimes <-  Pittsburgh_crime_stats %>%
  filter(AGE<18) %>%
  count()

juv.crimes
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1  2598
```

#### We have about 2598 juvenile criminals in our data set.

### Juvenile Crimes over time

```r
juv.crimestime <-  Pittsburgh_crime_stats %>%
  filter(AGE<18) %>%
  count(ARREST_DATE)
  ggplot(data = juv.crimestime, aes(x = ARREST_DATE, y = n)) +
  geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](PittsburghCrimeDataAnalysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

#### Eventhough there is a slight dip in numbers in 2019 compared 2017 and 2018, it is still a cause of concern for us. 

### Juveniles Gender Composition

```r
juv.gender <-  Pittsburgh_crime_stats %>%
  filter(grepl("M", GENDER) == TRUE) %>%
  filter(AGE<18) %>%
  count()
juv.gender
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1  1807
```
#### Juvenile Males form **70%** of all Juveniles while Females & others form the remaining 30%.

### Juveniles and Simple Assualts

```r
juv.simplecrimes <- Pittsburgh_crime_stats %>%
  filter(grepl("Simple", OFFENSES) == TRUE) %>%
  filter(AGE<18) %>%
  count()

juv.simplecrimes
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1   686
```
#### Juveniles have commited 686 Simple Assaults in Pittsburgh, which is a significant number considering the total was 2487. Juveniles have a **28%** representation in simple assualts overall.

### Juveniles and Marijuana 

```r
marijuana_incidents <- Pittsburgh_crime_stats %>%
  filter(grepl("Marijuana", OFFENSES) == TRUE) %>%
  filter(AGE<18) %>%
  count()

marijuana_incidents
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1   332
```
#### Juveniles have been caught in 332 Marijuana related cases in Pittsburgh. Juveniles have a **36%** representation in Marijuana related cases overall which is quite alarming.

### Juveniles and Substance Abuse

```r
 juv.substancecrimes <-  Pittsburgh_crime_stats %>%
  filter(grepl("Controlled Substance", OFFENSES) == TRUE) %>%
  filter(AGE<18) %>%
  count()

juv.substancecrimes
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1   149
```
#### Juveniles have been caught in 149 Substance abuse related cases in Pittsburgh. Juveniles have a **23%** representation in Substance Abuse related cases overall.

### Juveniles and Paraphernalia

```r
 juv.paraphernaliacrimes <-  Pittsburgh_crime_stats %>%
  filter(grepl("Paraphernalia", OFFENSES) == TRUE) %>%
  filter(AGE<18) %>%
  count()

juv.paraphernaliacrimes
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1    72
```
#### Juveniles have been arrested in 72 Paraphernalia  cases in the city. Juveniles have a **9%** representation in Paraphernalia related cases overall.

## **CONCLUSION & RECOMMENDATIONS**

### --> **DEMOGRAPHICS**

#### --> **AGE:** 
#### Analysis clearly indicates that the age group of 20 to 40 is the most common among the criminals. This piece of statistic was on the expected lines. What surprised us was that teenagers were also major offenders. We have discussed this in detail in the next section.
#### --> **GENDER:** 
#### Women representation is low as they only form a third of number of men committing crimes. This again did not come off as a surprise to us since it closely follows the pattern worldwide.
#### --> **RACE:** 
#### Blacks and Whites have commited more crimes than any other Race. This did interest us as we generally expect that there could be an equitable representation among the races but it turned out otherwise.

### --> **CRIME SPECIFICS**

#### --> **TIMING:** 
#### Early hours on the weekend (00.30 and 02.30) seems to be a popular time for the incidents. On the Weekdays, we can notice a good bit of criminal activity from 15.30 to 20.30. Safe hours include the early mornings from 4.00 to 8.00, where crime activity was the lowest. Our main recommendation here would be to provide better patrolling in the late hours on the weekends and day time patrolling during the weekdays, especially in the neighborhoods falling under Zones 1,2,3 & 5. This could go a long way in keeping atleast the organized crimes at bay.
#### --> **OFFENSES:** 
#### Pittsburgh's most common type of crimes were 2701 Simple Assualt, 9501 Bench Warrant and 9015 Failure to Appear. We could also see Marijuana, Paraphernalia  and Possesion of Controlled Subtances cases being caught in Pittsburgh in substantial numbers which is a dangerous sign for the city's image and could dent it's future growth and outlook. 
#### --> **LOCATION:** 
#### We find that "600 Block 1st AV Pittsburgh, PA 15219" is the most crime prone area. The most unsafe neighborhoods were Central Business District, South Side Flats, Carrick, East Allegheny &  Homewood South. From personal experience of living in the city, we could not agree more. These have somehow become the "hoods" of Pittsburgh for all kind of criminal activity,that the police, law & order agenices need to step up and take note.


### **JUVENILE CRIMES**

#### --> Juvenile criminal cases have been on the rise in the recent past. Eventhough, there has been a slight dip since 2019, the Juveniles have still commited about 2598 out of the 37219 crimes which translate to about 7% of all crimes commited. This is still a substantial amount that begs attention. Perhaps some sort of corrective measures and programmes like awareness campaigns, education about how not to fall prey to substance addiction, good parenting etc. are the need of the hour.

#### --> Most of the juvenile crimes have centred around Simple Assaults, Substance Abuse, Marijuana & Paraphernalia.  Their individual shares stood at 26%, 13%, 6% & 3% respectively. Overall, juveniles have a significant representation of close to 30% in these type of cases which is alarming.

#### --> Marijuana & Substance related addiction could be a reason for the Simple Assualts, Paraphernalia and Thefts that these juveniles might indulge in want of quick & easy cash. This is could be a correlation worth researching in detail with more data in the future.

#### **In conclusion, Crimes are a cause of concern for any society. Though a lot of external factors including social, political, and environmental factors strongly influence criminal behavior, the society as whole needs to realize, stand up and fight against this menace, with a special focus on Juvenile crimes, for a better tomorrow!** 


