The impact of weather events on healt and economy
=====================================================================
Synopsis
=======================
The aim of the report is to explore the NOAA Storm Database and answer some basic questions about severe weather events. The main goal is to identifz the events which are most harmful with respect to population health and which have the greatest economic consequences.


Data Processing
=======================
Loading and Processing the Raw Data

The data used for the report, comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA). The database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

Reading in all data from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2


```r
#downloading zip file  
url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
file='repdata%2Fdata%2FStormData.csv'
if (!file.exists(file)){
  url
  download.file(url,file)
}  
#reading data
DataIn<-read.csv(file)
```

Dataset contains 37 variables, however only healt and economy related variables will be needed for further research. Only those variables will be selected.

```r
DataIn<-DataIn[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
```

Summary for the dataset:

```r
head(DataIn)
```

```
##             BGN_DATE  EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP
## 1  4/18/1950 0:00:00 TORNADO          0       15    25.0          K
## 2  4/18/1950 0:00:00 TORNADO          0        0     2.5          K
## 3  2/20/1951 0:00:00 TORNADO          0        2    25.0          K
## 4   6/8/1951 0:00:00 TORNADO          0        2     2.5          K
## 5 11/15/1951 0:00:00 TORNADO          0        2     2.5          K
## 6 11/15/1951 0:00:00 TORNADO          0        6     2.5          K
##   CROPDMG CROPDMGEXP
## 1       0           
## 2       0           
## 3       0           
## 4       0           
## 5       0           
## 6       0
```

```r
summary(DataIn)
```

```
##               BGN_DATE                    EVTYPE         FATALITIES      
##  5/25/2011 0:00:00:  1202   HAIL             :288661   Min.   :  0.0000  
##  4/27/2011 0:00:00:  1193   TSTM WIND        :219940   1st Qu.:  0.0000  
##  6/9/2011 0:00:00 :  1030   THUNDERSTORM WIND: 82563   Median :  0.0000  
##  5/30/2004 0:00:00:  1016   TORNADO          : 60652   Mean   :  0.0168  
##  4/4/2011 0:00:00 :  1009   FLASH FLOOD      : 54277   3rd Qu.:  0.0000  
##  4/2/2006 0:00:00 :   981   FLOOD            : 25326   Max.   :583.0000  
##  (Other)          :895866   (Other)          :170878                     
##     INJURIES            PROPDMG          PROPDMGEXP        CROPDMG       
##  Min.   :   0.0000   Min.   :   0.00          :465934   Min.   :  0.000  
##  1st Qu.:   0.0000   1st Qu.:   0.00   K      :424665   1st Qu.:  0.000  
##  Median :   0.0000   Median :   0.00   M      : 11330   Median :  0.000  
##  Mean   :   0.1557   Mean   :  12.06   0      :   216   Mean   :  1.527  
##  3rd Qu.:   0.0000   3rd Qu.:   0.50   B      :    40   3rd Qu.:  0.000  
##  Max.   :1700.0000   Max.   :5000.00   5      :    28   Max.   :990.000  
##                                        (Other):    84                    
##    CROPDMGEXP    
##         :618413  
##  K      :281832  
##  M      :  1994  
##  k      :    21  
##  0      :    19  
##  B      :     9  
##  (Other):     9
```
PROPDMGEXP and CROPDMGEXP needed to be recoded to create new variables witch the value of PROPDMG and CROPDMG

```r
unique(DataIn$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(DataIn$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
DataIn$EXPPROP[DataIn$PROPDMGEXP=='H'|DataIn$PROPDMGEXP=='h'||DataIn$PROPDMGEXP=='2']<-100
DataIn$EXPPROP[DataIn$PROPDMGEXP=='K'|DataIn$PROPDMGEXP=='k'|DataIn$PROPDMGEXP=='3']<-1000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='4']<-10000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='5']<-100000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='M'|DataIn$PROPDMGEXP=='m'|DataIn$PROPDMGEXP=='6']<-1000000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='7']<-10000000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='8']<-10000000
DataIn$EXPPROP[DataIn$PROPDMGEXP=='B']<-1000000000
DataIn$EXPPROP[DataIn$PROPDMGEXP==''|DataIn$PROPDMGEXP=='0']<-1
DataIn$EXPPROP[DataIn$PROPDMGEXP=='-'|DataIn$PROPDMGEXP=='?'|DataIn$PROPDMGEXP=='+']<-0

DataIn$PROPDMG2<-DataIn$PROPDMG*DataIn$EXPPROP


DataIn$EXPCROP[DataIn$CROPDMGEXP=='?']<-0
DataIn$EXPCROP[DataIn$CROPDMGEXP=='0']<-1
DataIn$EXPCROP[DataIn$CROPDMGEXP=='2']<-100
DataIn$EXPCROP[DataIn$CROPDMGEXP=='K'|DataIn$CROPDMGEXP=='k']<-1000
DataIn$EXPCROP[DataIn$CROPDMGEXP=='M'|DataIn$CROPDMGEXP=='m']<-1000000
DataIn$EXPCROP[DataIn$CROPDMGEXP=='B']<-1000000000

DataIn$CROPDMG2<-DataIn$CROPDMG*DataIn$EXPCROP
```


EVENTS WITH HIGEST INJURIES AND FATALITIES
==========================================


TOTALS WERE CALCULATED AND THE TOP 10 EVENTS WERE CHOSEN

```r
FATALITIESAgg<-aggregate(FATALITIES~EVTYPE,data=DataIn,sum)
Top10Fatalities<-FATALITIESAgg[order(-FATALITIESAgg$FATALITIES),][1:10,]

INJURIESAgg<-aggregate(INJURIES~EVTYPE,data=DataIn,sum)
Top10Injuries<-INJURIESAgg[order(-INJURIESAgg$INJURIES),][1:10,]
```

THE TOP 10 EVENTS WITH HIGHEST INJURIES AND FATALITIES

```r
Top10Injuries
```

```
##                EVTYPE INJURIES
## 834           TORNADO    91346
## 856         TSTM WIND     6957
## 170             FLOOD     6789
## 130    EXCESSIVE HEAT     6525
## 464         LIGHTNING     5230
## 275              HEAT     2100
## 427         ICE STORM     1975
## 153       FLASH FLOOD     1777
## 760 THUNDERSTORM WIND     1488
## 244              HAIL     1361
```

```r
Top10Fatalities
```

```
##             EVTYPE FATALITIES
## 834        TORNADO       5633
## 130 EXCESSIVE HEAT       1903
## 153    FLASH FLOOD        978
## 275           HEAT        937
## 464      LIGHTNING        816
## 856      TSTM WIND        504
## 170          FLOOD        470
## 585    RIP CURRENT        368
## 359      HIGH WIND        248
## 19       AVALANCHE        224
```

```r
barplot(Top10Injuries$INJURIES, names.arg=Top10Injuries$EVTYPE,main="EVENTS WITH HIGHEST INJURIES ")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
barplot(Top10Fatalities$FATALITIES, names.arg=Top10Fatalities$EVTYPE,main="EVENTS WITH HIGEST FATALITIES ")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)


EVENTS WITH HIGEST IMPACT ON ECONOMY
==========================================


TOTALS WERE CALCULATED AND THE TOP 10 EVENTS WERE CHOSEN


Total damages for each weather Events.

```r
CropDmgAgg<-aggregate(CROPDMG2~EVTYPE,data=DataIn,sum)
Top10CropDmg<-CropDmgAgg[order(-CropDmgAgg$CROPDMG2),][1:10,]

PropDmgAgg<-aggregate(PROPDMG2~EVTYPE,data=DataIn,sum)
Top10PropDmg<-PropDmgAgg[order(-PropDmgAgg$PROPDMG2),][1:10,]
```



THE TOP 10 EVENTS WITH HIGHEST PROPERTY AND CROP DAMAGES


```r
Top10PropDmg
```

```
##                EVTYPE     PROPDMG2
## 170             FLOOD 144657709807
## 411 HURRICANE/TYPHOON  69305840000
## 834           TORNADO  56947380617
## 670       STORM SURGE  43323536000
## 153       FLASH FLOOD  16822673979
## 244              HAIL  15735267013
## 402         HURRICANE  11868319010
## 848    TROPICAL STORM   7703890550
## 972      WINTER STORM   6688497251
## 359         HIGH WIND   5270046260
```

```r
Top10CropDmg
```

```
##               EVTYPE    CROPDMG2
## 16           DROUGHT 13972566000
## 35             FLOOD  5661968450
## 99       RIVER FLOOD  5029459000
## 86         ICE STORM  5022113500
## 53              HAIL  3025954470
## 78         HURRICANE  2741910000
## 83 HURRICANE/TYPHOON  2607872800
## 30       FLASH FLOOD  1421317100
## 26      EXTREME COLD  1292973000
## 47      FROST/FREEZE  1094086000
```

```r
par(mfrow = c(2, 1))

barplot(Top10PropDmg$PROPDMG2, names.arg=Top10PropDmg$EVTYPE,main="EVENTS WITH THE HIGHEST PROPERTY DAMAGES ")
barplot(Top10CropDmg$CROPDMG2, names.arg=Top10CropDmg$EVTYPE,main="EVENTS WITH THE HIGHEST CROP DAMAGES ")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
RESULTS
=======

Tornados were the top reason for fatalities as well for injuries.

The highest property damage was caused by and Drought caused the highest crop damage.


