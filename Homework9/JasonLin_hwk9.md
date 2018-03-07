---
title: "Homework9"
author: "Jason Lin"
date: "March 5, 2018"
output: 
      html_document:
          keep_md: TRUE
---
#Question 1

```r
fileurl <- "http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1"
library(XML)
doc <- htmlTreeParse(fileurl,useInternal=TRUE)
cast <- xpathSApply(doc,"//span[@class='itemprop']",xmlValue)
Character <- xpathSApply(doc,"//td[@class='character']",xmlValue)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Character <- gsub("\n              \n            ","",Character)
Character <- gsub(" \n                      \n\n              \n          ","",Character)
Character <- gsub("  \n","",Character)
Character <- gsub(" \n","",Character)
Character <- gsub("\n ","",Character)
Character <- gsub(" /            "," ",Character)
Character <- gsub("Griphook Professor Filius Flitwick","Griphook Professor / Filius Flitwick",Character)
dfrole <- as.data.frame(Character)
dfcast <-as.data.frame(cast)
dfcombined <- cbind(dfcast,dfrole)
library(tidyr)
dfcombinedfinal=extract(dfcombined, cast, c("FirstName", "Surname"), "([^.*]+) (.*)")
head(dfcombinedfinal,10)
```

```
##    FirstName   Surname                            Character
## 1      Ralph   Fiennes                       Lord Voldemort
## 2    Michael    Gambon           Professor Albus Dumbledore
## 3       Alan   Rickman              Professor Severus Snape
## 4     Daniel Radcliffe                         Harry Potter
## 5     Rupert     Grint                          Ron Weasley
## 6       Emma    Watson                     Hermione Granger
## 7     Evanna     Lynch                        Luna Lovegood
## 8   Domhnall   Gleeson                         Bill Weasley
## 9   Clémence     Poésy                       Fleur Delacour
## 10   Warwick     Davis Griphook Professor / Filius Flitwick
```

#Question 2                    

```r
fileurl2 <- "http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs"
library(XML)
doc2 <- htmlTreeParse(fileurl2,useInternal=TRUE)
player <- xpathSApply(doc2,"//tr['PLAYER']",xmlValue)
playershoot<-player[-1:-20]
rawstats <- playershoot[c(-1,-2,-20)]
name <- substring(rawstats,1,17)
name2 <- gsub("(, ..|, ...|, ....|, .....|, ......|, .......|,)","",name)
test1 <- substring(rawstats,10,25)
pos <- gsub("^.*,","",test1)
pos2 <-substring(pos,1,3)
pos3 <-gsub("C.","C",pos2)
test4<-gsub('[[:alpha:],]+', '', rawstats)
fgm <- substr(test4,1,5)
fga <- substr(test4,6,9)
fga2 <- gsub("\\.$","",fga)
FG_pct <- substr(test4,9,13)
FG_pct2 <- gsub("^\\d","",FG_pct)
FG_pct3 <- substr(FG_pct2, 1, 4)
raw3pm <- substr(test4,13,16)
raw3pm1 <- gsub("^00","0",raw3pm)
raw3pm1 <- gsub("^81","1",raw3pm1)
final3pm <- substr(raw3pm1,1,3)
final3pm
```

```
##  [1] "0.4" "1.2" "0.7" "0.7" "1.8" "1.0" "1.8" "0.1" "0.2" "0.1" "1.2"
## [12] "1.3" "0.0" "0.3" "0.3" "0.0" "0.0"
```

```r
raw3pa <- substr(test4,16,19)
raw3pa1 <- gsub("^41","1",raw3pa)
raw3pa1 <- gsub("^23","3",raw3pa1)
final3pa <- substr(raw3pa1,1,3)
raw3pct <- substr(test4,19,23)
raw3pct2 <- gsub("^\\d","",raw3pct)
final3pct <- substr(raw3pct2,1,4)
rawftm <- substr(test4,23,27)
rawftm1 <- gsub("14","4",rawftm)
rawftm1 <- gsub("43","3",rawftm1)
finalftm <- substr(rawftm1,1,3)
rawfta <- substr(test4,26,29)
rawfta1 <- gsub("35","5",rawfta)
rawfta1 <- gsub("44","4",rawfta1)
finalfta <- substr(rawfta1,1,3)
rawftpct <- substr(test4,29,33)
rawftpct1 <- gsub("20","0",rawftpct)
finalftpct <- substr(rawftpct1,1,4)
raw2pm <- substr(test4,33,36)
raw2pm1 <- gsub("48","8",raw2pm)
raw2pm1 <- gsub("24","4",raw2pm1)
finalraw2pm <- substr(raw2pm1,1,3)
raw2pa <- substr(test4,36,40)
raw2pa1 <- gsub("316","16",raw2pa)
raw2pa1 <- gsub("68","8",raw2pa1)
final2pa <- gsub("\\.$|\\..$","",raw2pa1)
final2pa<-gsub("16","16.2",final2pa)
raw2pct <- substr(test4,39,44)
raw2pct1 <- gsub(".2","",raw2pct)
raw2pct1 <- gsub("^4","",raw2pct1)
final2pct <- substr(raw2pct1,1,4)
rawpps <- substr(test4,43,49)
rawpps1 <- gsub("^14","",rawpps)
rawpps1<-gsub("^9","",rawpps1)
rawpps1<-gsub("\\.$|0\\..$","",rawpps1)
finalpps <- substr(rawpps1,1,5)
rawafg <- substr(test4,47,54)
finalafg <- gsub("^..\\.|^...\\.|^....\\.","0.",rawafg)
fgm<-as.numeric(fgm)
fga2<-as.numeric(fga2)
FG_pct3<-as.numeric(FG_pct3)
final3pm<-as.numeric(final3pm)
final3pa<-as.numeric(final3pa)
final3pct<-as.numeric(final3pct)
finalftm<-as.numeric(finalftm)
finalfta<-as.numeric(finalfta)
finalftpct<-as.numeric(finalftpct)
finalraw2pm<-as.numeric(finalraw2pm)
final2pa<-as.numeric(final2pa)
final2pct<-as.numeric(final2pct)
finalpps<-as.numeric(finalpps)
finalafg<-as.numeric(finalafg)
shootingstats <- data.frame(name2, pos3, fgm, fga2,  FG_pct3, final3pm,final3pa,final3pct,finalftm,finalfta,finalftpct,finalraw2pm,final2pa,final2pct,finalpps,finalafg)
names(shootingstats)<-c("Name","Position","FGM","FGA","FGpct","PM3","PA3","Ppct3","FTM","FTA","FTpct","PM2","PA2","Ppct2","PPS","AFGpct")
shootingstats
```

```
##                 Name Position FGM  FGA FGpct PM3 PA3 Ppct3 FTM FTA FTpct
## 1  LaMarcus Aldridge       PF 8.7 17.5 0.500 0.4 1.3 0.321 4.3 5.2  0.84
## 2      Kawhi Leonard       SF 5.8 12.3 0.468 1.2 3.9 0.314 3.4 4.2  0.82
## 3           Rudy Gay       SF 4.3  9.1 0.470 0.7 2.1 0.333 2.1 2.7  0.78
## 4          Pau Gasol        C 4.0  8.6 0.460 0.7 1.7 0.382 2.2 2.8  0.76
## 5        Patty Mills       PG 3.3  8.0 0.420 1.8 4.8 0.379 1.2 1.4  0.88
## 6      Manu Ginobili       SG 3.3  7.6 0.437 1.0 3.2 0.327 1.6 1.9  0.86
## 7        Danny Green       SG 3.3  8.1 0.409 1.8 4.6 0.385 0.7 0.9  0.78
## 8        Tony Parker       PG 3.8  7.9 0.483 0.1 0.7 0.192 1.0 1.5  0.67
## 9      Kyle Anderson       SF 3.2  6.1 0.523 0.2 0.7 0.297 1.6 2.1  0.74
## 10  Dejounte Murray        PG 3.1  7.1 0.439 0.1 0.3 0.238 1.2 1.6  0.73
## 11       Bryn Forbes       SG 2.8  6.7 0.414 1.2 3.2 0.376 0.5 0.9  0.61
## 12     Davis Bertans        C 2.3  5.1 0.443 1.3 3.5 0.367 0.6 0.8  0.80
## 13 Joffrey Lauvergne        C 2.0  3.8 0.531 0.0 0.1 0.000 0.7 1.0  0.65
## 14     Derrick White       PG 0.8  1.6 0.480 0.3 0.5 0.500 0.8 1.1  0.67
## 15      Brandon Paul       SG 0.9  2.1 0.440 0.3 0.9 0.286 0.4 0.6  0.61
## 16  Darrun Hilliard        SG 0.4  1.4 0.263 0.0 0.4 0.000 0.4 0.5  0.86
## 17     Matt Costello       SF 0.3  1.0 0.333 0.0 0.0 0.000 0.0 0.0  0.00
##    PM2  PA2 Ppct2   PPS AFGpct
## 1  8.3 16.2 0.514 1.272   0.51
## 2  4.6  8.4 0.539 1.315   0.52
## 3  3.6  7.0 0.511 1.246   0.51
## 4  3.3  6.9 0.479 1.247   0.50
## 5  1.5  3.2 0.485 1.222   0.54
## 6  2.3  4.4 0.516 1.223   0.51
## 7  1.5  3.5 0.440 1.119   0.52
## 8  3.7  7.2 0.511 1.110   0.49
## 9  3.0  5.4 0.551 1.336   0.54
## 10 3.0  6.8 0.449 1.056   0.45
## 11 1.6  3.5 0.448 1.087   0.50
## 12 1.0  1.6 0.611 1.259   0.57
## 13 2.0  3.7 0.548 1.235   0.53
## 14 0.5  1.1 0.471 1.600   0.56
## 15 0.6  1.2 0.567 1.193   0.50
## 16 0.4  1.0 0.385 0.842   0.26
## 17 0.3  1.0 0.333 0.667   0.33
```

```r
str(shootingstats)
```

```
## 'data.frame':	17 obs. of  16 variables:
##  $ Name    : Factor w/ 17 levels "Brandon Paul",..: 11 9 16 15 14 12 3 17 10 6 ...
##  $ Position: Factor w/ 5 levels " C"," PF"," PG",..: 2 4 4 1 3 5 5 3 4 3 ...
##  $ FGM     : num  8.7 5.8 4.3 4 3.3 3.3 3.3 3.8 3.2 3.1 ...
##  $ FGA     : num  17.5 12.3 9.1 8.6 8 7.6 8.1 7.9 6.1 7.1 ...
##  $ FGpct   : num  0.5 0.468 0.47 0.46 0.42 0.437 0.409 0.483 0.523 0.439 ...
##  $ PM3     : num  0.4 1.2 0.7 0.7 1.8 1 1.8 0.1 0.2 0.1 ...
##  $ PA3     : num  1.3 3.9 2.1 1.7 4.8 3.2 4.6 0.7 0.7 0.3 ...
##  $ Ppct3   : num  0.321 0.314 0.333 0.382 0.379 0.327 0.385 0.192 0.297 0.238 ...
##  $ FTM     : num  4.3 3.4 2.1 2.2 1.2 1.6 0.7 1 1.6 1.2 ...
##  $ FTA     : num  5.2 4.2 2.7 2.8 1.4 1.9 0.9 1.5 2.1 1.6 ...
##  $ FTpct   : num  0.84 0.82 0.78 0.76 0.88 0.86 0.78 0.67 0.74 0.73 ...
##  $ PM2     : num  8.3 4.6 3.6 3.3 1.5 2.3 1.5 3.7 3 3 ...
##  $ PA2     : num  16.2 8.4 7 6.9 3.2 4.4 3.5 7.2 5.4 6.8 ...
##  $ Ppct2   : num  0.514 0.539 0.511 0.479 0.485 0.516 0.44 0.511 0.551 0.449 ...
##  $ PPS     : num  1.27 1.31 1.25 1.25 1.22 ...
##  $ AFGpct  : num  0.51 0.52 0.51 0.5 0.54 0.51 0.52 0.49 0.54 0.45 ...
```

#Bar Graph of Field Goals Percentage per Game by Players                 

```r
library(ggplot2)

ggplot(shootingstats,aes(Name,FGpct, fill=Position))+
    geom_bar(stat="identity")+
    theme_minimal()+ggtitle("Field Goals Percentage per Game by Player")+xlab("Players")+ylab("Field Goals Percentage per Game")+theme(plot.title = element_text(hjust = 0.5,size=22), axis.text.x=element_text(angle=90,hjust=1.25,vjust=0.2,size=10))
```

![](JasonLin_hwk9_files/figure-html/barplot-1.png)<!-- -->
