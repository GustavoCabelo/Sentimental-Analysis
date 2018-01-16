#Library####
library(readr)
library(lattice)
library(ggplot2)
library(caret)
library(corrplot)
library(ggplot2)
library(magrittr)
library(rattle)
library(data.table)
library(dplyr)

#----------------- Set Path ------------------------------------------

path <- "D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Course 4 - Big Data Web Mining"

############# Loading files / Overview ############################
#     Source: crawl-data/CC-MAIN-2017-51 (0000 to 0100 paths)---- 
#     Large Matrix sentiment  
#---- iPhone ----
iPhoneOriginal <- read.csv(paste0(path,"/Task 3/1-Large Matrixes/iPhoneLargeMatrix.csv"), sep = ";")  
iPhone <- iPhoneOriginal

# Feature selection for relevant attributes for iPhone----
{
  a <- colnames(iPhone[,which(colnames(iPhone) %like% "iphone")]) #<--- vector with columns name that contain "iphone"
  b <- colnames(iPhone[,which(colnames(iPhone) %like% "ios")]) #<--- vector with columns name that contain "ios"
  
  
  iPhone <-  iPhone[colnames(iPhone[,c("id",a,b)])] #<--- Create Iphone subset filtered with attributes selected
  rm(a,b)
  
  iPhone <-iPhone[,c(1:11,13:16,12)] #<---- Reorder iPhoneSentiment = last
}

# Overview ----
length(iPhone) 
# 16
str(iPhone)
# nrows: 5426   
# dependent variable: $iphoneSentiment
# attr types: int
summary(iPhone)
# $iphoneSentiment: 16 NA's
# id           iphone         iphonecampos      iphonecamneg       iphonecamunc      iphonedispos    
# Min.   :   0   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.00000   Min.   : 0.0000   Min.   : 0.0000  
# 1st Qu.:1356   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.00000   1st Qu.: 0.0000   1st Qu.: 0.0000  
# Median :2712   Median : 1.0000   Median : 0.0000   Median : 0.00000   Median : 0.0000   Median : 0.0000  
# Mean   :2712   Mean   : 0.8277   Mean   : 0.1382   Mean   : 0.08791   Mean   : 0.0916   Mean   : 0.2759  
# 3rd Qu.:4069   3rd Qu.: 1.0000   3rd Qu.: 0.0000   3rd Qu.: 0.00000   3rd Qu.: 0.0000   3rd Qu.: 0.0000  
# Max.   :5425   Max.   :18.0000   Max.   :21.0000   Max.   :16.00000   Max.   :18.0000   Max.   :20.0000  
# 
# iphonedisneg      iphonedisunc      iphoneperpos      iphoneperneg          ios           iosperpos      
# Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000   Min.   :0.0000   Min.   : 0.0000  
# 1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:0.0000   1st Qu.: 0.0000  
# Median : 0.0000   Median : 0.0000   Median : 0.0000   Median : 0.0000   Median :0.0000   Median : 0.0000  
# Mean   : 0.2108   Mean   : 0.2228   Mean   : 0.3043   Mean   : 0.2584   Mean   :0.3691   Mean   : 0.1979  
# 3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.:1.0000   3rd Qu.: 0.0000  
# Max.   :33.0000   Max.   :38.0000   Max.   :65.0000   Max.   :34.0000   Max.   :8.0000   Max.   :39.0000  
# 
# iosperneg         iosperunc        iphoneperunc    iphoneSentiment   
# Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.000   Min.   :-271.000  
# 1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.:   0.000  
# Median : 0.0000   Median : 0.0000   Median : 0.000   Median :   0.000  
# Mean   : 0.1439   Mean   : 0.1368   Mean   : 0.239   Mean   :   2.429  
# 3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.: 0.000   3rd Qu.:   0.000  
# Max.   :26.0000   Max.   :32.0000   Max.   :42.000   Max.   : 352.000  
# NA's   :16  

histogram(iPhone$iphoneSentiment)
table(iPhone$iphoneSentiment)
# 4172 of the cases returning as "0" = 77,11%

# Correlation between attributes
{
  corrData <- cor(iPhone)
  corrData
  corrplot(corrData)
}

# Save clean subset----
# write.csv(iPhone, paste0(path,"/Task 3/1-Large Matrixes/iPhoneonly.csv"), row.names=FALSE)
iPhone <- read.csv(paste0(path,"/Task 3/1-Large Matrixes/iPhoneonly.csv"))


###################### Pre-process #############################
#### Remove NAs ####
#---- Backup NAs----
# IphoneNAs <- iPhone[which(is.na(iPhone$iphoneSentiment)),]
# saveRDS(IphoneNAs, paste0(path,"/Task 3/1-Large Matrixes/IphoneNAs.rds"))

#---- NAs Omit ----
iPhone <- na.omit(iPhone)

##### iphoneSentiment: Center and Scale #####
# Before Centered and Scaled:

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -271.000    0.000    0.000    2.429    0.000  352.000 

#------------------- Center -----------------
# After Centered:

# Min.      1st Qu.  Median    Mean 3rd Qu.    Max. 
# -200.00    0.00    0.00    2.43    0.00  200.00

#---- Set max as 200 ----
for(i in (ncol(iPhone))){
  iPhone[which(iPhone[,i] >= 200), i] = 200
}
#---- Set min as -200 ----
for(i in (ncol(iPhone))){
  iPhone[which(iPhone[,i] <= -200), i] = -200
}

#------------------- Scale -----------------
# After Scaled:

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   5.000   5.000   5.061   5.000  10.000

#---- iphoneSentiment set min as 0 ----
iPhone$iphoneSentiment <- iPhone$iphoneSentiment+(min(iPhone$iphoneSentiment)*-1)

#---- iphoneSentiment set max as 5 ----
Scale <- function(x){
  return(x/(max(x)*0.2)) #<---- divide the value for its maximum (column)
}

# Apply function
iPhone$iphoneSentiment <- Scale(iPhone$iphoneSentiment)

#---- iphoneSentiment round intervals (ceiling) for nearest 0.5 ----
iPhone$iphoneSentiment <- iPhone$iphoneSentiment * 2
iPhone$iphoneSentiment <- ceiling(iPhone$iphoneSentiment)
iPhone$iphoneSentiment <- iPhone$iphoneSentiment / 2
table(iPhone$iphoneSentiment)

##### iphoneSentiment: Remove Noise #####

#---- Positive sentiment: subset ---- 
iPhonepos <- iPhone %>%
  filter(iphoneSentiment > 2.5)

