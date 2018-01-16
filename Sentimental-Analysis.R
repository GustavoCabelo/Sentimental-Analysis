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

#----------------- Set Path ------------------------------------------

path <- "D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Course 4 - Big Data Web Mining"

############# Loading files / Overview ############################
#     Source: crawl-data/CC-MAIN-2017-51 (0000 to 0100 paths)---- 
#     Large Matrix sentiment  
#---- iPhone ----
iPhone <- read.csv(paste0(path,"/Task 3/1-Large Matrixes/iPhoneLargeMatrix.csv"), sep = ";")  

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

# Save clean subset----
#write.csv(iPhone, paste0(path,"/Task 3/1-Large Matrixes/iPhoneonly.csv"))
iPhone <- read.csv(paste0(path,"/Task 3/1-Large Matrixes/iPhoneonly.csv"))
iPhone$X <- NULL
#---- Galaxy ----
Galaxy <- read.csv(paste0(path,"/Task 3/1-Large Matrixes/GalaxyLargeMatrix.csv"), sep = ";")  

# Feature selection for relevant attributes for Galaxy----
{
  a <- colnames(Galaxy[,which(colnames(Galaxy) %like% "samsung")]) #<--- vector with columns name that contain "Galaxy"
  b <- colnames(Galaxy[,which(colnames(Galaxy) %like% "google")]) #<--- vector with columns name that contain "ios"
  c <- colnames(Galaxy[,which(colnames(Galaxy) %like% "galaxy")]) #<--- vector with columns name that contain "ios"
  
  
  Galaxy <-  Galaxy[colnames(Galaxy[,c("id",a,b,"galaxySentiment")])] #<--- Create Galaxy subset filtered with attributes selected
  rm(a,b,c)
  
}

# Overview ----
length(Galaxy) # 16
str(Galaxy)
# nrows: 5426
# dependent variable: $galaxySentiment
# attr types: int
summary(Galaxy)
# $iphoneSentiment: 16 NA's

# id       samsunggalaxy      samsungcampos      samsungcamneg      samsungcamunc      samsungdispos    
# Min.   :   0   Min.   : 0.00000   Min.   : 0.00000   Min.   : 0.00000   Min.   : 0.00000   Min.   : 0.0000  
# 1st Qu.:1356   1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.: 0.0000  
# Median :2712   Median : 0.00000   Median : 0.00000   Median : 0.00000   Median : 0.00000   Median : 0.0000  
# Mean   :2712   Mean   : 0.08994   Mean   : 0.07667   Mean   : 0.02709   Mean   : 0.02635   Mean   : 0.1476  
# 3rd Qu.:4069   3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.: 0.0000  
# Max.   :5425   Max.   :11.00000   Max.   :16.00000   Max.   :16.00000   Max.   :14.00000   Max.   :17.0000  
# 
# samsungdisneg      samsungdisunc      samsungperpos     samsungperneg     samsungperunc      googleandroid    
# Min.   : 0.00000   Min.   : 0.00000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.00000   Min.   :0.00000  
# 1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.00000   1st Qu.:0.00000  
# Median : 0.00000   Median : 0.00000   Median : 0.0000   Median : 0.0000   Median : 0.00000   Median :0.00000  
# Mean   : 0.07354   Mean   : 0.05547   Mean   : 0.1574   Mean   : 0.1052   Mean   : 0.08883   Mean   :0.08367  
# 3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.: 0.00000   3rd Qu.:0.00000  
# Max.   :18.00000   Max.   :15.00000   Max.   :34.0000   Max.   :23.0000   Max.   :26.00000   Max.   :8.00000  
# 
# googleperpos      googleperneg       googleperunc      galaxySentiment  
# Min.   : 0.0000   Min.   : 0.00000   Min.   : 0.00000   Min.   :-81.000  
# 1st Qu.: 0.0000   1st Qu.: 0.00000   1st Qu.: 0.00000   1st Qu.:  0.000  
# Median : 0.0000   Median : 0.00000   Median : 0.00000   Median :  0.000  
# Mean   : 0.1576   Mean   : 0.09307   Mean   : 0.08238   Mean   :  2.242  
# 3rd Qu.: 0.0000   3rd Qu.: 0.00000   3rd Qu.: 0.00000   3rd Qu.:  0.000  
# Max.   :23.0000   Max.   :23.00000   Max.   :21.00000   Max.   :207.000  
# NA's   :16     
# Save clean subset----
write.csv(Galaxy, paste0(path,"/Task 3/1-Large Matrixes/Galaxyonly.csv"))

############# Pre-process ########################################
#-----------> Remove NAs----
#---- Backup NAs----
# IphoneNAs <- iPhone[which(is.na(iPhone$iphoneSentiment)),]
# saveRDS(IphoneNAs, paste0(path,"/Task 3/1-Large Matrixes/IphoneNAs.rds"))
#---- NAs Omit ----
iPhone <- na.omit(iPhone)


#--------------- Scale iphoneSentiment from 0 to Max() ------
iPhone$iphoneSentiment <- iPhone$iphoneSentiment+(min(iPhone$iphoneSentiment)*-1)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   271.0   271.0   273.4   271.0   623.0 

#--------------- Scale rating ranges from 1 to 10 ------
Scale <- function(x) {
  return(x/(max(x))) #<---- divide the value for its maximum (column)
}

#--------------- Normalize Waps intensity  ------
iPhone$iphoneSentiment <- apply(iPhone[,(ncol(iPhone))], 2, FUN = "Scale") #<----- normalize by ROW to weight higher the max values by row

#Finding Correlation between attributes
{
  
  corrData <- cor(iPhone)
  corrData
  corrplot(corrData)
}
## Cross Validation method set
CrossValidation <- trainControl(method = "repeatedcv", number = 4, repeats = 1)
