#summary(IWT_sample)
#summary(TFX_sample)

#summary(IWT)
#summary(Best.Western.Premium.Schwarzwald.1..Halbjahr.2019.Zusammenfassung)

library(tidyr)
library(dplyr)
library(fasttime)
#install.packages("lubridate")
library(lubridate)
library(ggplot2)
#install.packages("Microsoft365R")
library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)

#Retrieving all the CSV data from Sharepoint:
#Clean Token:
AzureAuth::clean_token_directory()
# Set the site and retrieve the link names
site <- get_sharepoint_site(site_url = "https://mssdconcept.sharepoint.com/sites/TeamMannheimBusinessSchool")
url <- "General/04_Data & Analysis/01_Data/Hotel am Kurpark_2981/"
drv <- site$get_drive()
links <- drv$list_items(url)
links <- links$name

# Create the correct path by adding the link names to the original path
list = c()
for (i in links){
  d =paste(url,i, sep ="")
  list = c(list,d)}
list <- list[grepl("csv", list)]
list


# Bulk download all the files from the Folder 
for (i in 1:length(list)){
  drv$download_file(list[i])
}


############################ Christians Area ##########################

## Merge Code ##

library(tidyverse)
#install.packages("here")
library(here)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(tidyr)
#install.packages("stringr")
library("stringr")

#import all files from working directory -> IMPORTANT TO SET WORKING DIRECTORY

getwd()
setwd("C:/Users/chris/Documents/Master/00_BAMP/Energy Daten Hotel Marburger Hof/Testdata")
getwd()

data_frame_names <- list.files(pattern = "*.csv")       # Get all file names
data_frame_names 
data_frame_list <- lapply(data_frame_names, read.csv2)  # Read all data frames
#data_frame_list #uncomment to see all files (takes a long time!)

#length(data_frame_list)

#data_frame_list[1]

dfcs <- data.frame()

#for loop transforms each file so there is one column with time stamp
# (existing), one with the measured KPI and one with the measured value
# All are saved in one data frame with this structure of three columns
i= 1
for (i in 1:length(data_frame_list)){
  
  var <- pivot_longer(as.data.frame(data_frame_list[i]) %>%
                        mutate(across(c(!"Zeit"), as.character)), cols = c(!"Zeit"), 
                      names_to = "Descr", values_to = "Value")
  
  dfcs <- rbind(dfcs, var)
  
}

#Exchange all occurences of .occ with .all.occ to prevent mismatch of columns 
#when separatig
dfcs$Descr <- str_replace_all(dfcs$Descr, ".Occ", ".all.Occ")

# separate ex-columntitles so different information is available in 
# several columns (i.e. room, roomType, ...)
dfcs <- dfcs %>% 
  separate(Descr, c("HotelID", "Room", "RoomType", "KPI"))

#head(dfcs)

#Split Occ from rest, since this is independent of room Type
dfcs_occ <- dfcs[dfcs$KPI == "Occ",]
dfcs_heat <- dfcs[dfcs$KPI != "Occ",]

#Long to wide Format for each df
dfcs_heat_wide2 <- spread(dfcs_heat, key = "KPI", value = "Value")
dfcs_occ_wide2 <- spread(dfcs_occ, key = "KPI", value = "Value")


#saving as csv --> CHANGE PATH anf FILE NAME!!!
write.table(dfcs_heat_wide2, "C:/Users/chris/Documents/Master/00_BAMP/Energy Daten Hotel Marburger Hof/heatingdata.txt")
write.table(dfcs_occ_wide2, "C:/Users/chris/Documents/Master/00_BAMP/Energy Daten Hotel Marburger Hof/occdata.txt")
#summary(dfcs)


## Merge Code ##


library(dplyr)

bw_heating <- Best.Western.Premium.Schwarzwald.1..Halbjahr.2019.Zusammenfassung

#Analysis of NAs in Occ
filtered <- filter(bw_heating, RoomType == "Occ")
sum(is.na(filtered$Value))
summary(as.factor(filtered$Value))

#Remove Rows where Value is NA (-> Occ is only removed if value if NA, compared to 
# just na.omit where Occ would always be removed since column KPI is NA)
bw_heating <- bw_heating[!(is.na(bw_heating$Value)),]

#Select subset for performance
bw_heating_sample <- bw_heating[500000:600000,]

#Correct mistake from merge-file: Occupancy in wrong column
i = 0
for (i in 1:nrow(bw_heating_sample)){
  ifelse(bw_heating_sample$RoomType[i] == "Occ", bw_heating_sample$KPI2[i] <- bw_heating_sample$RoomType[i], bw_heating_sample$KPI2[i] <- bw_heating_sample$KPI[i])
}

#drop old KPI column
bw_heating_sample <- bw_heating_sample %>% select(-KPI)

## Specify the Data Type for Heatig correctly:
bw_heating_sample$Zeit <- fastPOSIXct(bw_heating_sample$Zeit, required.components = 5L)

#Remove NAs
#bw_heating_sample <- na.omit(bw_heating_sample)

#Exchange RoomType Occ with Wohn (works only since there are no other room types in df)
summary(as.factor(bw_heating_sample$RoomType))
i = 0
for (i in 1:nrow(bw_heating_sample)){
  ifelse(bw_heating_sample$RoomType[i] == "Occ", bw_heating_sample$RoomType[i] <- "Wohn", bw_heating_sample$RoomType[i] <- bw_heating_sample$RoomType[i])
}

write.csv(bw_heating_sample, "C:/Users/chris/Desktop/bw_heating_sample.csv")

#Long to wide format
bw_heating_sample_spread <- spread(bw_heating_sample, key = KPI2, value = Value)

head(bw_heating_sample)
head(bw_heating_sample_spread)

bw_heating_sample_spread$idk <- as.factor(bw_heating_sample_spread$idk)
bw_heating_sample_spread$Room <- as.factor(bw_heating_sample_spread$Room)
bw_heating_sample_spread$RoomType <- as.factor(bw_heating_sample_spread$RoomType)
bw_heating_sample_spread$Occ <- as.factor(bw_heating_sample_spread$Occ)
bw_heating_sample_spread$Win <- as.factor(bw_heating_sample_spread$Win)
bw_heating_sample_spread$T <- as.numeric(bw_heating_sample_spread$T)
bw_heating_sample_spread$Td <- as.numeric(bw_heating_sample_spread$Td)
bw_heating_sample_spread$Val <- as.numeric(bw_heating_sample_spread$Val)
summary(bw_heating_sample_spread)

#Data preparation IWT table
IWT$Zeit <- fastPOSIXct(IWT$X, required.components = 5L)

#Merge Heating data and IWT -> key = timestamp
merged <- merge(bw_heating_sample_spread, IWT, by = "Zeit")
names(merged)[names(merged)=="T"] <- "ActT"
summary(merged)

#enhance dataset

merged$TempDelta <- merged$ActT - merged$Td
#merged$Time <- NULL
merged$Date <- fastDate(substr(merged$Zeit, 0, 10))
#merged$Time <- substr(merged$Zeit, 12, 19)
#merged$Time <- strptime(merged$Time, format = "%H:%M:%S")

merged$weekday <- as.factor(weekdays(merged$Date))

merged[merged$weekday == "Samstag" | merged$weekday ==  "Sonntag", "weekend"] <- 1
merged[merged$weekday == "Montag" | merged$weekday ==  "Dienstag" | merged$weekday ==  "Mittwoch" | 
         merged$weekday ==  "Donnerstag" | merged$weekday ==  "Freitag", "weekend"] <- 0

merged$weekend <- as.factor(merged$weekend)

write.csv(merged, "C:/Users/chris/Desktop/merged.csv")
summary(merged)

merged <- na.omit(merged)

#Merge Hating+IWT with TXF data > key = timestamp

#Only necessary if format of merged not correct
merged$idk <- as.factor(merged$idk)
merged$Room <- as.factor(merged$Room)
merged$RoomType <- as.factor(merged$RoomType)
merged$Occ <- as.factor(merged$Occ)
merged$Win <- as.factor(merged$Win)
merged$ActT <- as.numeric(merged$ActT)
merged$Td <- as.numeric(merged$Td)
merged$Val <- as.numeric(merged$Val)
merged$Zeit <- fastPOSIXct(merged$Zeit, required.components = 5L)

TFX$Zeit <- fastPOSIXct(TFX$X, required.components = 5L)
merged2 <- merge(merged, TFX, by = "Zeit")
summary(merged2)

#Rename columns merged 2:
names(merged2)[names(merged2)=="X.x"] <- "X.IWT"
names(merged2)[names(merged2)=="Erzeuger.x"] <- "Erzeuger.IWT"
names(merged2)[names(merged2)=="Friteuse"] <- "Friteuse.IWT"
names(merged2)[names(merged2)=="Kaffeemaschine"] <- "Kaffeemaschine.IWT"
names(merged2)[names(merged2)=="Netz"] <- "Netz.IWT"
names(merged2)[names(merged2)=="Sauna"] <- "Sauna.IWT"
names(merged2)[names(merged2)=="Schwimmbad.x"] <- "Schwimmbad.IWT"
names(merged2)[names(merged2)=="Verbraucher.x"] <- "Verbraucher.IWT"
names(merged2)[names(merged2)=="Gesamt.x"] <- "Gesamt.IWT"
names(merged2)[names(merged2)=="Gesamt.y"] <- "Gesamt.TFX"
names(merged2)[names(merged2)=="Erzeuger.y"] <- "Erzeuger.TFX"
names(merged2)[names(merged2)=="Schwimmbad.y"] <- "Schwimmbad.TFX"
names(merged2)[names(merged2)=="Verbraucher.y"] <- "Verbraucher.TFX"
names(merged2)[names(merged2)=="Hotel"] <- "Hotel.TFX"
names(merged2)[names(merged2)=="Apartmenthaus"] <- "Apartmenthaus.TFX"

write.csv(merged2, "C:/Users/chris/Desktop/merged2.csv")

###linear model (basic)

lm1 <- lm(Verbraucher.TFX ~ Occ, data = merged2)
summary(lm1)

lm2 <- lm(Verbraucher.TFX ~ Occ + ActT + Td + Val + Win + Room, data = merged2)
summary(lm2)

#add. lin. models

lm3 <- lm(Verbraucher ~ TempDelta + Val, data = merged)
summary(lm3)

plot(merged$TempDelta, merged$Val)

lm4 <- lm(Verbraucher ~ weekday, data = merged)
summary(lm4)

lm5 <- lm(Verbraucher ~ weekend + weekday + Occ + TempDelta, data = merged)
summary(lm5)

lm6 <- lm(Verbraucher ~ Val, data = merged)
summary(lm6)

plot(merged$Verbraucher, merged$Val)



###Polynomial Regression Models

merged <- na.omit(merged)

poly_1 <- lm(Verbraucher ~ poly(as.numeric(TempDelta), 1), data = merged)
poly_3 <- lm(Verbraucher ~ poly(as.numeric(TempDelta), 3), data = merged)
poly_5 <- lm(Verbraucher ~ poly(as.numeric(TempDelta), 5), data = merged)
poly_7 <- lm(Verbraucher ~ poly(as.numeric(TempDelta), 7), data = merged)
summary(poly_5)
#plot(poly_3)
ggplot(merged) + 
  geom_point(aes(Verbraucher, TempDelta, col = "Original")) +
  stat_smooth(method = "lm", formula = y~poly(x,1), aes(TempDelta, poly_1$fitted.values, col = "Order 1")) +
  stat_smooth(method = "lm", formula = y~poly(x,3), aes(TempDelta, poly_3$fitted.values, col = "Order 3")) +
  stat_smooth(method = "lm", formula = y~poly(x,5), aes(TempDelta, poly_5$fitted.values, col = "Order 5")) +
  stat_smooth(method = "lm", formula = y~poly(x,7), aes(TempDelta, poly_7$fitted.values, col = "Order 7"))


#When having a test and a training set, model can be run with test data. Then RSS
#can be calculated for each degree and optimal no. of degrees can be derived.
#See pdf on polynomial functions for Code

###

plot(merged$Date, merged$Verbraucher)
plot(merged$Verbraucher, merged$Occ)
plot(merged$Verbraucher, merged$weekday)
plot(merged$Verbraucher, merged$weekend)
plot(merged$Verbraucher, merged$TempDelta)
plot(merged$Verbraucher, merged$Val)

#

### Data preparation for Lasso / Ridge Regression

summary(bw_heating_sample_spread)
head(bw_heating_sample_spread)

bw_heating_sample_spread <- na.omit(bw_heating_sample_spread)

#Long to wide for KPI and Rooms
bw_heating_sample_spread_lasso <- reshape(data = bw_heating_sample_spread,
                                          v.names = c("Occ", "T", "Td", "Val", "Win"),
                                          timevar = "Room",
                                          idvar = c("Zeit", "idk", "RoomType"),
                                          direction = "wide")

#na.omit deletes a lot of rows -> only because of sample data, since there are more
#observations for Zi05 than Zi04 (based on sample selection)
bw_heating_sample_spread_lasso <- na.omit(bw_heating_sample_spread_lasso)
head(bw_heating_sample_spread_lasso)

#Data preparation IWT table
IWT$Zeit <- fastPOSIXct(IWT$X, required.components = 5L)

#Merge with IWT
merged_lasso <- merge(bw_heating_sample_spread_lasso, IWT, by = "Zeit")
names(merged_lasso)[names(merged_lasso)=="T"] <- "ActT"
summary(merged_lasso)

#Lasso
#Merged using Datetime, not required onwards (only used as key)
y <- merged_lasso$Verbraucher
x <- data.matrix(merged_lasso[, c(4:13)])

library(glmnet)
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <-cv_model$lambda.min
#best_lambda

plot(cv_model)

lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_model)

#If test data available, R-squared could be calculated.


###Aggregation

bw_heating2 <- bw_heating

#Subset for performance
bw_heating2_sample <- bw_heating2[500000:600000,]
#bw_heating2_sample2 <- bw_heating2[sample(nrow(bw_heating2), size = 100000),]

i = 0
for (i in 1:nrow(bw_heating2_sample)){
  ifelse(bw_heating2_sample$RoomType[i] == "Occ", bw_heating2_sample$KPI2[i] <- bw_heating2_sample$RoomType[i], bw_heating2_sample$KPI2[i] <- bw_heating2_sample$KPI[i])
}

summary(as.factor(bw_heating2_sample$RoomType))
i = 0
for (i in 1:nrow(bw_heating2_sample)){
  ifelse(bw_heating2_sample$RoomType[i] == "Occ", bw_heating2_sample$RoomType[i] <- "Wohn", bw_heating2_sample$RoomType[i] <- bw_heating2_sample$RoomType[i])
}

bw_heating2_sample$KPI <- NULL
bw_heating2_sample_spread <- spread(bw_heating2_sample, key = KPI2, value = Value)

bw_heating2_sample_spread$Date <- fastDate(substr(bw_heating2_sample_spread$Zeit, 0, 10))

summary(bw_heating2_sample_spread)
bw_heating2_sample_spread$idk <- as.factor(bw_heating2_sample_spread$idk)
bw_heating2_sample_spread$Room <- as.factor(bw_heating2_sample_spread$Room)
bw_heating2_sample_spread$RoomType <- as.factor(bw_heating2_sample_spread$RoomType)
bw_heating2_sample_spread$Occ <- as.factor(bw_heating2_sample_spread$Occ)
bw_heating2_sample_spread$Win <- as.factor(bw_heating2_sample_spread$Win)
bw_heating2_sample_spread$T <- as.numeric(bw_heating2_sample_spread$T)
bw_heating2_sample_spread$Td <- as.numeric(bw_heating2_sample_spread$Td)
bw_heating2_sample_spread$Val <- as.numeric(bw_heating2_sample_spread$Val)
summary(bw_heating2_sample_spread)

bw_heating2_sample_spread <- na.omit(bw_heating2_sample_spread)

#Come up with idea to summarise Win and Occ? Majority?
bw_heating2_sample_spread_agg <- bw_heating2_sample_spread %>%
  group_by(Date, Room) %>%
  summarise(
    T_mean = mean(T, na.rm = TRUE),
    Td_mean = mean(Td, na.rm = TRUE),
    Val_mean= mean(Val, na.rm = TRUE)
  ) %>%
  as.data.frame()

summary(bw_heating2_sample_spread_agg)

write.csv(bw_heating2_sample_spread_agg, "C:/Users/chris/Desktop/bw_heating2_sample_spread_agg.csv")

## Time Series
#summary(merged_ts)
merged_ts <- subset(bw_heating2_sample_spread_agg, Room == "Zi05")
#merged_ts$Date <- fastPOSIXct(merged_ts$Date, required.components = 3L)
head(merged_ts, 10)
merged_ts <- merged_ts[,c(1,3)]
merged_ts <- ts(merged_ts, frequency = 365, start = c(2019,1,1))


plot.ts(merged_ts)

############################ Christians Area ##########################

############################ Fabians Area #############################



############################ Fabian Area ###############################

############################ Niclas Area ###############################

### Get the data from the sharepoint 
site <- get_sharepoint_site(site_url = "https://mssdconcept.sharepoint.com/sites/TeamMannheimBusinessSchool")
url <- "General/04_Data & Analysis/01_Data/Hotel am Kurpark_2981/Heating Data/Hotel am Kurpark 1. Halbjahr 2020/"
drv <- site$get_drive()
links <- drv$list_items(url)
links <- links$name

# Create the correct path by adding the link names to the original path
list = c()
for (i in links){
  d =paste(url,i, sep ="")
  list = c(list,d)}
list <- list[grepl("txt", list)]
list


# Bulk download all the files from the Folder 
for (i in 1:length(list)){
  drv$download_file(list[i])
}

# Add all IWT_all data from 4 files to 1 file
TFX_all <- rbind(TFX_all_1, TFX_all_2, TFX_all_3, TFX_all_4)

write.csv(TFX_all, "C:/Users/Niclas Bardo Bayer/Desktop/TFX_all.csv")
heating_data <- read.delim("Heating_all_0120.txt", sep = " ")
summary(heating_data)

# Add all the Wetter data from 4 files to 1 file
Wetter_all <- rbind(Wetter_1, Wetter_2, Wetter_3, Wetter_4)

write.csv(Wetter_all, "C:/Users/Niclas Bardo Bayer/Desktop/Wetter_all.csv")

# As we now have all the raw data we merge it into one file to create a model based on the data
# Therefore we need to merge the data from Heating. TFX_all and Wetter into one file. 
# Before we can do this we need to Merge the heating data files into on file per 6 month

## Merge Code ##

#import all files from working directory -> IMPORTANT TO SET WORKING DIRECTORY

url <- "General/04_Data & Analysis/01_Data/Hotel am Kurpark_2981/Heating Data/Hotel am Kurpark 1. Halbjahr 2020/"
drv <- site$get_drive()
links <- drv$list_items(url)
links <- links$name

# Create the correct path by adding the link names to the original path
list = c()
for (i in links){
  d =paste(url,i, sep ="")
  list = c(list,d)}
list <- list[grepl("csv", list)]
list


# Bulk download all the files from the Folder 
for (i in 1:length(list)){
  drv$download_file(list[i])
}

data_frame_names <- list.files(pattern = "*min.csv")       # Get all file names
data_frame_names 
data_frame_list <- lapply(data_frame_names, read.csv2)  # Read all data frames
#data_frame_list #uncomment to see all files (takes a long time!)

#length(data_frame_list)

#data_frame_list[1]

dfcs <- data.frame()

#for loop transforms each file so there is one column with time stamp
# (existing), one with the measured KPI and one with the measured value
# All are saved in one data frame with this structure of three columns
i= 1
for (i in 1:length(data_frame_list)){
  
  var <- pivot_longer(as.data.frame(data_frame_list[i]) %>%
                        mutate(across(c(!"Zeit"), as.character)), cols = c(!"Zeit"), 
                      names_to = "Descr", values_to = "Value")
  
  dfcs <- rbind(dfcs, var)
  
}

#Exchange all occurences of .occ with .all.occ to prevent mismatch of columns 
#when separatig
dfcs$Descr <- str_replace_all(dfcs$Descr, ".Occ", ".all.Occ")
dfcs$Descr <- str_replace_all(dfcs$Descr, ".Build", ".all.Build")
dfcs$Descr <- str_replace_all(dfcs$Descr, ".Clln", ".all.Clln")

# separate ex-columntitles so different information is available in 
# several columns (i.e. room, roomType, ...)
dfcs <- dfcs %>% 
  separate(Descr, c("HotelID", "Room", "RoomType", "KPI"))

#head(dfcs)

#Split Occ from rest, since this is independent of room Type
#dfcs_occ <- dfcs[dfcs$KPI == "Occ",]
#dfcs_heat <- dfcs[dfcs$KPI != "Occ",]

#Long to wide Format for each df
#dfcs_heat_wide2 <- spread(dfcs_heat, key = "KPI", value = "Value")
#dfcs_occ_wide2 <- spread(dfcs_occ, key = "KPI", value = "Value")
dfcs_all_wide <- spread(dfcs, key = "KPI", value = "Value")

#saving as csv --> CHANGE PATH anf FILE NAME!!!
write.table(dfcs_all_wide, "C:/Users/Niclas Bardo Bayer/Desktop/Heating_all_0120.txt")

#summary(dfcs)

#Now we need to merge the Heating Data (dfcs) and the TFX Data
names(TFX_all)[1] <- "Zeit" 
dfcs_all_wide$Zeit <- fastPOSIXct(dfcs_all_wide$Zeit, required.components = 5L)
merged <- merge(dfcs_all_wide, TFX_all, by = "Zeit")
write.table(merged, "C:/Users/Niclas Bardo Bayer/Desktop/MergedData_KurparkHotel_0120.txt")


## Use of simple Regression Tree for Prediction of the Energy Consumption ##
install.packages("ISLR")
install.packages("tree")
library(ISLR)
library(tree)

merged$idk <- as.factor(merged$idk)
merged$Room <- as.factor(merged$Room)
merged$RoomType <- as.factor(merged$RoomType)
merged$Occ <- as.factor(merged$Occ)
merged$Win <- as.factor(merged$Win)
merged$weekday <- as.factor(merged$weekday)
merged$weekend <- as.factor(merged$weekend)
summary(merged)

cleaned_merged <- merged[c("idk","Room","RoomType","Occ","ActT", "Td", "Val","Win","Netz","TempDelta","weekday","weekend")]
summary(cleaned_merged)
View(merged)

inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
summary(cleaned_merged)

library(ggplot)
library(tidyverse)
ggplot(data = cleaned_merged) +
geom_freqpoly(aes(x = Netz, color = weekday), binwidth = 1)
ggplot(data = cleaned_merged) +
geom_freqpoly(aes(x = weekday, y= Netz), binwidth = 1)
ggplot(data = cleaned_merged) +
geom_point(aes(x = weekday, y= Netz))
mytree<-tree(Netz ~  , data=train.set)
summary(merged)
summary(cleaned_merged)
mytree<-tree(Netz ~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekday + weekend , data=train.set)
summary(mytree)
(mytree)
plot(mytree)
text(mytree)
title("This is a tree with one regressor")
### Pruning the tree ###
prune.mytree = prune.tree(mytree, best=3)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with 3 terminal nodes!")
mytree<-tree(Netz ~. , data=train.set)
summary(mytree)
plot(mytree)
text(mytree)
### Pruning the tree ###
prune.mytree = prune.tree(mytree, best=3)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with 3 terminal nodes!")
### Chose the optimal complexity of the tree with cross validation ###
set.seed(2)
cv.model <- cv.tree(mytree)
cv.model
cv.model$dev  # gives the deviance for each K (small is better)
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size
plot(cv.model,type='b')
# View the data
head(Hitters)
# Variable names
names(Hitters)
# Clean the data and eliminate NA
Hitters<-na.omit(Hitters)
# Create a training and a test set
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(Hitters), alpha * nrow(Hitters))
train.set <- Hitters[inTrain,]
test.set  <- Hitters[-inTrain,]
# Plot the salary against the years
plot(train.set$Years,train.set$Salary, main= "Salary by years", ylab="Salary",xlab="Years")
# Plot the salary against the logarithm of the years
plot(train.set$Years,log(train.set$Salary), main= "Salary by years", xlab="Log Salary",ylab="Years")
# Plot the model
# This is the tree
mytree<-tree(log(Salary) ~ Years , data=train.set) #log because salary is a huge number
summary(mytree) #SAVED NAME MY TREE
plot(mytree)
text(mytree)
title("This is a tree with one regressor")
# Prune the tree
prune.mytree = prune.tree(mytree, best=3)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with 3 terminal nodes!")
# We can extend the tree by including the hits
mytree=tree(log(Salary)~ Years + Hits, data=train.set)
summary(mytree) #SAVED NAME MY TREE
plot(mytree)
text(mytree)
title("This is a tree with two regressors")
# Partition tree
partition.tree(mytree, add = FALSE, main="This is a partition tree")
# Unprunned tree
# Mincut stands for the minimum number of observations in each node
mytree=tree(log(Salary)~ ., mincut = 5, data=train.set)
summary(mytree)
plot(mytree)
text(mytree)
title("This is an unpruned tree using all the regressors and a cut at 5 memebers per region")
# Try a tree with 10 at least 10 members per region
mytree=tree(log(Salary)~ ., mincut = 10, data=train.set)
summary(mytree)
plot(mytree)
text(mytree)
title("This is an unpruned tree using all the regressors and a cut at 10 memebers per region")
# Chose the optimal complexity of the tree, which in this example is?
set.seed(2)
cv.model <- cv.tree(mytree)
cv.model
cv.model$dev  # gives the deviance for each K (small is better)
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size
plot(cv.model,type='b')
plot(cv.model,type='b')
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
mytree<-tree(Netz ~. , data=train.set)
summary(mytree)
plot(mytree)
text(mytree)
title("Tree for Predicting the energy consumption")
### Pruning the tree ###
prune.mytree = prune.tree(mytree, best=3)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with 3 terminal nodes!")
### Chose the optimal complexity of the tree with cross validation ###
set.seed(2)
cv.model <- cv.tree(mytree)
cv.model
cv.model$dev  # gives the deviance for each K (small is better)
best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size
plot(cv.model,type='b')
### Prune tree on best fit according to CV
prune.mytree = prune.tree(mytree, best=best.size)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with an optimal number of terminal nodes!")
summary(prune.mytree)
Wine <- read.csv("C:/Users/Niclas Bardo Bayer/OneDrive/Studium/Master/Data Science for Business 2/Data/Wine.csv", sep=";")
View(Wine)
###
tree_2 <-rpart(Netz ~. , subset()=train.set)
###
tree_2 <-rpart(Netz ~. , subset =train.set)
library(rpart)
###
tree_2 <-rpart(Netz ~. , subset =train.set)
###
tree_2 <-rpart(Netz ~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekend + weekday , subset =train.set)
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
###
tree_2 <-rpart(Netz ~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekend + weekday , subset =train.set)
cleaned_merged$Netz<-as.factor(cleaned_merged$Netz)
summary(cleaned_merged$Netz)
tree_2 <-rpart(Netz ~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekend + weekday , subset =train.set)
train.set
tree_2 <-rpart(Netz~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekend + weekday , subset =train.set)
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
tree_2 <-rpart(Netz~ Room + RoomType + Occ + ActT + Td + Val + Win + TempDelta + weekend + weekday , subset =train.set)
summary(cleaned_merged$Netz)
head(merged)
summary(merged)
merged$idk <- as.factor(merged$idk)
merged$Room <- as.factor(merged$Room)
merged$RoomType <- as.factor(merged$RoomType)
merged$Occ <- as.factor(merged$Occ)
merged$Win <- as.factor(merged$Win)
merged$weekday <- as.factor(merged$weekday)
merged$weekend <- as.factor(merged$weekend)
summary(merged)
cleaned_merged <- merged[c("Room","RoomType","Occ","ActT", "Td", "Val","Win","Netz","TempDelta","weekday","weekend")]
summary(cleaned_merged)
ggplot(data = cleaned_merged) +
geom_point(aes(x = weekday, y= Netz))
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
tree_2 <-rpart(formula = Netz ~., data = train.set, method = "anova")
rpart.plot(m1)
library(rpart.plot)
install.packages("rpart.plot")
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m1)
rpart.plot(tree_2)
## Use of simple Regression Tree for Prediction of the Energy Consumption ##
#install.packages("ISLR")
#install.packages("tree")
library(ISLR)
library(tree)
library(tidyverse)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
head(merged)
summary(merged)
merged$idk <- as.factor(merged$idk)
merged$Room <- as.factor(merged$Room)
merged$RoomType <- as.factor(merged$RoomType)
merged$Occ <- as.factor(merged$Occ)
merged$Win <- as.factor(merged$Win)
merged$weekday <- as.factor(merged$weekday)
merged$weekend <- as.factor(merged$weekend)
summary(merged)
cleaned_merged <- merged[c("Room","RoomType","Occ","ActT", "Td", "Val","Win","Netz","TempDelta","weekday","weekend")]
summary(cleaned_merged)
ggplot(data = cleaned_merged) +
geom_point(aes(x = weekday, y= Netz))
set.seed(2)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(cleaned_merged), alpha * nrow(cleaned_merged))
train.set <- cleaned_merged[inTrain,]
test.set  <- cleaned_merged[-inTrain,]
tree_2 <-rpart(formula = Netz ~., data = train.set, method = "anova")
rpart.plot(tree_2)
png("Plot.png",res = 300)
rpart.plot(tree_2)
dev.off()
png("Plot.png",res = 300)
dev.off()
tree_2 <-rpart(formula = Netz ~., data = train.set, method = "anova")
rpart.plot(tree_2)
png("Plot.png",res = 300)
dev.off()
plotcp(tree_2)

############################ Niclas Area ###############################
