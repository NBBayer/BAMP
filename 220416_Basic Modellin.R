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
# Set the site and retrieve the link names
site <- get_sharepoint_site(site_url = "https://mssdconcept.sharepoint.com/sites/TeamMannheimBusinessSchool")
url <- "General/04_Data & Analysis/01_Data/Test Hotel/"
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

### NB: Discussion if the Import might be also be done automaticaly??

#Merge the Heating Data from the Sharepoint File
#I will continue working on this part 

############################ Christians Area ##########################

####### Merge Code #######

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


###### End of Merge Code ######


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

###linear model (basic)

lm1 <- lm(Verbraucher ~ Occ, data = merged)
summary(lm1)

lm2 <- lm(Verbraucher ~ Occ + ActT + Td + Val + Win + Room, data = merged)
summary(lm2)

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

###### Data prep 17/05/2022 #######

dfcs <- Heating_all_0120
summary(dfcs)

dfcs$Room <- as.factor(dfcs$Room)
dfcs$RoomType <- as.factor(dfcs$RoomType)
dfcs$Occ <- as.factor(dfcs$Occ)
dfcs$Win <- as.factor(dfcs$Win)

#Creating subsets for each level of information

dfcs_building <- dfcs[dfcs$RoomType == "Build",]
dfcs_room <- dfcs[dfcs$RoomType == "all",]
dfcs_roomtype <- dfcs[dfcs$RoomType != "all",]
dfcs_roomtype <- dfcs_roomtype[dfcs_roomtype$RoomType != "Build",]

#Prepara each subset individually -> long to wide
dfcs_building <- subset(dfcs_building, select = -c(Occ, T, Td, Val, Win, Room, RoomType))
names(dfcs_building) <- c("Zeit", "HotelID", "ClIn.all.Build", "Text.all.Build")


dfcs_room <- subset(dfcs_room, select = -c(T, Td, Val, Win, Text, ClIn))
dfcs_room$keylw <- paste("Occ", dfcs_room$Room, dfcs_room$RoomType, sep = ".")
dfcs_room <- subset(dfcs_room, select = -c(Room, RoomType))
dfcs_room_wide <- spread(dfcs_room, key = "keylw", value = "Occ")


dfcs_roomtype <- subset(dfcs_roomtype, select = -c(Occ, Text, ClIn))
dfcs_roomtype$keylw <- paste(dfcs_roomtype$Room, dfcs_roomtype$RoomType, sep = ".")
dfcs_roomtype <- subset(dfcs_roomtype, select = -c(Room, RoomType))
dfcs_roomtype_wide <- reshape(data = dfcs_roomtype, 
                              v.names = c("T", "Td", "Val", "Win"),
                              timevar = "keylw",
                              idvar = c("Zeit", "HotelID"),
                              direction = "wide")

#merge wide subsets to entire data set, based on timestamp
dfcshelp1 <- merge(dfcs_building, dfcs_room_wide, by = "Zeit")
dfcshelp1 <- subset(dfcshelp1, select = -c(HotelID.y))

heating_widemax <- merge(dfcshelp1, dfcs_roomtype_wide, by = "Zeit")
heating_widemax <- subset(heating_widemax, select = -c(HotelID))
summary(heating_widemax)

write.csv(heating_widemax, "C:/Users/chris/Desktop/heating_widemax.csv")

#merge with TFX_all data

TFX_all <- TFX_all_1

TFX_all <- subset(TFX_all, select = c(X, Kessel.1.2.Leistung, BHKW.1.4.Leistung))
names(TFX_all) <- c("Zeit", "Kessel_Leistung", "BHKW_Leistung")
TFX_all$Zeit <- fastPOSIXct(TFX_all$Zeit, required.components = 5L)
summary(TFX_all)
heating_widemax$Zeit <- fastPOSIXct(heating_widemax$Zeit, required.components = 5L)
#heating_widemax_TFX <- merge(heating_widemax, TFX_all, by = "Zeit", all.x = TRUE)
#heating_widemax_TFX <- NULL


#Legt join keeps all rows from heating --> many NAs since dates don't overlap entirely
heating_widemax_TFX <- heating_widemax %>% left_join(TFX_all, by = "Zeit")

heating_widemax_TFX <- heating_widemax_TFX %>% drop_na(Kessel_Leistung)
summary(heating_widemax_TFX$BHKW_Leistung)

write.csv(heating_widemax_TFX, "C:/Users/chris/Desktop/heating_widemax_TFX.csv")

summary(TFX_all$Zeit)
summary(heating_widemax$Zeit)

###### End of Data prep 17/05/2022 #######

###### Data Exploration "raw" heating data ######

heating_data <- Heating_all_0120
summary(heating_data)

heating_data$ClIn <- NULL
heating_data$Text <- NULL

heating_data_rooms <- subset(heating_data, grepl("Zi", heating_data$Room))
heating_data_rooms$Occ <- NULL
#summary(heating_data_rooms)
heating_data_rooms$Room <- as.factor(heating_data_rooms$Room)
heating_data_rooms$RoomType <- as.factor(heating_data_rooms$RoomType)
heating_data_rooms$Zeit <- fastPOSIXct(heating_data_rooms$Zeit, required.components = 5L)
heating_data_rooms$Win <- as.factor(heating_data_rooms$Win)

#remove all N/A's (entire rows!)
heating_data_rooms <- na.omit(heating_data_rooms)
summary(heating_data_rooms)

ggplot(data = heating_data_rooms, aes(x = Zeit, y = T)) +
  geom_point()

#Plot mean Temperature over all rooms per point in time
ggplot(data = heating_data_rooms, aes(x = Zeit, y = T)) +
  stat_summary(aes(y = T), fun = "mean", geom = "line", colour = "red") +
  stat_summary(aes(y=Td), fun = "mean", geom = "line", colour = "blue")

#Same chart as above, but data filtered so T and Td have to be larger than 10°C
# --> anything below not plausible (threshold of 10 to be discussed)
filter(heating_data_rooms, T > 10 & Td > 10) %>%
  ggplot(aes(x = Zeit)) +
    stat_summary(aes(y = T, colour = "T"), fun = "mean", geom = "line") +
    stat_summary(aes(y=Td, colour = "Td"), fun = "mean", geom = "line") +
    ylab("Temperature") +
    xlab("Date") +
    labs(title = "Mean T and Td over all rooms per point in time Hotel Kurpark") +
    scale_color_manual("Legend", values = c("T" = "red", "Td" = "blue"))

#Check plot: aggregate: Mean Temperature per point in time
#aggregate(heating_data_rooms$T, by = list(heating_data_rooms$Zeit), FUN = "mean")
#aggregate(heating_data_rooms$Td, by = list(heating_data_rooms$Zeit), FUN = "mean")

heating_data_rooms$TempDelta <- heating_data_rooms$T - heating_data_rooms$Td

#Delta of T and Td + Mean Val per point in time
filter(heating_data_rooms, T > 10 & Td > 10) %>%
  ggplot(aes(x = Zeit)) +
  stat_summary(aes(y = TempDelta, colour = "Delta T and Td"), fun = "mean", geom = "line") +
  geom_hline(yintercept=0, color = "black", size = 2) +
  stat_summary(aes(y=Val, colour = "Val"), fun = "mean", geom = "line") +
  ylab("Temperature-Delta") +
  xlab("Date") +
  labs(title = "Mean Delta of T and Td (T-Td) over all rooms per point in time Hotel Kurpark") +
  scale_color_manual("Legend", values = c("Delta T and Td" = "blue", "Val" = "green"))

##aggregation

df2 <- heating_data_rooms %>%
  group_by(Zeit, Room) %>%
  summarise_at(c("T", "Val"), mean)

#Correlation of T and Val per Room (mean over time)
df2_cor <- df2 %>%
  group_by(Room) %>%
  summarise(r = cor(T, Val))



##### Moddeling with heating_widemax_TFX-dataset #####
summary(heating_widemax_TFX)
heating_widemax_TFX$Zeit <- fastPOSIXct(heating_widemax_TFX$Zeit, required.components = 5L)
ggplot(data = heating_widemax_TFX, aes(x = Zeit, y = Kessel_Leistung)) +
         geom_line()

heating_widemax_TFX$Gesamt <- heating_widemax_TFX$Kessel_Leistung + heating_widemax_TFX$BHKW_Leistung
heating_widemax_TFX <- subset(heating_widemax_TFX, select = -c(Kessel_Leistung, BHKW_Leistung))

ggplot(data = heating_widemax_TFX, aes(x = Zeit, y = Gesamt)) +
        geom_line()
plot(heating_widemax_TFX$Gesamt)

#subset only containing occupancy data
df1 <- heating_widemax_TFX[, grepl("Occ", names(heating_widemax_TFX))]
df1[] <- lapply(df1, as.factor)



#remove "general" rooms since they are assumed to have to influence in change in temp
# -> basis that never changes
# remove columns with nas --> IDEA: can this be done automatically

df1$Occ.TGRhoenI.all <- NULL
df1$Occ.TGRhoenII.all <-  NULL
df1$Occ.TGRhoenIII.all <- NULL
df1$Occ.TGVogelsberg.all <- NULL
df1$Occ.Zi18.all <- NULL
df1$Occ.Zi9A.all <- NULL
df1$Occ.TGVogelsberg.all <- NULL

#subtract 1 from each value in df1 since true = 2 and false = 1
df1[] <- lapply(df1, as.integer)
subtract1 <- function(x){
  return(x-1)
}
df1[] <- lapply(df1, subtract1)
df1 <- cbind(df1, heating_widemax_TFX$Zeit)
summary(df1)

#summary(df1)

#sum(df1$Occ.Zi1.all)/length(df1$Occ.Zi1.all)-1


df1$AuslastungSum <- rowSums(df1[,0:35])
ggplot(data = df1, aes(x = heating_widemax_TFX$Zeit, y = AuslastungSum)) +
  geom_line()

#Quote
df1$Auslastungsquote <- df1$AuslastungSum/35
ggplot(data = df1, aes(x = heating_widemax_TFX$Zeit, y = Auslastungsquote)) +
  geom_line()

df1$Date <- fastDate(substr(df1$`heating_widemax_TFX$Zeit`, 0, 10))
df1$weekday <- as.factor(weekdays(df1$Date))
df1[df1$weekday == "Samstag" | df1$weekday ==  "Sonntag", "weekend"] <- 1
df1[df1$weekday == "Montag" | df1$weekday ==  "Dienstag" | df1$weekday ==  "Mittwoch" | 
      df1$weekday ==  "Donnerstag" | df1$weekday ==  "Freitag", "weekend"] <- 0

df1$weekend <- as.integer(df1$weekend)
cor(df1$Auslastungsquote, df1$weekend)

#BarChart with mean Auslastungsquote per Weekday
meanAuslastungsquote <- aggregate(df1$Auslastungsquote, list(df1$weekday), mean)
ggplot(meanAuslastungsquote, aes(Group.1, x)) +
  geom_bar(stat = "identity")



#Boxplot with mean Auslastungsquote per Weekday
ggplot(df1, aes(x = factor(weekday), y = Auslastungsquote)) + 
  geom_boxplot()





#subset only containing occupancy data
df1 <- heating_widemax_TFX[, grepl("Occ", names(heating_widemax_TFX))]
df1[] <- lapply(df1, as.factor)

df1 <- cbind(df1, heating_widemax_TFX$Gesamt)
df1$Occ.TGVogelsberg.all <- NULL
df1$Occ.TGRhoenI.all <- NULL
df1$Occ.TGRhoenIII.all <- NULL
df1$Occ.Zi18.all <- NULL
df1$Occ.Zi9A.all <- NULL
summary(df1)

lm1 <- lm(heating_widemax_TFX$Gesamt ~ ., data = df1)
summary(lm1)


#Lasso
y <- df1$`heating_widemax_TFX$Gesamt`
x <- data.matrix(df1[, c(2:36)])

library(glmnet)
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <-cv_model$lambda.min
#best_lambda

plot(cv_model)

lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(lasso_model)




############################ Christians Area ##########################

############################ Fabians Area #############################



############################ Fabian Area ###############################

############################ Niclas Area ###############################



############################ Niclas Area ###############################
