summary(IWT_sample)
summary(TFX_sample)

summary(IWT)
summary(Best.Western.Premium.Schwarzwald.1..Halbjahr.2019.Zusammenfassung)

library(tidyr)
library(dplyr)
library(fasttime)
#install.packages("lubridate")
library(lubridate)
library(ggplot2)

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
for (i in 1:nrow(bw_heating_sample)){
  ifelse(bw_heating_sample$RoomType[i] == "Occ", bw_heating_sample$KPI2[i] <- bw_heating_sample$RoomType[i], bw_heating_sample$KPI2[i] <- bw_heating_sample$KPI[i])
}

#drop old KPI column
bw_heating_sample <- bw_heating_sample %>% select(-KPI)

## Specify the Data Type for Heatig correctly:
#bw_heating_sample$Value <- as.numeric(bw_heating_sample$Value)
bw_heating_sample$Zeit <- fastPOSIXct(bw_heating_sample$Zeit, required.components = 5L)
#bw_heating_sample$idk <- as.factor(bw_heating_sample$idk)
#bw_heating_sample$Room <- as.factor(bw_heating_sample$Room)
#bw_heating_sample$RoomType <- as.factor(bw_heating_sample$RoomType)
#bw_heating_sample$KPI2 <- as.factor(bw_heating_sample$KPI2)
#summary(bw_heating_sample)

#Remove NAs
#bw_heating_sample <- na.omit(bw_heating_sample)

#Exchange RoomType Occ with Wohn (works only since there are no other room types in df)
summary(as.factor(bw_heating_sample$RoomType))
for (i in 1:nrow(bw_heating_sample)){
  ifelse(bw_heating_sample$RoomType[i] == "Occ", bw_heating_sample$RoomType[i] <- "Wohn", bw_heating_sample$RoomType[i] <- bw_heating_sample$RoomType[i])
}

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



#linear model (basic)

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


#add. lin. models

lm3 <- lm(Verbraucher ~ TempDelta + Val, data = merged)
summary(lm3)

lm4 <- lm(Verbraucher ~ weekday, data = merged)
summary(lm4)

lm5 <- lm(Verbraucher ~ weekend, data = merged)
summary(lm5)


#Polynomial Regression Models

merged <- na.omit(merged)

poly_3 <- lm(Verbraucher ~ poly(as.numeric(TempDelta), 3), data = merged)
summary(poly_3)
#plot(poly_3)
ggplot(merged) + 
  geom_point(aes(Verbraucher, TempDelta, col = "Original")) +
  stat_smooth(method = "lm", formula = y~poly(x,3), aes(TempDelta, poly_3$fitted.values, col = "Order 3"))


###

plot(merged$Date, merged$Verbraucher)
plot(merged$Verbraucher, merged$Occ)
plot(merged$Verbraucher, merged$weekday)
plot(merged$Verbraucher, merged$weekend)
plot(merged$Verbraucher, merged$TempDelta)
plot(merged$Verbraucher, merged$Val)


#



#####


#bw_heating_spread <- spread(bw_heating, key = KPI, value = Value)
#write.csv(bw_heating_spread, "C:/Users/chris/Desktop/data.csv")

