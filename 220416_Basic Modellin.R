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



