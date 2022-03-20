#one <- read.csv("211117T0000_211124T2359_2929.Zi7.csv", sep = ";")
#two <- read.csv("211117T0000_211124T2359_2929.Zi8.csv", sep = ";")

library(tidyverse)
install.packages("here")
library(here)

library(dplyr)
library(dbplyr)
library(tidyverse)
library(tidyr)

#import all files from working directory -> IMPORTANT TO SET WORKING DIRECTORY

getwd()
setwd("C:/Users/Niclas Bardo Bayer/Desktop/Unsortierte Dateien/BAMP Daten Analyse/Abfrage Messdaten/Abfrage Messdaten/Best Western Premium Schwarzwald 1. Halbjahr 2019/tabellen")
getwd()

data_frame_names <- list.files(pattern = "*.csv")       # Get all file names
data_frame_names 
data_frame_list <- lapply(data_frame_names, read.csv2)  # Read all data frames
#data_frame_list #uncomment to see all files (takes a long time!)

length(data_frame_list)

#data_frame_list[1]

dfcs <- data.frame()

#for loop transforms each file so there is one column with time stamp
# (existing), one with the measured KPI and one with the measured value
# All are saved in one data frame with this structure of three columns

for (i in 1:length(data_frame_list)){
  
  var <- pivot_wider(as.data.frame(data_frame_list[i]))%>%
    mutate(across(c(!"Zeit"), as.character)) cols = c(!"Zeit")
  
}

# separate ex-columntitles so different information is available in 
# several columns (i.e. room, roomType, ...)
dfcs <- dfcs %>% 
  separate(Descr, c("idk", "Room", "RoomType", "KPI"))


head(dfcs)

#saving as csv --> CHANGE PATH anf FILE NAME!!!
write.table(dfcs, "C:/Users/Niclas Bardo Bayer/Desktop/Unsortierte Dateien/BAMP Daten Analyse/SHS Beispieldaten/Dfcs.txt")
summary(dfcs)

## -- old code during testing --

#rlang::last_trace()
  

#as.data.frame(data_frame_list[i])

#test1$X2929.Zi12.Bad.Val <- as.character(test1$X2929.Zi12.Bad.Val)
#test1$X2929.Zi12.Schl.Val <- as.character(test1$X2929.Zi12.Schl.Val)

#test1unpiv <- pivot_longer(test1, cols = c(!"Zeit"), names_to = "Descr",
                        #   values_to = "Value")



#head(one)
#head(two)

#unoivot: move columns to rows, only one value per row
#onenew <- pivot_longer(one, cols = c("X2929.Zi7.Schl.T", "X2929.Zi7.Schl.Td", 
                    #                "X2929.Zi7.Schl.Val"), names_to = "Descr", 
                    #   values_to = "Value")

#split information from previous column titles
#onenew <- onenew %>% 
  #separate(Descr, c("idk", "Room", "RoomType", "KPI"))


#unoivot: move columns to rows, only one value per row
#twonew <- pivot_longer(two, cols = c("X2929.Zi8.Schl.T", "X2929.Zi8.Schl.Td", 
                       #              "X2929.Zi8.Schl.Val"), names_to = "Descr", 
                       #values_to = "Value")

#split information from previous column titles
#twonew <- twonew %>% 
 # separate(Descr, c("idk", "Room", "RoomType", "KPI"))

#head(twonew)

#merged <- rbind(onenew, twonew)
