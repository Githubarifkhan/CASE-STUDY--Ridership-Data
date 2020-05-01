install.packages("readxl")
library(readxl)

mb <- read_excel("C:/Users/PRABHAT/Desktop/Data Science R- by Eckovation/Database for R/data uploaded in course/mbta.xlsx", skip = 1)
# the first row 


# ---------------------- Examine the data --------------------


# structure of data
str(mb)


# head of data (first 6 rows) 
head(mb)

# summary of data
summary(mb)

nrow(mb)
ncol(mb)
# no.of row are lesser than no.of columns


# ---------------------- Removing unnecessary rows and columns -------------- 


# Rows 1,7,11 have unneccesary data, Remove them


mb2 <- mb[-c(1, 7, 11), ]

install.packages("glimpse")

glimpse (mb2)


# Remove the first column, since it gives row numbers

mb3 <- mb2[,-1]

glimpse(mb3)

#  Observations are stored in columns in this data
# here months are our observations

head(mb3)

# Load tidyr
library(tidyr)
?gather

# Gather columns
mb4 <- gather(mb3, month, thou_riders, -mode)

# View the head of mbta4
head(mb4)


# Coerce thou_riders to numeric
mb4$thou_riders <- as.numeric(mb4$thou_riders)
head(mb4)

# load tidyr 

# Spread the contents

?spread

mb5 <- spread(mb4, mode, thou_riders)

# head of mb5
head(mb5)

# ------------   Separating columns --------------


# since month and year are in the same column together, we will split them in to month and year columns

mb6 <- separate(mb5, month, c('year', 'month'))

head(mb6)

summary(mb6)


# histogram of Boat ridership

hist(mb6$Boat)
# we see the values are concentrated around 0 - 5, and very few around 40

# the boat ridership jumped from 4thousand to 40 thousand, we will replace the incorrect value
# 40 is basically an outlier, we will treat it by replacing


# Find the row number of the incorrect value
incorrect <- which(mb6$Boat == 40)
incorrect


# Replace the incorrect value with 4
mb6$Boat[incorrect] <- 4


# Generate a histogram of Boat column
hist(mb6$Boat)
  

library(dplyr)
library(ggplot2)


## need to use the old mbta data frame because ggplot likes the variables to be melted
head(mb6)


table(mb4$mode)


mb_all <- mb6 %>%
  unite(year_mon, year, month, sep = "") %>%
  gather(mode, thou_riders, -year_mon)

mb_boat <- mb_all %>%
  filter(mode %in% c("Boat","Trackless Trolley"))

head(mb_boat)

table(mb_boat$mode)


# Look at Boat and Trackless Trolley ridership over time
## The old outlier point for boat is still in here


ggplot(mb_boat, aes(x = year_mon, y = thou_riders, col = mode)) +  geom_point() + 
  scale_x_discrete(name = "Month", breaks = c(200701, 200801, 200901, 201001, 201101)) + 
  scale_y_continuous(name = "Avg Weekday Ridership (thousands)")



# Look at all T ridership over time

ggplot(mb_all, aes(x = year_mon, y = thou_riders, col = mode)) + geom_point() + 
  scale_x_discrete(name = "Month", breaks = c(200701, 200801, 200901, 201001, 201101)) +  
  scale_y_continuous(name = "Avg Weekday Ridership (thousands)")


