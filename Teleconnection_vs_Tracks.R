#### Calling libraries ####
library(ggplot2)
library(rsoi)
library(data.table)
library(readxl)
library(viridis)
library(ggmap)
library(sp)
library(zoo)
library(rsoi)
library(rpdo)
library(maps)
library(mapdata)
library(raster) # install.packages("raster", dependencies = TRUE)
library(tidyr)
library(dplyr)
library(Rmpfr)
library(matrixStats)
library(readr)
library(Brq)
library(lubridate)
library(progress)
library(stringr)
library(rgdal)
library(sf)
library(foreach)
library(doParallel)
library(RColorBrewer)


list.of.packages <- c("ggplot2", "rsoi", "data.table", "readxl", "sp", "ggmap", "viridis", "rpdo", "maps", "mapdata", "raster", "tidyr",
                      "dplyr", "Rmpfr", "matrixStats", "readr", "Brq", "lubridate", "progress")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, repos='https://cloud.r-project.org/')
# install.packages("rvest")
library(rvest)

unique_tracks_90 <- read.csv("unique_tracks_90.csv").

#### Climate Pattern Indices ####

mei <- download_mei()

mjo <- read.csv("E:/ERA_5/MJO.csv", header = FALSE, sep = ",")
# Rename the columns
colnames(mjo) <- c("Date", "MJO_Index")
mjo <- mjo[-1, ]

# Convert the Date column to character type
mjo$Date <- as.character(mjo$Date)

# Split the Date column into three separate columns for day, month, and year
mjo <- mjo %>% 
  separate(Date, into = c("Day", "month", "Year"), sep = " ")

# Convert the Day, month, and Year columns to the appropriate data types
mjo$Day <- as.integer(mjo$Day)
# Trim white spaces in the month column
mjo$month <- trimws(mjo$month)

# Convert the month column to lowercase
mjo$month <- tolower(mjo$month)

# Parse the month column using the lubridate package
mjo$month <- month(parse_date_time(mjo$month, "b"))

# Convert the month column to integer type
mjo$month <- as.integer(mjo$month)

mjo$Year <- as.integer(mjo$Year)

# Convert the MJO_Index column to numeric
mjo$MJO_Index <- as.numeric(as.character(mjo$MJO_Index))

# Group the data by month and year, calculate the average MJO index
monthly_avg_mjo <- mjo %>%
  group_by(month, Year) %>%
  summarize(avg_MJO_index = mean(MJO_Index))

MJO <- monthly_avg_mjo
colnames(MJO)[3] <- "MJO"


pna <- read.csv("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.pna.monthly.b5001.current.ascii",
                header = FALSE, sep="")
amo <-  read.csv ("https://psl.noaa.gov/data/correlation/amon.sm.data",
                  header = FALSE, sep="")[-1,] # Smoothed short data at https://psl.noaa.gov/data/timeseries/AMO/
ao  <- rsoi::download_ao()
nao <- rsoi::download_nao()
mo2Num <- function(x) match(tolower(x), tolower(month.abb))
nao$Month <- mo2Num(nao$Month)
nao$Date <- as.factor(paste(nao$Year, "-", nao$Month, "-01", sep=""))
nao$Date <- as.Date(nao$Date, format="%Y-%m-%d")

pdo <- read.csv("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
                header = FALSE, sep = "")[-1,]
pdo <- pdo[complete.cases(pdo), ]
# Remove the first row (column names)
pdo <- pdo[-1, ]
# Reshape the data
pdo_reshaped <- pivot_longer(pdo, cols = -V1, names_to = "Month", values_to = "PDO")

# Rename the columns
colnames(pdo_reshaped) <- c("Year", "Month", "PDO")

# Convert the Year and Month columns to numeric
pdo_reshaped$Year <- as.numeric(as.character(pdo_reshaped$Year))
pdo_reshaped$Month <- rep(1:12, length.out = nrow(pdo_reshaped))
pdo <- pdo_reshaped[!(pdo_reshaped$PDO == "99.99"), ]

#nao <- read.csv ("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
#                 header = FALSE, sep="") # https://www.ncdc.noaa.gov/teleconnections/nao/
#pdo <-  rpdo::download_pdo()

#ao  <-  read.csv ("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table",
#                header = TRUE, sep="") # https://www.ncdc.noaa.gov/teleconnections/ao/

summary(mei)
names(pna)[1] <- "Year"
names(pna)[2] <- "Month"
names(pna)[3] <- "PNA"


month_abbreviations <- c("DJ", "JF", "FM", "MA", "AM", "MJ", "JJ", "JA", "AS", "SO", "ON", "ND")
mei <- mutate(mei, Month = match(Month, month_abbreviations)) # Convert month abbreviation to numeric representation
amo <- amo[complete.cases(amo), ]
# Reshape dataset
amo_reshaped <- data.frame(Year = rep(amo$V1, each = 12),
                           Month = rep(1:12, nrow(amo)),
                           AMO = c(t(amo[, 2:13])))
# Remove rows with -99.99 values
amo <- amo_reshaped[!(amo_reshaped$AMO == "-99.990"), ]

############################################################################
# submatched_dataset <- read.csv("submatched_dataset.csv") 

# 1_ ENSO and VIL
summary(mei)
mei$month <- lubridate::month(mei$Date)
unique_tracks_90$month <- lubridate::month(unique_tracks_90$Date_time)
VIL_MEI <- left_join(mei, unique_tracks_90, by = c("Year", "month"))
mei <- mei[!is.na(mei$MEI), ]

# Exclude NA values from VIL_MEI
VIL_MEI <- VIL_MEI[complete.cases(VIL_MEI), ]

####################################################
# Exploring the lag effect
####################################################

# 1_ENSO

MEI_djf <- mei %>%
  filter(month %in% c(1,2,3))
VIL_MEI_DJF <- left_join(MEI_djf, unique_tracks_90, by = "Year")
VIL_MEI_DJF <- VIL_MEI_DJF[complete.cases(VIL_MEI_DJF),]


# MEI and VIL
result_table <- data.frame()
calculate_mei <- function(months){ 
MEI_djf <- mei %>%
  filter(month %in% months)
VIL_MEI_DJF <- left_join(MEI_djf, unique_tracks_90, by = "Year")
VIL_MEI_DJF <- VIL_MEI_DJF[complete.cases(VIL_MEI_DJF),]

Lagged_VIL_MEI <- VIL_MEI_DJF %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            MEI = mean(MEI)) %>%
  mutate(Phase = case_when(
    MEI < -0.5 ~ "La Nina",
    MEI >= -0.5 & MEI <= 0.5 ~ "Neutral",
    MEI > 0.5 ~ "El Nino"))

calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_lag_MEI <- replicate(1000, {
  sampled_data <- Lagged_VIL_MEI[sample(nrow(Lagged_VIL_MEI), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})



# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Lagged_VIL_MEI %>%
    filter(Phase == "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Lagged_VIL_MEI %>%
    filter(Phase != "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_lag_MEI, 0.9)
percentile_95 <- quantile(differences_lag_MEI, 0.95)
percentile_10 <- quantile(differences_lag_MEI, 0.10)
percentile_05 <- quantile(differences_lag_MEI, 0.05)

# Create a new row for the current month combination
new_row <- data.frame(
  Month_Combination = paste(months, collapse = "-"),
  Mean_Difference = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05,
  stringsAsFactors = FALSE
)

return(new_row)
}

# Print the result table
result_table <- rbind(result_table, calculate_mei(c(12,1,2)))
result_table <- rbind(result_table, calculate_mei(c(3,4,5)))
result_table <- rbind(result_table, calculate_mei(c(6,7,8)))
result_table <- rbind(result_table, calculate_mei(c(1:9)))
result_table <- rbind(result_table, calculate_mei(c(10:12,1)))
result_table <- rbind(result_table, calculate_mei(c(11:7)))
result_table <- rbind(result_table, calculate_mei(c(1:7)))
result_table <- rbind(result_table, calculate_mei(c(7:12)))
result_table <- rbind(result_table, calculate_mei(c(8:12)))






##########################################################################################
# 2_AMO

amo$month <- amo$Month
amo$AMO <- as.numeric(as.character(amo$AMO))
amo$month <- as.numeric(amo$month)
amo$Year <- as.numeric(as.character(amo$Year))
amo <- amo %>% mutate(phase = case_when(
  AMO < 0 ~ "-ve",
  AMO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))

result_table_lag_AMO <- data.frame()
calculate_amo <- function(months){ 
  AMO_djf <- amo %>%
    filter(Month %in% months)
  AMO_djf$Year <- as.numeric(as.character(AMO_djf$Year))
  VIL_AMO_DJF <- left_join(AMO_djf, unique_tracks_90, by = "Year")
  VIL_AMO_DJF <- VIL_AMO_DJF[complete.cases(VIL_AMO_DJF),]
  VIL_AMO_DJF$AMO <- as.numeric(as.character(VIL_AMO_DJF$AMO))
  
  
  Lagged_VIL_AMO <- VIL_AMO_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              AMO = mean(AMO)) 
  Lagged_VIL_AMO$Phase = ifelse(Lagged_VIL_AMO$AMO > 0, "+ve", ifelse(Lagged_VIL_AMO$AMO < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_AMO <- replicate(1000, {
    sampled_data <- Lagged_VIL_AMO[sample(nrow(Lagged_VIL_AMO), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_AMO %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_AMO %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_AMO, 0.9)
  percentile_95 <- quantile(differences_lag_AMO, 0.95)
  percentile_10 <- quantile(differences_lag_AMO, 0.10)
  percentile_05 <- quantile(differences_lag_AMO, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_amo <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_amo)
}

# Print the result table
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(12,1,2)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(3,4,5)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(6,7,8)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(1:9)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(9:12,1:4)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(11,12,1:7)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(1:7)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(9:12)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(6:12)))
result_table_lag_AMO <- rbind(result_table_lag_AMO, calculate_amo(c(2:4)))


##########################################################################################
# 3_MJO
mjo$Month <- mjo$month
mjo$MJO <- as.numeric(as.character(mjo$MJO))
mjo$month <- as.numeric(mjo$month)

MJO <- MJO %>% mutate(phase = case_when(
  MJO < 0 ~ "-ve",
  MJO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))

result_table_lag_MJO <- data.frame()
calculate_mjo <- function(months){ 
  MJO_djf <- mjo %>%
    filter(month %in% months)
  MJO_djf$Year <- as.numeric(as.character(MJO_djf$Year))
  VIL_MJO_DJF <- left_join(MJO_djf, unique_tracks_90, by = "Year")
  VIL_MJO_DJF <- VIL_MJO_DJF[complete.cases(VIL_MJO_DJF),]
  VIL_MJO_DJF$MJO <- as.numeric(as.character(VIL_MJO_DJF$MJO))
  
  
  Lagged_VIL_MJO <- VIL_MJO_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              MJO = mean(MJO)) 
  Lagged_VIL_MJO$Phase = ifelse(Lagged_VIL_MJO$MJO > 0, "+ve", ifelse(Lagged_VIL_MJO$MJO < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_MJO <- replicate(1000, {
    sampled_data <- Lagged_VIL_MJO[sample(nrow(Lagged_VIL_MJO), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_MJO %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_MJO %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_MJO, 0.9)
  percentile_95 <- quantile(differences_lag_MJO, 0.95)
  percentile_10 <- quantile(differences_lag_MJO, 0.10)
  percentile_05 <- quantile(differences_lag_MJO, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_mjo <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_mjo)
}

# Print the result table
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(12,1,2)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(3,4,5)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(6,7,8)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(1:9)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(11,12,1:4)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(11,12,1:7)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(1:7)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(9:12)))
result_table_lag_MJO <- rbind(result_table_lag_MJO, calculate_mjo(c(6:12)))

##########################################################################################
# 4_NAO

nao$month <- nao$Month
nao$NAO <- as.numeric(as.character(nao$NAO))
nao$month <- as.numeric(nao$month)
nao$Year <- as.numeric(as.character(nao$Year))
nao <- nao %>% mutate(phase = case_when(
  NAO < 0 ~ "-ve",
  NAO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_NAO <- left_join(nao, unique_tracks_90, by = c("Year", "month"))
VIL_NAO <- VIL_NAO[complete.cases(VIL_NAO), ]

result_table_lag_NAO <- data.frame()
calculate_nao <- function(months){ 
  NAO_djf <- nao %>%
    filter(month %in% months)
  NAO_djf$Year <- as.numeric(as.character(NAO_djf$Year))
  VIL_NAO_DJF <- left_join(NAO_djf, unique_tracks_90, by = "Year")
  VIL_NAO_DJF <- VIL_NAO_DJF[complete.cases(VIL_NAO_DJF),]
  VIL_NAO_DJF$NAO <- as.numeric(as.character(VIL_NAO_DJF$NAO))
  
  
  Lagged_VIL_NAO <- VIL_NAO_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              NAO = mean(NAO)) 
  Lagged_VIL_NAO$Phase = ifelse(Lagged_VIL_NAO$NAO > 0, "+ve", ifelse(Lagged_VIL_NAO$NAO < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_NAO <- replicate(1000, {
    sampled_data <- Lagged_VIL_NAO[sample(nrow(Lagged_VIL_NAO), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_NAO %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_NAO %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_NAO, 0.9)
  percentile_95 <- quantile(differences_lag_NAO, 0.95)
  percentile_10 <- quantile(differences_lag_NAO, 0.10)
  percentile_05 <- quantile(differences_lag_NAO, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_nao <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_nao)
}

# Print the result table
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(12,1,2)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(3,4,5)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(6,7,8)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(1:9)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(11,12,1:4)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(11,12,1:7)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(1:7)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(9:12)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(6:12)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(12,1:4)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(11,12,1:3)))
result_table_lag_NAO <- rbind(result_table_lag_NAO, calculate_nao(c(11,12,1:5)))


##########################################################################################
# 5_PDO

pdo$month <- pdo$Month
pdo$PDO <- as.numeric(as.character(pdo$PDO))
pdo$month <- as.numeric(pdo$month)

pdo <- pdo %>% mutate(phase = case_when(
  PDO < 0 ~ "-ve",
  PDO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))


VIL_PDO <- left_join(pdo, unique_tracks_90, by = c("Year", "month"))
VIL_PDO <- VIL_PDO[complete.cases(VIL_PDO), ]

result_table_lag_PDO <- data.frame()
calculate_pdo <- function(months){ 
  PDO_djf <- pdo %>%
    filter(month %in% months)
  PDO_djf$Year <- as.numeric(as.character(PDO_djf$Year))
  VIL_PDO_DJF <- left_join(PDO_djf, unique_tracks_90, by = "Year")
  VIL_PDO_DJF <- VIL_PDO_DJF[complete.cases(VIL_PDO_DJF),]
  VIL_PDO_DJF$PDO <- as.numeric(as.character(VIL_PDO_DJF$PDO))
  
  
  Lagged_VIL_PDO <- VIL_PDO_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              PDO = mean(PDO)) 
  Lagged_VIL_PDO$Phase = ifelse(Lagged_VIL_PDO$PDO > 0, "+ve", ifelse(Lagged_VIL_PDO$PDO < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_PDO <- replicate(1000, {
    sampled_data <- Lagged_VIL_PDO[sample(nrow(Lagged_VIL_PDO), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_PDO %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_PDO %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_PDO, 0.9)
  percentile_95 <- quantile(differences_lag_PDO, 0.95)
  percentile_10 <- quantile(differences_lag_PDO, 0.10)
  percentile_05 <- quantile(differences_lag_PDO, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_pdo <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_pdo)
}

# Print the result table
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(12,1,2)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(3,4,5)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(6,7,8)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(1:9)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(11,12,1:4)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(11,12,1:7)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(1:7)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(9:12)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(6:12)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(1:3)))
result_table_lag_PDO <- rbind(result_table_lag_PDO, calculate_pdo(c(1:4)))


##########################################################################################
# 6_AO

ao$month <- ao$Month
ao$AO <- as.numeric(as.character(ao$AO))
ao$month <- as.numeric(ao$month)
ao$Year <- as.numeric(as.character(ao$Year))
ao <- ao %>% mutate(phase = case_when(
  AO < 0 ~ "-ve",
  AO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_AO <- left_join(ao, unique_tracks_90, by = c("Year", "month"))
VIL_AO <- VIL_AO[complete.cases(VIL_AO), ]

result_table_lag_AO <- data.frame()
calculate_ao <- function(months){ 
  AO_djf <- ao %>%
    filter(month %in% months)
  AO_djf$Year <- as.numeric(as.character(AO_djf$Year))
  VIL_AO_DJF <- left_join(AO_djf, unique_tracks_90, by = "Year")
  VIL_AO_DJF <- VIL_AO_DJF[complete.cases(VIL_AO_DJF),]
  VIL_AO_DJF$AO <- as.numeric(as.character(VIL_AO_DJF$AO))
  
  
  Lagged_VIL_AO <- VIL_AO_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              AO = mean(AO)) 
  Lagged_VIL_AO$Phase = ifelse(Lagged_VIL_AO$AO > 0, "+ve", ifelse(Lagged_VIL_AO$AO < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_AO <- replicate(1000, {
    sampled_data <- Lagged_VIL_AO[sample(nrow(Lagged_VIL_AO), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_AO %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_AO %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_AO, 0.9)
  percentile_95 <- quantile(differences_lag_AO, 0.95)
  percentile_10 <- quantile(differences_lag_AO, 0.10)
  percentile_05 <- quantile(differences_lag_AO, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_ao <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_ao)
}

# Print the result table
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(12,1,2)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(3,4,5)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(6,7,8)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(1:9)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(11,12,1:4)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(11,12,1:7)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(1:8)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(9:12)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(6:12)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(2:5)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(1:5)))
result_table_lag_AO <- rbind(result_table_lag_AO, calculate_ao(c(3:6)))



##########################################################################################
# 7_PNA

pna$month <- pna$Month
pna$PNA <- as.numeric(as.character(pna$PNA))
pna$month <- as.numeric(pna$month)
pna$Year <- as.numeric(as.character(pna$Year))
pna <- pna %>% mutate(phase = case_when(
  PNA < 0 ~ "-ve",
  PNA >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_PNA <- left_join(pna, unique_tracks_90, by = c("Year", "month"))
VIL_PNA <- VIL_PNA[complete.cases(VIL_PNA), ]
result_table_lag_PNA <- data.frame()
calculate_pna <- function(months){ 
  PNA_djf <- pna %>%
    filter(month %in% months)
  PNA_djf$Year <- as.numeric(as.character(PNA_djf$Year))
  VIL_PNA_DJF <- left_join(PNA_djf, unique_tracks_90, by = "Year")
  VIL_PNA_DJF <- VIL_PNA_DJF[complete.cases(VIL_PNA_DJF),]
  VIL_PNA_DJF$PNA <- as.numeric(as.character(VIL_PNA_DJF$PNA))
  
  
  Lagged_VIL_PNA <- VIL_PNA_DJF %>%
    group_by(Year) %>%
    summarise(max_VIL = median(max_VIL),
              PNA = mean(PNA)) 
  Lagged_VIL_PNA$Phase = ifelse(Lagged_VIL_PNA$PNA > 0, "+ve", ifelse(Lagged_VIL_PNA$PNA < 0, "-ve", "neutral"))
  
  calculate_mean_diff <- function(data) {
    top_chunk <- data %>% sample_n(size = 5)
    bottom_chunk <- data %>% sample_n(size = 5)
    mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
    return(mean_diff)
  }
  
  # Perform the composite analysis and store the differences in a vector
  differences_lag_PNA <- replicate(1000, {
    sampled_data <- Lagged_VIL_PNA[sample(nrow(Lagged_VIL_PNA), replace = TRUE), ]
    calculate_mean_diff(sampled_data)
  })
  
  
  
  # Calculate mean of "Dry" values for specific month combinations
  mean_dry_grid <-  {
    mean_vil <- Lagged_VIL_PNA %>%
      filter(Phase == "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_Dry = mean_vil$mean_vil
    )
  }
  
  # Calculate mean of "All" values for specific month combinations
  mean_all_grid <-  {
    mean_vil <- Lagged_VIL_PNA %>%
      filter(Phase != "+ve") %>%
      summarize(mean_vil = mean(max_VIL))
    data.frame(
      Mean_All = mean_vil$mean_vil
    )
  }
  
  # Calculate the difference in means for each month combination
  mean_diff_grid <-  {
    mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
  }
  
  # Calculate the 90th and 95th percentiles for the differences
  percentile_90 <- quantile(differences_lag_PNA, 0.9)
  percentile_95 <- quantile(differences_lag_PNA, 0.95)
  percentile_10 <- quantile(differences_lag_PNA, 0.10)
  percentile_05 <- quantile(differences_lag_PNA, 0.05)
  
  # Create a new row for the current month combination
  new_row_lag_pna <- data.frame(
    Month_Combination = paste(months, collapse = "-"),
    Mean_Difference = mean_diff_grid,
    Percentile_90 = percentile_90,
    Percentile_95 = percentile_95,
    Percentile_10 = percentile_10,
    Percentile_05 = percentile_05,
    stringsAsFactors = FALSE
  )
  
  return(new_row_lag_pna)
}

# Print the result table
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(12,1,2)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(3,4,5)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(6,7,8)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(1:9)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(11,12,1:4)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(11,12,1:7)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(1:7)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(9:12)))
result_table_lag_PNA <- rbind(result_table_lag_PNA, calculate_pna(c(6:12)))

################################################################################################
## FIND THE RELATIONSHIP BETWEEN VIL AND TELECONNECTION INDEX AND PLOT THE DISTRIBUTION   ##
################################################################################################
############
## 1_ENSO ##
############

# DJF
MEI_djf <- mei %>%
  filter(month %in% c(12,1,2))
VIL_MEI_DJF <- left_join(MEI_djf, unique_tracks_90, by = "Year")
VIL_MEI_DJF <- VIL_MEI_DJF[complete.cases(VIL_MEI_DJF),]
VIL_MEI_DJF_month <- VIL_MEI_DJF %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_MEI = mean(MEI))

# Find the correlation
cor_meidjf <- cor.test(VIL_MEI_DJF_month$mean_max_VIL, VIL_MEI_DJF_month$mean_MEI,
                        method = "pearson")
cor_meidjf$estimate

# Plot the distribution of monthly mean max_VIL and MEI for the concerned months
ggplot(VIL_MEI_DJF_month, aes(x = mean_max_VIL, y = mean_MEI, color = ifelse(mean_MEI > 0.5, "El Nino", ifelse(mean_MEI < -0.5, "La Nina", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "MEI", color = "MEI Range") +
  scale_color_manual(values = c("El Nino" = "blue", "La Nina" = "red", "Neutral" = "grey")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_meidjf$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and DJF-MEI")+
  guides(color = guide_legend(title = "MEI Classification"))


# MAM
MEI_mam <- mei %>%
  filter(month %in% c(3,4,5))
VIL_MEI_MAM <- left_join(MEI_mam, unique_tracks_90, by = "Year")
VIL_MEI_MAM <- VIL_MEI_MAM[complete.cases(VIL_MEI_MAM),]
VIL_MEI_MAM_month <- VIL_MEI_MAM %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_MEI = mean(MEI))

# Find the correlation
cor_meimam <- cor.test(VIL_MEI_MAM_month$mean_max_VIL, VIL_MEI_MAM_month$mean_MEI,
                       method = "pearson")
cor_meimam$estimate

# Plot the distribution of monthly mean max_VIL and MEI for the concerned months
ggplot(VIL_MEI_MAM_month, aes(x = mean_max_VIL, y = mean_MEI, color = ifelse(mean_MEI > 0.5, "El Nino", ifelse(mean_MEI < -0.5, "La Nina", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "MEI", color = "MEI Range") +
  scale_color_manual(values = c("El Nino" = "blue", "La Nina" = "red", "Neutral" = "grey")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_meimam$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and MAM-MEI")+
  guides(color = guide_legend(title = "MEI Classification"))



# JJA
MEI_jja <- mei %>%
  filter(month %in% c(6,7,8))
VIL_MEI_JJA <- left_join(MEI_jja, unique_tracks_90, by = "Year")
VIL_MEI_JJA <- VIL_MEI_JJA[complete.cases(VIL_MEI_JJA),]
VIL_MEI_JJA_month <- VIL_MEI_JJA %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_MEI = mean(MEI))

# Find the correlation
cor_meijja <- cor.test(VIL_MEI_JJA_month$mean_max_VIL, VIL_MEI_JJA_month$mean_MEI,
                       method = "pearson")
cor_meijja$estimate

# Plot the distribution of monthly mean max_VIL and MEI for the concerned months
ggplot(VIL_MEI_JJA_month, aes(x = mean_max_VIL, y = mean_MEI, color = ifelse(mean_MEI > 0.5, "El Nino", ifelse(mean_MEI < -0.5, "La Nina", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "MEI", color = "MEI Range") +
  scale_color_manual(values = c("El Nino" = "blue", "La Nina" = "red", "Neutral" = "grey")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_meijja$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and JJA-MEI")+
  guides(color = guide_legend(title = "MEI Classification"))



# 811
MEI_711 <- mei %>%
  filter(month %in% c(8:11))
VIL_MEI_711 <- left_join(MEI_711, unique_tracks_90, by = "Year")
VIL_MEI_711 <- VIL_MEI_711[complete.cases(VIL_MEI_711),]
VIL_MEI_711_month <- VIL_MEI_711 %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_MEI = mean(MEI))

# Find the correlation
cor_mei711 <- cor.test(VIL_MEI_711_month$mean_max_VIL, VIL_MEI_711_month$mean_MEI,
                       method = "pearson")
cor_mei711$estimate

# Plot the distribution of monthly mean max_VIL and MEI for the concerned months
ggplot(VIL_MEI_711_month, aes(x = mean_max_VIL, y = mean_MEI, color = ifelse(mean_MEI > 0.5, "El Nino", ifelse(mean_MEI < -0.5, "La Nina", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "MEI", color = "MEI Range") +
  scale_color_manual(values = c("El Nino" = "blue", "La Nina" = "red", "Neutral" = "grey")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_mei711$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and August through Novemver-MEI")+
  guides(color = guide_legend(title = "MEI Classification"))

############
## 2_AMO ##
############

# JJA
AMO_jja <- amo %>%
  filter(month %in% c(6,7,8))
VIL_AMO_JJA <- left_join(AMO_jja, unique_tracks_90, by = "Year")
VIL_AMO_JJA <- VIL_AMO_JJA[complete.cases(VIL_AMO_JJA),]
VIL_AMO_JJA_month <- VIL_AMO_JJA %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_AMO = mean(AMO))

# Find the correlation
cor_amojja <- cor.test(VIL_AMO_JJA_month$mean_max_VIL, VIL_AMO_JJA_month$mean_AMO,
                       method = "pearson")
cor_amojja$estimate

# Plot the distribution of monthly mean max_VIL and AMO for the concerned months
ggplot(VIL_AMO_JJA_month, aes(x = mean_max_VIL, y = mean_AMO, color = ifelse(mean_AMO > 0, "Positive AMO", ifelse(mean_AMO < 0, "Negative AMO", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "AMO", color = "AMO Range") +
  scale_color_manual(values = c("Positive AMO" = "blue", "Negative AMO" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_amojja$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and JJA-AMO") +
  guides(color = guide_legend(title = "AMO Classification"))

############
## 3_MJO ##
############

# DJF
MJO_djf <- mjo %>%
  filter(month %in% c(12,1,2))
VIL_MJO_DJF <- left_join(MJO_djf, unique_tracks_90, by = "Year")
VIL_MJO_DJF <- VIL_MJO_DJF[complete.cases(VIL_MJO_DJF),]
VIL_MJO_DJF_month <- VIL_MJO_DJF %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_MJO = mean(MJO))

# Find the correlation
cor_mjodjf <- cor.test(VIL_MJO_DJF_month$mean_max_VIL, VIL_MJO_DJF_month$mean_MJO,
                       method = "pearson")
cor_mjodjf$estimate

# Plot the distribution of monthly mean max_VIL and MJO for the concerned months
ggplot(VIL_MJO_DJF_month, aes(x = mean_max_VIL, y = mean_MJO, color = ifelse(mean_MJO > 0, "Positive MJO", ifelse(mean_MJO < 0, "Negative MJO", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "MJO", color = "MJO Range") +
  scale_color_manual(values = c("Positive MJO" = "blue", "Negative MJO" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_mjodjf$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and DJF-MJO") +
  guides(color = guide_legend(title = "MJO Classification"))


############
## 4_NAO ##
############

# 115
NAO_115 <- nao %>%
  filter(month %in% c(11,12,1:5))
VIL_NAO_115 <- left_join(NAO_115, unique_tracks_90, by = "Year")
VIL_NAO_115 <- VIL_NAO_115[complete.cases(VIL_NAO_115),]
VIL_NAO_115_month <- VIL_NAO_115 %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_NAO = mean(NAO))

# Find the correlation
cor_nao115 <- cor.test(VIL_NAO_115_month$mean_max_VIL, VIL_NAO_115_month$mean_NAO,
                       method = "pearson")
cor_nao115$estimate

# Plot the distribution of monthly mean max_VIL and NAO for the concerned months
ggplot(VIL_NAO_115_month, aes(x = mean_max_VIL, y = mean_NAO, color = ifelse(mean_NAO > 0, "Positive NAO", ifelse(mean_NAO < 0, "Negative NAO", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "NAO", color = "NAO Range") +
  scale_color_manual(values = c("Positive NAO" = "blue", "Negative NAO" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_nao115$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and November Through May-NAO") +
  guides(color = guide_legend(title = "NAO Classification"))


############
## 5_PDO ##
############

# JFM
PDO_JFM <- pdo %>%
  filter(month %in% c(1,2,3))
VIL_PDO_JFM <- left_join(PDO_JFM, unique_tracks_90, by = "Year")
VIL_PDO_JFM <- VIL_PDO_JFM[complete.cases(VIL_PDO_JFM),]
VIL_PDO_JFM_month <- VIL_PDO_JFM %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_PDO = mean(PDO))

# Find the correlation
cor_pdoJFM <- cor.test(VIL_PDO_JFM_month$mean_max_VIL, VIL_PDO_JFM_month$mean_PDO,
                       method = "pearson")
cor_pdoJFM$estimate

# Plot the distribution of monthly mean max_VIL and PDO for the concerned months
ggplot(VIL_PDO_JFM_month, aes(x = mean_max_VIL, y = mean_PDO, color = ifelse(mean_PDO > 0, "Positive PDO", ifelse(mean_PDO < 0, "Negative PDO", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "PDO", color = "PDO Range") +
  scale_color_manual(values = c("Positive PDO" = "blue", "Negative PDO" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_pdoJFM$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and January Through March-PDO") +
  guides(color = guide_legend(title = "PDO Classification"))



############
## 6_AO ##
############

# MAMJ
AO_MAMJ <- ao %>%
  filter(month %in% c(3:6))
VIL_AO_MAMJ <- left_join(AO_MAMJ, unique_tracks_90, by = "Year")
VIL_AO_MAMJ <- VIL_AO_MAMJ[complete.cases(VIL_AO_MAMJ),]
VIL_AO_MAMJ_month <- VIL_AO_MAMJ %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_AO = mean(AO))

# Find the correlation
cor_aoMAMJ <- cor.test(VIL_AO_MAMJ_month$mean_max_VIL, VIL_AO_MAMJ_month$mean_AO,
                       method = "pearson")
cor_aoMAMJ$estimate

# Plot the distribution of monthly mean max_VIL and AO for the concerned months
ggplot(VIL_AO_MAMJ_month, aes(x = mean_max_VIL, y = mean_AO, color = ifelse(mean_AO > 0, "Positive AO", ifelse(mean_AO < 0, "Negative AO", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "AO", color = "AO Range") +
  scale_color_manual(values = c("Positive AO" = "blue", "Negative AO" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_aoMAMJ$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and March Through June-AO") +
  guides(color = guide_legend(title = "AO Classification"))

############
## 7_PNA ##
############

# 912
PNA_912 <- pna %>%
  filter(month %in% c(9,10,11,12))
VIL_PNA_912 <- left_join(PNA_912, unique_tracks_90, by = "Year")
VIL_PNA_912 <- VIL_PNA_912[complete.cases(VIL_PNA_912),]
VIL_PNA_912_month <- VIL_PNA_912 %>%
  group_by(Year, Month) %>%
  summarize(mean_max_VIL = mean(max_VIL),
            mean_PNA = mean(PNA))

# Find the correlation
cor_pna912 <- cor.test(VIL_PNA_912_month$mean_max_VIL, VIL_PNA_912_month$mean_PNA,
                       method = "pearson")
cor_pna912$estimate

# Plot the distribution of monthly mean max_VIL and PNA for the concerned months
ggplot(VIL_PNA_912_month, aes(x = mean_max_VIL, y = mean_PNA, color = ifelse(mean_PNA > 0, "Positive PNA", ifelse(mean_PNA < 0, "Negative PNA", "Neutral")))) +
  geom_point(shape = 16) +
  labs(x = "Monthly Mean max_VIL", y = "PNA", color = "PNA Range") +
  scale_color_manual(values = c("Positive PNA" = "blue", "Negative PNA" = "red")) +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation:", round(cor_pna912$estimate, 2)), hjust = 1, vjust = 1) +
  ggtitle("Scatter Plot of Monthly Mean max_VIL and September Through December-PNA") +
  guides(color = guide_legend(title = "PNA Classification"))

################################################################################################
       ## Regridding the max_VIL data to 0.1*0.1and find the relationship with DJF   ##
################################################################################################
############
## 1_ENSO ##
############
# # Subset data for both scenarios for DJF and for VIL:
# 
# # Subset data for La Nina cases
# la_nina_data <- subset(VIL_MEI_DJF, Phase == "Cool Phase/La Nina")
# 
# # Plot for La Nina cases
# ggplot(la_nina_data, aes(x = Lon, y = Lat, fill = max_VIL)) +
#   geom_tile() +
#   scale_fill_gradient(low = "blue", high = "red") +
#   labs(x = "Longitude", y = "Latitude", fill = "Max VIL") +
#   ggtitle("Max VIL for La Nina Cases")
# 
# # Subset data for El Nino cases
# el_nino_data <- subset(VIL_MEI_DJF, Phase == "Warm Phase/El Nino")
# 
# # Plot for El Nino cases
# ggplot(el_nino_data, aes(x = Lon, y = Lat, fill = max_VIL)) +
#   geom_tile() +
#   scale_fill_gradient(low = "blue", high = "red") +
#   labs(x = "Longitude", y = "Latitude", fill = "Max VIL") +
#   ggtitle("Max VIL for El Nino Cases")
# 
# 
# # Regrid based on 0.1*0.1 and for La Nina Case, the average max_VIL in each grid is
# # Define the grid size
# grid_size <- 0.1
# 
# # Create grid categories based on longitude and latitude
# la_nina_data <- la_nina_data %>%
#   mutate(
#     lon_grid = cut(Lon, breaks = seq(min(Lon), max(Lon) + grid_size, by = grid_size)),
#     lat_grid = cut(Lat, breaks = seq(min(Lat), max(Lat) + grid_size, by = grid_size))
#   )
# 
# # Calculate the average max_VIL values within each grid
# grid_data_l <- la_nina_data %>%
#   group_by(lon_grid, lat_grid) %>%
#   summarize(avg_max_VIL = mean(max_VIL))
# 
# # Plot the grid data
# # Define a custom color palette with 10 colors
# colors <- brewer.pal(10, "YlGnBu")
# ggplot(grid_data_l, aes(x = lon_grid, y = lat_grid, fill = avg_max_VIL)) +
#   geom_tile() +
#   scale_fill_gradientn(colors = colors, values = rescale(c(0, 100)), guide = "legend") +
#   scale_x_discrete(breaks = unique(grid_data_l$lon_grid)[1], labels = unique(grid_data_l$lon_grid)[1]) +
#   scale_y_discrete(breaks = unique(grid_data_l$lat_grid)[1], labels = unique(grid_data_l$lat_grid)[1]) +
#   labs(x = "Longitude", y = "Latitude", fill = "Average Max VIL") +
#   ggtitle("Average Max VIL in Grids for La Nina, DJF")
# 
# # Regrid based on 0.1*0.1 and for El Nino Case, the average max_VIL in each grid is
# # Define the grid size
# grid_size <- 0.1
# 
# # Create grid categories based on longitude and latitude
# el_nino_data <- el_nino_data %>%
#   mutate(
#     lon_grid = cut(Lon, breaks = seq(min(Lon), max(Lon) + grid_size, by = grid_size)),
#     lat_grid = cut(Lat, breaks = seq(min(Lat), max(Lat) + grid_size, by = grid_size))
#   )
# 
# # Calculate the average max_VIL values within each grid
# grid_data_e <- el_nino_data %>%
#   group_by(lon_grid, lat_grid) %>%
#   summarize(avg_max_VIL = mean(max_VIL))
# 
# 
# # Plot the grid data
# # Define a custom color palette with 10 colors
# ggplot(grid_data_e, aes(x = lon_grid, y = lat_grid, fill = avg_max_VIL)) +
#   geom_tile() +
#   scale_fill_gradientn(colors = colors, values = rescale(c(0, 100)), guide = "legend") +
#   scale_x_discrete(breaks = unique(grid_data_e$lon_grid)[1], labels = unique(grid_data_e$lon_grid)[1]) +
#   scale_y_discrete(breaks = unique(grid_data_e$lat_grid)[1], labels = unique(grid_data_e$lat_grid)[1]) +
#   labs(x = "Longitude", y = "Latitude", fill = "Average Max VIL") +
#   ggtitle("Average Max VIL in Grids for El Nino, DJF")
# 
# ############
# ## 2_AMO ##
# ############
# # Subset data for both scenarios for DJF and for VIL:
# 
# # Subset data for +ve cases
# positive_amo_data <- subset(VIL_AMO_DJF, Phase == "+ve")
# 
# # Subset data for -ve cases
# negative_amo_data <- subset(VIL_AMO_DJF, Phase == "-ve")
# 
# 
# 
# # Regrid based on 0.1*0.1 and for -ve and +ve Cases, the average max_VIL in each grid is
# 
# 
# # Create grid categories based on longitude and latitude
# positive_amo_data <- positive_amo_data %>%
#   mutate(
#     lon_grid = cut(Lon, breaks = seq(min(Lon), max(Lon) + grid_size, by = grid_size)),
#     lat_grid = cut(Lat, breaks = seq(min(Lat), max(Lat) + grid_size, by = grid_size))
#   )
# 
# # Calculate the average max_VIL values within each grid
# grid_data_p <- positive_amo_data %>%
#   group_by(lon_grid, lat_grid) %>%
#   summarize(avg_max_VIL = mean(max_VIL))
# 
# # Plot the grid data
# # Define a custom color palette with 10 colors
# colors <- brewer.pal(10, "YlGnBu")
# ggplot(grid_data_p, aes(x = lon_grid, y = lat_grid, fill = avg_max_VIL)) +
#   geom_tile() +
#   scale_fill_gradientn(colors = colors, values = rescale(c(0, 100)), guide = "legend") +
#   scale_x_discrete(breaks = unique(grid_data_p$lon_grid)[1], labels = unique(grid_data_p$lon_grid)[1]) +
#   scale_y_discrete(breaks = unique(grid_data_p$lat_grid)[1], labels = unique(grid_data_p$lat_grid)[1]) +
#   labs(x = "Longitude", y = "Latitude", fill = "Average Max VIL") +
#   ggtitle("Average Max VIL in Grids for Positive AMO, DJF")
# 
# 
# # Create grid categories based on longitude and latitude
# negative_amo_data <- negative_amo_data %>%
#   mutate(
#     lon_grid = cut(Lon, breaks = seq(min(Lon), max(Lon) + grid_size, by = grid_size)),
#     lat_grid = cut(Lat, breaks = seq(min(Lat), max(Lat) + grid_size, by = grid_size))
#   )
# 
# # Calculate the average max_VIL values within each grid
# grid_data_n <- negative_amo_data %>%
#   group_by(lon_grid, lat_grid) %>%
#   summarize(avg_max_VIL = mean(max_VIL))
# 
# 
# # Plot the grid data
# # Define a custom color palette with 10 colors
# ggplot(grid_data_n, aes(x = lon_grid, y = lat_grid, fill = avg_max_VIL)) +
#   geom_tile() +
#   scale_fill_gradientn(colors = colors, values = rescale(c(0, 100)), guide = "legend") +
#   scale_x_discrete(breaks = unique(grid_data_n$lon_grid)[1], labels = unique(grid_data_n$lon_grid)[1]) +
#   scale_y_discrete(breaks = unique(grid_data_n$lat_grid)[1], labels = unique(grid_data_n$lat_grid)[1]) +
#   labs(x = "Longitude", y = "Latitude", fill = "Average Max VIL") +
#   ggtitle("Average Max VIL in Grids for Negative AMO, DJF")

######################################################################################################################
######################################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one MEI for Positive Scenarios ##
######################################################################################################################
######################################################################################################################
# 1_ ENSO
# Create a boxplot of max_VIL for each year
ggplot(VIL_MEI, aes(x = factor(Year), y = max_VIL, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "max_VIL") +
  ggtitle("Boxplots of max_VIL for Each Year")


# Same thing, Create a boxplot of MEI for each year
ggplot(VIL_MEI, aes(x = factor(Year), y = MEI, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "MEI") +
  ggtitle("Boxplots of MEI for Each Year")


# Create histograms of max_VIL distribution and frequencies for each year
ggplot(VIL_MEI, aes(x = max_VIL)) +
  geom_histogram(binwidth = 10, aes(y = ..count..)) +
  labs(x = "max_VIL", y = "Frequency") +
  ggtitle("Histograms of max_VIL distribution and Frequencies for Each Year") +
  facet_wrap(~ Year, ncol = 6)

# Create histograms of MEI distribution and frequencies for each year
ggplot(VIL_MEI, aes(x = month, y = MEI)) +
  geom_point() +
  labs(x = "Month", y = "MEI Value") +
  ggtitle("Scatter Plot of MEI Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of MEI
######
extreme_VIL_MEI <- VIL_MEI %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            MEI = mean(MEI)) %>%
              mutate(phase = case_when(
                MEI < -0.5 ~ "La Nina",
                MEI >= -0.5 & MEI <= 0.5 ~ "Neutral",
                MEI > 0.5 ~ "El Nino"
            ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme <- replicate(1000, {
  sampled_data <- extreme_VIL_MEI[sample(nrow(extreme_VIL_MEI), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme), aes(x = differences_extreme)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_MEI %>%
    filter(phase == "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_MEI %>%
    filter(phase != "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme, 0.9)
percentile_95 <- quantile(differences_extreme, 0.95)
percentile_10 <- quantile(differences_extreme, 0.10)
percentile_05 <- quantile(differences_extreme, 0.05)

# Create a dataframe to store the results
results_extreme_MEI <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of MEI
Average_VIL_MEI <- VIL_MEI %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            MEI = mean(MEI)) %>%
  mutate(phase = case_when(
    MEI < -0.5 ~ "La Nina",
    MEI >= -0.5 & MEI <= 0.5 ~ "Neutral",
    MEI > 0.5 ~ "El Nino"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average <- replicate(1000, {
  sampled_data <- Average_VIL_MEI[sample(nrow(Average_VIL_MEI), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average), aes(x = differences_average)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_MEI %>%
    filter(phase == "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_MEI %>%
    filter(phase != "El Nino") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average, 0.9)
percentile_95 <- quantile(differences_average, 0.95)
percentile_10 <- quantile(differences_average, 0.10)
percentile_05 <- quantile(differences_average, 0.05)

# Create a dataframe to store the results
results_average_MEI <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)
################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one AMO  ##
################################################################################################
# 2_ AMO
amo$month <- amo$Month
amo$AMO <- as.numeric(as.character(amo$AMO))
amo$month <- as.numeric(amo$month)
amo$Year <- as.numeric(as.character(amo$Year))
amo <- amo %>% mutate(phase = case_when(
  AMO < 0 ~ "-ve",
  AMO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))


VIL_AMO <- left_join(amo, unique_tracks_90, by = c("Year", "month"))
VIL_AMO <- VIL_AMO[complete.cases(VIL_AMO), ]

# Create a boxplot of max_VIL for each year
ggplot(VIL_AMO, aes(x = factor(Year), y = max_VIL, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "max_VIL") +
  ggtitle("Boxplots of max_VIL for Each Year")


# Same thing, Create a boxplot of AMO for each year
ggplot(VIL_AMO, aes(x = factor(Year), y = AMO, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "AMO") +
  ggtitle("Boxplots of AMO for Each Year")


# Create histograms of max_VIL distribution and frequencies for each year
ggplot(VIL_AMO, aes(x = max_VIL)) +
  geom_histogram(binwidth = 10, aes(y = ..count..)) +
  labs(x = "max_VIL", y = "Frequency") +
  ggtitle("Histograms of max_VIL distribution and Frequencies for Each Year") +
  facet_wrap(~ Year, ncol = 6)

# Create histograms of AMO distribution and frequencies for each year
ggplot(VIL_AMO, aes(x = month, y = AMO)) +
  geom_point() +
  labs(x = "Month", y = "AMO Value") +
  ggtitle("Scatter Plot of AMO Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of AMO
######
extreme_VIL_AMO <- VIL_AMO %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            AMO = mean(AMO)) %>%
  mutate(phase = case_when(
    AMO < 0 ~ "-ve",
    AMO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_amo <- replicate(1000, {
  sampled_data <- extreme_VIL_AMO[sample(nrow(extreme_VIL_AMO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_amo), aes(x = differences_extreme_amo)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_amo, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_amo, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, AMO")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_AMO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_AMO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_amo, 0.9)
percentile_95 <- quantile(differences_extreme_amo, 0.95)
percentile_10 <- quantile(differences_extreme_amo, 0.10)
percentile_05 <- quantile(differences_extreme_amo, 0.05)

# Create a dataframe to store the results
results_extreme_amo <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of AMO
Average_VIL_AMO <- VIL_AMO %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            AMO = mean(AMO)) %>%
  mutate(phase = case_when(
    AMO < 0 ~ "-ve",
    AMO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_amo <- replicate(1000, {
  sampled_data <- Average_VIL_AMO[sample(nrow(Average_VIL_AMO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_amo), aes(x = differences_average_amo)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_amo, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_amo, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_AMO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_AMO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_amo, 0.9)
percentile_95 <- quantile(differences_average_amo, 0.95)
percentile_10 <- quantile(differences_average_amo, 0.10)
percentile_05 <- quantile(differences_average_amo, 0.05)

# Create a dataframe to store the results
results_average_amo <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)
################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one MJO  ##
################################################################################################
# 3_ MJO
MJO <- MJO %>% mutate(phase = case_when(
  MJO < 0 ~ "-ve",
  MJO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))


VIL_MJO <- left_join(MJO, unique_tracks_90, by = c("Year", "month"))
VIL_MJO <- VIL_MJO[complete.cases(VIL_MJO), ]

# Create a boxplot of MJO for each year
ggplot(VIL_MJO, aes(x = factor(Year), y = MJO, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "MJO") +
  ggtitle("Boxplots of MJO for Each Year")

# Create histograms of MJO distribution and frequencies for each year
ggplot(VIL_MJO, aes(x = month, y = MJO)) +
  geom_point() +
  labs(x = "Month", y = "MJO Value") +
  ggtitle("Scatter Plot of MJO Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of MJO
######
extreme_VIL_MJO <- VIL_MJO %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            MJO = mean(MJO)) %>%
  mutate(phase = case_when(
    MJO < 0 ~ "-ve",
    MJO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_MJO <- replicate(1000, {
  sampled_data <- extreme_VIL_MJO[sample(nrow(extreme_VIL_MJO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_MJO), aes(x = differences_extreme_MJO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_MJO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_MJO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, MJO")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_MJO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_MJO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_MJO, 0.9)
percentile_95 <- quantile(differences_extreme_MJO, 0.95)
percentile_10 <- quantile(differences_extreme_MJO, 0.10)
percentile_05 <- quantile(differences_extreme_MJO, 0.05)

# Create a dataframe to store the results
results_extreme_MJO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of MJO
Average_VIL_MJO <- VIL_MJO %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            MJO = mean(MJO)) %>%
  mutate(phase = case_when(
    MJO < 0 ~ "-ve",
    MJO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_MJO <- replicate(1000, {
  sampled_data <- Average_VIL_MJO[sample(nrow(Average_VIL_MJO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_MJO), aes(x = differences_average_MJO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_MJO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_MJO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_MJO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_MJO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_MJO, 0.9)
percentile_95 <- quantile(differences_average_MJO, 0.95)
percentile_10 <- quantile(differences_average_MJO, 0.10)
percentile_05 <- quantile(differences_average_MJO, 0.05)

# Create a dataframe to store the results
results_average_MJO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one PDO  ##
################################################################################################
# 4_ PDO

pdo$month <- pdo$Month
pdo$PDO <- as.numeric(as.character(pdo$PDO))
pdo$month <- as.numeric(pdo$month)

pdo <- pdo %>% mutate(phase = case_when(
  PDO < 0 ~ "-ve",
  PDO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))


VIL_PDO <- left_join(pdo, unique_tracks_90, by = c("Year", "month"))
VIL_PDO <- VIL_PDO[complete.cases(VIL_PDO), ]

# Create a boxplot of PDO for each year
ggplot(VIL_PDO, aes(x = factor(Year), y = PDO, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "PDO") +
  ggtitle("Boxplots of PDO for Each Year")

# Create histograms of PDO distribution and frequencies for each year
ggplot(VIL_PDO, aes(x = month, y = PDO)) +
  geom_point() +
  labs(x = "Month", y = "PDO Value") +
  ggtitle("Scatter Plot of PDO Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of PDO
######
extreme_VIL_PDO <- VIL_PDO %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            PDO = mean(PDO)) %>%
  mutate(phase = case_when(
    PDO < 0 ~ "-ve",
    PDO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_PDO <- replicate(1000, {
  sampled_data <- extreme_VIL_PDO[sample(nrow(extreme_VIL_PDO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_PDO), aes(x = differences_extreme_PDO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_PDO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_PDO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, PDO")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_PDO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_PDO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_PDO, 0.9)
percentile_95 <- quantile(differences_extreme_PDO, 0.95)
percentile_10 <- quantile(differences_extreme_PDO, 0.10)
percentile_05 <- quantile(differences_extreme_PDO, 0.05)

# Create a dataframe to store the results
results_extreme_PDO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of PDO
Average_VIL_PDO <- VIL_PDO %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            PDO = mean(PDO)) %>%
  mutate(phase = case_when(
    PDO < 0 ~ "-ve",
    PDO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_PDO <- replicate(1000, {
  sampled_data <- Average_VIL_PDO[sample(nrow(Average_VIL_PDO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_PDO), aes(x = differences_average_PDO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_PDO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_PDO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_PDO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_PDO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_PDO, 0.9)
percentile_95 <- quantile(differences_average_PDO, 0.95)
percentile_10 <- quantile(differences_average_PDO, 0.10)
percentile_05 <- quantile(differences_average_PDO, 0.05)

# Create a dataframe to store the results
results_average_PDO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one AO  ##
################################################################################################
# 5_ AO

ao$month <- ao$Month
ao$AO <- as.numeric(as.character(ao$AO))
ao$month <- as.numeric(ao$month)
ao$Year <- as.numeric(as.character(ao$Year))
ao <- ao %>% mutate(phase = case_when(
  AO < 0 ~ "-ve",
  AO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_AO <- left_join(ao, unique_tracks_90, by = c("Year", "month"))
VIL_AO <- VIL_AO[complete.cases(VIL_AO), ]

# Create a boxplot of AO for each year
ggplot(VIL_AO, aes(x = factor(Year), y = AO, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "AO") +
  ggtitle("Boxplots of AO for Each Year")

# Create histograms of AO distribution and frequencies for each year
ggplot(VIL_AO, aes(x = month, y = AO)) +
  geom_point() +
  labs(x = "Month", y = "AO Value") +
  ggtitle("Scatter Plot of AO Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of AO
######
extreme_VIL_AO <- VIL_AO %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            AO = mean(AO)) %>%
  mutate(phase = case_when(
    AO < 0 ~ "-ve",
    AO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_AO <- replicate(1000, {
  sampled_data <- extreme_VIL_AO[sample(nrow(extreme_VIL_AO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_AO), aes(x = differences_extreme_AO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_AO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_AO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, AO")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_AO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_AO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_AO, 0.9)
percentile_95 <- quantile(differences_extreme_AO, 0.95)
percentile_10 <- quantile(differences_extreme_AO, 0.10)
percentile_05 <- quantile(differences_extreme_AO, 0.05)

# Create a dataframe to store the results
results_extreme_AO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of AO
Average_VIL_AO <- VIL_AO %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            AO = mean(AO)) %>%
  mutate(phase = case_when(
    AO < 0 ~ "-ve",
    AO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_AO <- replicate(1000, {
  sampled_data <- Average_VIL_AO[sample(nrow(Average_VIL_AO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_AO), aes(x = differences_average_AO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_AO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_AO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_AO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_AO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_AO, 0.9)
percentile_95 <- quantile(differences_average_AO, 0.95)
percentile_10 <- quantile(differences_average_AO, 0.10)
percentile_05 <- quantile(differences_average_AO, 0.05)

# Create a dataframe to store the results
results_average_AO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)
################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one NAO  ##
################################################################################################
# 6_ NAO

nao$month <- nao$Month
nao$NAO <- as.numeric(as.character(nao$NAO))
nao$month <- as.numeric(nao$month)
nao$Year <- as.numeric(as.character(nao$Year))
nao <- nao %>% mutate(phase = case_when(
  NAO < 0 ~ "-ve",
  NAO >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_NAO <- left_join(nao, unique_tracks_90, by = c("Year", "month"))
VIL_NAO <- VIL_NAO[complete.cases(VIL_NAO), ]

# Create a boxplot of NAO for each year
ggplot(VIL_NAO, aes(x = factor(Year), y = NAO, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "NAO") +
  ggtitle("Boxplots of NAO for Each Year")

# Create histograms of NAO distribution and frequencies for each year
ggplot(VIL_NAO, aes(x = month, y = NAO)) +
  geom_point() +
  labs(x = "Month", y = "NAO Value") +
  ggtitle("Scatter Plot of NAO Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of NAO
######
extreme_VIL_NAO <- VIL_NAO %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            NAO = mean(NAO)) %>%
  mutate(phase = case_when(
    NAO < 0 ~ "-ve",
    NAO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_NAO <- replicate(1000, {
  sampled_data <- extreme_VIL_NAO[sample(nrow(extreme_VIL_NAO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_NAO), aes(x = differences_extreme_NAO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_NAO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_NAO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, NAO")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_NAO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_NAO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_NAO, 0.9)
percentile_95 <- quantile(differences_extreme_NAO, 0.95)
percentile_10 <- quantile(differences_extreme_NAO, 0.10)
percentile_05 <- quantile(differences_extreme_NAO, 0.05)

# Create a dataframe to store the results
results_extreme_NAO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of NAO
Average_VIL_NAO <- VIL_NAO %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            NAO = mean(NAO)) %>%
  mutate(phase = case_when(
    NAO < 0 ~ "-ve",
    NAO >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_NAO <- replicate(1000, {
  sampled_data <- Average_VIL_NAO[sample(nrow(Average_VIL_NAO), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_NAO), aes(x = differences_average_NAO)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_NAO, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_NAO, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_NAO %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_NAO %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_NAO, 0.9)
percentile_95 <- quantile(differences_average_NAO, 0.95)
percentile_10 <- quantile(differences_average_NAO, 0.10)
percentile_05 <- quantile(differences_average_NAO, 0.05)

# Create a dataframe to store the results
results_average_NAO <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

################################################################################################
## Now let's try this approach of taking the one representative value of max_VIL and one PNA  ##
################################################################################################
# 7_ PNA

pna$month <- pna$Month
pna$PNA <- as.numeric(as.character(pna$PNA))
pna$month <- as.numeric(pna$month)
pna$Year <- as.numeric(as.character(pna$Year))
pna <- pna %>% mutate(phase = case_when(
  PNA < 0 ~ "-ve",
  PNA >= 0 ~ "+ve",
  TRUE ~ "Neutral"
))



VIL_PNA <- left_join(pna, unique_tracks_90, by = c("Year", "month"))
VIL_PNA <- VIL_PNA[complete.cases(VIL_PNA), ]

# Create a boxplot of PNA for each year
ggplot(VIL_PNA, aes(x = factor(Year), y = PNA, group = Year)) +
  geom_boxplot() +
  labs(x = "Year", y = "PNA") +
  ggtitle("Boxplots of PNA for Each Year")

# Create histograms of PNA distribution and frequencies for each year
ggplot(VIL_PNA, aes(x = month, y = PNA)) +
  geom_point() +
  labs(x = "Month", y = "PNA Value") +
  ggtitle("Scatter Plot of PNA Value for Each Month across All Years") +
  facet_wrap(~ Year, ncol = 6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
######
# Let's create a new data frame containing the 90th percentile of each year's max_VIL and the mean of PNA
######
extreme_VIL_PNA <- VIL_PNA %>%
  group_by(Year) %>%
  summarise(max_VIL = quantile(max_VIL, 0.9),
            PNA = mean(PNA)) %>%
  mutate(phase = case_when(
    PNA < 0 ~ "-ve",
    PNA >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_extreme_PNA <- replicate(1000, {
  sampled_data <- extreme_VIL_PNA[sample(nrow(extreme_VIL_PNA), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_extreme_PNA), aes(x = differences_extreme_PNA)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_extreme_PNA, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_extreme_PNA, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for extreme values case, PNA")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- extreme_VIL_PNA %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- extreme_VIL_PNA %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_extreme_PNA, 0.9)
percentile_95 <- quantile(differences_extreme_PNA, 0.95)
percentile_10 <- quantile(differences_extreme_PNA, 0.10)
percentile_05 <- quantile(differences_extreme_PNA, 0.05)

# Create a dataframe to store the results
results_extreme_PNA <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

## Now let's do the same for median max_VIL 
# Let's create a new datafram containing the median of each year's max_VIL and the mean of PNA
Average_VIL_PNA <- VIL_PNA %>%
  group_by(Year) %>%
  summarise(max_VIL = median(max_VIL),
            PNA = mean(PNA)) %>%
  mutate(phase = case_when(
    PNA < 0 ~ "-ve",
    PNA >= 0 ~ "+ve",
    TRUE ~ "Neutral"
  ))
calculate_mean_diff <- function(data) {
  top_chunk <- data %>% sample_n(size = 5)
  bottom_chunk <- data %>% sample_n(size = 5)
  mean_diff <- mean(top_chunk$max_VIL) - mean(bottom_chunk$max_VIL)
  return(mean_diff)
}

# Perform the composite analysis and store the differences in a vector
differences_average_PNA <- replicate(1000, {
  sampled_data <- Average_VIL_PNA[sample(nrow(Average_VIL_PNA), replace = TRUE), ]
  calculate_mean_diff(sampled_data)
})

# Plot the distribution of the differences
ggplot(data.frame(differences_average_PNA), aes(x = differences_average_PNA)) +
  geom_density(fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = quantile(differences_average_PNA, 0.05)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(differences_average_PNA, 0.95)), color = "red", linetype = "dashed") +
  labs(x = "Difference in Means", y = "Density") +
  ggtitle("Distribution of Differences between Means of max_VIL for average values case")


# Calculate mean of "Dry" values for specific month combinations
mean_dry_grid <-  {
  mean_vil <- Average_VIL_PNA %>%
    filter(phase == "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_Dry = mean_vil$mean_vil
  )
}

# Calculate mean of "All" values for specific month combinations
mean_all_grid <-  {
  mean_vil <- Average_VIL_PNA %>%
    filter(phase != "+ve") %>%
    summarize(mean_vil = mean(max_VIL))
  data.frame(
    Mean_All = mean_vil$mean_vil
  )
}

# Calculate the difference in means for each month combination
mean_diff_grid <-  {
  mean_all_grid$Mean_All - mean_dry_grid$Mean_Dry
}

# Calculate the 90th and 95th percentiles for the differences
percentile_90 <- quantile(differences_average_PNA, 0.9)
percentile_95 <- quantile(differences_average_PNA, 0.95)
percentile_10 <- quantile(differences_average_PNA, 0.10)
percentile_05 <- quantile(differences_average_PNA, 0.05)

# Create a dataframe to store the results
results_average_PNA <- data.frame(
  Mean_Diff = mean_diff_grid,
  Percentile_90 = percentile_90,
  Percentile_95 = percentile_95,
  Percentile_10 = percentile_10,
  Percentile_05 = percentile_05
)

# Combine the results into a single table
combined_results <- bind_rows(
  results_extreme_MEI %>% mutate(Method = "MEI_Extreme"),
  results_average_MEI %>% mutate(Method = "MEI_Average"),
  results_extreme_amo %>% mutate(Method = "AMO_Extreme"),
  results_average_amo %>% mutate(Method = "AMO_Average"),
  results_extreme_AO %>% mutate(Method = "AO_Extreme"),
  results_average_AO %>% mutate(Method = "AO_Average"),
  results_extreme_MJO %>% mutate(Method = "MJO_Extreme"),
  results_average_MJO %>% mutate(Method = "MJO_Average"),
  results_extreme_NAO %>% mutate(Method = "NAO_Extreme"),
  results_average_NAO %>% mutate(Method = "NAO_Average"),
  results_extreme_PNA %>% mutate(Method = "PNA_Extreme"),
  results_average_PNA %>% mutate(Method = "PNA_Average")
)

# Data: teleconnection index and the corresponding months of significance
teleconnection_data <- data.frame(
  Index = factor(c("MEI", "AMO", "MJO", "NAO", "PDO", "AO", "PNA"), 
                 levels = c("MEI", "AMO", "MJO", "NAO", "PDO", "AO", "PNA")),
  Start_Month = c(8, 6, 12, 11, 1, 3, 9),  # Corresponding start months (numerical)
  End_Month = c(11, 8, 2, 5, 3, 6, 12),    # Corresponding end months (numerical)
  Label = c("August-November", "JJA", "DJF", "Nov-May", "JFM", "MAMJ", "Sept-Dec")
)

# Create a ggplot with the teleconnections and month ranges
ggplot(teleconnection_data) +
  geom_rect(aes(xmin = Start_Month, xmax = End_Month, ymin = as.numeric(Index) - 0.3, 
                ymax = as.numeric(Index) + 0.3, fill = Index), color = "black") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Show month names
  scale_y_discrete(limits = levels(teleconnection_data$Index)) +  # Treat Index as a discrete axis
  scale_fill_brewer(palette = "Set3") +  # Use a nice color palette
  labs(x = "Months", y = "Teleconnection Index", title = "Significant Teleconnection Months") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))


######################

# Updated teleconnection data
teleconnection_data <- data.frame(
  Index = factor(c("MEI", "AMO", "MJO", "NAO", "PDO", "AO", "PNA"), 
                 levels = c("MEI", "AMO", "MJO", "NAO", "PDO", "AO", "PNA")),
  Start_Month = c(8, 6, 1, 11, 1, 3, 9),  # Corresponding start months
  End_Month = c(11, 8, 9, 5, 3, 6, 12),   # Corresponding end months
  Year = c("Prev. Yr", "Prev. Yr", "Prev. Yr", "Prev. Yr", "Curr. Yr", "Curr. Yr", "Prev. Yr")
)

ggplot(teleconnection_data) +
  geom_rect(aes(xmin = Start_Month + ifelse(Year == "Curr. Yr", 12, 0), 
                xmax = End_Month + ifelse(Year == "Curr. Yr", 12, 0), 
                ymin = as.numeric(Index) - 0.3, 
                ymax = as.numeric(Index) + 0.3, 
                fill = Index), color = "black") +
  scale_x_continuous(breaks = 1:21, 
                     labels = c("Jan (Prev. Yr)", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                "Sep", "Oct", "Nov", "Dec", "Jan (Curr. Yr)", "Feb", "Mar", "Apr", 
                                "May", "Jun", "Jul", "Aug", "Sep")) +
  scale_y_discrete(limits = levels(teleconnection_data$Index)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Months", y = "Teleconnection Index", title = "Significant Teleconnection Months Across Two Hail Seasons",
       caption = "January-December corresponds to the preceding year. January-September represents the current hail season. PDO reflects Jan-Mar of the current year.") +
  geom_vline(xintercept = 12.5, linetype = "dashed", color = "gray") +  # Year transition line
  annotate("text", x = 12.7, y = max(as.numeric(teleconnection_data$Index)) + 0.2, 
           label = "Current Hail Season", hjust = 0, color = "gray", size = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),  # Increase x-axis label size
    axis.text.y = element_text(size = 12),  # Increase y-axis label size
    axis.title.x = element_text(size = 14), # Increase x-axis title size
    axis.title.y = element_text(size = 14)  # Increase y-axis title size
  )

