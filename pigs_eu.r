#Project 10 (PIG POPULATION)

#############################_libraries_##########################

if (!("magick" %in% rownames(installed.packages()))) install.packages("magick")
if (!("here" %in% rownames(installed.packages()))) install.packages("here")
if (!("stringi" %in% rownames(installed.packages()))) install.packages("stringi")
if (!("dplyr" %in% rownames(installed.packages()))) install.packages("dplyr")
if (!("lattice" %in% rownames(installed.packages()))) install.packages("lattice")
if (!("ggplot2" %in% rownames(installed.packages()))) install.packages("ggplot2")
library(magick)
library(here)
library(stringi)
library(dplyr)
library(lattice)
library(ggplot2)

setwd(here())  # Directory
getwd()
Sys.setenv(LANG = "en")

#############################_functions_#############################

# Clearing and loading data #
chrum <- function(func_data, delete_name){
  func_data_file <- paste(func_data,'csv', sep = '.') # paste name to simplify using function
  
  func_data <- read.csv(func_data_file, header = TRUE, sep = ";", skip = 1, encoding = "UTF-8", col.names = c("chrum", 2019:1969)) # loading csv
  func_data[func_data == ": "] <- NA # replacing ":" with NA
  
  func_data_div <- func_data[1] # taking the first column of loaded data
  func_data_names <- data.frame(matrix(NA, nrow = nrow(func_data), ncol = 4)) # creating empty data frame to paste there divided first column
  for (i in 1 : nrow(func_data_names)){ # going through previously created data frame
    func_split <-  stri_split_coll(func_data_div[i, 1], ",") # splitting string after ","
    func_vec <- unlist(func_split) # unlisting previously created strings / creating vector with data
    
    func_data_names[i, ] <- func_vec # pasting created divided string to empty data frame
  }
  
  colnames(func_data_names) <- c("Pig type", "Month", "Unit", "Country") # naming columns in created divided data frame
  func_data <- select(func_data, -c(1)) # deleting first column of data loaded from csv
  func_data <- cbind(func_data_names, func_data, all = TRUE) # pasting data frame with divided strings to first data frame
  
  for (i in 5 : 55){
    func_data[,i] <- gsub('\\D',"", func_data[,i]) # removing letters from some string values
    func_data[,i] <- as.numeric(func_data[,i]) # converting all string values to numeric
  }
  
  for (i in 1 : 4){
    func_data[,i] <- as.character(func_data[,i]) # converting all string values to character
  }
  
  return(as.data.frame(func_data))
}

# Delete rows that contain given string
delete_row <- function(func_data, column, delete_name){
  func_data <- func_data[!grepl(paste(delete_name), column),]
}

# Keep only rows that contain given string
keep_row <- function(func_data, column, keep_name){
  func_data <- func_data[grepl(paste(keep_name), column),]
}

# Create filtered data frames with selected country #
country_pigs <- function(data_name, country){
  new_df <- filter(data_name, Country == country)
  return(new_df)
}

# Max value from chosen year #
max1 <- function(df, column){
  b <- as.data.frame(column, drop=false)
  b <- paste(deparse(substitute(column)),sep="=")
  b <- stri_split_fixed(b, "$")
  b <- unlist(b)[2*(1:length(b))  ]
  
  wartosc <- max(column, na.rm = TRUE)
  vector <- df[which.max(column),]
  return(cat('Most pigs of type', paste(df[1,1]), 'bred in:', paste(vector[4]), 'Year:', paste(b),
             ',from month', paste(vector[2])))
}

# Max value from chosen column #
max2 <- function(column){
  wartosc <- max(column, na.rm = TRUE)
  return(wartosc)
}

# Plotting number of pigs bred in chosen country throughout years #
graph_year_type <- function(df, column){
  b <- as.data.frame(column, drop=false)
  b <- paste(deparse(substitute(column)),sep="=")
  b <- stri_split_fixed(b, "$")
  b <- unlist(b)[2*(1:length(b))  ]
  
  v <- paste('Number of pigs in thousands heads of type ', df[1,1], 'in year', paste(b), 'in EU', sep=' ')
  g <- ggplot(df, aes(x= Country, y= column))
  g1 <- g + labs(title=v, xy="Country", y="Number of pigs", caption="pigs") +
    geom_point(col="darkorange", size=3) + geom_smooth(method="lm", col="forestgreen", size = 1)
  plot(g1)
}

# Plot showing how max number of pigs of given type was changing #
max_plot <- function(df){
  result <- c()
  for (i in 5:55 ){
    c <- (max2(df[,i]))
    result[i-4] <- c   
  }
  plot(c(2019:1969), result, main=paste('Change of max number of pigs bred of type ', df[1,1], 'throughout years'), 
       xlab="Year", ylab="Number of pigs", names.arg=c(2019:1969), type="l", col="orange", lwd=4)
}

# Retrive object name
object_name <- function(object) {
  deparse(substitute(object))
}

# Ilustrated bubble sort from one column (not really useful) #
sort_pigs <- function(column){
  dir_out <- paste(here(),"/gif", sep = '')
  dir.create(dir_out)
  
  b <- as.data.frame(column, drop=false)
  b <- paste(deparse(substitute(column)),sep="=")
  
  vector_sort <- as.vector(column)
  vector_sort <-vector_sort[!is.na(vector_sort)]
  
  lEl <- length(vector_sort) 
  (vOryg <- vector_sort)
  vSort <- vOryg
  
  for (j in 1:(lEl-1)){
    png(paste(here(),"/gif/", sprintf("bubble_%05d.png", j-1), sep = ''), width=1024, height = 1024)
    plot(vSort, 
         col = 2, pch = 19, 
         xlab = '', ylab = '',
         main = paste("Bubble sort of column", b))
    points(vOryg, col = "grey", pch = 21)
    legend("bottomright", xjust = 1, yjust = 0.5,
           c(sprintf("loop no %05d",j-1))) 
    dev.off()
    
    for (i in 1:(lEl-j)){
      if (vSort[i] > vSort[i+1]){
        temp <- vSort[i]
        vSort[i] <- vSort[i+1]
        vSort[i+1] <- temp
      }
    }
  }
  
  imgs <- list.files(dir_out, full.names = TRUE) #######
  img_list <- lapply(imgs, image_read)
  img_joined <- image_join(img_list)
  img_animated <- image_animate(img_joined, fps = 2)
  image_write(image = img_animated,
              path = paste("bubble_sort_animated.gif", sep = ""))
  
  return(cat("Sorted values of chosen column:", vector_sort)) 
}

#############################_loading_data_####################

# All
A3100_all <- chrum('A3100')
A3110_all <- chrum('A3110')
A3120_all <- chrum('A3120')
A3120K_all <- chrum('A3120K')
A3131_all <- chrum('A3131')
A3132_all <- chrum('A3132')
A3132X_all <- chrum('A3132X')
A3132Y_all <- chrum('A3132Y')
A3132Z_all <- chrum('A3132Z')
A3133_all <- chrum('A3133')

# Countries
A3100 <- delete_row(A3100_all, A3100_all$Country, 'EU')
A3110 <- delete_row(A3110_all, A3110_all$Country, 'EU')
A3120 <- delete_row(A3120_all, A3120_all$Country, 'EU')
A3120K <- delete_row(A3120K_all, A3120K_all$Country, 'EU')
A3131 <- delete_row(A3131_all, A3131_all$Country, 'EU')
A3132 <- delete_row(A3132_all, A3132_all$Country, 'EU')
A3132X <- delete_row(A3132X_all, A3132X_all$Country, 'EU')
A3132Y <- delete_row(A3132Y_all, A3132Y_all$Country, 'EU')
A3132Z <- delete_row(A3132Z_all, A3132Z_all$Country, 'EU')
A3133 <- delete_row(A3133_all, A3133_all$Country, 'EU')

# EU
A3100_EU <- keep_row(A3100_all, A3100_all$Country, 'EU')

#############################_filtering_########################

# Filtering types and countries #

# Poland
pl_A3110 <- country_pigs(A3110, 'PL')
pl_A3120 <- country_pigs(A3120, 'PL')
pl_A3120K <- country_pigs(A3120K, 'PL')
pl_A3131 <- country_pigs(A3131, 'PL')
pl_A3132 <- country_pigs(A3132, 'PL')
pl_A3132X <- country_pigs(A3132X, 'PL')
pl_A3132Y <- country_pigs(A3132Y, 'PL')
pl_A3132Z <- country_pigs(A3132Z, 'PL')
pl_A3133 <- country_pigs(A3133, 'PL')

pl_pigs <- rbind(pl_A3110, pl_A3120, pl_A3120K, pl_A3131, pl_A3132, pl_A3132X, pl_A3132Y, pl_A3132Z, pl_A3133) # combining pig types #
remove(pl_A3110, pl_A3120, pl_A3120K, pl_A3131, pl_A3132, pl_A3132X, pl_A3132Y, pl_A3132Z, pl_A3133) # removing to prevent unnecesary clutter #
write.csv(pl_pigs, "pigs_poland.csv") # writing csv file #

# Germany
de_A3110 <- country_pigs(A3110, 'DE')
de_A3120 <- country_pigs(A3120, 'DE')
de_A3120K <- country_pigs(A3120K, 'DE')
de_A3131 <- country_pigs(A3131, 'DE')
de_A3132 <- country_pigs(A3132, 'DE')
de_A3132X <- country_pigs(A3132X, 'DE')
de_A3132Y <- country_pigs(A3132Y, 'DE')
de_A3132Z <- country_pigs(A3132Z, 'DE')
de_A3133 <- country_pigs(A3133, 'DE')

de_pigs <- rbind(de_A3110, de_A3120, de_A3120K, de_A3131, de_A3132, de_A3132X, de_A3132Y, de_A3132Z, de_A3133)
remove(de_A3110, de_A3120, de_A3120K, de_A3131, de_A3132, de_A3132X, de_A3132Y, de_A3132Z, de_A3133)
write.csv(de_pigs, "pigs_germany.csv") 

# Belgium
be_A3100 <- country_pigs(A3100, 'DE')
be_A3110 <- country_pigs(A3110, 'DE')
be_A3120 <- country_pigs(A3120, 'DE')
be_A3120K <- country_pigs(A3120K, 'DE')
be_A3131 <- country_pigs(A3131, 'DE')
be_A3132 <- country_pigs(A3132, 'DE')
be_A3132X <- country_pigs(A3132X, 'DE')
be_A3132Y <- country_pigs(A3132Y, 'DE')
be_A3132Z <- country_pigs(A3132Z, 'DE')
be_A3133 <- country_pigs(A3133, 'DE')

be_pigs <- rbind(be_A3100, be_A3110, be_A3120, be_A3120K, be_A3131, be_A3132, be_A3132X, be_A3132Y, be_A3132Z, be_A3133)
remove(be_A3100, be_A3110, be_A3120, be_A3120K, be_A3131, be_A3132, be_A3132X, be_A3132Y, be_A3132Z, be_A3133)
write.csv(be_pigs, "pigs_belgium.csv") 

# Netherlands
nl_A3100 <-  country_pigs(A3100, 'NL')
nl_A3110 <-  country_pigs(A3110, 'NL')
nl_A3120 <-  country_pigs(A3120, 'NL')
nl_A3120K <-  country_pigs(A3120K, 'NL')
nl_A3131 <-  country_pigs(A3131, 'NL')
nl_A3132 <-  country_pigs(A3132, 'NL')
nl_A3132X <-  country_pigs(A3132X, 'NL')
nl_A3132Y <-  country_pigs(A3132Y, 'NL')
nl_A3132Z <-  country_pigs(A3132Z, 'NL')
nl_A3133 <-  country_pigs(A3133, 'NL')

nl_pigs <- rbind(nl_A3100, nl_A3110, nl_A3120, nl_A3120K, nl_A3131, nl_A3132, nl_A3132X, nl_A3132Y, nl_A3132Z, nl_A3133)
remove(nl_A3100, nl_A3110, nl_A3120, nl_A3120K, nl_A3131, nl_A3132, nl_A3132X, nl_A3132Y, nl_A3132Z, nl_A3133)
write.csv(nl_pigs, "pigs_netherlands.csv") 

#############################_plots_#############################

# # Netherlands

## A3100 ##
nl_pigs_v <- as.character(A3100[101,])
nl_pigs_v <- nl_pigs_v[! nl_pigs_v %in% c("A3100", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v <- as.numeric(nl_pigs_v)

barplot(nl_pigs_v, main="Population of pigs of type A3100 throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v, main="Density of occurrence type A3100 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density",
            border="orange")

## A3110 ##
nl_pigs_v1 <- as.character(A3110[97,])
nl_pigs_v1 <- nl_pigs_v1[! nl_pigs_v1 %in% c("A3110", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v1 <- as.numeric(nl_pigs_v1)

barplot(nl_pigs_v1, main="Population of pigs of type A3110 throughout years in Netherlands",  xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v1, main="Density of occurrence type A3110 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density",
            border="orange")

## A3120 ##
nl_pigs_v2 <- as.character(A3120[95,])
nl_pigs_v2 <- nl_pigs_v2[! nl_pigs_v2 %in% c("A3120", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v2 <- as.numeric(nl_pigs_v2)

barplot(nl_pigs_v2, main="Population of pigs of type A3120 throughout years in Netherlands",  xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v2, main="Density of occurrence type A3120 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density",
            border="orange")

## A3120K ##
nl_pigs_v3 <- as.character(A3120K[96,])
nl_pigs_v3 <- nl_pigs_v3[! nl_pigs_v3 %in% c("A3120K", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v3 <- as.numeric(nl_pigs_v3)

barplot(nl_pigs_v3, main="Population of pigs of type A3120K throughout years in Netherlands",  xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v3, main="Density of occurrence type A3120K in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density",
            border="orange")


## A3131 ##
nl_pigs_v4 <- as.character(A3131[97,])
nl_pigs_v4 <- nl_pigs_v4[! nl_pigs_v4 %in% c("A3131", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v4 <- as.numeric(nl_pigs_v4)

barplot(nl_pigs_v4, main="Population of pigs of type A3131 throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v4, main="Density of occurrence type A3131 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

## A3132 ##
nl_pigs_v5 <- as.character(A3132[97,])
nl_pigs_v5 <- nl_pigs_v5[! nl_pigs_v5 %in% c("A3132", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v5 <- as.numeric(nl_pigs_v5)

barplot(nl_pigs_v5, main="Population of pigs of type A3132 throughout years in Netherlands",  xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v5, main="Density of occurrence type A3132 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

## A3132X ##
nl_pigs_v6 <- as.character(A3132X[97,])
nl_pigs_v6 <- nl_pigs_v6[! nl_pigs_v6 %in% c("A3132X", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v6 <- as.numeric(nl_pigs_v6)

barplot(nl_pigs_v6, main="Population of pigs of type A3132X throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v6, main="Density of occurrence type A3132X in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

## A3132Y ##
nl_pigs_v7 <- as.character(A3132Y[96,])
nl_pigs_v7 <- nl_pigs_v7[! nl_pigs_v7 %in% c("A3132Y", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v7 <- as.numeric(nl_pigs_v7)

barplot(nl_pigs_v7, main="Population of pigs of type A3132Y throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v7, main="Density of occurrence type A3132Y in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

## A3132Z ##
nl_pigs_v8 <- as.character(A3132Z[96,])
nl_pigs_v8 <- nl_pigs_v8[! nl_pigs_v8 %in% c("A3132Z", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v8 <- as.numeric(nl_pigs_v8)

barplot(nl_pigs_v8, main="Population of pigs of type A3132Z throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v8, main="Density of occurrence type A3132Z in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

## A3133 ##
nl_pigs_v9 <- as.character(A3133[95,])
nl_pigs_v9 <- nl_pigs_v9[! nl_pigs_v9 %in% c("A3133", "M12", 'THS_HD', 'SE', 'NL', 'TRUE')]
nl_pigs_v9 <- as.numeric(nl_pigs_v9)

barplot(nl_pigs_v9, main="Population of pigs of type A3133 throughout years in Netherlands", xlab="Year", ylab="Number of pigs (thousands heads)",
        names.arg=c(2019:1969), border="orange", col = heat.colors(length(nl_pigs_v)))

densityplot(nl_pigs_v9, main="Density of occurrence type A3133 in Netherlands", xlab="Number of pigs (thousands heads)", ylab="Density", border="orange")

# Plotting with previously created functions #

#1999
graph_year_type(A3100, A3100$X1999)
graph_year_type(A3110, A3110$X1999)
graph_year_type(A3120, A3120$X1999)
graph_year_type(A3120K, A3120K$X1999)
graph_year_type(A3131, A3131$X1999)
graph_year_type(A3132, A3132$X1999)
graph_year_type(A3132X, A3132X$X1999)
graph_year_type(A3132Y, A3132Y$X1999)
graph_year_type(A3132Z, A3132Z$X1999)
graph_year_type(A3133, A3133$X1999)

#2004
graph_year_type(A3100, A3100$X2004)
graph_year_type(A3110, A3110$X2004)
graph_year_type(A3120, A3120$X2004)
graph_year_type(A3120K, A3120K$X2004)
graph_year_type(A3131, A3131$X2004)
graph_year_type(A3132, A3132$X2004)
graph_year_type(A3132X, A3132X$X2004)
graph_year_type(A3132Y, A3132Y$X2004)
graph_year_type(A3132Z, A3132Z$X2004)
graph_year_type(A3133, A3133$X2004)

#2007
graph_year_type(A3100, A3100$X2007)
graph_year_type(A3110, A3110$X2007)
graph_year_type(A3120, A3120$X2007)
graph_year_type(A3120K, A3120K$X2007)
graph_year_type(A3131, A3131$X2007)
graph_year_type(A3132, A3132$X2007)
graph_year_type(A3132X, A3132X$X2007)
graph_year_type(A3132Y, A3132Y$X2007)
graph_year_type(A3132Z, A3132Z$X2007)
graph_year_type(A3133, A3133$X2007)


#2018
graph_year_type(A3100, A3100$X2018)
graph_year_type(A3110, A3110$X2018)
graph_year_type(A3120, A3120$X2018)
graph_year_type(A3120K, A3120K$X2018)
graph_year_type(A3131, A3131$X2018)
graph_year_type(A3132, A3132$X2018)
graph_year_type(A3132X, A3132X$X2018)
graph_year_type(A3132Y, A3132Y$X2018)
graph_year_type(A3132Z, A3132Z$X2018)
graph_year_type(A3133, A3133$X2018)

#max of every type
max_plot(A3100)
max_plot(A3110)
max_plot(A3120)
max_plot(A3120K)
max_plot(A3131)
max_plot(A3132)
max_plot(A3132X)
max_plot(A3132Y)
max_plot(A3132Z)
max_plot(A3133)

#############################_statistics_###########################

# Information about country with max pigs bred in given year from given type #

# 2019
max1(A3100, A3100$X2019)
max1(A3120, A3110$X2019)
max1(A3120, A3120$X2019)
max1(A3120K, A3120K$X2019)
max1(A3131, A3131$X2019)
max1(A3132, A3132$X2019)
max1(A3132X, A3132X$X2019)
max1(A3132Y, A3132Y$X2019)
max1(A3132Z, A3132Z$X2019)
max1(A3133, A3133$X2019)

# 2018
max1(A3100, A3100$X2018)
max1(A3120, A3110$X2018)
max1(A3120, A3120$X2018)
max1(A3120K, A3120K$X2018)
max1(A3131, A3131$X2018)
max1(A3132, A3132$X2018)
max1(A3132X, A3132X$X2018)
max1(A3132Y, A3132Y$X2018)
max1(A3132Z, A3132Z$X2018)
max1(A3133, A3133$X2018)

#############################_sorting__##########################

# Sorting. Gif exported to script directory #
sort_pigs(nl_pigs$X1998)
sort_pigs(pl_pigs$X2000)

#############################_end_###########################
