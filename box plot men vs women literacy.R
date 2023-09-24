library(readxl)
data <- read_excel("D:/Desktop/MFCS dataset/R/h1.xls")
#Preprocessing of data
#Change the name of the columns
colnames(data) <- c("States", "Area", "HouseSurveyed", "Women", "Men", "FemaleAttendSchool", "PopBelow15", "SexRatio", "LiveElec", "LiveSanitation", "WomenLiterate", "MenLiterate", "Women10PlusSch", "Men10PlusSch", "FertilityRate", "InfMortRate", "InstBirthPub", "ChildPolioVac", "MenOb", "WomenOb", "WomenLand", "Voilence", "WomAlc", "MenAlc")
colnames(data)

#show the datatype of the columns
str(data)

#convert all the non-numeric attributes to numeric ones
data$MenLiterate  <- as.numeric(data$MenLiterate)
data$Men10PlusSch <- as.numeric(data$Men10PlusSch)
data$FertilityRate <- as.numeric(data$FertilityRate)
data$InfMortRate <- as.numeric(data$InfMortRate)
data$InstBirthPub <- as.numeric(data$InstBirthPub)
data$ChildPolioVac <- as.numeric(data$ChildPolioVac)
data$MenOb <- as.numeric(data$MenOb)
data$WomenOb <- as.numeric(data$WomenOb)
data$WomenLand <- as.numeric(data$WomenLand)
data$Voilence <- as.numeric(data$Voilence)

#some values become negative after converting from string to numeric
#so make them +ve again

columns_to_make_positive <- c("HouseSurveyed", "Women", "Men", "FemaleAttendSchool", "PopBelow15", "SexRatio", "LiveElec", "LiveSanitation", "WomenLiterate", "MenLiterate", "Women10PlusSch", "Men10PlusSch", "FertilityRate", "InfMortRate", "InstBirthPub", "ChildPolioVac", "MenOb", "WomenOb", "WomenLand", "Voilence", "WomAlc", "MenAlc")

# Use `lapply` to apply `abs` to the selected columns
data[columns_to_make_positive] <- lapply(data[columns_to_make_positive], abs)

View(data)

#show the five point summary
summary(data)


#urban
data_u <- data[data$Area == "Urban",]

#rural
data_r <- data[data$Area == "Rural",]

#total
data_total <- data[data$Area == "Total",]

library(plotly)
fig <- plot_ly(data_total,y = ~WomenLiterate, type = "box",name="Women",box = list(line = list(color = "deeppink")))
fig <- fig %>% add_trace(data_total,y = ~MenLiterate, name="Men",box = list(line = list(color = "teal")))
fig
