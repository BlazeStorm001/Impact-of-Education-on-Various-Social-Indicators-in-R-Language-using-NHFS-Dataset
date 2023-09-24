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

x <- c("Women","Men")
WomInt <- data[3,4]
WomInt
MenInt <- data[3,5]
MenInt

y <- c(WomInt, MenInt)
y <- unname(y)
str(y)

# Plot the pie chart
plotly::plot_ly(data,values=y,labels=x,
                marker=list(colors=c("deeppink","teal")),
                type="pie") %>% layout(title="% of Men And Women Surveyed")




