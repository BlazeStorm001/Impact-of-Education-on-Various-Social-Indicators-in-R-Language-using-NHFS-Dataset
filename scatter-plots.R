library(readxl)
data <- read_excel("D:/Desktop/MFCS dataset/R/h1.xls")
#Preprocessing of data
#Change the name of the columns
colnames(data) <- c("States", "Area", "HouseSurveyed", "Women", "Men", "FemaleAttendSchool", "PopBelow15", "SexRatio", "LiveElec", "LiveSanitation", "WomenLiterate", "MenLiterate", "Women10PlusSch", "Men10PlusSch", "FertilityRate", "InfMortRate", "InstBirthPub", "ChildPolioVac", "MenOb", "WomenOb", "WomenLand", "Voilence", "WomAlc", "MenAlc","ChildFullVac","UnderFiveMortRate")
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
data$UnderFiveMortRate <- as.numeric(data$UnderFiveMortRate)
data$ChildFullVac <- as.numeric(data$ChildFullVac)

#some values become negative after converting from string to numeric
#so make them +ve again

columns_to_make_positive <- c("HouseSurveyed", "Women", "Men", "FemaleAttendSchool", "PopBelow15", "SexRatio", "LiveElec", "LiveSanitation", "WomenLiterate", "MenLiterate", "Women10PlusSch", "Men10PlusSch", "FertilityRate", "InfMortRate", "InstBirthPub", "ChildPolioVac", "MenOb", "WomenOb", "WomenLand", "Voilence", "WomAlc", "MenAlc","ChildFullVac","UnderFiveMortRate")

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

View(data_total)

library(plotly)
fig <- plot_ly(x = data_total$WomenLiterate, y = data_total$SexRatio, z = data_total$FertilityRate, type = "scatter", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Literacy Rate of Women"),
                      yaxis = list(title = "Sex Ratio"),
                      zaxis = list(title = "Fertility Rate")))
fig
fig <- plot_ly(data_total, x = ~WomenLiterate, y = ~SexRatio, z = ~FertilityRate, marker = list(size = 8),color = ~States)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Literacy Rate of Women'),
                                   yaxis = list(title = 'Sex Ratio'),
                                   zaxis = list(title = 'Fertility Rate')))
fig

fig <- plot_ly(data_total, x = ~WomenLiterate, y = ~ChildFullVac, z = ~UnderFiveMortRate, marker = list(size = 8),color = ~States)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Literacy Rate of Women'),
                                   yaxis = list(title = '% Children aged 12-23 months fully vaccinated'),
                                   zaxis = list(title = 'Under five mortality rate')))
fig

