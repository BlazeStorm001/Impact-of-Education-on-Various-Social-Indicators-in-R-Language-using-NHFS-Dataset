require("rgdal")  # needed to load shapefiles

# obtain India administrative shapefiles and unzip
download.file("http://biogeo.ucdavis.edu/data/diva/adm/IND_adm.zip", 
              destfile = "IND_adm.zip")
unzip("IND_adm.zip", overwrite = TRUE)

# load shapefiles
india <- readOGR(dsn = "shapes/", "IND_adm1")

# check they've loaded correctly with a plot
plot(india)

# all fine. Let's plot an example variable using ggplot2
require("ggplot2")
require("rgeos")  # for fortify() with SpatialPolygonsDataFrame types

india@data$test <- sample(65000:200000000, size = nrow(india@data),
                          replace = TRUE)

# breaks the shapefile down to points for compatibility with ggplot2
indiaF <- fortify(india, region = "ID_1")
indiaF <- merge(indiaF, india, by.x = "id", by.y = "ID_1")

library(readxl)
data <- read_excel("h1.xls")
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

#map values
#find the heatmap for literacy rate of women
indiaF$test[indiaF$NAME_1 == "Andaman and Nicobar"] <- data_total$WomenLiterate[data_total$States== "Andaman & Nicobar Islands"]
indiaF$test[indiaF$NAME_1 == "Goa"] <- data_total$WomenLiterate[data_total$States== "Goa"]
indiaF$test[indiaF$NAME_1 == "Gujarat"] <- data_total$WomenLiterate[data_total$States== "Gujarat"]
indiaF$test[indiaF$NAME_1 == "Andhra Pradesh"] <- data_total$WomenLiterate[data_total$States== "Andhra Pradesh"]
indiaF$test[indiaF$NAME_1 == "Arunachal Pradesh"] <- data_total$WomenLiterate[data_total$States== "Arunachal Pradesh"]
indiaF$test[indiaF$NAME_1 == "Assam"] <- data_total$WomenLiterate[data_total$States== "Assam"]
indiaF$test[indiaF$NAME_1 == "Bihar"] <- data_total$WomenLiterate[data_total$States== "Bihar"]
indiaF$test[indiaF$NAME_1 == "Chandigarh"] <- data_total$WomenLiterate[data_total$States== "Chandigarh"]
indiaF$test[indiaF$NAME_1 == "Chhattisgarh"] <- data_total$WomenLiterate[data_total$States== "Chhattisgarh"]
indiaF$test[indiaF$NAME_1 == "Dadra and Nagar Haveli"] <- data_total$WomenLiterate[data_total$States== "Dadra and Nagar Haveli & Daman and Diu"]
indiaF$test[indiaF$NAME_1 == "Daman and Diu"] <- data_total$WomenLiterate[data_total$States== "Dadra and Nagar Haveli & Daman and Diu"]
indiaF$test[indiaF$NAME_1 == "Haryana"] <- data_total$WomenLiterate[data_total$States== "Haryana"]
indiaF$test[indiaF$NAME_1 == "Himachal Pradesh"] <- data_total$WomenLiterate[data_total$States== "Himachal Pradesh"]
indiaF$test[indiaF$NAME_1 == "Jammu and Kashmir"] <- data_total$WomenLiterate[data_total$States== "Jammu & Kashmir"]
indiaF$test[indiaF$NAME_1 == "Jharkhand"] <- data_total$WomenLiterate[data_total$States== "Jharkhand"]
indiaF$test[indiaF$NAME_1 == "Karnataka"] <- data_total$WomenLiterate[data_total$States== "Karnataka"]
indiaF$test[indiaF$NAME_1 == "Kerala"] <- data_total$WomenLiterate[data_total$States== "Kerala"]
indiaF$test[indiaF$NAME_1 == "Lakshadweep"] <- data_total$WomenLiterate[data_total$States== "Lakshadweep"]
indiaF$test[indiaF$NAME_1 == "Madhya Pradesh"] <- data_total$WomenLiterate[data_total$States== "Madhya Pradesh"]
indiaF$test[indiaF$NAME_1 == "Maharashtra"] <- data_total$WomenLiterate[data_total$States== "Maharastra"]
indiaF$test[indiaF$NAME_1 == "Manipur"] <- data_total$WomenLiterate[data_total$States== "Manipur"]
indiaF$test[indiaF$NAME_1 == "Meghalaya"] <- data_total$WomenLiterate[data_total$States== "Meghalaya"]
indiaF$test[indiaF$NAME_1 == "Mizoram"] <- data_total$WomenLiterate[data_total$States== "Mizoram"]
indiaF$test[indiaF$NAME_1 == "Nagaland"] <- data_total$WomenLiterate[data_total$States== "Nagaland"]
indiaF$test[indiaF$NAME_1 == "Delhi"] <- data_total$WomenLiterate[data_total$States== "NCT of Delhi"]
indiaF$test[indiaF$NAME_1 == "Orissa"] <- data_total$WomenLiterate[data_total$States== "Odisha"]
indiaF$test[indiaF$NAME_1 == "Puducherry"] <- data_total$WomenLiterate[data_total$States== "Puducherry"]
indiaF$test[indiaF$NAME_1 == "Punjab"] <- data_total$WomenLiterate[data_total$States== "Punjab"]
indiaF$test[indiaF$NAME_1 == "Rajasthan"] <- data_total$WomenLiterate[data_total$States== "Rajasthan"]
indiaF$test[indiaF$NAME_1 == "Sikkim"] <- data_total$WomenLiterate[data_total$States== "Sikkim"]
indiaF$test[indiaF$NAME_1 == "Tamil Nadu"] <- data_total$WomenLiterate[data_total$States== "Tamil Nadu"]
indiaF$test[indiaF$NAME_1 == "Telangana"] <- data_total$WomenLiterate[data_total$States== "Telangana"]
indiaF$test[indiaF$NAME_1 == "Tripura"] <- data_total$WomenLiterate[data_total$States== "Tripura"]
indiaF$test[indiaF$NAME_1 == "Uttar Pradesh"] <- data_total$WomenLiterate[data_total$States== "Uttar Pradesh"]
indiaF$test[indiaF$NAME_1 == "Uttaranchal"] <- data_total$WomenLiterate[data_total$States== "Uttarakhand"]
indiaF$test[indiaF$NAME_1 == "Rajasthan"] <- data_total$WomenLiterate[data_total$States== "Rajasthan"]
indiaF$test[indiaF$NAME_1 == "West Bengal"] <- data_total$WomenLiterate[data_total$States== "West Bengal"]
View(indiaF)
colnames(indiaF)[colnames(indiaF) == "test"] <- "Literacy_Rate"

# plots the polygon and fills them with the value of 'test'
ggplot() +
  geom_polygon(data = indiaF, aes(x = long, y = lat, group = group,
                                  fill = Literacy_Rate)) + scale_fill_gradient(low = "red", high = "green")+
coord_equal()

#map values
#find the heatmap for literacy rate of men
indiaF$Literacy_Rate[indiaF$NAME_1 == "Andaman and Nicobar"] <- data_total$MenLiterate[data_total$States== "Andaman & Nicobar Islands"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Goa"] <- data_total$MenLiterate[data_total$States== "Goa"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Gujarat"] <- data_total$MenLiterate[data_total$States== "Gujarat"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Andhra Pradesh"] <- data_total$MenLiterate[data_total$States== "Andhra Pradesh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Arunachal Pradesh"] <- data_total$MenLiterate[data_total$States== "Arunachal Pradesh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Assam"] <- data_total$MenLiterate[data_total$States== "Assam"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Bihar"] <- data_total$MenLiterate[data_total$States== "Bihar"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Chandigarh"] <- data_total$MenLiterate[data_total$States== "Chandigarh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Chhattisgarh"] <- data_total$MenLiterate[data_total$States== "Chhattisgarh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Dadra and Nagar Haveli"] <- data_total$MenLiterate[data_total$States== "Dadra and Nagar Haveli & Daman and Diu"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Daman and Diu"] <- data_total$MenLiterate[data_total$States== "Dadra and Nagar Haveli & Daman and Diu"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Haryana"] <- data_total$MenLiterate[data_total$States== "Haryana"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Himachal Pradesh"] <- data_total$MenLiterate[data_total$States== "Himachal Pradesh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Jammu and Kashmir"] <- data_total$MenLiterate[data_total$States== "Jammu & Kashmir"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Jharkhand"] <- data_total$MenLiterate[data_total$States== "Jharkhand"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Karnataka"] <- data_total$MenLiterate[data_total$States== "Karnataka"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Kerala"] <- data_total$MenLiterate[data_total$States== "Kerala"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Lakshadweep"] <- data_total$MenLiterate[data_total$States== "Lakshadweep"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Madhya Pradesh"] <- data_total$MenLiterate[data_total$States== "Madhya Pradesh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Maharashtra"] <- data_total$MenLiterate[data_total$States== "Maharastra"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Manipur"] <- data_total$MenLiterate[data_total$States== "Manipur"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Meghalaya"] <- data_total$MenLiterate[data_total$States== "Meghalaya"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Mizoram"] <- data_total$MenLiterate[data_total$States== "Mizoram"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Nagaland"] <- data_total$MenLiterate[data_total$States== "Nagaland"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Delhi"] <- data_total$MenLiterate[data_total$States== "NCT of Delhi"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Orissa"] <- data_total$MenLiterate[data_total$States== "Odisha"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Puducherry"] <- data_total$MenLiterate[data_total$States== "Puducherry"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Punjab"] <- data_total$MenLiterate[data_total$States== "Punjab"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Rajasthan"] <- data_total$MenLiterate[data_total$States== "Rajasthan"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Sikkim"] <- data_total$MenLiterate[data_total$States== "Sikkim"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Tamil Nadu"] <- data_total$MenLiterate[data_total$States== "Tamil Nadu"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Telangana"] <- data_total$MenLiterate[data_total$States== "Telangana"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Tripura"] <- data_total$MenLiterate[data_total$States== "Tripura"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Uttar Pradesh"] <- data_total$MenLiterate[data_total$States== "Uttar Pradesh"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Uttaranchal"] <- data_total$MenLiterate[data_total$States== "Uttarakhand"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "Rajasthan"] <- data_total$MenLiterate[data_total$States== "Rajasthan"]
indiaF$Literacy_Rate[indiaF$NAME_1 == "West Bengal"] <- data_total$MenLiterate[data_total$States== "West Bengal"]
View(indiaF)

# plots the polygon and fills them with the value of 'test'
ggplot() +
  geom_polygon(data = indiaF, aes(x = long, y = lat, group = group,
                                  fill = Literacy_Rate)) + scale_fill_gradient(low = "red", high = "green")+
  coord_equal()

#finding the bar graph of areawise avg literacy
library(plotly)
avgLitMenTotal <- mean(data_total$MenLiterate)
avgLitMenRural <- mean(na.omit(data_r$MenLiterate))

avgLitMenUrban <- mean(data_u$MenLiterate)

avgLitWomenTotal <- mean(data_total$WomenLiterate)
avgLitWomenRural <- mean(data_r$WomenLiterate)
avgLitWomenUrban <- mean(data_u$WomenLiterate)

wmen <- c(avgLitWomenTotal,avgLitWomenRural,avgLitWomenUrban)
wmen <- unname(wmen)
men <- c(avgLitMenTotal,avgLitMenRural,avgLitMenUrban)
men <- unname(men)

fig <- plot_ly(data_total, x = c("Overall","Rural","Urban"), y = wmen, type = 'bar', name = 'Women',marker = list(color = 'deeppink'))
fig <- fig %>% add_trace(y = men, name = 'Men',marker = list(color = 'teal'))
fig <- fig %>% layout(yaxis = list(title = 'Mean Literacy Rates By Area'), barmode = 'group')
fig

#finding the statewise literacy rates of men and women overall
fig <- plot_ly(data_total, x = ~States, y = ~WomenLiterate, type = 'bar', name = 'Women',marker = list(color = 'deeppink'))
fig <- fig %>% add_trace(y = ~MenLiterate, name = 'Men',marker = list(color = 'teal'))
fig <- fig %>% layout(yaxis = list(title = 'Literacy Rate'), barmode = 'group')
fig

#finding the statewise literacy rates of men and women in rural only
fig <- plot_ly(data_r, x = ~States, y = ~WomenLiterate, type = 'bar', name = 'Women',marker = list(color = 'deeppink'))
fig <- fig %>% add_trace(y = ~MenLiterate, name = 'Men',marker = list(color = 'teal'))
fig <- fig %>% layout(yaxis = list(title = 'Literacy Rate'), barmode = 'group')
fig

#finding the statewise literacy rates of men and women in urban only
fig <- plot_ly(data_u, x = ~States, y = ~WomenLiterate, type = 'bar', name = 'Women',marker = list(color = 'deeppink'))
fig <- fig %>% add_trace(y = ~MenLiterate, name = 'Men',marker = list(color = 'teal'))
fig <- fig %>% layout(yaxis = list(title = 'Literacy Rate'), barmode = 'group')
fig

#pie chart for no. of men and women interviewed
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

#fertility rate, sex ratio and women literacy rate scatter plot

fig <- plot_ly(data_total, x = ~WomenLiterate, y = ~SexRatio, z = ~FertilityRate, marker = list(size = 8),color = ~States)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Literacy Rate of Women'),
                                   yaxis = list(title = 'Sex Ratio'),
                                   zaxis = list(title = 'Fertility Rate')))
fig

#mortality rate, child vaccination rate and women literacy rate scatter plot

fig <- plot_ly(data_total, x = ~WomenLiterate, y = ~ChildFullVac, z = ~UnderFiveMortRate, marker = list(size = 8),color = ~States)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Literacy Rate of Women'),
                                   yaxis = list(title = '% Children aged 12-23 months fully vaccinated'),
                                   zaxis = list(title = 'Under five mortality rate')))
fig

#box plot of men and women literacy rate
fig <- plot_ly(data_total,y = ~WomenLiterate, type = "box",name="Women",box = list(line = list(color = "deeppink")))
fig <- fig %>% add_trace(data_total,y = ~MenLiterate, name="Men",box = list(line = list(color = "teal")))
fig

