#Import required libraries
library(ggplot2)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ggplot2)
library(plotly)
library(leaflet)

#Part 1
#a -Histogram - Dataset from data.gov.ie - The R basic graphics

BinCollectedData = read.csv("https://data.smartdublin.ie/dataset/50daf22e-453d-4413-ad90-c7fc647bd3da/resource/94dc192e-2a34-4c56-9bcd-d05b9b7a6c47/download/dccwastecollectionbrownbinweightslifts2009to2011p20110917-1551.csv",
                            header = TRUE,  stringsAsFactors = FALSE,sep = ",")
attach(BinCollectedData)
str(BinCollectedData)

hist(BinCollectedData$Lift.count,main = "Brown Bin Collected Per Week Count",xlab = "Lift Count",
     breaks = 20,
     col = "#A05444", xlim = c(0,60000))


#b-Histogram using GGPLOT2 with colour

qplot(BinCollectedData$Lift.count,
      geom="histogram",       #Type of graph
      binwidth=2000,          #Binwidth is width of bar i.e one bar contain 1000 of values
      main = "Histogram for Brown Bin Collected Per Week Count ", #Title for histogram
      xlab = "Lift Count",  #X axis label
      ylab = "Frequency",
      fill=I("#A05444"),    #Colour of bars
      col=I("white"),       #Colour of borders
      alpha=I(0.8),         #Change transperency of bars colours 0-1 in range.
      xlim=c(0,60000))      #Values of x-axis start and end's

#a -Scatter plot - Dataset from data.gov.ie - The R basic graphics

plot(BinCollectedData$Lift.count, BinCollectedData$Average.weight.per.bin,
     col=ifelse(BinCollectedData$Average.weight.per.bin<=24, "Blue", "Red"),
     main = "Scatterplot for Brown Bin Collected Per Week Count",
     xlab = "Lift count", 
     ylab = "Average Weight of bins in kgs",
     axes = TRUE)

abline(lm(Average.weight.per.bin ~ LiftCount, data = BinCollectedData),
       col = "black")

legend(locator(1),title = "Average weight",c("More than 24kgs","Less than 24kgs"),pch = c(1,1), col = c(2,4),cex=0.8, pt.cex = 1)

#a -Scatter plot - Dataset from data.gov.ie - The R ggplot2

ggplot(BinCollectedData, aes(x = Lift.count, y = Average.weight.per.bin))+
       geom_point(color = "#A05444", 
                  size = 2)+
  geom_smooth(method = lm, se = FALSE)



BinCollectedData %>% mutate(Average_Weights = ifelse(BinCollectedData$Average.weight.per.bin > mean(BinCollectedData$Average.weight.per.bin), "More than 24kgs","Less than 24kgs")) %>%

ggplot(aes(x = BinCollectedData$Lift.count, y = BinCollectedData$Average.weight.per.bin, 
                             color = Average_Weights))+
  geom_point()+
  labs(x="Lift Count", y="Average weight of bins in kgs", 
       title = "Scatterplot for Brown Bin Collected Per Week Count")+
  geom_smooth(method = lm, se = FALSE)



#Part 2
#a - Time series plot - using “Second Hand Apartment Prices” datase - The R basic graphics
#Clean dataset by performing below steps before loading into r.
#Delete First row and replace blank with year in first column heading.
#Select all columns except year column and formate cell as number and remove comma seprator in number. 
#If this not done then R taking those column value as character.

SecondHandApartment = read.csv("/Users/ganesh_dhasade/Documents/Applied Cust Analytics (R)/Assignment/Assignment 2/Second hand Appartment Prices.csv",
                            header = TRUE,  stringsAsFactors = FALSE)

#Convert all columns from int into numeric
SecondHandApartment$YEAR <- as.numeric(SecondHandApartment$YEAR)
SecondHandApartment$National <- as.numeric(SecondHandApartment$National)
SecondHandApartment$Dublin <- as.numeric(SecondHandApartment$Dublin)
SecondHandApartment$Cork <- as.numeric(SecondHandApartment$Cork)
SecondHandApartment$Galway <- as.numeric(SecondHandApartment$Galway)
SecondHandApartment$Limerick <- as.numeric(SecondHandApartment$Limerick)
SecondHandApartment$Waterford <- as.numeric(SecondHandApartment$Waterford)
SecondHandApartment$Other.Areas <- as.numeric(SecondHandApartment$Other.Areas)

#Time series plot using R basic commands.

attach(SecondHandApartment)  #Attach dataset

ApartmentDataFrame = subset(SecondHandApartment, select = -c(YEAR) ) #Skip year column from dataframe
ApartmentDataFrame <- ts(ApartmentDataFrame,start=1997,end = 2015) #Assign dataframe with time series object
ts.plot(ApartmentDataFrame,xlab = "Year", ylab = "Price", lty=c(1:7),col=c(1:7),
        main="Second Hand Apartment Prices Nation and countywise")

legend(locator(1),title = "Area",c("Nation","Dublin","Cork","Galway","Limerick","Waterford","Other.Areas"),
                lty = c(1:7),col = c(1:7),cex=0.7, pt.cex = 1)

#b. Time series plot using other R functions which will enable you to produce a more professional and higher
#quality graphic.
SecondHandApartment = read.csv("/Users/ganesh_dhasade/Documents/Applied Cust Analytics (R)/Assignment/Assignment 2/Second hand Appartment Prices.csv",
                               header = TRUE,  stringsAsFactors = FALSE)
attach(SecondHandApartment)
str(SecondHandApartment)
ApartmentDataFrame = subset(SecondHandApartment, select = -c(YEAR) ) #Skip year column from dataframe
ApartmentDataFrame <- ts(ApartmentDataFrame,start=1997,end = 2015)


PlotGraph<-ggplot(SecondHandApartment, aes(x=YEAR))+
  geom_line(aes(y=National,color="Nation"),linetype = 1)+
  geom_line(aes(y=Dublin,color="Dublin"),linetype = 2)+
  geom_line(aes(y=Cork,color="Cork"),linetype = 3)+
  geom_line(aes(y=Galway,color="Galway"),linetype = 4)+
  geom_line(aes(y=Limerick,color="Limerick"),linetype = 5)+
  geom_line(aes(y=Waterford,color="Waterford"),linetype = 6)+
  geom_line(aes(y=Other.Areas,color="Other.Areas"),linetype = 7)+
  xlab("Years") + 
  ylab("Prices")+
  theme(legend.background = element_rect(fill="lightblue", size=0.5, linetype = (1:7)))+
  labs(color="Price") +
  ggtitle("Second Hand Apartment Prices Nation and countywise")

PlotGraph



#Part 3
#Towns in irish county

IrelandTownCordinates = read.csv("/Users/ganesh_dhasade/Documents/Applied Cust Analytics (R)/Assignment/Assignment 2/Irish Towns Co Ordinates.csv",
                               header = TRUE,  stringsAsFactors = FALSE)
attach(IrelandTownCordinates)
SelectedCountyDataFrame <- IrelandTownCordinates[IrelandTownCordinates$county=="Galway",]


greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = SelectedCountyDataFrame) %>% addTiles() %>%
  addMarkers(lng= ~ longitude, 
             lat= ~ latitude, 
             popup = paste("<b>","Name:", SelectedCountyDataFrame$name, "<br>",
                           "Irish Name:", SelectedCountyDataFrame$irish_name,"<br>",
                           "County:", SelectedCountyDataFrame$county, "<br>",
                           "Postal Town:", SelectedCountyDataFrame$postal_town
                           ), 
             label = ~ name,
             icon = greenLeafIcon,
            )











