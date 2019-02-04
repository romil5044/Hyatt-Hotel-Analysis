# R Project
install.packages("ggplot2")
library(ggplot2)
install.packages("sqldf")
library(sqldf)


JanDataHyatt<- read.csv("out-201501.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

MarchDataHyatt <- read.csv("out-201403.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

MayDataHyatt <- read.csv("out-201405.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

JulyDataHyatt<- read.csv("out-201407.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

OctDataHyatt<- read.csv("out-201410.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

DecDataHyatt <- read.csv("out-201412.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

#Combining all the data sets into a data set
CompletesixMonthData <-  rbind(JanDataHyatt,MarchDataHyatt,MayDataHyatt,JulyDataHyatt,OctDataHyatt,DecDataHyatt)

colnames(sixMonthData) <-
  c("LengthofCustomerStay","PurposeofVisit","NumberOfRoomsBooked","NightlyRate","CountryOfCustomer",
    "OfferUsedByCustomer", "CustomerMetric_LikelihoodtoRecommend","CustomerMetric_OverallStatisfaction",
    "CustomerMetric_GuestRoomStatisfaction","CustomerMetric_Tranquility","CustomerMetric_ConditionOfHotel",
    "CusotmerMetric_QualityofCustomerService","CustomerMetric_StaffCare","CustomerMetric_InternetSatisfaction",
    "CustomerMetric_CheckInProcessQuality","CustomerMetric_FBOverallExperience","StateOfHotel",
    "CountryOfHotel","LatitudeOfHotel","LongitudeOfHotel","Hotel_NPS_Goals","HotelFlag_BusinessCenter","HotelFlag_Casino",
    "HotelFlag_ConventionSpace","HotelFlag_FitnessCenter","HotelFlag_Resort","HotelFlag_RestaurantOnsite",
    "HotelFlag_ShuttleService","HotelFlag_SPA","CustomerType_NPS","Days_Until_Next_Stay")

View(sixMonthData)

# Creating general promoter count and detractor count plot  
#Plot1 :Creating a plot with count for no. of promoters in each country
ggplot(subset(sixMonthData, CustomerType_NPS =="Promoter"),aes(x=CountryOfHotel)) + 
  geom_bar(col="white",fill="green") + ggtitle("Country wise Promoter Score") + theme(axis.text.x=element_text(angle=90,hjust=1))
labs(x = "State", y = "Promoter Count") 

#Plot2 :Creating a plot with count for no. of detractors in each country
ggplot(subset(sixMonthData, CustomerType_NPS =="Detractor"),aes(x=CountryOfHotel)) + 
  geom_bar(col="white",fill="red") + ggtitle("Country wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + theme(axis.text.x=element_text(angle=90,hjust=1))


#STEP NPS CALCULATION
#Calculating Promoter Percentage, Detractor Percentage, NPS percentage
NPS_file <- sqldf('select count(*) as No_of_promoters ,CountryofHotel from
                  sixMonthData where CustomerType_NPS = "Promoter" Group By CountryofHotel')
View(NPS_file)

NPS_file1 <- sqldf('select count(*) as No_of_detractors,CountryofHotel from 
                   sixMonthData where CustomerType_NPS ="Detractor" Group By CountryofHotel')
View(NPS_file1)

NPS_file2 <- sqldf('Select No_of_promoters,No_of_detractors,a.CountryofHotel from NPS_file a ,
                   NPS_file1 b where a.CountryofHotel = b.CountryofHotel')
View(NPS_file2)


sqldf('insert into NPS_file2 select No_of_promoters, 0,CountryofHotel from NPS_file a 
      where not exists (select * from NPS_file1 b where a.CountryofHotel = b.CountryofHotel)')

NPS_file3 <- sqldf('select a.No_of_promoters, b.No_of_detractors,
                   a.CountryofHotel from NPS_file a LEFT JOIN NPS_file1 b ON a.CountryofHotel = b.CountryofHotel')
View(NPS_file3)

NPS_file4 <- sqldf('select count(*) as No_of_Passive ,CountryofHotel
                   from sixMonthData where CustomerType_NPS = "Passive" Group By
                   CountryofHotel')
View(NPS_file4)

NPS_file5 <- sqldf('select a.No_of_promoters, a.No_of_detractors,
                   a.CountryofHotel,b.No_of_Passive from NPS_file3 a LEFT JOIN NPS_file4
                   b ON a.CountryofHotel = b.CountryofHotel')

View(NPS_file5)

NPS_file5$No_of_promoters[is.na(NPS_file5$No_of_promoters)] <- 0
NPS_file5$No_of_detractors[is.na(NPS_file5$No_of_detractors)] <- 0
NPS_file5$No_of_Passive[is.na(NPS_file5$No_of_Passive)] <- 0

sqldf('select * from NPS_file1 a where not exists (select * from
      NPS_file b where a.CountryofHotel = b.CountryofHotel)')

NPS_file5$Total_Visiters <- NPS_file5$No_of_promoters +
  NPS_file5$No_of_detractors + NPS_file5$No_of_Passive

NPS_file5$Promoter_Percentage <- NPS_file5$No_of_promoters/
  NPS_file5$Total_Visiters * 100

NPS_file5$Detractor_Percentage <- NPS_file5$No_of_detractors/
  NPS_file5$Total_Visiters * 100

NPS_file5$NPS <- NPS_file5$Promoter_Percentage - NPS_file5$Detractor_Percentage


NPS_file5 <- sqldf('select * from NPS_file5 order by Total_Visiters ')
View(NPS_file5)
library(ggplot2)
#Creating a plot of Promoter percentage according to country of hotel
ggplot(NPS_file5,aes(CountryOfHotel,Promoter_Percentage)) + 
  geom_bar(stat="identity",col="Black",fill="light blue") + ggtitle("Country wise Promoter Percentage") + 
  labs(x = "Country ", y = "Promoter Percentage") + theme(axis.text.x=element_text(angle=90,hjust=1))

#Creating a plot of Detractor percentage according to country of hotel
ggplot(NPS_file5,aes(CountryOfHotel,Detractor_Percentage)) + 
  geom_bar(stat="identity",col="Black",fill="red") + ggtitle("Country wise Detractor Percentage") + 
  labs(x = "Country ", y = "Detractor Percentage") + theme(axis.text.x=element_text(angle=90,hjust=1))

#Creating a plot of NPS value according to country of hotel
ggplot(NPS_file5,aes(CountryOfHotel,NPS)) + 
  geom_bar(stat="identity",col="white",fill="dark blue") + ggtitle("Country wise NPS value") + 
  labs(x = "Country ", y = "NPS value") + theme(axis.text.x=element_text(angle=90,hjust=1))

#Creating a scatter plot of NPS value according to country of hotel
ggplot(NPS_file5, aes(NPS,CountryOfHotel)) + geom_point(colour="red",size=2) + ggtitle("Country wise NPS value")+
  theme(axis.text.x = element_text(hjust = 0.5,colour = "Black"))

#Creating a world map with NPS values plotted on the maps
install.packages("rworldmap")
library(rworldmap)
NPS_file5$CountryOfHotel<- gsub("([a-z])([A-Z])", "\\1 \\2",NPS_file5$CountryOfHotel)
Worldmap <- joinCountryData2Map(NPS_file5, joinCode="NAME",
                            nameJoinColumn="CountryOfHotel")
mapCountryData(Worldmap, nameColumnToPlot="NPS",colourPalette = "topo",catMethod="fixedWidth")

#Applying association rule mining to find out interesting relationships that affect the NPS value
#Convert the data type into factors

#Subsetting the data 

AmenitiesData <- subset(sixMonthData,select = c(2,22:29,30))

AmenitiesData$HotelFlag_BusinessCenter <- as.factor(AmenitiesData$HotelFlag_BusinessCenter)
AmenitiesData$HotelFlag_Casino <- as.factor(AmenitiesData$HotelFlag_Casino)
AmenitiesData$HotelFlag_ConventionSpace <- as.factor(AmenitiesData$HotelFlag_ConventionSpace)
AmenitiesData$HotelFlag_FitnessCenter <- as.factor(AmenitiesData$HotelFlag_FitnessCenter)
AmenitiesData$HotelFlag_Resort <- as.factor(AmenitiesData$HotelFlag_Resort)
AmenitiesData$HotelFlag_RestaurantOnsite<- as.factor(AmenitiesData$HotelFlag_RestaurantOnsite)
AmenitiesData$HotelFlag_ShuttleService <- as.factor(AmenitiesData$HotelFlag_ShuttleService)
AmenitiesData$HotelFlag_SPA <- as.factor(AmenitiesData$HotelFlag_SPA)
AmenitiesData$PurposeofVisit<- as.factor(AmenitiesData$PurposeofVisit)
AmenitiesData$CustomerType_NPS<- as.factor(AmenitiesData$CustomerType_NPS)
View(AmenitiesData)

#Importing the packages needed for association mining
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
#Applying the rule
rules_amenities<-apriori(AmenitiesData,parameter=list(support=0.5,confidence= 0.6))
View(rules_amenities)
#Inspect the rules obtained
inspect(rules_amenities)

#plot the rules
plot(rules_amenities)

rulesDF <-data.frame(LHS = labels(lhs(rules_amenities)), RHS = labels(rhs(rules_amenities)),quality(rules_amenities))
View(rulesDF)

rulesDFfinal <- sqldf('select * from rulesDF order by lift ')
View(rulesDFfinal)

rulesPromoter <- rulesDFfinal[rulesDFfinal$RHS=='{CustomerType_NPS=Promoter}',]
rulesPurposeofVisit <- rulesDFfinal[rulesDFfinal$RHS=='{PurposeofVisit=BUSINESS}',]
View(rulesPromoter)
View(rulesPurposeofVisit)

#RANDOM FOREST MODEL

#Subsetting the data 
MetricData <- subset(sixMonthData,select = c(2,7:16,18,30))
View(MetricData)
install.packages("randomForest")
library("randomForest")
MetricData_RF <- MetricData
MetricData_RF$CustomerMetric_LikelihoodtoRecommend <- ifelse(MetricData_RF$CustomerMetric_LikelihoodtoRecommend  >= "6",1,0)
MetricData_RF$CustomerMetric_LikelihoodtoRecommend <- as.character(MetricData_RF$CustomerMetric_LikelihoodtoRecommend )
MetricData_RF$CustomerMetric_LikelihoodtoRecommend  <- as.factor(MetricData_RF$CustomerMetric_LikelihoodtoRecommend)
MetricData_RF$CustomerMetric_LikelihoodtoRecommend[is.na(MetricData_RF$CustomerMetric_LikelihoodtoRecommend)] <- 0
View(MetricData_RF)
output.forest1 <- randomForest(CustomerMetric_LikelihoodtoRecommend ~ CustomerMetric_Tranquility , data = MetricData_RF)


#STEP WORKING ON THE US DATA
#Working on the usdata
uscustomers<- subset(sixMonthData,sixMonthData$CountryOfHotel=='United States')
View(uscustomers)
#Working on the USDATA by promoters
uscustomerspromoters<-subset(sixMonthData,sixMonthData$CountryOfHotel=='United States'& sixMonthData$CustomerType_NPS=='Promoter',)
View(uscustomerspromoters)

#Graph of promoters
ggplot(subset(uscustomerspromoters, CustomerType_NPS =="Promoter"),aes(x=StateOfHotel)) + 
  geom_bar(col="white",fill="dark green") + ggtitle("Country wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + theme(axis.text.x=element_text(angle=90,hjust=1))

#Working on the USDATA by detractors
uscustomersdetractors<-subset(sixMonthData,sixMonthData$CountryOfHotel=='United States'& sixMonthData$CustomerType_NPS=='Detractor',)
View(uscustomersdetractors)

#Graph of detractors
ggplot(subset(uscustomersdetractors, CustomerType_NPS =="Detractor"),aes(x=StateOfHotel)) + 
  geom_bar(col="white",fill="red") + ggtitle("Country wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + theme(axis.text.x=element_text(angle=90,hjust=1))

# Create the	Support Vector Machine (SVM)	model
install.packages("kernlab")
library(kernlab)

# Convert the column of Likelihood to Recommend to factors
sixMonthData$CustomerMetric_LikelihoodtoRecommend <-ifelse(sixMonthData$CustomerMetric_LikelihoodtoRecommend>=6,1,0)
sixMonthData$CustomerMetric_LikelihoodtoRecommend <- as.factor(sixMonthData$CustomerMetric_LikelihoodtoRecommend)	

# Randomly	sample 2/3	data	as a training dataset and the	rest	data # as a test dataset
set.seed(10)
randIndex <- sample(1:dim(sixMonthData)[1])
cutPoint2_3 <- floor(2*dim(sixMonthData)[1]/3)
trainData <- sixMonthData[randIndex[1:cutPoint2_3],]
testData <- sixMonthData[randIndex[(cutPoint2_3+1):dim(sixMonthData)[1]],]
View(testData)

# Apply the function of SVM
svmModel1 <- ksvm(CustomerMetric_LikelihoodtoRecommend ~ HotelFlag_BusinessCenter + HotelFlag_Casino 
                  + HotelFlag_ConventionSpace+ HotelFlag_Resort + HotelFlag_SPA, data = trainData,	kernel="rbfdot",	kpar="automatic",	C=20,	cross=3)	

svmModel1




CountryWiseDetractorDataYearly<- sqldf("select count(CustomerType_NPS),CountryofHotel,CustomerType_NPS from
                                       sixMonthData where CustomerType_NPS = 'Detractor'group by CountryofHotel order by count(CustomerType_NPS) DESC")
View(CountryWiseDetractorDataYearly)
CompletesixMonthData$CustomerType_NPS <-  as.character(CompletesixMonthData$CustomerType_NPS)
CompletesixMonthData$CountryOfHotel<- as.character(CompletesixMonthData$CountryOfHotel)

CompletesixMonthData[is.na(CompletesixMonthData$CustomerType_NPS)] <- " "
CompletesixMonthData[is.na(CompletesixMonthData$CountryOfHotel)] <- " "



Promoterpercentage <- 
#Plot3

#Plot4

#Removing the null values
sixMonthData <- na.omit(sixMonthData)

# Renaming the columns of the dataset





#Us map
install.packages("ggmap")
library(ggmap)
USAMap <- geocode("USA")
USA.map <- get_map(location = USAMap, zoom = 4)
ggmap(USA.map)
mapdata <- subset(uscustomers,uscustomers$CustomerType_NPS != "")
nrow(mapdata)
ggUSA <- gg + geom_point(data = uscustomers, aes(x=uscustomers$LongitudeOfHotel, y=uscustomers$LatitudeOfHotel,colour= uscustomers$CustomerType_NPS))
ggUSA <- ggUSA + ggtitle("NPS Type in USA")
ggUSA
View(uscustomers)
length(uscustomers$LatitudeOfHotel)

LocationDataSetYearly <- subset(sixMonthData,select =c(16:20))
UserInfoDataSetYearly <- subset(sixMonthData,select = c(19,20,1:5,34))


