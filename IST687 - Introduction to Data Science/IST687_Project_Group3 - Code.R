# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

#Download and install tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Downlod and install jsonlite library
#install.packages("jsonlite")
library(jsonlite)

#Downlod and install ggplot2 library
#install.packages("ggplot2")
library(ggplot2)

#Download and Install ggmap
#install.packages("maps")
library(maps)

#Download and install arules package
#install.packages("arules")
library(arules)

#download and install arulesViz package
#install.packages("arulesViz")
library(arulesViz)

#Load data from json into datafram
df<-jsonlite::fromJSON("fall2019-survey-M04.json")

#Explore structure of imported data
#str(df)
#summary(df)

check_na<-function(df){
  #Excute for loop to identify where NAs are present in data set
  for (Var in names(df)) {
    #Sum number of elements that are NA
    missing <- sum(is.na(df[,Var]))
    #If sum of NAs is greater than zero, print the column name to identify for processing
    if (missing > 0) {
      print(c(Var,missing))
    }
  }
}

#Function to create groups by quartile, stores in a new column, and converts to a factor.  It also creates labels based on the calculated quartile boundaries to make it more intuitive during later analysis
QuartileGrouping<-function(columnName,newColumnName){
  #Creates group less than 25 percentile
  newColumnName[columnName <= quantile(columnName, probs=0.25, na.rm=TRUE)]<-paste("<=",quantile(columnName, probs=0.25, na.rm=TRUE)) 
  #Createsgroup between 25 and 50 percentile
  newColumnName[columnName > quantile(columnName, probs=0.25, na.rm=TRUE) & columnName <= quantile(columnName, probs=0.5, na.rm=TRUE)]<-paste(quantile(columnName, probs=0.25, na.rm=TRUE),"-",quantile(columnName, probs=0.5, na.rm=TRUE))
  #Creates group betwen 50 and 75 percentile
  newColumnName[columnName> quantile(columnName, probs=0.5, na.rm=TRUE) & columnName <=quantile(columnName, probs=0.75, na.rm=TRUE)]<-paste(quantile(columnName, probs=0.5, na.rm=TRUE),"-",quantile(columnName, probs=0.75, na.rm=TRUE))
  #Creates group greater than 75 percentile
  newColumnName[columnName >quantile(columnName, probs=0.75, na.rm=TRUE)]<-paste(">",quantile(columnName, probs=0.75, na.rm=TRUE))
  #Converts column to factor
  newColumnName<-as.factor(newColumnName)
  return(newColumnName)
}

clean_data<-function(df){
  #Convert approriate variables to factors
  df$Destination.State<-as.factor(df$Destination.State)
  df$Origin.State<-as.factor(df$Origin.State)
  df$Airline.Status<-as.factor(df$Airline.Status)
  df$Type.of.Travel<-as.factor(df$Type.of.Travel)
  df$Class<-as.factor(df$Class)
  df$Flight.cancelled<-as.factor(df$Flight.cancelled)
  df$Partner.Code<-as.factor(df$Partner.Code)
  df$Gender<-as.factor(df$Gender)
  df$Price.Sensitivity<-as.factor(df$Price.Sensitivity)
  df$Total.Freq.Flyer.Accts<-as.factor(df$Total.Freq.Flyer.Accts)
  #df$Likelihood.to.recommend<-as.factor(df$Likelihood.to.recommend)

  #Remove State Abbreviations from destination and origin cit fields. Data is already caputured in another field
  df$Origin.City<-gsub(", ..","",df$Origin.City)
  df$Destination.City<-gsub(", ..","",df$Destination.City)
  
  #If flight not cancelled and no arrival delay, replace with 0 -> assume it arrived on-time
  df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="No"]<-0
  
  #create new dataframe where flight time was missing but flight was not cancelled
  missingflighttime <- df%>%
    filter(is.na(df$Flight.time.in.minutes)&df$Flight.cancelled=="No")
  
  #Calculate average flight time from completed segments between city pairs to fill in applicable missing flight times
  #Iterate through origin cities with missing flight times
  for (var1 in unique(missingflighttime$Origin.City)){
    #Iterate through destination cities with missing flight times
    for (var2 in unique(missingflighttime$Destination.City)){
      #Calculate mean of know flight times between city pairs (origin to destination OR destination to origin) to replace NAs
      df$Flight.time.in.minutes[df$Origin.City==var1&df$Destination.City==var2&df$Flight.cancelled=="No"]<-mean(df$Flight.time.in.minutes[(df$Origin.City==var1&df$Destination.City==var2)|(df$Origin.City==var2&df$Destination.City==var1)], na.rm=TRUE)
    }
  }
  #Replace delay times in flights that were cancelled with NAs to make removing for calculation easier
  df$Departure.Delay.in.Minutes[df$Flight.cancelled=="Yes"]<-NA
  
  #Convert Flight.Date to date format field instead of chr
  df$Flight.date<-as.Date(df$Flight.date,"%m/%d/%y")
  
  #Add day of the week factor calculated from flight date
  df$Day.of.Week<-as.factor(weekdays(df$Flight.date))
  
  #Add month of year factor calculated from flight date
  df$Month.of.Year<-as.factor(months(df$Flight.date))
  
  #Create groups of arrival delay in 15 min increments and store as factor in new column.
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes <= 15]<-"0-15"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >15 & df$Arrival.Delay.in.Minutes <=30 ]<-"16-30"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >30 & df$Arrival.Delay.in.Minutes <=45 ]<-"31-45"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >45 & df$Arrival.Delay.in.Minutes <=60 ]<-"46-60"
  df$Arrival.Delay.Group[df$Arrival.Delay.in.Minutes >60]<-">60"
  df$Arrival.Delay.Group<-as.factor(df$Arrival.Delay.Group)
  
  #Create groups of departure delay in 15 min increments and store as factor in new column.
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes <= 15]<-"0-15"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>15 & df$Departure.Delay.in.Minutes <=30 ]<-"16-30"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>30 & df$Departure.Delay.in.Minutes <=45 ]<-"31-45"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes>45 & df$Departure.Delay.in.Minutes <=60 ]<-"46-60"
  df$Departure.Delay.Group[df$Departure.Delay.in.Minutes >60]<-">60"
  df$Departure.Delay.Group<-as.factor(df$Departure.Delay.Group)
  
  #df$Departure.Delay.Group<-QuartileGrouping(df$Departure.Delay.in.Minutes,df$Departure.Delay.Group)
  
  #df$Arrival.Delay.Group<-QuartileGrouping(df$Arrival.Delay.in.Minutes,df$Arrival.Delay.Group)

  #Create groups of flights times based on quartiles of data
  df$Flight.Time.Group<-QuartileGrouping(df$Flight.time.in.minutes,df$Flight.Time.Group)
  
  #Create age groupings based on quartiles of data
  df$ageGroup<-QuartileGrouping(df$Age,df$ageGroup)
  
  #Create year of first flight groupings based on quartiles of data
  df$yearOfFirstFlightGroup<-QuartileGrouping(df$Year.of.First.Flight,df$yearOfFirstFlightGroup)
  
  #Create eating spending groupings based on quartiles of data
  df$eatingGroup<-QuartileGrouping(df$Eating.and.Drinking.at.Airport,df$eatingGroup)
  
  #Create shopping amount groupings based on quartiles of data
  df$shoppingGroup<-QuartileGrouping(df$Shopping.Amount.at.Airport,df$shoppingGroup)
  
  #Create loyalty groupings based on quartiles of data
  df$loyaltyGroup<-QuartileGrouping(df$Loyalty,df$loyaltyGroup)
  
  #Create flights per year groupings based on quartiles of data
  df$flightsperyearGroup<-QuartileGrouping(df$Flights.Per.Year,df$flightsperyearGroup)
  
  #Create depature hour groupings based on quartiles of data
  df$departurehourGroup<-QuartileGrouping(df$Scheduled.Departure.Hour,df$departurehourGroup)
  
  
  #Create Groups of Promoters, Passives and Detractors
  df$Promoter.Score[df$Likelihood.to.recommend==9|df$Likelihood.to.recommend==10]<-"Promoter"
  df$Promoter.Score[df$Likelihood.to.recommend==7 | df$Likelihood.to.recommend==8]<-"Passive"
  df$Promoter.Score[df$Likelihood.to.recommend < 7]<-"Detractor"
  df$Promoter.Score<-as.factor(df$Promoter.Score)
  
  
  
  return(df)
}

# Determine locations of NAs 
#check_na(df)

#Clean data frame using define function
df<-clean_data(df)

#Convert Dest and Orgin Cities to Factors (for some reason does not work in fucntion - it returns chr variables)
df$Destination.City<-as.factor(df$Destination.City)
df$Origin.City<-as.factor(df$Origin.City)

# Confirm cleaning operations (change to factors) 
str(df)
# Look at statistics of dataframe
summary(df)
#Confim elimination of NAs
#check_na(df)

#Confirm if all missing departure delay minutes are associated with a cancelled flight
#length(df$Departure.Delay.in.Minutes[is.na(df$Departure.Delay.in.Minutes) & df$Flight.cancelled=="Yes"])
#Confirm if all missing arrival delay minutes are associated with a cancelled flight
#length(df$Arrival.Delay.in.Minutes[is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="Yes"])

#Create histogram of ages of passengers across all partners
agehisto<-ggplot(df)+
  aes(x=Age)+
  #Specify histogram geomtry and define bin numbers
  geom_histogram(bins=5,color="black", fill="white")+
  #Add title
  ggtitle("Distribution of ages")
#Display age plot
agehisto

#Create box plot of ages seperated by promoter rating
agebox<-ggplot(df)+
  #Seperate by promoter rating with stats from age
  aes(x=Promoter.Score,y=Age,fill=Promoter.Score)+
  #Specify box plot geometry
  geom_boxplot()+
  #Add title
  ggtitle("Distribution of ages")
#Display age box plot
agebox

#Create histogram of gender
genderhisto<-ggplot(df)+
  aes(x=Gender)+
  #Specify histrogram geometry
  geom_histogram(stat = "count", color="black", fill="white")+
  # Add axis labels
  labs(x="Airline Partner",y="Number of Passengers")+
  # Adjust axis text to be readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  #Add title
  ggtitle("Distribution of Male and Female Passengers")
#Dispaly gender histogram
genderhisto

#Create box plot of likelihood to recommend seperated by gender
genderbox<-ggplot(df)+
  aes(x=Gender, y=Likelihood.to.recommend)+
  #Specify box plot geomtry
  geom_boxplot()
#Display gender box plot
genderbox

loyaltybox<-ggplot(df)+
  aes(x=Promoter.Score, y=Loyalty,fill=Promoter.Score)+
  geom_boxplot()
loyaltybox

accountsbox<-ggplot(df)+
  aes(x=Promoter.Score, y=as.numeric(Total.Freq.Flyer.Accts),fill=Promoter.Score)+
  geom_boxplot()
accountsbox

pricebox<-ggplot(df)+
  aes(x=Promoter.Score,y=as.numeric(Price.Sensitivity),fill=Promoter.Score)+
  geom_boxplot()
pricebox

partnerhisto<-ggplot(df)+
  aes(x=Partner.Name)+
  geom_histogram(stat = "count", color="black", fill="white")+
  labs(x="Airline Partner",y="Number of Passengers")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  ggtitle("Distribution of Passengers per partner")
partnerhisto

partnerbox<-ggplot(df)+
  aes(x=Partner.Name,y=Likelihood.to.recommend)+
  geom_boxplot()+
  labs(x="Airline Partner",y="Likelihood to Recommend")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  ggtitle("Distribution of Likelihood to Recommend by Partner")
partnerbox

typehisto<-ggplot(df)+
  aes(x=Type.of.Travel)+
  geom_histogram(stat = "count", color="black", fill="white")+
  labs(x="Type of Travel",y="Passenger Count")+
  ggtitle("Distribution of Travel Types")
typehisto

statushisto<-ggplot(df)+
  aes(x=Airline.Status)+
  geom_histogram(stat = "count", color="black", fill="white")+
  ggtitle("Distribution of Airline Status")
statushisto


arrdelayhisto<-ggplot(df)+
  aes(x=Arrival.Delay.Group)+
  geom_histogram(stat = "count", color="black", fill="white")+
  ggtitle("Distribution of arrival delay in minutes")
arrdelayhisto
str(df)

flightdurationhisto<-ggplot(df)+
  aes(x=Flight.time.in.minutes)+
  geom_histogram(bins =10,color="black", fill="white")+
  ggtitle("Distribution flight time in minutes")
flightdurationhisto

summary(df$Eating.and.Drinking.at.Airport)
eathisto<-ggplot(df)+
  aes(x=Eating.and.Drinking.at.Airport)+
  geom_histogram(bins=5,color="black", fill="white")+
  ggtitle("Distribution of money spent on eating and drinking")
eathisto

NPShisto<-ggplot(df)+
  aes(x=Promoter.Score)+
  geom_histogram(stat="count",color="black", fill="white")+
  ggtitle("Promoter/Passive/Detractor Numbers")
NPShisto

################################################################################

#Store passenger threshold value as variable
passthreshold<-750

#Create tibble to display number of passengers per partner.  Used to create passenger threshold for association rules mining
partnerpasscount<-df%>%
  group_by(Partner.Code,Partner.Name)%>%
  summarise(count = n())
partnerpasscount

#Create table of number of promoters/detractors/passive per partner
rawPromoter<-table(df$Partner.Name,df$Promoter.Score)
rawPromoter
#Convert table to data fram for plotting
rawPromoterdf<-as.data.frame(rawPromoter)
#https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
#Create stacked bar chart visualizing table of number of promoters/detractors/passive per partner in decending order
ggplot(rawPromoterdf, aes(fill=Var2, y=Freq, x=reorder(Var1, -Freq))) + 
  geom_bar(position="stack", stat="identity")+  
  #Add axis labels
  labs(x="Airline Partner",y="Passenger Count",fill="Promoter/Detractor Rating")+
  #Adjust axis label to be readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  #Add horizontal line to show 750 passenger threshold
  geom_hline(yintercept = 750)+
  #Add title
  ggtitle("Count of Promoters/Detractors/Passives per Partners")

#Create plot of number of promoters/detractors/passive per partner with passenger counter greater than passthreshold
ggplot(rawPromoterdf[rawPromoterdf$Var1 %in% partnerpasscount$Partner.Name[partnerpasscount$count>passthreshold],], aes(fill=Var2, y=Freq, x=reorder(Var1, -Freq)))+ 
  geom_bar(position="stack", stat="identity")+  
  #Add axis labels
  labs(x="Airline Partner",y="Passenger Count",fill="Promoter/Detractor Rating")+
  #Adjust axis label to be readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  #Add title
  ggtitle("Count of Promoters/Detractors/Passives per Partners")


#Create table of percent promoters/detractors/passive per partner
percentPromoter<-round(100*table(df$Partner.Name,df$Promoter.Score)/rowSums(table(df$Partner.Name,df$Promoter.Score)),digits = 2)
#percentPromoter
#Create stacked bar chart visualizing table of percent promoters/detractors/passive per partner
#ggplot(as.data.frame(percentPromoter), aes(fill=Var2, y=Freq, x=reorder(Var1, -Freq))) + 
#  geom_bar(position="stack", stat="identity")+  
#  labs(x="Airline Partner",y="Percentage",fill="Promoter/Detractor Rating")+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
#  ggtitle("Percentage of Promoters/Detractors/Passives per Partners")

#Convert table to matrix dataframe
percentPromoterdf<-as.data.frame.matrix(percentPromoter)
#Calculate overall NPS score by subtracting percent detractors from percent promoters
percentPromoterdf$NPS<-percentPromoterdf$Promoter-percentPromoterdf$Detractor
#copy row name value to new column
percentPromoterdf$Partner.Name<-row.names(percentPromoterdf)
View(percentPromoterdf)

#Plot NPS score for each airline partner
ggplot(percentPromoterdf, aes(y=NPS,x=reorder(Partner.Name, -NPS)))+
  geom_bar(stat="identity")+
  #Adjust axis label to be readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  #Add title
  ggtitle("Calculated NPS Score Per Partner")

#Plot NPS score for each airline partner with greater than passenger threshold
ggplot(percentPromoterdf[percentPromoterdf$Partner.Name %in% partnerpasscount$Partner.Name[partnerpasscount$count>passthreshold], ], aes(y=NPS,x=reorder(Partner.Name, -NPS)))+
  geom_bar(stat="identity")+
  #Add axis labels
  labs(x="Airline Partner", y="NPS Score")+
  #Adjust axis label to be readable
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  #Add title
  ggtitle("Calculated NPS Score Per Partner")

################################################################################

#Create table of percentage Detractor/Passive/Promoter per partner to establish "dumb guess" and see what rules are worthwhile 
round(100*table(df$Partner.Name,df$Promoter.Score)/rowSums(table(df$Partner.Name,df$Promoter.Score)),digits = 2)

#Create data frame with only airlines with greater than 750 passengers
dfPassthres<-df%>%
  filter(Partner.Code %in% partnerpasscount$Partner.Code[partnerpasscount$count>passthreshold])
#Remove elminated factors (Personal and Mileage tickets) from dataframe structure
dfPassthres$Partner.Code<-as.factor(as.character(dfPassthres$Partner.Code))

#create table of percentages of promoter rating for each of the top six airlines to establish "dumb guess"
percentPassPromoter<-round(100*table(dfPassthres$Partner.Name,dfPassthres$Promoter.Score)/rowSums(table(dfPassthres$Partner.Name,dfPassthres$Promoter.Score)),digits = 2)
percentPassPromoter


#Create df with only Northwest Business Airlines
OOrulesdf<-df%>%
  filter(Partner.Code=="OO")%>%
  #Select only columns that contain factors so the resulting dataframe can be converted to a transactions element.  Remove partner code column so to remove uneccessary variable in rule creation
  select(colnames(df[,sapply(df,is.factor)]),-Partner.Code)

#Create Transactions dataframe for rules mining
OOrulesdfX<-as(OOrulesdf, "transactions")

#Find rules Predicting detractors on Northwest
OOsrulesetDetractor<-apriori(OOrulesdfX,
                                        # Specify parameters for rule creation.  Set minimum support threshold at 2% and the confidence threshold at 75%
                                        parameter = list(support=0.2, confidence=0.75), 
                                        # Restrict rule creation to associations that correspond to passengers being Detractors.
                                        appearance = list(default="lhs",rhs=("Promoter.Score=Detractor")))
#View created rules in interactive window
inspectDT(OOsrulesetDetractor)

#Look at rules for passive flyer on OO, promoter population too small for any meaningful insight but moving detractor to passive would improve promoter score
OOsrulesetPassive<-apriori(OOrulesdfX,
                             # Specify parameters for rule creation.  Set minimum support threshold at 2% and the confidence threshold at 45%
                             parameter = list(support=0.2, confidence=0.45), 
                             # Restrict rule creation to associations that correspond to passengers being passive.
                             appearance = list(default="lhs",rhs=("Promoter.Score=Passive")))
#View created rules in interactive window
inspectDT(OOsrulesetPassive)

################################################################################

#Create df with only FlyFast
EVrulesdf<-df%>%
  filter(Partner.Code=="EV")%>%
  #Select only columns that contain factors so the resulting dataframe can be converted to a transactions element
  select(colnames(df[,sapply(df,is.factor)]),-Partner.Code)

#Create Transactions dataframe for rules mining
EVrulesdfX<-as(EVrulesdf, "transactions")

#Find rules Predicting detractors on FlyFast
EVsrulesetDetractor<-apriori(EVrulesdfX,
                             # Specify parameters for rule creation.  Set minimum support threshold at 2% and the confidence threshold at 50%
                             parameter = list(support=0.2, confidence=0.50), 
                             # Restrict rule creation to associations that correspond to passengers being Detractor.
                             appearance = list(default="lhs",rhs=("Promoter.Score=Detractor")))
#View created rules in interactive window
inspectDT(EVsrulesetDetractor)

#Not worth while, could not find any promoter rules with greater confidence than dumb guess
#Find rules Predicting Promoters on Flyfast
EVsrulesetPromoter<-apriori(EVrulesdfX,
                             # Specify parameters for rule creation.  Set minimum support threshold at 0.75% and the confidence threshold at 24%
                             parameter = list(support=0.075, confidence=0.24), 
                             # Restrict rule creation to associations that correspond to passengers being Promtoer.
                             appearance = list(default="lhs",rhs=("Promoter.Score=Promoter")))
#View created rules in interactive window
inspectDT(EVsrulesetPromoter)

#Find rules Predicting Passivess on Flyfast
EVsrulesetPassive<-apriori(EVrulesdfX,
                            # Specify parameters for rule creation.  Set minimum support threshold at 0.5% and the confidence threshold at 32%
                            parameter = list(support=0.05, confidence=0.32), 
                            # Restrict rule creation to associations that correspond to passengers being passive.
                            appearance = list(default="lhs",rhs=("Promoter.Score=Passive")))
#View created rules in interactive window
inspectDT(EVsrulesetPassive)


################################################################################

#Create table showing percentages of promoter ratings across top six airline combined
round(100*table(dfPassthres$Promoter.Score)/(2936+2675+2399), digits = 2)

#Create rule set from DF using partners with passeger count greater than passthreshold
PassthresRulesdf<-dfPassthres%>%
  #Select only columns that contain factos so the resulting dataframe can be converted to a transactions element
  select(colnames(df[,sapply(df,is.factor)]))

#Create transactions element based on partners with passenger count greater than passthreshold
PassthresRulesdfX<-as(PassthresRulesdf, "transactions")

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have promoter rating
PassthresrulesetPromoter<-apriori(PassthresRulesdfX,
                                          # Specify parameters for rule creation.  Set minimum support threshold at 15% and the confidence threshold at 40%
                                          parameter = list(support=0.15, confidence=0.40), 
                                          # Restrict rule creation to associations that correspond to passengers being promoter.
                                          appearance = list(default="lhs",rhs=("Promoter.Score=Promoter")))
#View created rules in interactive window
inspectDT(PassthresrulesetPromoter)

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have detractor rating
PassthresrulesetDetractor<-apriori(PassthresRulesdfX,
                                           # Specify parameters for rule creation.  Set minimum support threshold at 15% and the confidence threshold at 51%
                                           parameter = list(support=0.15, confidence=0.51), 
                                           # Restrict rule creation to associations that correspond to passengers surviving.
                                           appearance = list(default="lhs",rhs=("Promoter.Score=Detractor")))
inspectDT(PassthresrulesetDetractor)

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have passive rating
PassthresrulesetPassive<-apriori(PassthresRulesdfX,
                                   # Specify parameters for rule creation.  Set minimum support threshold at 15% and the confidence threshold at 36%
                                   parameter = list(support=0.15, confidence=0.36), 
                                   # Restrict rule creation to associations that correspond to passengers surviving.
                                   appearance = list(default="lhs",rhs=("Promoter.Score=Passive")))
inspectDT(PassthresrulesetPassive)

################################################################################
#Create table of percentage Detractor/Passive/Promoter per type of travel to establish "dumb guess" and see what rules are worthwhile 
round(100*table(df$Type.of.Travel,df$Promoter.Score)/rowSums(table(df$Type.of.Travel,df$Promoter.Score)),digits = 2)

#df$Partner.Name[df$Type.of.Travel=="Personal Travel"]

#Create data frame with only personal travelers from airline with passenger count greater than passenger threshold
PersonalRulesdf<-df%>%
  filter(Type.of.Travel == "Personal Travel")%>%
  filter(Partner.Code %in% partnerpasscount$Partner.Code[partnerpasscount$count>passthreshold])%>%
  #Select only columns that contain factors so the resulting dataframe can be converted to a transactions element.  Remove partner code column so to remove uneccessary variable in rule creation
  select(colnames(df[,sapply(df,is.factor)]))

#Create transactions element from personal traveler dataframe
PersonalRulesdfX<-as(PersonalRulesdf, "transactions")

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have promoter rating
#Did not work too few personal travelers 
PersonalrulesetPromoter<-apriori(PersonalRulesdfX,
                                  # Specify parameters for rule creation.  Set minimum support threshold at .5% and the confidence threshold at 30%
                                  parameter = list(support=0.025, confidence=0.7), 
                                  # Restrict rule creation to associations that correspond to passengers being promoter.
                                  appearance = list(default="lhs", rhs=("Promoter.Score=Promoter")))
#View created rules in interactive window
inspectDT(PersonalrulesetPromoter)

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have promoter rating
PersonalrulesetPassive<-apriori(PersonalRulesdfX,
                                          # Specify parameters for rule creation.  Set minimum support threshold at 7.5% and the confidence threshold at 50%
                                          parameter = list(support=0.06, confidence=0.30), 
                                          # Restrict rule creation to associations that correspond to passengers being promoter.
                                          appearance = list(default="lhs", rhs=("Promoter.Score=Passive")))
#View created rules in interactive window
inspectDT(PersonalrulesetPassive)

#Create ruleset predicting factors that lead to travels in airline with passenger count greater than passthreshold to have promoter rating
PersonalrulesetDetractor<-apriori(PersonalRulesdfX,
                                # Specify parameters for rule creation.  Set minimum support threshold at 7.5% and the confidence threshold at 50%
                                parameter = list(support=0.25, confidence=0.69), 
                                # Restrict rule creation to associations that correspond to passengers being promoter.
                                appearance = list(default="lhs", rhs=("Promoter.Score=Detractor")))
#View created rules in interactive window
inspectDT(PersonalrulesetDetractor)

################################################################################
#Create table of percent promoters/detractors/passive per partner for business travelers
percentBusinessPromoter<-round(100*table(df$Partner.Name[df$Type.of.Travel=="Business travel"],df$Promoter.Score[df$Type.of.Travel=="Business travel"])/rowSums(table(df$Partner.Name[df$Type.of.Travel=="Business travel"],df$Promoter.Score[df$Type.of.Travel=="Business travel"])),digits = 2)
percentBusinessPromoter
#Create stacked bar chart visualizing table of percent promoters/detractors/passive per partner for business travelers
ggplot(as.data.frame(percentBusinessPromoter), aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Airline Partner",y="Percentage",fill="Promoter/Detractor Rating")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  ggtitle("Percentage of Promoters/Detractors/Passives per Partners for Business Travelers")



#Create data frame with only business passengers and on airlines with greater than 750 passengers
dfbusinessPassthres<-df%>%
  filter(Partner.Code %in% partnerpasscount$Partner.Code[partnerpasscount$count>passthreshold],Type.of.Travel=="Business travel")
#Remove elminated factors (Personal and Mileage tickets) from dataframe structure
dfbusinessPassthres$Type.of.Travel<-as.factor(as.character(dfbusinessPassthres$Type.of.Travel))

#create table 
percentBusinessPassPromoter<-round(100*table(dfbusinessPassthres$Partner.Name,dfbusinessPassthres$Promoter.Score)/rowSums(table(dfbusinessPassthres$Partner.Name,dfbusinessPassthres$Promoter.Score)),digits = 2)
percentBusinessPassPromoter
#Create stacked bar chart visualizing table of percent promoters/detractors/passive per partner for business travelers
ggplot(as.data.frame(percentBusinessPassPromoter), aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Airline Partner",y="Percentage",fill="Promoter/Detractor Rating")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
  ggtitle("Percentage of Promoters/Detractors/Passives per Partners for Business Travelers with Blue Status on airlines with greater than 750 passengers")

# Create table of percentage Detractor/Passive/Promoter per type of travel for passengers and on airline that have greater then 750 passengers to establish "dumb guess" and see what rules are worthwhile
round(100*table(dfbusinessPassthres$Type.of.Travel,dfbusinessPassthres$Promoter.Score)/rowSums(table(dfbusinessPassthres$Type.of.Travel,dfbusinessPassthres$Promoter.Score)),digits = 2)

#Create rule set from DF using only business type traveler and partners with greater than 750 passengers
#https://stackoverflow.com/questions/35845903/dplyr-filter-value-is-contained-in-a-vector
businessPassthresRulesdf<-df%>%
  filter(Type.of.Travel=="Business travel" & Partner.Code %in% partnerpasscount$Partner.Code[partnerpasscount$count>passthreshold] & Airline.Status=="Blue")%>%
  #Select only columns that contain factos so the resulting dataframe can be converted to a transactions element
  select(colnames(df[,sapply(df,is.factor)]),-Type.of.Travel,-Airline.Status)

#Create transactions element based on business type traveler and partners with greater than 750 passengers
businessPassthresRulesdfX<-as(businessPassthresRulesdf, "transactions")

#Create ruleset predicting factors that lead to business travelers having a Promoter rating
businessPassthresrulesetPromoter<-apriori(businessPassthresRulesdfX,
                                          # Specify parameters for rule creation.  Set minimum support threshold at 7.5% and the confidence threshold at 50%
                                          parameter = list(support=0.075, confidence=0.40), 
                                          # Restrict rule creation to associations that correspond to passengers being promoter.
                                          appearance = list(default="lhs",rhs=("Promoter.Score=Promoter")))
#View created rules in interactive window
inspectDT(businessPassthresrulesetPromoter)

#Create ruleset predicting factors that lead to business travelers with blue status having a detractor rating
businessPassthresrulesetPassive<-apriori(businessPassthresRulesdfX,
                                         # Specify parameters for rule creation.  Set minimum support threshold at 0.5% and the confidence threshold at 50%
                                         parameter = list(support=0.05, confidence=0.30), 
                                         # Restrict rule creation to associations that correspond to passengers surviving.
                                         appearance = list(default="lhs",rhs=("Promoter.Score=Passive")))
inspectDT(businessPassthresrulesetPassive)

#Create ruleset predicting factors that lead to business travelers with blue status having a detractor rating
businessPassthresrulesetDetractor<-apriori(businessPassthresRulesdfX,
                                           # Specify parameters for rule creation.  Set minimum support threshold at 0.5% and the confidence threshold at 50%
                                           parameter = list(support=0.05, confidence=0.30), 
                                           # Restrict rule creation to associations that correspond to passengers surviving.
                                           appearance = list(default="lhs",rhs=("Promoter.Score=Detractor")))
inspectDT(businessPassthresrulesetDetractor)

################################################################################
us<-map_data("state") #Save state map information as dataframe from maps package

df$Origin.State<-tolower(df$Origin.State) #Convert state names to lower case to match map_data

#Create vector of 12 largest cities with FlyFast and Northwest Business Airlines
cities<-c("Los Angeles","Chicago","Salt Lake City","Phoenix","San Francisco","Denver","Detroit","Atlanta","Houston","San Diego","Portland","Seattle")

top6contUS<-df%>%
  #Remove hawaii (no state data)
  filter(Origin.State!="hawaii")%>%
  #Remove alaska flights (no map  data)
  filter(Origin.State!="alaska")%>%
  filter(Partner.Code %in% partnerpasscount$Partner.Code[partnerpasscount$count>passthreshold])%>%
  #group by origin city, patner name, state, and coordinates
  group_by(Origin.City,Partner.Name,olong,olat,Origin.State)%>%
  #Create a count of passengers to plot by size
  summarise(count = n())
#Copy NPS scores from percentpromter df
top6contUS<-merge(top6contUS,percentPromoterdf, by = "Partner.Name" )

#Create ggplot using summaryDF as base data and relate map objects by matching stateName field (all-lowercase state names)
NPSmap<-ggplot(top6contUS, aes(map_id = Origin.State))+
  #Specify map geometry for plot using "us" map data loaded before.
  geom_map(map=us, color = "red", bg = "white")+
  #Add scatter plot layer to map.  Plot points using orgini city coordinates with size corresponding to total number of passengers per partner. Each partner is respresented by a different shape.  Spread overlapping shape horizontally
  geom_point(data = top6contUS,aes(x=olong, y=olat, size = count, col = NPS, shape = Partner.Name), position = position_dodge(width=4))+
  #Expand limits of plot based on built-in lat and long values of US -> to ensure the entire US is visible in plot.
  expand_limits(x=us$long, y=us$lat)+
  #Prevent map from being distorted or streched
  coord_map()+
  #Modify labels on plot
  labs(x="Longitude",y="Latitiude",size="Partner Passenger Count",fill="NPS")+
  #Apply title to plot
  ggtitle("Partner Presence by Airport")
#Display Plot
NPSmap

#Create ggplot using summaryDF as base data and relate map objects by matching stateName field (all-lowercase state names)
filteredNPSmap<-ggplot(top6contUS, aes(map_id = Origin.State))+
  #Specify map geometry for plot using "us" map data loaded before.
  geom_map(map=us, color = "red", bg = "white")+
  #Add scatter plot layer to map.  Plot points using orgini city coordinates with size corresponding to total number of passengers per partner. Each partner is respresented by a different shape.  Spread overlapping shape horizontally
  geom_point(data = top6contUS[top6contUS$Origin.City %in% cities,],aes(x=olong, y=olat, size = count, col = NPS, shape = Partner.Name), position = position_dodge(width=7))+
  #Expand limits of plot based on built-in lat and long values of US -> to ensure the entire US is visible in plot.
  expand_limits(x=us$long, y=us$lat)+
  #Prevent map from being distorted or streched
  coord_map()+
  #Modify labels on plot
  labs(x="Longitude",y="Latitiude",size="Partner Passenger Count",fill="NPS")+
  #Apply title to plot
  ggtitle("Top 12 Cities for Northwest Business Airlines")
#Display Plot
filteredNPSmap

################################################################################
# text mining part
#1. analyze the word that customer use in the comment and form a word cloud <as a whole>
#(1) remove NA to get pure text that we want to analyze
comment <- df$freeText[!is.na(df$freeText)]   # Find out the part that do have comments

#(2) remove those useless parts(Capital, number, punctuatiion, meaningless words)
#install.packages("tm")
library(tm)
word.vec <- VectorSource(comment)
word.corpus <- Corpus(word.vec)
word.corpus <- tm_map(word.corpus,content_transformer(tolower))   # change uppercase letter to lowercase
word.corpus <- tm_map(word.corpus,removePunctuation)   # make each character a combine of words, no more sentences
word.corpus <- tm_map(word.corpus,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
word.corpus <- tm_map(word.corpus,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
term_Document_Matrix <- TermDocumentMatrix(word.corpus)
term_Document_Matrix
inspect(term_Document_Matrix)
#(3) word cloud
#install.packages("wordcloud")
library(wordcloud)
tdm <- as.matrix(term_Document_Matrix)   # term in rows, documents in column
counts <- rowSums(tdm) %>% sort(decreasing=TRUE) # count the number of appearance of each term, and sort from the most popular to the least popular
View(counts)
cloudframe <- data.frame(word=names(counts),freqency=counts,row.names=NULL)  # create a dataframe to store the word and their number of appearance
View(cloudframe)   # we can see which word appear the most
head(cloudframe,30)   # just look at the top 30
pal = brewer.pal(5,"Reds")     # used inside "colors" parameter to make the darkest color to the most frequently shown word, the lightest color to the least frequently shown one.
# resource:https://www.rdocumentation.org/packages/RColorBrewer/versions/1.1-2/topics/RColorBrewer
# resource:https://www.r-bloggers.com/word-cloud-in-r/
wordcloud(cloudframe$word,cloudframe$freqency,min.freq=30,colors=pal)



################################################################################
#2. Sensitive analysis

#(1) read txt files of positive word and negative word
positive_txt <- scan("positive-words.txt",character(0),sep="\n")
negative_txt <- scan("negative-words.txt",character(0),sep="\n")
# remove headers
positive_txt <- positive_txt[-1:-34]
negative_txt <- negative_txt[-1:-34]

#(2) find positive words and show positive word cloud
totalwords <- sum(counts)
totalwords
words <- names(counts)

positive_matched <- match(words,positive_txt,nomatch=0)
positive_count <- counts[which(positive_matched!=0)]
positive_word <- names(positive_count)
View(positive_count)  # show the positive words and their appearance frequency
positive_number <- sum(positive_count)
positive_number
positive_cloud <- data.frame(positive_word,positive_count,row.names=NULL)  # create a dataframe for word cloud
View(positive_cloud)
wordcloud(positive_cloud$positive_word,positive_cloud$positive_count,min.freq=5,colors=pal)

#(3) find negative words and show negative word cloud
negative_matched <- match(words,negative_txt,nomatch=0)
negative_count <- counts[which(negative_matched!=0)]
negative_word <- names(negative_count)
View(negative_count)  # show the negative words and their appearance frequency
negative_number <- sum(negative_count)
negative_number
negative_cloud <- data.frame(negative_word,negative_count,row.names = NULL)  # create a dataframe for word cloud
View(negative_cloud)
wordcloud(negative_cloud$negative_word,negative_cloud$negative_count,min.freq=5)

#(4) count the positive/negative ratio
positive_rate <- positive_number/totalwords
positive_rate    #[1] 0.08772164
negative_rate <- negative_number/totalwords
negative_rate    #[1] 0.06759099

#(5) focus on those who both have positive words
tdm_p <- tdm[positive_word,]     
number_of_positive_in_document <- colSums(tdm_p)
positive_document <- df[10000+which(number_of_positive_in_document!=0),] # 216 comments from 282 comments
View(positive_document)
# draw histogram about detractor/passive/promoter
positive_bar <- ggplot(positive_document)+aes(x=Promoter.Score)+geom_histogram(stat="count",color="black",fill="white")
positive_bar
# focus on those who have positive words and are promoter
positive_promoter <- positive_document %>% filter(Promoter.Score=="Promoter")   # filter the dataframe
word.vec.P <- VectorSource(positive_promoter$freeText)
word.corpus.P <- Corpus(word.vec.P)
word.corpus.P <- tm_map(word.corpus.P,content_transformer(tolower))   # change uppercase letter to lowercase
word.corpus.P <- tm_map(word.corpus.P,removePunctuation)   # make each character a combine of words, no more sentences
word.corpus.P <- tm_map(word.corpus.P,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
word.corpus.P <- tm_map(word.corpus.P,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
term_Document_Matrix.P <- TermDocumentMatrix(word.corpus.P)
Ptdm <- as.matrix(term_Document_Matrix.P)   # term in rows, documents in column
Pcounts <- rowSums(Ptdm) %>% sort(decreasing=TRUE)
positive_promoter_matched <- match(names(Pcounts),positive_txt,nomatch=0)
positive_promoter_count <- Pcounts[which(positive_promoter_matched!=0)]
View(positive_promoter_count)
positive_promoter_word <- names(positive_promoter_count)
positive_promoter_cloud <- data.frame(positive_promoter_word,positive_promoter_count,row.names=NULL)  # create a dataframe for word cloud
View(positive_promoter_cloud)
wordcloud(positive_promoter_cloud$positive_promoter_word,positive_promoter_cloud$positive_promoter_count)

#(6) focus on those who both have negative words and are detractor
tdm_n <- tdm[negative_word,]     
number_of_negative_in_document <- colSums(tdm_n)
negative_document <- df[10000+which(number_of_negative_in_document!=0),] # 204 comments from 282 comments
View(negative_document)
# draw histogram about detractor/passive/promoter
negative_bar <- ggplot(negative_document)+aes(x=Promoter.Score)+geom_histogram(stat="count",color="black",fill="white")
negative_bar
# focus on those who are detractor
negative_detractor <- negative_document %>% filter(Promoter.Score=="Detractor")   # 113 comments from 204 comments
word.vec.N <- VectorSource(negative_detractor$freeText)
word.corpus.N <- Corpus(word.vec.N)
word.corpus.N <- tm_map(word.corpus.N,content_transformer(tolower))   # change uppercase letter to lowercase
word.corpus.N <- tm_map(word.corpus.N,removePunctuation)   # make each character a combine of words, no more sentences
word.corpus.N <- tm_map(word.corpus.N,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
word.corpus.N <- tm_map(word.corpus.N,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
term_Document_Matrix.N <- TermDocumentMatrix(word.corpus.N)
tdm.N <- as.matrix(term_Document_Matrix.N)   # term in rows, documents in column
View(tdm.N)
Ncounts <- rowSums(tdm.N) %>% sort(decreasing=TRUE)
View(Ncounts)
negative_detractor_matched <- match(names(Ncounts),negative_txt,nomatch=0)
negative_detractor_count <- Ncounts[which(negative_detractor_matched!=0)]
View(negative_detractor_count)
negative_detractor_word <- names(negative_detractor_count)
negative_detractor_cloud <- data.frame(negative_detractor_word,negative_detractor_count,row.names=NULL)  # create a dataframe for word cloud
View(negative_detractor_cloud)
wordcloud(negative_detractor_cloud$negative_detractor_word,negative_detractor_cloud$negative_detractor_count)



#(6.5) function
#positive_function
positive_function <- function(dataframe){
  word.vec <- VectorSource(dataframe$freeText)
  word.corpus <- Corpus(word.vec)
  word.corpus <- tm_map(word.corpus,content_transformer(tolower))   # change uppercase letter to lowercase
  word.corpus <- tm_map(word.corpus,removePunctuation)   # make each character a combine of words, no more sentences
  word.corpus <- tm_map(word.corpus,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
  word.corpus <- tm_map(word.corpus,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
  term_Document_Matrix <- TermDocumentMatrix(word.corpus)
  Ptdm <- as.matrix(term_Document_Matrix)   # term in rows, documents in column
  Pcounts <- rowSums(Ptdm) %>% sort(decreasing=TRUE)
  positive_matched <- match(names(Pcounts),positive_txt,nomatch=0)
  positive_count <- Pcounts[which(positive_matched!=0)]
  positive_word <- names(positive_count)
  positive_cloud <- data.frame(positive_word,positive_count,row.names=NULL)  # create a dataframe for word cloud
  wordcloud(positive_cloud$positive_word,positive_cloud$positive_count,min.freq=1)
  return(positive_count)
}
#negative_function
negative_function <- function(dataframe){
  word.vec <- VectorSource(dataframe$freeText)
  word.corpus <- Corpus(word.vec)
  word.corpus <- tm_map(word.corpus,content_transformer(tolower))   # change uppercase letter to lowercase
  word.corpus <- tm_map(word.corpus,removePunctuation)   # make each character a combine of words, no more sentences
  word.corpus <- tm_map(word.corpus,removeNumbers)       # since we only analyze words, not numbers. and we do not have reference data used to compare number
  word.corpus <- tm_map(word.corpus,removeWords,stopwords("english")) # remove words that appear a lot but meaningless
  term_Document_Matrix <- TermDocumentMatrix(word.corpus)
  tdm <- as.matrix(term_Document_Matrix)   # term in rows, documents in column
  Ncounts <- rowSums(tdm) %>% sort(decreasing=TRUE)
  negative_matched <- match(names(Ncounts),negative_txt,nomatch=0)
  negative_count <- Ncounts[which(negative_matched!=0)]
  negative_word <- names(negative_count)
  negative_cloud <- data.frame(negative_word,negative_count,row.names=NULL)  # create a dataframe for word cloud
  wordcloud(negative_cloud$negative_word,negative_cloud$negative_count,min.freq=1)
  return(negative_count)
}

#(7) focus on cheapseat that have positive words
cheapseat_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Partner.Name=="Cheapseats Airlines Inc.")
View(cheapseat_comment)     # 39 comments in total
positive_cheapseat <- positive_document %>% filter(Partner.Name=="Cheapseats Airlines Inc.")
View(positive_cheapseat)    #25 records out of 39 comment records of Cheapseat
positive_cheapseat_count <- positive_function(positive_cheapseat)  # create wordcloud and show count of each positive word
View(positive_cheapseat_count)
# then focus on promoter
positive_promoter_cheapseat <- positive_document %>% filter(Partner.Name=="Cheapseats Airlines Inc.")%>% filter(Promoter.Score=="Promoter")
View(positive_promoter_cheapseat)    #14 records out of 39 comment records of Cheapseat
positive_promoter_cheapseat_count <- positive_function(positive_promoter_cheapseat)  # create wordcloud and show count of each positive word
View(positive_promoter_cheapseat_count)

#(8) focus on cheapseat that have negative words
negative_cheapseat <- negative_document %>% filter(Partner.Name=="Cheapseats Airlines Inc.")
View(negative_cheapseat)    #30 records out of 39 comment records of Cheapseat
negative_cheap_count <- negative_function(negative_cheapseat)
View(negative_cheap_count)
# then focus on detractor
negative_detractor_cheapseat <- negative_document %>% filter(Partner.Name=="Cheapseats Airlines Inc.")%>% filter(Promoter.Score=="Detractor")
View(negative_detractor_cheapseat)    #18 records out of 30 positive comment records of Cheapseat
negative_detractor_cheap_count <- negative_function(negative_detractor_cheapseat)
View(negative_detractor_cheap_count)


#(9) focus on Northwest that have positive words
Northwest_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Partner.Name=="Northwest Business Airlines Inc.")
View(Northwest_comment)     # 22 records in total
positive_Northwest <- positive_document %>% filter(Partner.Name=="Northwest Business Airlines Inc.")
View(positive_Northwest)    #14 records out of 22 record of Northwest
positive_Northwest_count <- positive_function(positive_Northwest)
View(positive_Northwest_count)
#focus only on promoter
positive_promoter_Northwest <- positive_document %>% filter(Partner.Name=="Northwest Business Airlines Inc.")%>% filter(Promoter.Score=="Promoter")
View(positive_promoter_Northwest)    #9 records out of 14 record of Northwest
positive_promoter_Northwest_count <- positive_function(positive_promoter_Northwest)
View(positive_promoter_Northwest_count)

#(10) focus on Northwest that have negative words
negative_Northwest <- negative_document %>% filter(Partner.Name=="Northwest Business Airlines Inc.")
View(negative_Northwest)    #17 records out of 22 record of Northwest
negative_Northwest_count <- negative_function(negative_Northwest)
View(negative_Northwest_count)
#focus only on detractor
negative_detractor_Northwest <- negative_document %>% filter(Partner.Name=="Northwest Business Airlines Inc.")%>% filter(Promoter.Score=="Detractor")
View(negative_detractor_Northwest)    #9 records out of 17 record of Northwest
negative_detractor_Northwest_count <- negative_function(negative_detractor_Northwest)
View(negative_detractor_Northwest_count)


#(11) focus on positive comment of female in 6 top company
top6_female_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Gender=="Female") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(top6_female_comment)     # 105 records in total
positive_top6_female <- positive_document %>% filter(Gender=="Female") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_top6_female)    # 77 records out of 105 record 
positive_top6_female_count <- positive_function(positive_top6_female)
View(positive_top6_female_count)
# focus only on promoter
positive_promoter_top6_female <- positive_document %>% filter(Promoter.Score=="Promoter") %>% filter(Gender=="Female") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_promoter_top6_female)    # 32 records out of 77 record  
positive_promoter_top6_female_count <- positive_function(positive_promoter_top6_female)
View(positive_promoter_top6_female_count)


#(12) focus on negative comment of female in 6 top company
negative_top6_female <- negative_document %>% filter(Gender=="Female") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_top6_female)    # 76 records out of 105 record 
negative_top6_female_count <- negative_function(negative_top6_female)
View(negative_top6_female_count)
#focus only on detractor
negative_detractor_top6_female <- negative_document %>% filter(Promoter.Score=="Detractor") %>% filter(Gender=="Female") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_detractor_top6_female)    # 47 records out of 76 record 
negative_detractor_top6_female_count <- negative_function(negative_detractor_top6_female)
View(negative_detractor_top6_female_count)



#(13) focus on positive comment of male in 6 top company
top6_male_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Gender=="Male") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(top6_male_comment)     # 88 records in total
positive_top6_male <- positive_document %>% filter(Gender=="Male") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_top6_male)    # 70 records out of 88 record 
positive_top6_male_count <- positive_function(positive_top6_male)
View(positive_top6_male_count)
# focus only on promoter
positive_promoter_top6_male <- positive_document %>% filter(Promoter.Score=="Promoter") %>% filter(Gender=="Male") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_promoter_top6_male)    # 39 records out of 70 record 
positive_promoter_top6_male_count <- positive_function(positive_promoter_top6_male)
View(positive_promoter_top6_male_count)


#(14) focus on negative comment of male in 6 top company
negative_top6_male <- negative_document %>% filter(Gender=="Male") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_top6_male)    # 69 records out of 88 record 
negative_top6_male_count <- negative_function(negative_top6_male)
View(negative_top6_male_count)
# focus only on detractor
negative_detractor_top6_male <- negative_document %>% filter(Promoter.Score=="Detractor")%>% filter(Gender=="Male") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_detractor_top6_male)    # 30 records out of 69 record 
negative_detractor_top6_male_count <- negative_function(negative_detractor_top6_male)
View(negative_detractor_top6_male_count)

#(15) focus on positive comment of business travel in 6 top company
top6_Business_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Type.of.Travel=="Business travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(top6_Business_comment)     #  records in total
positive_top6_Business <- positive_document %>% filter(Type.of.Travel=="Business travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_top6_Business)    #  records out of  record of 
positive_top6_Business_count <- positive_function(positive_top6_Business)
View(positive_top6_Business_count)
# focus only on promoter
positive_promoter_top6_Business <- positive_document %>% filter(Promoter.Score=="Promoter") %>% filter(Type.of.Travel=="Business travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_promoter_top6_Business)    #  records out of  record of 
positive_promoter_top6_Business_count <- positive_function(positive_promoter_top6_Business)
View(positive_promoter_top6_Business_count)

#(16) focus on negative comment of business travel in 6 top company
negative_top6_Business <- negative_document %>% filter(Type.of.Travel=="Business travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_top6_Business)    #  records out of  record 
negative_top6_Business_count <- negative_function(negative_top6_Business)
View(negative_top6_Business_count)
# focus only on detractor
negative_detractor_top6_Business <- negative_document %>% filter(Promoter.Score=="Detractor")%>% filter(Type.of.Travel=="Business travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_detractor_top6_Business)    # records out of record 
negative_detractor_top6_Business_count <- negative_function(negative_detractor_top6_Business)
View(negative_detractor_top6_Business_count)

#(17) focus on positive comment of Personal Travel in 6 top company
top6_Personal_comment <- df %>% filter(!is.na(df$freeText)) %>% filter(Type.of.Travel=="Personal Travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(top6_Personal_comment)     #  records in total
positive_top6_Personal <- positive_document %>% filter(Type.of.Travel=="Personal Travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_top6_Personal)    #  records out of  record 
positive_top6_Personal_count <- positive_function(positive_top6_Personal)
View(positive_top6_Personal_count)
# focus only on promoter
positive_promoter_top6_Personal <- positive_document %>% filter(Promoter.Score=="Promoter") %>% filter(Type.of.Travel=="Personal Travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(positive_promoter_top6_Personal)    #  records out of  record 
positive_promoter_top6_Personal_count <- positive_function(positive_promoter_top6_Personal)
View(positive_promoter_top6_Personal_count)

#(18) focus on negative comment of Personal Travel in 6 top company
negative_top6_Personal <- negative_document %>% filter(Type.of.Travel=="Personal Travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_top6_Personal)    #  records out of  record 
negative_top6_Personal_count <- negative_function(negative_top6_Personal)
View(negative_top6_Personal_count)
# focus only on detractor
negative_detractor_top6_Personal <- negative_document %>% filter(Promoter.Score=="Detractor")%>% filter(Type.of.Travel=="Personal Travel") %>% filter(Partner.Name=="Northwest Business Airlines Inc."|Partner.Name=="Cheapseats Airlines Inc."|Partner.Name=="FlyFast Airways Inc."|Partner.Name=="Oursin Airlines Inc."|Partner.Name=="Sigma Airlines Inc."|Partner.Name=="Southeast Airlines Co.")
View(negative_detractor_top6_Personal)    #  records out of  record 
negative_detractor_top6_Personal_count <- negative_function(negative_detractor_top6_Personal)
View(negative_detractor_top6_Personal_count)

################################################################################
View(df)
install.packages("psych")
library(psych)
df_new <- df[,c(4,7:9,12,13,15.21:23,25:31)]
View(df_new)

df_new$Age <- as.numeric(df_new$Age)
df_new$Year.of.First.Flight <-as.numeric(df_new$Year.of.First.Flight)
df_new$Flights.Per.Year <- as.numeric(df_new$Flights.Per.Year)
df_new$Shopping.Amount.at.Airport <- as.numeric(df_new$Shopping.Amount.at.Airport)
df_new$Eating.and.Drinking.at.Airport <- as.numeric(df_new$Eating.and.Drinking.at.Airport)
df_new$Day.of.Month <- as.numeric(df_new$Day.of.Month)
df_new$Scheduled.Departure.Hour <- as.numeric(df_new$Scheduled.Departure.Hour)
df_new$Departure.Delay.in.Minutes <- as.numeric(df_new$Departure.Delay.in.Minutes)
df_new$Flight.Distance <- as.numeric(df_new$Flight.Distance)
df_new <- df_new[,c(-8:-12,-14,-15,-17)]
str(df_new)
cor_matrix <- cor(df_new)
View(cor_matrix)
df.pr <- principal(cor_matrix,nfactors=5) 
df.pr
factor.plot(df.pr)
################################################################################
#Correlation between delays and promoter score
unique(df$Departure.Delay.Group)
#[1] 0-15  16-30 46-60 31-45 >60   <NA> 
#Levels: >60 0-15 16-30 31-45 46-60
df$Departure.Delay.Group.Score[df$Departure.Delay.in.Minutes <= 15]<-1
df$Departure.Delay.Group.Score[df$Departure.Delay.in.Minutes>15 & df$Departure.Delay.in.Minutes <=30 ]<-2
df$Departure.Delay.Group.Score[df$Departure.Delay.in.Minutes>30 & df$Departure.Delay.in.Minutes <=45 ]<-3
df$Departure.Delay.Group.Score[df$Departure.Delay.in.Minutes>45 & df$Departure.Delay.in.Minutes <=60 ]<-4
df$Departure.Delay.Group.Score[df$Departure.Delay.in.Minutes >60]<-5

unique(df$Promoter.Score)
#[1] Promoter  Passive   Detractor
df$Promoter.Score.Number[df$Likelihood.to.recommend==9|df$Likelihood.to.recommend==10]<-3
df$Promoter.Score.Number[df$Likelihood.to.recommend==7 | df$Likelihood.to.recommend==8]<-2
df$Promoter.Score.Number[df$Likelihood.to.recommend < 7]<-1

model<-lm(Promoter.Score.Number~Departure.Delay.Group.Score,data=df)
summary(model)
#Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  2.080723   0.013506 154.062   <2e-16 ***
#Departure.Delay.Group.Score -0.062979   0.006981  -9.022   <2e-16 ***
#We can see that the delay statistical significantly effects the promoter score.

#And then I thought there are some aspects the company need to improve.
#I made a linear model to see if all considering variables indeed need to improve by checking p-value, or giving the order of improvement by significance.
unique(df$Price.Sensitivity)
df$Price.Sensitivity.Score <- as.numeric(df$Price.Sensitivity) 

unique(df$Partner.Code)
df$Partner.Company.Group[df$Partner.Code=="US"]<-1
df$Partner.Company.Group[df$Partner.Code=="OU"]<-2
df$Partner.Company.Group[df$Partner.Code=="OO"]<-3
df$Partner.Company.Group[df$Partner.Code=="WN"]<-4
df$Partner.Company.Group[df$Partner.Code=="AS"]<-5
df$Partner.Company.Group[df$Partner.Code=="DL"]<-6
df$Partner.Company.Group[df$Partner.Code=="MQ"]<-7
df$Partner.Company.Group[df$Partner.Code=="B6"]<-8
df$Partner.Company.Group[df$Partner.Code=="EV"]<-9
df$Partner.Company.Group[df$Partner.Code=="AA"]<-10
df$Partner.Company.Group[df$Partner.Code=="FL"]<-11
df$Partner.Company.Group[df$Partner.Code=="VX"]<-12
df$Partner.Company.Group[df$Partner.Code=="F9"]<-13
df$Partner.Company.Group[df$Partner.Code=="HA"]<-14
unique(df$Partner.Company.Group)

unique(df$Flight.cancelled)
df$Flight.cancelled.Dummy[df$Flight.cancelled=="No"]<-0
df$Flight.cancelled.Dummy[df$Flight.cancelled=="Yes"]<-1
unique(df$Flight.cancelled.Dummy)

library(tidyverse)
#Cheapseats Airlines Inc.(WN)
df4 <- df %>% filter(Partner.Company.Group==4)
model4 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df4)
summary(model4)
#FlyFast Airways Inc.(EV)
df9 <- df %>% filter(Partner.Company.Group==9)
model9 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df9)
summary(model9)
#Northwest Business Airlines Inc.(OO)
df3 <- df %>% filter(Partner.Company.Group==3)
model3 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df3)
summary(model3)
#Oursin Airlines Inc.(OU)
df2 <- df %>% filter(Partner.Company.Group==2)
model2 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df2)
summary(model2)
#Sigma Airlines Inc.(DL)
df6 <- df %>% filter(Partner.Company.Group==6)
model6 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df6)
summary(model6)
#Southeast Airlines Co(US)
df1 <- df %>% filter(Partner.Company.Group==1)
model1 <- lm(Promoter.Score.Number~Departure.Delay.Group.Score+Price.Sensitivity.Score+Flight.cancelled.Dummy,data=df1)
summary(model1)
#Coefficients: (1 not defined because of singularities)
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  2.23743    0.13099  17.081   <2e-16 ***
#Departure.Delay.Group.Score -0.05715    0.02722  -2.100   0.0361 *  
#Price.Sensitivity.Score     -0.01406    0.05319  -0.264   0.7916    
#Flight.cancelled.Dummy            NA         NA      NA       NA      
df1$Flight.cancelled.Dummy


#########################################################################
#impact on NPS when they Shopped/ate/drank at airport when there was a delay or no delay
#supply food - delayed - make happy
df$Departure.Delay.Dummy[df$Departure.Delay.in.Minutes == 0] <- 1
df$Departure.Delay.Dummy[df$Departure.Delay.in.Minutes > 0] <- 0

df11 <- df %>% filter(Partner.Company.Group==c(1,2,3,4,6,9))
model11 <- lm(Shopping.Amount.at.Airport~Departure.Delay.Dummy,data=df11)
summary(model11)

model12 <- lm(Eating.and.Drinking.at.Airport~Departure.Delay.Dummy,data=df11)
summary(model12)

#It's not statistical significantly to say that whether the airline is delayed will effect the shopping, eating and drinking.

model13 <- lm(Promoter.Score.Number~Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data=df11)
summary(model13)

################################################################################
#Create three groups of type of travel and store as factor in new column.
df$Type.of.Travel.Group[df$Type.of.Travel == "Business travel"] <- 1
df$Type.of.Travel.Group[df$Type.of.Travel == "Personal Travel"] <- 2
df$Type.of.Travel.Group[df$Type.of.Travel == "Mileage tickets"] <- 3

df$Business.Dummy[df$Type.of.Travel.Group == 1] <- 1
df$Business.Dummy[df$Type.of.Travel.Group != 1] <- 0

df$Personal.Dummy[df$Type.of.Travel.Group == 2] <- 1
df$Personal.Dummy[df$Type.of.Travel.Group != 2] <- 0

#Linear model for business & promoter subset for top 6 airlines
df22 <- df %>% filter(Partner.Company.Group==c(1,2,3,4,6,9))#&Type.of.Travel.Group==1)
View(df22)
model21 <- lm(Promoter.Score.Number~Business.Dummy,data=df22)
summary(model21)
model22 <- lm(Promoter.Score.Number~Personal.Dummy,data=df22)
summary(model22)
model23 <- lm(Promoter.Score.Number~Business.Dummy+Personal.Dummy,data=df22)
summary(model23)

model24 <- lm(Promoter.Score.Number~Business.Dummy+Personal.Dummy+Price.Sensitivity.Score+Flight.Distance+Departure.Delay.Group.Score,data=df22)
summary(model24)

df23 <- df %>% filter(Partner.Company.Group==c(1,2,3,4,6,9)&Type.of.Travel.Group==1)
model25 <- lm(Promoter.Score.Number~Price.Sensitivity.Score+Flight.Distance+Departure.Delay.Group.Score,data=df23)
summary(model25)

df24 <- df %>% filter(Partner.Company.Group==c(1,2,3,4,6,9)&Type.of.Travel.Group==2)
model26 <- lm(Promoter.Score.Number~Price.Sensitivity.Score+Flight.Distance+Departure.Delay.Group.Score,data=df24)
summary(model26)

#Linear model for personal & detractor subset for top 6 airlines (just personal travel alone predicts twice as much as the average - found from association rules mining)

