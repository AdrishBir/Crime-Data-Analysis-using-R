rpivotTable(Titanic)
library(plyr)
library(dplyr)
# study about rpivotTable()
# project about crime analysis
crime<-read.csv(file='G:/nivt_data_analytics/R/Project/crime.csv',sep=',',header=T)
View(crime)
str(crime)
library(plyr)
typeofcrime<-count(crime,Category)
View(typeofcrime)
ggplot(typeofcrime, aes(fill=typeofcrime$Category, y=typeofcrime$freq , x=typeofcrime$Category)) +
  geom_bar(stat="identity")+ xlab('Types of crime') + ylab("Number of incidents")+
  labs(title  ="Crime Data 2003-2016", subtitle = "(Count based on types)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#year of crime
crime$date<-as.Date(crime$Date,'%m/%d/%Y')
#View(crime)
is.Date(crime$date)
#crime$date1<-parse_date_time(x =crime$Date,orders = c("m d y","%m/%d/%Y"))
crime$year<-format(crime$date,'%Y')
View(crime)
crimeyear<-count(crime,'year')
View(crimeyear)
rpivotTable(crimeyear)
ggplot(crimeyea,aes(fill=crimeyear$year,y=crimeyear$freq)) +
# geom_bar(stat="identity", position="fill",colour='black')
crime$time<-strptime(crime$Time,format='%H:%M')
crime$time<-format(crime$time,'%H%M')
View(crime)
is.timespan(crime$time)
#percentage of crime
crime_per<-typeofcrime
View(crime_per)
x<-sum(typeofcrime$n)
x
crime_per$n=round(((crime_per$n)/x)*100,digits = 4)
write_xlsx(crime_per,'G:/nivt_data_analytics/R/Project/crime_percentage.xlsx')
rpivotTable(typeofcrime)
barplot(typeofcrime$freq,names.arg = c(1:39),col = rainbow(length(typeofcrime$Category)))
legend('right',as.vector(typeofcrime$Category),cex=0.7,fill=rainbow(length(typeofcrime$Category)))
dayofcrime<-count(crime,DayOfWeek)
View(dayofcrime)
pie3D(dayofcrime$n,labels =paste(dayofcrime$DayOfWeek,'(',dayofcrime$n,')',sep=' '),col =rainbow(length(dayofcrime$DayOfWeek)),explode=0.25,main='Day of crime')
#legend("topright",dayofcrime$DayOfWeek, cex =0.8,fill = rainbow(length(dayofcrime$DayOfWeek)))
resofcrime<-count(crime,'Resolution')
View(resofcrime)
unsolved<-subset(resofcrime,Resolution %in%c('NONE','UNFOUNDED'))
View(unsolved)
solved<-subset(resofcrime,!(Resolution %in%c('NONE','UNFOUNDED')))
View(solved)
df<-data.frame(c('solved','unsolved'),c(sum(solved$freq),sum(unsolved$freq)))
v<-c(sum(solved$freq),sum(unsolved$freq))
v
View(df)
colnames(df)<-c('category','freq')
xx<-barplot(df$freq,names.arg = df$category,col=c('green','red'))
text(x=xx,y = df$freq, label = df$freq, pos = 1, cex = 0.8, col = "black")
districtofcrime<-group_by(crime,PdDistrict,Category)
View(districtofcrime)
disofcrime<-summarise(districtofcrime,count=n())
View(disofcrime)
ggplot(disofcrime, aes(fill=disofcrime$Category, y=disofcrime$count, x=disofcrime$PdDistrict)) +
  geom_bar(stat="identity")
ggplot(disofcrime, aes(fill=disofcrime$Category, y=disofcrime$count, x=disofcrime$PdDistrict)) +
  geom_bar(position="dodge",stat="identity")
library(rpivotTable)
rpivotTable(disofcrime)
library(ggplot2)
colnames(typeofcrime)<-c('Category','freq')
ggplot(typeofcrime,aes(fill=typeofcrime$Category,y=typeofcrime$freq,x='crime % rate')) +
  geom_bar(stat="identity", position="fill",colour='black')
View(typeofcrime)
View(crime[grep("MARIJUANA", crime$Descript),])
high_crime<-crime[grepl("MARIJUANA", crime$Descript),] # you can use 'grep' also
high_crime1<-group_by(high_crime,PdDistrict)
View(high_crime)
high_crime1<-summarise(high_crime1,count=n())
View(high_crime1)
ggplot(high_crime1, aes(y=high_crime1$count, x=high_crime1$PdDistrict)) +
  geom_bar(stat="identity",fill=rainbow(length(high_crime1$PdDistrict)))
str(crime)
#time_crime<-as.integer(crime$Time)
View(time_crime)
time_crime <- gsub("[:]", "" , crime$Time, perl=TRUE)
str(time_crime)
colnames(time_crime)<-c('time')
time_crime<-as.integer(time_crime)
str(time_crime)
time_crime<-as.data.frame(time_crime)
time_crime$code<-ifelse((time_crime$time>=0000 & time_crime$time<600),'midnight',ifelse((time_crime$time>=600 & time_crime$time<1200),'morning',ifelse((time_crime$time>=1200 & time_crime$time<1800),'noon','night')))
View(time_crime)
df<-count(time_crime,'code')
pie3D(df$freq,labels =paste(round((df$freq/sum(df$freq))*100),'%','(',df$freq,')',sep=' '),col =rainbow(length(df$code)),main='time at which max crime occurs')
legend('bottomleft',v, cex = 0.6, fill =rainbow(length(df$code)))
df$code
v<-as.vector(df$code)
v
View(df)

library(leaflet)
m=leaflet() %>% addTiles() %>%
  addMarkers(data,lng =high_crime$X,lat = high_crime$Y,popup = high_crime$PdDistrict )
m
