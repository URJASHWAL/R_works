#ampere percent decided for holiday
per<-75

#import Dataset
#dataset = read.csv("energy_data.csv",fill=TRUE,header = TRUE)
dataset= read.delim2("energy_data.txt", header = TRUE, sep = "|")
dataset=dataset[-1,]
dataset=dataset[dataset$ctid=='1',c(1,6)]

dataset[,1] <- as.POSIXct(as.numeric(dataset[,1]),tz="UTC",origin = "1970-01-01 00:00:00")
plot(dataset$uts,dataset$amp,type='p',col="brown")
#dataset[,4]<-weekdays.Date(dataset[,1])
#names(dataset)[4]<-paste("day")
#mode(dataset$day)

#install.packages("chron")
library(chron)
dataset$Date <- as.Date(dataset$uts)
dataset$Time <- (format(dataset$uts, "%T"))
#dataset$Date<-as.Date(dataset$Date)
class(dataset$Date)
dataset$Time <- chron(times=dataset$Time)
class(dataset$Time)

#average ampere
dataset=dataset[!is.na(dataset$amp),]    #dataset after removing NA.
non_zero=dataset[dataset$amp!=0,]        #remove zero data from data base

#3sigma method for mean ampere to detect outlier.
mean_amp=mean(non_zero$amp)
si<-var(non_zero$amp)
up<-mean_amp+3*(sqrt(si))
lo<-mean_amp-3*sqrt(si)
plot(non_zero$uts,non_zero$amp,type='l',col="blue",xlab="Time",ylab="Ampere",main = "Ampere pattern w.r.t. Time")
abline(h=up)
abline(h=mean_amp)
abline(h=lo)
non_zero<-non_zero[non_zero$amp<up,]
non_zero<-non_zero[non_zero$amp>lo,]
plot(non_zero$uts,non_zero$amp,type='p',col="blue",xlab="Time",ylab="Ampere",main = "Ampere pattern w.r.t. Time")
mean_amp1=mean(non_zero$amp)

h<-1
y<-1
av<-c()
nf_time<-data.frame(on_time=numeric(0),off_time=numeric(0))
d<-c()
l<-c()
days <- seq(from=as.Date(head(dataset$uts, n=1)),to=as.Date(tail(dataset$uts, n=1)),by='days')
for ( i in seq_along(days) )
{
  sum<-0
  print(i)
  l[i]<-length(dataset$amp[dataset$Date==days[i]])

  for(j in 2:length(dataset$amp))
  {
    
    if(dataset[j,3]==days[i])
    { sum=sum+dataset[j,2]
    
    
   if(dataset[j,2]==0 & dataset[j+1,2]!=0)
    { nf_time[y,1]<-dataset[j+1,1]
    y=y+1}
    
    if(dataset[j,2]==0 & dataset[j-1,2]!=0)
    {nf_time[h,2]<-dataset[j,1]
    h=h+1
    }
    }
  }
  av[i]=sum
  if(sum/l[i]<=(1-per/100)*mean_amp1)
  {
    d[i]<-days[i]
    
  }
}
l #number of data each date as they vary
avgamp_day=av/l
avgamp_day #average ampere for every day

#on off timing for machine
nf_time[,1] <- as.POSIXct(as.numeric(nf_time[,1]), tz="UTC",origin = "1970-01-01 00:00:00")
nf_time[,2] <- as.POSIXct(as.numeric(nf_time[,2]), tz="UTC",origin = "1970-01-01 00:00:00")
paste("Time at which machine will switch on: ",nf_time$on_time,"Time at which machine will switch off:",nf_time$off_time)

##To find time that any zero present or not
zero=dataset$uts[dataset$Time==0]
zero

#remove holidays having Zero Ampere
if(!is.null(d))
{
#install.packages("zoo")
library(zoo)
zero_d<-as.Date(d[!is.na(d)])
paste("Total holidays",length(zero_d))
zero_d
#Refine Dataset
dataset<-dataset[ !(dataset$Date %in% zero_d), ]
}








#3SIGMA METHOD TO DETECT OUTLIER
#anomaly detection and removal
mue<-mean(dataset$amp)
sig<-var(dataset$amp)
upper<-mue+3*(sqrt(sig))
low<-mue-3*sqrt(sig)
plot(dataset$uts,dataset$amp,type='l',col="blue",xlab="Time",ylab="Ampere",main = "Ampere pattern w.r.t. Time")
abline(h=upper)
abline(h=mue)
abline(h=low)
tym<-dataset$uts[dataset$amp>=upper]
tym
paste("Date & Time of anomalous data is ",tym,sep = ":")
dataset<-dataset[dataset$amp<upper,]
plot(dataset$uts,dataset$amp,type='l',col="purple",xlab="Time",ylab="Ampere",main = "Ampere pattern w.r.t. Time")
abline(reg=lm(dataset$amp~dataset$uts),col='red')
lm(dataset$amp~dataset$uts)

#2ND METHOD
#anomaly detection in dataset
dataset2<-dataset[,1:2]
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
##considering first machine
res = AnomalyDetectionTs(dataset2, max_anoms=0.1, direction='both', plot=TRUE,na.rm=TRUE)
res$plot
#tym=res$anoms$timestamp
#paste("Date & Time of anomalous data is ",tym,sep = ":")
#No anomaly found

dataset<-dataset[order(dataset$uts),]     #order the dataset
count_ma = ts(dataset$amp, frequency=720)
decomp = stl(count_ma, s.window="periodic")
plot(decomp)
decomp

#Split dataset
training_set=dataset[1:19500,1:3]
test_set=dataset[19501:19803,1:3]

#install.packages("forecast")
library(forecast)
cnt_ma = ma(training_set$amp, order=350)
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_line(aes(x =training_set$uts, y =training_set$amp, colour = "Counts")) +
  geom_line(aes(x =training_set$uts, y = cnt_ma,   colour = "daily Moving Average")) +xlab('time')+ylab("amps moving avg")


library('tseries')
acf(training_set$amp)
pacf(training_set$amp)
#stationarity Check of Data ie ampere by Dickey Fuller Test and Kpss test
adf.test(training_set$amp,alternative = "stationary")
#p-value is less than printed value, so this is a stationary data 
library(forecast)
auto.arima(training_set$amp)
train<-arima(training_set$amp,order=c(4,1,5))
train
resid=residuals(train)
Box.test(resid,lag = 15,type='Ljung-Box')
#null:white noise(residual is random(no autocorrelation))
#now model is good,P-value is greater than printed value 

hist(resid,xlim=c(-10,10),breaks=seq(min(resid),max(resid),length=50),
     xlab = "Amps", ylab= "Density",
     main = "Normal curve over Histogram",
     prob= TRUE, col= "lightgray")
points(seq(min(resid), max(resid), length.out=500),
       dnorm(seq(min(resid), max(resid), length.out=500),
             mean(resid), sd(resid)), type="l", col="red")
lines(density(resid,adjust=4), col = "blue")

fore<-forecast(train,level=c(90,95),h=657)
fore
fore<-as.data.frame(fore)

# Fitting Decision Tree Classification to the Training set
#install.packages('rpart')
library(rpart)
classifier = rpart(formula = amp ~ as.numeric(uts),
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier,newdata = as.numeric(test_set[,1]), type = 'class')
y_pred

# Making the Confusion Matrix
cm = table(test_set[, 2], y_pred)


library(ggplot2)
ggplot() +
  geom_point(aes(x =training_set$uts, y =training_set$amp, colour = "Training_set"))+
  geom_point(aes(x =test_set$uts, y =test_set$amp, colour = "Test_set"))+
  geom_point(aes(x =test_set$uts, y =fore$`Point Forecast`, colour = "Forecast"))+
  geom_line(aes(x =training_set$uts, y = cnt_ma, colour = "daily Moving Average"))+
  stat_smooth(method=lm)+
  geom_abline(aes(slope=2.951e-06,intercept=-4.372e+03,color="lm_line"))

#New approch pattern 
dataset[,5]<-weekdays.Date(dataset[,1])
names(dataset)[5]<-paste("day")
dataset3<-dataset[order(dataset$Time),]
dataset4<-dataset3[dataset3$amp!=0,]
#dataset3<-dataset[order(dataset$day),]
#day<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

#install.packages("ggalt")
library(ggalt)
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
  ggtitle("Current Distribution")+
  xlab("Time")+
  ylab("Current(in Ampere)")+
  #geom_point(aes(x =(dataset4$Time[dataset4$day=="Saturday"]), y =dataset4$amp[dataset4$day=="Saturday"], colour = "Saturday"))+
  #geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Saturday"]), y =dataset4$amp[dataset4$day=="Saturday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Saturday"])
#mean(dataset4$amp[dataset4$day=="Saturday"])
#max(dataset4$amp[dataset4$day=="Saturday"])

#geom_point(aes(x =(dataset4$Time[dataset4$day=="Sunday"]), y =dataset4$amp[dataset4$day=="Sunday"], colour = "Sunday"))+
#geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Sunday"]), y =dataset4$amp[dataset4$day=="Sunday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Sunday"])
#mean(dataset4$amp[dataset4$day=="Sunday"])
#max(dataset4$amp[dataset4$day=="Sunday"])

#geom_point(aes(x =(dataset4$Time[dataset4$day=="Monday"]), y =dataset4$amp[dataset4$day=="Monday"], colour = "Monday"))+
#geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Monday"]), y =dataset4$amp[dataset4$day=="Monday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Monday"])
#mean(dataset4$amp[dataset4$day=="Monday"])
#max(dataset4$amp[dataset4$day=="Monday"])

#geom_point(aes(x =(dataset4$Time[dataset4$day=="Tuesday"]), y =dataset4$amp[dataset4$day=="Tuesday"], colour = "Tuesday"))+
#geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Tuesday"]), y =dataset4$amp[dataset4$day=="Tuesday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Tuesday"])
#mean(dataset4$amp[dataset4$day=="Tuesday"])
#max(dataset4$amp[dataset4$day=="Tuesday"])

geom_point(aes(x =(dataset4$Time[dataset4$day=="Wednesday"]), y =dataset4$amp[dataset4$day=="Wednesday"], colour = "Wednesday"))+
geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Wednesday"]), y =dataset4$amp[dataset4$day=="Wednesday"]),alpha=0.9,s_shape=1, expand=0.001)
min(dataset4$amp[dataset4$day=="Wednesday"])
mean(dataset4$amp[dataset4$day=="Wednesday"])
max(dataset4$amp[dataset4$day=="Wednesday"])

#geom_point(aes(x =(dataset4$Time[dataset4$day=="Thursday"]), y =dataset4$amp[dataset4$day=="Thursday"], colour = "Thursday"))+
#geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Thursday"]), y =dataset4$amp[dataset4$day=="Thursday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Thursday"])
#mean(dataset4$amp[dataset4$day=="Thursday"])
#max(dataset4$amp[dataset4$day=="Thursday"])

#geom_point(aes(x =(dataset4$Time[dataset4$day=="Friday"]), y =dataset4$amp[dataset4$day=="Friday"], colour = "Friday"))+
#geom_encircle(aes(x =(dataset4$Time[dataset4$day=="Friday"]), y =dataset4$amp[dataset4$day=="Friday"]),alpha=0.9,s_shape=1, expand=0.001)
#min(dataset4$amp[dataset4$day=="Friday"])
#mean(dataset4$amp[dataset4$day=="Friday"])
#max(dataset4$amp[dataset4$day=="Friday"])

hist(dataset4$amp[dataset4$day=="Friday"],xlim=c(65,85),breaks=seq(min(dataset4$amp[dataset4$day=="Friday"]),max(dataset4$amp[dataset4$day=="Friday"]),length=25),
     xlab = "Amps", ylab= "Density",
     main = "Normal curve over Histogram",
     prob= TRUE, col= "brown")
points(seq(min(dataset4$amp[dataset4$day=="Friday"]), max(dataset4$amp[dataset4$day=="Friday"]), length.out=500),
       dnorm(seq(min(dataset4$amp[dataset4$day=="Friday"]), max(dataset4$amp[dataset4$day=="Friday"]), length.out=500),
             mean(dataset4$amp[dataset4$day=="Friday"]), sd(dataset4$amp[dataset4$day=="Friday"])), type="l", col="pink")




lines(density(dataset4$amp[dataset4$day=="Friday"],adjust=4), col = "blue")

fore<-forecast(train,level=c(90,95),h=657)
fore
fore<-as.data.frame(fore)











