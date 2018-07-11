#Set working directory
setwd("C:/Users/pchak/Desktop/NREM/DropBox/Datasets")


#Import the data file
flow_data = read.csv('flow_data2.csv')
sample = read.csv('sample2.csv')


#Install package "lubridate" and load it in the library before using TL1 & TH1 metric functions:
#install.packages('lubridate')
library(lubridate)


#############################################################################################################
#Part1.a) MA1 - Below function code is to find mean daily discharge between any two dates(i.e., any month of interest, year of interest, season of interest.)
mean_daily_discharge = function(df,a,b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
mean_daily = subset(df, Date>=a & Date <=b, select=-c(1))
output = colMeans(mean_daily[sapply(mean_daily, is.numeric)])
return(output)
}
#Enter DataFrame, start Date & End Date in YYYY-MM-DD format. Below is one example shown:-
mean_daily_discharge(flow_data,"1980-05-01","1980-05-31")
mean_daily_discharge(sample,"1998-01-01", "1998-12-31" )


#Part1.b) - Below function code is to find mean daily discharge for any month between two years.
Mean_month_discharge = function(df, c, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
df$Year = as.numeric(format(df$Date, "%Y"))
df$Month = as.numeric(format(df$Date, "%m"))
Mean_particularmonth = subset(df, Month==c & (Year>=a & Year<=b),select=-c(1))
output=colMeans(Mean_particularmonth[sapply(Mean_particularmonth, is.numeric)])
return(output)
}
#Enter DataFrame, Month of interest, start Year & End Year in YYYY format. Below is one example shown:-
Mean_month_discharge(flow_data, 5, 1980, 1990)
Mean_month_discharge(sample, 12, 1994, 2000)
#############################################################################################################


#############################################################################################################
#Part2.a) - MA3 - Calculation of Coefficient of variation between any two dates( i.e., for any month of interest, year of interest, season of interest)
Coeff_Var_daily = function(df, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
coeff_daily = subset(df, Date>=a & Date <=b, select=-c(1))
colMeans(coeff_daily[sapply(coeff_daily, is.numeric)])
sapply(coeff_daily, sd)
output = sapply(coeff_daily, sd)/colMeans(coeff_daily[sapply(coeff_daily, is.numeric)])*100
return(output)
}
#Enter DataFrame, start Date & End Date in YYYY-MM-DD format. Below is one example shown:-
Coeff_Var_daily(flow_data,"1980-01-01", "1980-01-31")


#Part2.b) - MA3 - Calculation of Coefficient of variation for any month between two years
Coeff_Var_month = function(df, c, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
df$Year = as.numeric(format(df$Date, "%Y"))
df$Month = as.numeric(format(df$Date, "%m"))
coeff_particularmonth = subset(df, Month==c & (Year>=a & Year<=b),select=-c(1))
colMeans(coeff_particularmonth[sapply(coeff_particularmonth, is.numeric)])
sapply(coeff_particularmonth, sd)
output = sapply(coeff_particularmonth, sd)/colMeans(coeff_particularmonth[sapply(coeff_particularmonth, is.numeric)])*100
return(output)
}
#Enter DataFrame, Month of interest, start Year & End Year in YYYY format. Below is one example shown:-
Coeff_Var_month(flow_data, 1, '1980', '1980')
Coeff_Var_month(sample, 4, '1998', '2013')
#############################################################################################################


#############################################################################################################
#Part3.a) - MA27 - Calculation of Coefficient of variations for April month in any year
Coeff_Var_Aprildaily = function(df, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
coeff_Aprildaily = subset(df, Date>=a & Date<=b, select=-c(1))
colMeans(coeff_Aprildaily[sapply(coeff_Aprildaily, is.numeric)])
sapply(coeff_Aprildaily, sd)
output = sapply(coeff_Aprildaily, sd)/colMeans(coeff_Aprildaily[sapply(coeff_Aprildaily, is.numeric)])*100
return(output)
}
#Enter DataFrame, start Date & End Date in YYYY-04-DD (April month only) format. Below is one example shown:-
Coeff_Var_Aprildaily(flow_data,"1980-04-01", "1980-04-27")


#Part3.b) - MA27 - Calculation of Coefficient of variation for April month between two years
Coeff_Var_Aprilmonth = function(df, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
df$Year = as.numeric(format(df$Date, "%Y"))
df$Month = as.numeric(format(df$Date, "%m"))
coeff_Aprilmonth = subset(df, Month==4 & (Year>=a & Year<=b),select=-c(1))
colMeans(coeff_Aprilmonth[sapply(coeff_Aprilmonth, is.numeric)])
sapply(coeff_Aprilmonth, sd)
output = sapply(coeff_Aprilmonth, sd)/colMeans(coeff_Aprilmonth[sapply(coeff_Aprilmonth, is.numeric)])*100
return(output)
}
#Enter DataFrame, start Year & End Year in YYYY format. Below is one example shown:-
Coeff_Var_Aprilmonth(flow_data,'1988', '1993')
Coeff_Var_Aprilmonth(sample,'1998', '2013')
#############################################################################################################


#############################################################################################################
#Part 4.a) - ML5 - Mean minimum daily flows for May between any two years
may_annual_minimum = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$Year = as.numeric(format(df$Date, "%Y"))
  df$Month = as.numeric(format(df$Date, "%m"))
  df = subset(df,Month==5 & (Year>=a & Year<=b), select=-c(1))
  k=list()
  for (i in unique(df[,"Year"]))
  {
    values = min(df[,e][df[,"Year"]==i])
    k=append(k,values)
  }
  k
  output=mean(sapply(k,mean))
  return(output)
}
#Enter DataFrame, Column of interest, start Year & End Year in YYYY format. Below is one example shown:-
may_annual_minimum(flow_data,"seg_outflow_0018_gage_07331300","1980","2000")


#Part 4.b) -ML5 - Mean monthly flow for May between any two years
Mean_may_discharge = function(df, a, b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
df$Year = as.numeric(format(df$Date, "%Y"))
df$Month = as.numeric(format(df$Date, "%m"))
Mean_maymonth = subset(df, Month==5 & (Year>=a & Year<=b),select=-c(1))
output=colMeans(Mean_maymonth[sapply(Mean_maymonth, is.numeric)])
return(output)
}
#Enter DataFrame, Month of interest, start Year & End Year in YYYY format. Below is one example shown:-
Mean_may_discharge(flow_data,'1988', '1993')
Mean_may_discharge(sample,'1998', '1999')
#############################################################################################################


#############################################################################################################
#Part5.a) - MH17 - High flow discharge index - 25% of exceedance value a discharge at the 75th percentile is the same as a discharge at the 25th percent exceedance
High_flow_discharge_index = function(df,a,b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
high_daily = subset(df, Date>=a & Date <=b , select=-c(1))
sapply(high_daily, median)
quants = c(0.75)
apply(high_daily,2,quantile,probs = quants)
output= apply(high_daily,2,quantile,probs = quants)/sapply(high_daily, median)
return(output)
}
#Enter DataFrame, start Date & End Date in YYYY-MM-DD format. Below is one example shown:-
High_flow_discharge_index(flow_data,"1980-01-01", "2000-12-31")
High_flow_discharge_index(sample,"1994-01-01", "2014-12-31")


#Part5.b) - MH17 - High flow discharge index - 25% of exceedance value or the 75th percentile
High_flow_discharge_index_month = function(df,c,a,b)
{
df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
df$Year = as.numeric(format(df$Date, "%Y"))
df$Month = as.numeric(format(df$Date, "%m"))
high_month = subset(df, Month==c & (Year>=a & Year<=b), select=-c(1))
sapply(high_month, median)
quants = c(0.75)
apply(high_month,2,quantile,probs = quants)
output= apply(high_month,2,quantile,probs=quants)/sapply(high_month, median)
return(output)
}
#Enter DataFrame, Month, start Year & End Year in YYYY format. Below is one example shown:-
High_flow_discharge_index_month(flow_data,5,1980, 2000)
High_flow_discharge_index_month(sample,5,1998, 2000)
#############################################################################################################


#############################################################################################################
#Part 6.a.1)- FL3 Frequency of low pulse spells- Daily flows
frequency_low_pulse = function(df,e)
{
  k1= df[,e]
  k2= mean(k1)*0.05
  k3 = subset(k1, k1<k2)
  return(list(mean(k3), length(k3)))
}
#Enter DataFrame, Column of Interest. Below is one example shown:- 
#Calculates the mean of those annual values and number of days
frequency_low_pulse(flow_data, "seg_outflow_0019_gage_07331000")
frequency_low_pulse(sample, "N15")


#Part 6.a.2)- FL3 Frequency of low pulse spells-Calculating annual number of discrete events between any two days
frequency_low_discrete = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df = subset(df, Date>=a & Date<=b, select=-c(1))
  k1= df[,e]
  N = length(k1)
  k2= mean(k1)*0.05
  event=0
  flag=0
  for (i in 1:N)
  {
    if(k1[i]<k2)
    {
      flag=1
    }
    else
    {
      if(flag==1)
      {
        event=event + 1
        flag=0
      }
    }
  } 
  if(flag==1)
  {
    event=event + 1
  }
  return(event)
}
#Enter DataFrame, Column of Interest, Start & End Date in YYYY-MM-DD format. Below is one example shown:-
frequency_low_discrete(flow_data,"seg_outflow_0019_gage_07331000","1980-01-01","2000-12-31")
frequency_low_discrete(sample,"N15","1995-01-01","2000-12-31")


#Part 6.b) - FL3 Frequency of low pulse spells- Monthly flows
frequency_low_pulse_month = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$Year = as.numeric(format(df$Date, "%Y"))
  df$Month = as.numeric(format(df$Date, "%m"))
  k1= df[,e]
  k2= mean(k1)*0.05
  k=list()
  df1 = subset(df, Year>=a & Year<=b, select=-c(1))
  for (i in unique(df1[,"Year"]))
  {
    k3=df1$Month[df1[,e]<k2 & df1[,"Year"]==i]
    values=c(i,length(unique(k3)))
    k=append(k,values)
  } 
  return(sapply(k,mean,is.data.frame=TRUE))
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
frequency_low_pulse_month(flow_data,"seg_outflow_0074_gage_07311500","1980","2000")
#############################################################################################################


#############################################################################################################
#Part 7.a.1)- FH5 Flood frequency for daily flows
flood_frequency= function(df,e)
{
  k1= df[,e]
  k2= median(k1)
  k3 = subset(k1, k1>k2)
  return(list(mean(k3), length(k3)))
}
#Enter DataFrame, Column of Interest. Below is one example shown:- 
flood_frequency(flow_data, "seg_outflow_0036_gage_07303000")


#Part 7.a.2)- FH5 Flood frequency - Calculating annual number of discrete events between any two days
flood_frequency_month_discrete = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df = subset(df, Date>=a & Date<=b, select=-c(1))
  k1= df[,e]
  N = length(k1)
  k2= median(k1)
  event=0
  flag=0
  for (i in 1:N)
  {
    if(k1[i]>k2)
    {
      flag=1
    }
    else
    {
      if(flag==1)
      {
        event=event + 1
        flag=0
      }
    }
  } 
  if(flag==1)
  {
    event=event + 1
  }
  return(event)
}
#Enter DataFrame, Column of Interest, Start & End Date in YYYY-MM-DD format. Below is one example shown:-
flood_frequency_month_discrete(flow_data,"seg_outflow_0018_gage_07331300","1980-01-01","1980-12-31")
#############################################################################################################


#############################################################################################################
#Part 8- FH3 Flood frequency calculated monthly
flood_frequency_month = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$Year = as.numeric(format(df$Date, "%Y"))
  df$Month = as.numeric(format(df$Date, "%m"))
  k1= df[,e]
  k2= median(k1)*3
  k=list()
  df1 = subset(df, Year>=a & Year<=b, select=-c(1))
  for (i in unique(df1[,"Year"]))
  {
    k3=df1$Month[df1[,e]>k2 & df1[,"Year"]==i]
    values=c(i,length(unique(k3)))
    k=append(k,values)
  } 
  return(sapply(k,mean,is.data.frame=TRUE))
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
flood_frequency_month(flow_data,"seg_outflow_0018_gage_07331300","1980","2000")
#############################################################################################################


#############################################################################################################
#Part 9.a.1) - DL16 Low flow pulse duration - Calculates the mean of the annual values 
Low_pulse= function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df1= subset(df, Date>=a & Date <=b, select=-c(1))
  k1= df1[,e]
  k2= quantile(k1,0.25)
  k3 = subset(k1, k1<k2)
  return(mean(k3))
}
Low_pulse(flow_data, "seg_outflow_0036_gage_07303000","1980-01-01","2000-12-31")


#Part 9.a.2) - DL16 Low flow pulse duration - Calculating the average duration of discrete events
low_pulse_duration = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df= subset(df, Date>=a & Date <=b, select=-c(1))
  k1 = df[,e]
  k2= quantile(k1,0.25)
  data_above = 1L*(k1<k2)
  id_start = which(diff(c(0L,data_above))==1)
  id_end = which(diff(c(data_above,0L))== -1)
  res = cbind(k1[id_start],Duration=id_end-id_start+1)
  avg_duration = mean(res[,'Duration'])
  return(avg_duration)
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY-MM-DD format. Below is one example shown:-
low_pulse_duration(flow_data, "seg_outflow_0036_gage_07303000","1981-01-01","1981-12-31")
#############################################################################################################


#############################################################################################################
#Part 10.a.1)- DH17 High flow duration - Calculates the mean of the annual values. 
High_flow= function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df1= subset(df, Date>=a & Date <=b, select=-c(1))
  k1= df1[,e]
  k2= quantile(k1,0.50)
  k3 = subset(k1, k1>k2)
  return(mean(k3))
}
#Enter DataFrame, Column of Interest, Start & End Date in YYYY-MM-DD format. Below is one example shown:-
High_flow(flow_data, "seg_outflow_0036_gage_07303000","1980-01-01","2000-12-31")


#Part 10.a.2) - DH17 High flow duration - Calculating the average duration of discrete events
High_flow_duration = function(df,e,a,b){
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df= subset(df, Date>=a & Date <=b, select=-c(1))
  k1 = df[,e]
  k2= quantile(k1,0.50)
  data_above = 1L*(k1>k2)
  id_start = which(diff(c(0L,data_above))==1)
  id_end = which(diff(c(data_above,0L))== -1)
  res = cbind(k1[id_start],Duration=id_end-id_start+1)
  avg_duration = mean(res[,'Duration'])
  return(avg_duration)
}
#Enter DataFrame, Column of Interest, Start & End Date in YYYY-MM-DD format. Below is one example shown:-
High_flow_duration(flow_data, "seg_outflow_0036_gage_07303000","1981-01-01","1981-12-31")
#############################################################################################################


#############################################################################################################
#Part 12- TL1 - Julian date of annual minimum
# Install package "lubridate" and load it in the library before using this function below: -
julian_annual_minimum = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$julian = yday(as.Date(df$Date,"%m/%d/%Y"))
  df$Year = as.numeric(format(df$Date, "%Y"))
  df = subset(df, Year>=a & Year<=b, select=-c(1))
  k=list()
  for (i in unique(df[,"Year"]))
  {
    k1 = min(df[,e][df[,"Year"]==i])
    values=min(df$julian[df[,e]==k1 & df[,"Year"]==i])
    k=append(k,values)
  }
  output=mean(sapply(k,mean))
  return(output)
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
julian_annual_minimum(flow_data,"seg_outflow_0065_gage_07311200","1980","2000")


julian_annual_minimum_month = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$Year = as.numeric(format(df$Date, "%Y"))
  df$Month = as.numeric(format(df$Date, "%m"))
  df = subset(df, Year>=a & Year<=b, select=-c(1))
  k=list()
  for (i in unique(df[,"Year"]))
  {
    k1 = min(df[,e][df[,"Year"]==i])
    values=min(df$Month[df[,e]==k1 & df[,"Year"]==i])
    k=append(k,values)
  }
  output=mean(sapply(k,mean))
  return(output)
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
julian_annual_minimum_month(flow_data,"seg_outflow_0065_gage_07311200","1980","2000")
#############################################################################################################


#############################################################################################################
#Part 13- TH1 - Julian date of annual maximum
julian_annual_maximum = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$julian = yday(as.Date(df$Date,"%m/%d/%Y"))
  df$Year = as.numeric(format(df$Date, "%Y"))
  df = subset(df, Year>=a & Year<=b, select=-c(1))
  k=list()
  for (i in unique(df[,"Year"]))
  {
    k1 = max(df[,e][df[,"Year"]==i])
    values=min(df$julian[df[,e]==k1 & df[,"Year"]==i])
    k=append(k,values)
  }
  output=mean(sapply(k,mean))
  return(output)
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
julian_annual_maximum(flow_data,"seg_outflow_0018_gage_07331300","1980","2000")


julian_annual_maximum_month = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df$Year = as.numeric(format(df$Date, "%Y"))
  df$Month = as.numeric(format(df$Date, "%m"))
  df = subset(df, Year>=a & Year<=b, select=-c(1))
  k=list()
  for (i in unique(df[,"Year"]))
  {
    k1 = max(df[,e][df[,"Year"]==i])
    values=min(df$Month[df[,e]==k1 & df[,"Year"]==i])
    k=append(k,values)
  }
  output=mean(sapply(k,mean))
  return(output)
}
#Enter DataFrame, Column of Interest, Start & End Year in YYYY format. Below is one example shown:-
julian_annual_maximum_month(flow_data,"seg_outflow_0018_gage_07331300","1980","2000")
#############################################################################################################


#############################################################################################################
#Part 14- RA8 - Number of Reversals
number_of_reversals = function(df,e,a,b)
{
  df$Date=as.Date.character(as.character(df$Date), format="%m/%d/%Y")
  df = subset(df, Date>=a & Date<=b, select=-c(1))
  k1 = diff(df[,e])
  values=0
  i=1
  while (i < length(k1))
  {
    if (k1[i]>0)
    {
      if (k1[i+1]<0)
      {
        values = values + 1
      }
    } 
    else if (k1[i]<0)
    {
      if (k1[i+1]>0)
      {
        values = values + 1
      }
    }
    i = i + 1
  }
  return(values)
}
#Enter DataFrame, Column of Interest, Start & End Date in YYYY-MM-DD format. Below is one example shown:-
number_of_reversals(flow_data,"seg_outflow_0018_gage_07331300","1980-01-01","1980-01-31")
#############################################################################################################





