library("tidyverse")
library(ggplot2)
######bar chart######
df.dummy_data<- data.frame(category.var =c("A","B","C","D","E"),numeric_var =c(5,2,9,4,5))
df.dummy_data
str(df.dummy_data)
ggplot(data=df.dummy_data,aes(x=category.var,y=numeric_var))+geom_bar(stat="identity")
###scatterplot####
df.test_data <- data.frame(x_var = 1:50 + rnorm(50,sd=15),
                           y_var = 1:50 + rnorm(50,sd=2)                          
)

ggplot(df.test_data,aes(x=x_var,y=y_var))+geom_point()

#bubble chart####

set.seed(53)
x_var<- rnorm(n=15,mean=5,sd=4)
y_var<- x_var+rnorm(n=15,mean=5,sd=4)
size_var<- runif(15,1,10)
df.test_data<- data.frame(x_var,y_var,size_var)
df.test_data


ggplot(data=df.test_data,aes(x=x_var,y=y_var))+ geom_point(aes(size=size_var))+
scale_size_continuous(range=c(2,10))+
# Modify the size of the bubbles.
theme(legend.position="none")
# Remove the legend, just to simplify the plot.

##line chart###
df.dummy_data<- data.frame(dummy_metric<- cumsum(1:20),date=seq.Date(as.Date("1980-01-01"),by="1 year",length.out = 20))
df.dummy_data
ggplot(data=df.dummy_data,aes(x=date,y=dummy_metric))+geom_line()+geom_point()

year <- c("1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
co2_emission_per_cap_qt <- as.numeric(c("0.836046900792028","0.661428164381093","0.640001899360285","0.625646053941047","0.665524211218076","0.710891381561055","0.574162146975018","0.60545199674633","0.725149509123457","0.942934534989582","1.04223969658961","1.08067663654397","1.09819569131687","1.09736711056811","1.25012409495905","1.28528313553995","1.38884289658754","1.52920110964112","1.5426750986837","1.49525074931082","1.46043181655825","1.56673968353113","1.62905590778943","1.75044806018373","1.87105479144466","1.93943425697654","2.03841068876927","2.1509052249848","2.15307791087471","2.16770307659104","2.24590127565651","2.31420729031649","2.44280065934625","2.56599389177193","2.75575496636525","2.84430958153669","2.82056789057578","2.67674598026467","2.64864924664833","2.69686243322549","2.74212081298895","2.88522504139331","3.51224542766222","4.08013890554173","4.4411506949345","4.89272709798477","5.15356401658718","5.31115185538876","5.77814318390097","6.19485757472686"))
df.china_co2 <- data.frame(year,co2_emission_per_cap_qt)
df.china_co2
#The line geom requires the "group" parameter. That is, you always need to have group= in your aes() call.
#What it does is tell ggplot() what "group" the points belong to. You can imagine a case where you want to plot three separate lines. These three separate lines would each belong to different "groups."
#In the above case, we're only plotting one line, so there's only one group. The way to specify that is with group=1.
ggplot(data=df.china_co2, aes(x=year, y=co2_emission_per_cap_qt,group=1)) +
  geom_line(color="blue", size=1.75) +
  geom_point(color="blue", size=3.5) +
  scale_x_discrete(breaks=c("1961","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010"))+
  ggtitle("China CO2 Emissions,Yearly")+labs(x="",y="CO2 Emissions\n(metric tonnes per capita)")+
  theme(axis.title.y = element_text(size=14, family="Trebuchet MS", color="#666666")) +
  theme(plot.title = element_text(size=26, family="Trebuchet MS", face="bold", hjust=0, color="#666666")) +
  theme(axis.text = element_text(size=16, family="Trebuchet MS"))

###histograms###
set.seed(11)
df.histogram_dummy<-data.frame(x_var=rnorm(2000))
df.histogram_dummy
#default bandwidth(range/30)
ggplot(df.histogram_dummy,aes(x=x_var))+geom_histogram()
#specifying binwidth
ggplot(df.histogram_dummy,aes(x=x_var))+geom_histogram(binwidth=0.5)

####chloropleth map####

ggplot() +
  geom_polygon(data=map.states, aes(x=long,y=lat,group=group), fill=NA, color="#FFFFFF") +
  geom_polygon(data=map.choropleth, aes(x=long,y=lat,group=group,fill=unemp_bin_pct), color="#EEEEEE")  +
  scale_fill_brewer(palette ="YlOrRd") +
  theme(panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  coord_map("polyconic")

print(TSLA_start_to_2018_10_26_CLEAN)

ggplot(data=TSLA_start_to_2018_10_26_CLEAN,aes(x=date,y=close_price))+geom_line(color = '#E51837', size = .6)+labs(title='Tesla stock price from IPO to Oct 2018',
                                                                                       subtitle="TSLA price increased over 10x, but with substantial volatility",x='Date',y='Closing Price',caption='Based on TSLA stock data gathered from Yahoo Finance')+theme(text = element_text(color = "#444444", family = 'Helvetica Neue'),plot.title = element_text(size = 26, color = '#333333'),plot.subtitle = element_text(size = 13),
                                                                                                                                                                                                                                                                  axis.title = element_text(size = 16, color = '#333333'),
                                                                                                                                                                                                                                                                  axis.title.y = element_text(angle = 0, vjust = .5))library(dplyr)
library(ggplot2)

###################
# IMPORT datasets #
###################


df.car_torque <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
df.car_0_60_times  <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
df.car_engine_size <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
df.car_horsepower  <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
df.car_top_speed   <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
df.car_power_to_weight <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))

head(df.car_torque)
head(df.car_0_60_times)
head(df.car_engine_size)
head(df.car_horsepower)
head(df.car_top_speed)
head(df.car_power_to_weight)


df.car_torque %>% group_by(car_full_nm) %>% summarize(count=n()) %>% filter(count!=1)
df.car_0_60_times  %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_horsepower  %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_top_speed   %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

df.car_0_60_times  <- distinct(df.car_0_60_times ,car_full_nm,.keep_all=TRUE)
df.car_engine_size <- distinct(df.car_engine_size ,car_full_nm,.keep_all=TRUE)
df.car_horsepower  <- distinct(df.car_horsepower ,car_full_nm,.Keep_all=TRUE)
df.car_top_speed   <- distinct(df.car_top_speed ,car_full_nm,.keep_all=TRUE)
df.car_torque      <- distinct(df.car_torque ,car_full_nm,.keep_all=TRUE)
df.car_power_to_weight <- distinct(df.car_power_to_weight,car_full_nm,.keep_all=TRUE)

##JOINING DATA###

df.car_spec_data<- left_join(df.car_horsepower,df.car_torque)











