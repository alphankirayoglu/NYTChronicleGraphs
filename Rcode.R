require(ggplot2)
require(reshape)
require(RColorBrewer)
require(gridExtra)

# Read data and convert to percentage of total articles from # of articles
data<-read.csv("~/Desktop/clean_nyt.csv")
percentdata<-cbind(Year=data$Year,(data[,2:9]/data[,2])*100)
data<-melt(percentdata,id='Year')

#Israel vs Palestine Graph
data3<-subset(data,(variable=="Israel"|variable=="Palestine")&Year>=1900)

r <- ggplot(data3, aes(x=Year, y=value, group=variable))+
  geom_line(aes(colour=variable),size=0.85,alpha=0.5)+theme(legend.key.width = unit(1, "cm"),legend.position = "bottom",legend.text=element_text(size=12))+
  labs(colour = "")+xlab("")+ylab("% of NY Times Articles for a Given Year")+
  scale_x_continuous(breaks =seq(1900,2014,10))+
  annotate("text", x =1917 , y = 1.9 , label = "Belfour Declaration",size = 3.5,angle=90)+
  annotate("text", x =1948 , y = 4.2 , label = "State of Israel Declared",size = 3.5,angle=90)+
  annotate("text", x =1967 , y = 4 , label = "Six Day War",size = 3.5,angle=90)+
  annotate("text", x =1973 , y = 5.1 , label = "Yom Kippur War",size = 3.5,angle=90)+
  annotate("text", x =1978 , y = 3.6 , label = "Camp David Accords",size = 3.5,angle=90)+
  annotate("text", x =1988 , y = 2.5 , label = "State of Palestine Declared",size = 3.5,angle=90)+
  annotate("text", x =1993 , y = 3.5 , label = "Oslo Peace Accord",size = 3.5,angle=90)+
  annotate("text", x =1995 , y = 1.8 , label = "Rabin Assassinated",size = 3.5,angle=90)+
  annotate("text", x =2000 , y = 1.25 , label = "Second Intifada",size = 3.5,angle=90)+
  annotate("text", x =2006 , y = 3.8 , label = "Second Lebanon War",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 3.9 , label = "Operation Pillar Edge",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 6 , label = "",size = 3,angle=90)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Articles containing 'Israel' or 'Palestine' (source:nytlabs chronicle)")
  

#Palestine vs WestBank and Gaza  
data4<-subset(data,(variable=="West_Bank"|variable=="Gaza"|variable=="Palestine")&Year>=1900)

r2 <- ggplot(data4, aes(x=Year, y=value, group=variable))+
  geom_line(aes(colour=variable),size=0.85,alpha=0.6)+theme(legend.key.width = unit(1, "cm"),legend.position = "bottom",legend.text=element_text(size=12))+
  labs(colour = "")+xlab("")+ylab("% of NY Times Articles for a Given Year")+
  scale_x_continuous(breaks =seq(1900,2014,10))+
  annotate("text", x =1917 , y = 1.2 , label = "Belfour Declaration",size = 3.5,angle=90)+
  annotate("text", x =1948 , y = 1.4 , label = "State of Israel Declared",size = 3.5,angle=90)+
  annotate("text", x =1967 , y = 0.7 , label = "Six Day War",size = 3.5,angle=90)+
  annotate("text", x =1973 , y = 1.35 , label = "Yom Kippur War",size = 3.5,angle=90)+
  annotate("text", x =1978 , y = 1.65 , label = "Camp David Accords",size = 3.5,angle=90)+
  annotate("text", x =1988 , y = 1. , label = "State of Palestine Declared",size = 3.5,angle=90)+
  annotate("text", x =1993 , y = 1.15 , label = "Oslo Peace Accord",size = 3.5,angle=90)+
  annotate("text", x =2000 , y = 1.3 , label = "Second Intifada",size = 3.5,angle=90)+
  annotate("text", x =2006 , y = 1.2 , label = "Second Lebanon War",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 1.45 , label = "Operation Pillar Edge",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 3 , label = "",size = 3.5,angle=90)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Articles containing 'Palestine' vs 'Gaza' and 'West Bank' (source:nytlabs chronicle)")


# Palestinian Authority
data5<-subset(data,(variable=="West_Bank"|variable=="Gaza"|variable=="Palestinian_Authority")&Year>=1959)

r3 <- ggplot(data5, aes(x=Year, y=value, group=variable))+
  geom_line(aes(colour=variable),size=0.85,alpha=0.6)+theme(legend.key.width = unit(1, "cm"),legend.position = "bottom",legend.text=element_text(size=12))+
  labs(colour = "")+xlab("")+ylab("% of NY Times Articles for a Given Year")+
  scale_x_continuous(breaks =seq(1960,2014,10))+
  annotate("text", x =1967 , y = 0.5 , label = "Six Day War",size = 3.5,angle=90)+
  annotate("text", x =1973 , y = 0.65 , label = "Yom Kippur War",size = 3.5,angle=90)+
  annotate("text", x =1978 , y = 1.5 , label = "Camp David Accords",size = 3.5,angle=90)+
  annotate("text", x =1988 , y = 1.35 , label = "State of Palestine Declared",size = 3.5,angle=90)+
  annotate("text", x =1993 , y = 0.95 , label = "Oslo Peace Accord",size = 3.5,angle=90)+
  annotate("text", x =2000 , y = 0.95 , label = "Second Intifada",size = 3.5,angle=90)+
  annotate("text", x =2006 , y = 1 , label = "Second Lebanon War",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 1.1 , label = "Operation Pillar Edge",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 1.6 , label = "",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 2 , label = "",size = 3.5,angle=90)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle("Articles containing 'Palestinian Authority' vs 'Gaza'/'West Bank' (source:nytlabs chronicle)")


# People vs State
data6<-subset(data,(variable=="Palestine"|variable=="Palestinian"|variable=="Israeli"|variable=="Israel")&Year>=1959)

r4 <- ggplot(data6, aes(x=Year, y=value, group=variable))+
  geom_line(aes(colour=variable),size=0.85,alpha=0.5)+theme(legend.key.width = unit(1, "cm"),legend.position = "bottom",legend.text=element_text(size=12))+
  labs(colour = "")+xlab("")+ylab("% of NY Times Articles for a Given Year")+
  scale_x_continuous(breaks =seq(1960,2014,10))+
  annotate("text", x =1967 , y = 3.8 , label = "Six Day War",size = 3.5,angle=90)+
  annotate("text", x =1973 , y = 3.2 , label = "Yom Kippur War",size = 3.5,angle=90)+
  annotate("text", x =1978 , y = 4 , label = "Camp David Accords",size = 3.5,angle=90)+
  annotate("text", x =1988 , y = 3.65 , label = "State of Palestine Declared",size = 3.5,angle=90)+
  annotate("text", x =1993 , y = 3.1 , label = "Oslo Peace Accord",size = 3.5,angle=90)+
  annotate("text", x =2000 , y = 3.6 , label = "Second Intifada",size = 3.5,angle=90)+
  annotate("text", x =2006 , y = 3.6 , label = "Second Lebanon War",size = 3.5,angle=90)+
  annotate("text", x =2013 , y = 3.6 , label = "Operation Pillar Edge",size = 3.5,angle=90)+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  ggtitle(paste("Articles containing 'Palestinian' vs 'Palestine' compared to 'Israeli' vs 'Israel","(source:nytlabs chronicle)",sep='\n'))
