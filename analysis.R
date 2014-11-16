library(ggplot2)
dat<-read.csv("Crimes_-_2001_to_present.csv") #Note this data is not actually from 2001 to present; as downloaded November 2014, it included April 2003 to present
childcrimes<-grep("CHILD|MINOR|JUVEN",dat$Description) #Take everything from the table that includes words law enforcement uses regarding children
childcrimes<-childcrimes[grep("NO/MINOR INJURY|CONSUMPTION BY MINOR|TOBACCO PRODUCTS|POSSESSION BY MINOR|MISREPRESENT AGE",dat$Description[childcrimes],invert=TRUE)] #Remove the one instance of homonyms messing that query up, plus underage drinking and drugs (non-ideal, but removes lesser offenses and leaves mainly straightforward victimization of children)
childcrimes<-childcrimes[!is.na(dat$Date[childcrimes])]
x<-dat$Longitude[childcrimes]
y<-dat$Latitude[childcrimes]
t<-strptime(dat$Date[childcrimes],format="%m/%d/%Y %I:%M:%S %p",tz="America/Chicago")
yr<-as.POSIXlt(t)$year+1900
desc<-dat$Description[childcrimes]
rm(dat) #Not strictly necessary, but removes memory-hogging raw data
df <- data.frame(x=x,y=y,t=t,yr=yr,desc=desc)
df <- df[which(df$yr>2003 & df$yr<2014),] #Don't use first or last year, as data from those years do not have all 12 months
rm(x)
rm(y)
rm(yr)
rm(desc)

ggplot(df,aes(x=x,y=y))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colours=c("#3794bf", "#FFFFFF", "#FF0020")) +
	facet_wrap( ~ yr, ncol=5);

library(animation)
ani.record(reset=TRUE)
ani.options(interval=c(1,1,1,1,1,1,1,1,1,3)) #Extra 2 seconds of pause after completing cycle
for (i in 2004:2013) {
dft <- df[df$yr==i,]
print(ggplot(dft,aes(x=x,y=y))+
	stat_density2d(aes(fill=..level..), geom="polygon") +
	scale_fill_gradientn(colours=c("#3794bf", "#FFFFFF", "#FF0020"),limits=c(5,36)) +
	ggtitle(as.character(i)) +
	xlim(-87.82,-87.52) +
	ylim(41.63,42.04))
ani.record()
}

saveGIF(ani.replay(),movie.name="crimesagainstkids.gif")
