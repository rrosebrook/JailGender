library(dplyr)
library(acs)
library(ggplot2)
library(reshape2)
library(scales)



#Import the male/female date and break up by character width
mm <- "C:\\Users\\rrosebro\\Desktop\\da09517-0001.txt"
ww <- "C:\\Users\\rrosebro\\Desktop\\da09517-0002.txt"
wid <- c(4, 1, 1, 4, 4, 1, 2, 2, 7, 7, 
          7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 9)
men <- read.fwf(mm, widths = wid)
women <- read.fwf(ww, widths = wid)

#Replace NA value of 9999999 with NA
men <- na_if(men, 9999999)
women <- na_if(women, 9999999)

#Manage column titles
cols <- c("9517", "1", "1", "Code", "Year", "Region", "State", 
          "StateCode", "TotalAdmit", "CourtAdmit", 
          "ParoleAdmit", "EscapeAdmitted", "COReturn", 
          "MentalReturn", "BondReturn", "Transfer", "OtherAdmit", 
          "ParoleAdmitNew", "ParoleAdmitNoNew", "CondRelAmitNew", 
          "CondRelAdmitNoNew", "GeneralPopulation")
colnames(men) <- cols
colnames(women) <- cols

#Remove first 4 columns as they're identical for all
#Except for the 4th column which is just sequential
men <- men[,-(1:4)]
women <- women[,-(1:4)]

#Filter out data just for the US
menus <- filter(men, StateCode == 52)
womus <- filter(women, StateCode == 52)

#filter out data just for Federal prisons
menfed <- filter(men, StateCode == 53)
womfed <- filter(women, StateCode == 53)

#filter out datate just for State Prisons
menst <- filter(men, StateCode == 54)
womst <- filter(women, StateCode == 54)

#Filter out data just for Colorado
menco <- filter(men, StateCode == 06) 
womco <- filter(women, StateCode == 06) 

#Filter out data by Region, summarized by year
###The Northeast (Connecticut, Maine, Mass, New Hampshire,
### New Jersey, NY, Penn, Rhode Island, Vermont)
menne <- filter(men, Region == 01) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
womne <- filter(women, Region == 01) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
northeast <- merge(menne, womne, by="Year")
colnames(northeast) <- c("Year", "men.NorthEast", "women.NorthEast")
###North Central (Illinois, Indiana, Iowa, Kansas, Michigan, 
### Minnesota, Missouri, Nebraska, North Dakota, Ohio, 
### South Dakota, Wisconsin)
mennc <- filter(men, Region == 02) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
womnc <- filter(women, Region == 02) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
northcen <- merge(mennc, womnc, by="Year")
colnames(northcen) <- c("Year", "men.NorthCentral", "women.NorthCentral")
###South (Alabama, Arkansas, Deleware, DC, Florida, Geogia, 
### Kentucky, Louisiana, Maryland, Mississippi, North Carolina
### Oklahoma, South Carolina, Tennessee, Texas, Virginia, WV)
menso <- filter(men, Region == 03) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
womso <- filter(women, Region == 03) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
south <- merge(menso, womso, by="Year")
colnames(south) <- c("Year", "men.South", "women.South")
###West (Alaska, Arizona, California, Colorado, Hawaii, Idaho
### Montana, Nevada, New Mexico, Orgeon, Utah, Washington, Wyoming)
menwe <- filter(men, Region == 04) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
womwe <- filter(women, Region == 04) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
west <- merge(menwe, womwe, by="Year")
colnames(west) <- c("Year", "men.West", "women.West")
###All (Total US, Total Federal, Total State)
menall <- filter(men, Region == 08) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
womall <- filter(women, Region == 08) %>% group_by(Year) %>%
  summarise(avg = median(TotalAdmit))
all <- merge(menall, womall, by="Year")
colnames(all) <- c("Year", "avg.menall", "avg.womall")

###Aggregating the regions into one dataframe
total <- merge(northeast, northcen, by="Year")
total <- merge(total, south, by="Year")
total <- merge(total, west, by="Year")
total <- merge(total, all, by="Year")
total$men.ColoOnly <- menco$TotalAdmit
total$women.ColoOnly <- womco$TotalAdmit

##Graphing the data
theme_set(theme_light())

##Tidy data to be graphed
total2 <- reshape2::melt(total, id.var="Year")

##"All" data is too out of scope, removing
total2 <- filter(total2, variable != "avg.menall")
total2 <- filter(total2, variable != "avg.womall")


#Setting up the legend
groups <- data.frame(do.call('rbind', strsplit(as.character(total2$variable),
                                               '.',fixed=TRUE))) 
colnames(groups) <- c("Gender", "Region")
total2$Gender <- groups$Gender
total2$Region <- groups$Region  


#Graphing the data
ggplot(total2, aes(x=Year, y = value, color = Region, linetype = Gender)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  xlim(1974, 1987) + ylab("Average Regional Incarceration") + 
  ggtitle("Incarceration Rates of Men vs Women") +
  theme(legend.text = element_text(size=12))

#head(total2)

#Graph of regional averages
##ggplot(data = total, aes(x = Year)) +
##  geom_line(aes(y = avg.menne), color="red", size = 1) +
##  geom_line(aes(y = avg.womne), color="red", linetype="dashed", size = 1) +
##  geom_line(aes(y = avg.mennc), color="steelblue", size = 1) +
##  geom_line(aes(y = avg.womnc), color="steelblue", linetype="dashed", size = 1) +
##  geom_line(aes(y = avg.menso), color="darkolivegreen4", size = 1) +
##  geom_line(aes(y = avg.womso), color="darkolivegreen4", 
##            linetype="dashed", size = 1) +
##  geom_line(aes(y = avg.menwe), color="purple", size = 1) +
##  geom_line(aes(y = avg.womwe), color="purple", linetype="dashed", size = 1) +
##  geom_line(aes(y = avg.menco), color="darkgoldenrod2", size = 1) +
##  geom_line(aes(y = avg.womco), color="darkgoldenrod2", linetype="dashed", 
##            size = 1) +
##  xlim(1974,1987) + ylab("Average Regional Incarceration") +
##  ggtitle("Incarceration Rates of Men vs Women") 

#head(total)
#tail(filter(total2, variable == "avg.menco"))
