library(dplyr)

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

#Remove first 4 columns as they're idential for all
#Except for the 4th column which is just sequential
men <- men[,-(1:4)]
women <- women[,-(1:4)]

#Filter out data just for the US
menus <- filter(men, StateCode == 52)
womus <- filter(women, StateCode == 52)

#filter out data just for Federal prisons
menfed <- filter(men, StateCode = 53)
womfed <- filter(women, StateCode == 53)

#filter out datate just for State Prisons
menst <- filter(men, StateCode == 54)
womst <- filter(women, StateCode == 54)

#Filter out data just for Colorado
menco <- filter(men, StateCode == 06)
womco <- filter(women, StateCode == 06)

#Filter out data by Region
###The Northeast (Connecticut, Maine, Mass, New Hampshire,
### New Jersey, NY, Penn, Rhode Island, Vermont)
menne <- filter(men, Region == 01)
womne <- filter(women, Region == 01)
###North Central (Illinois, Indiana, Iowa, Kansas, Michigan, 
### Minnesota, Missouri, Nebraska, North Dakota, Ohio, 
### South Dakota, Wisconsin)
mennc <- filter(men, Region == 02)
womnc <- filter(women, Region == 02)
###South (Alabama, Arkansas, Deleware, DC, Florida, Geogia, 
### Kentucky, Louisiana, Maryland, Mississippi, North Carolina
### Oklahoma, South Carolina, Tennessee, Texas, Virginia, WV)
menso <- filter(men, Region == 03)
womso <- filter(women, Region == 03)
###West (Alaska, Arizona, California, Colorado, Hawaii, Idaho
### Montana, Nevada, New Mexico, Orgeon, Utah, Washington, Wyoming)
menwe <- filter(men, Region == 04)
womwe <- filter(women, Region == 04)
###All (Total US, Total Federal, Total State)
menall <- filter(men, Region == 08)
womall <- filter(women, Region == 08)




