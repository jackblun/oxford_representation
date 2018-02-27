### Analysis of representation of groups at Oxford

# Jack Blundell

# 0. Set up -------------------------------------------------


rm(list = ls()) 	
options("scipen"=100, "digits"=4)


setwd("~/Dropbox/Documents/Projects/Oxford under-representation/")

### Load packages

x <- c("dplyr","ggplot2","RColorBrewer","tidyr","forcats")
lapply(x, library, character.only = TRUE) # load the required packages

### Load data

# ethnicity
ox.eth <- read.csv("raw/Oxford_data_ethnicity.csv",sep = "\t",stringsAsFactors =  F)
census.eth <- read.csv("raw/census_ethnicgroup.csv", sep = "\t",stringsAsFactors = F)
grades.eth <- read.csv("raw/dfe_ethnicity.csv",stringsAsFactors = F)

# region
ox.reg <- read.csv("raw/Oxford_data_region.csv",sep = "\t",stringsAsFactors =  F)
ons.reg <- read.csv("raw/onsmid2011population_region.csv", sep = "\t",stringsAsFactors = F)


# 1. Clean -------------------------------------------------

### 1.1 Ethnicity

### 1.1.1 Census data

# Percentages

census.eth <- census.eth[complete.cases(census.eth),]
census.eth$pop.perc <- census.eth$Age.10.to.14/census.eth$Age.10.to.14[which(census.eth$Ethnic.Group == "All categories: Ethnic group" )]*100
census.eth <- census.eth[order(census.eth$pop.perc),]

ethnicities.census <- c("White: Total","Black/African/Caribbean/Black British: Total","Asian/Asian British: Total",
                 "Mixed/multiple ethnic group: Total","Other ethnic group: Total","Chinese","Arab", "White and Black African",
                 "Any other ethnic group","Other Black","Other Mixed","Caribbean","White and Asian","Bangladeshi",
                 "Other Asian","White and Black Caribbean","Indian","African","Pakistani")

census.eth <- census.eth[which(census.eth$Ethnic.Group %in% ethnicities.census),]

# Rename

census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "White: Total")] <- "White"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "Black/African/Caribbean/Black British: Total")] <- "Black"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "Asian/Asian British: Total")] <- "Asian"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "Mixed/multiple ethnic group: Total")] <- "Mixed"

census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "White and Black African")] <- "Mixed Black African/White"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "White and Asian")] <- "Mixed Asian/White"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "White and Black Caribbean")] <- "Mixed Black Caribbean/White"

census.eth$type <- NA
census.eth$type[which(census.eth$Ethnic.Group %in% c("White","Black","Asian","Mixed","Chinese"))] <- "Broad"
census.eth$type[which(census.eth$Ethnic.Group %in% c("Arab","Bangladeshi","Pakistani",
                                                     "Indian","African","Caribbean","Mixed Black African/White",
                                                     "Mixed Asian/White","Mixed Black Caribbean/White"))] <- "Detailed"
census.eth <- census.eth[complete.cases(census.eth),]
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "African")] <- "Black African"
census.eth$Ethnic.Group[which(census.eth$Ethnic.Group == "Caribbean")] <- "Black Caribbean"

# Add detailed white category and chinese category
census.add <- census.eth[which(census.eth$Ethnic.Group == "White" | census.eth$Ethnic.Group == "Chinese"),]
census.add$type <- "Detailed"

census.eth <- rbind(census.eth,census.add)

names(census.eth) <- c("Ethnic group","Population","Population percentage","type")

census.eth.clean <- census.eth


# detailed breakdown
#"White and Asian"
#"Other Asian"
#"Other Black"
#"White and black Caribbean"
#"White"
#"Black Caribbean"
#"Arab"
#"Chinese"
#"Indian"
#"Pakistani"
#"Bangladeshi"
#"White and black African"
#"Other"
#"Other mixed"

# broad breakdown
#"White"
#"Mixed/multiple ethnic groups"
#"Black"
#"Asian"
#"Other"

### 1.1.1 Oxford admissions data

### Generate percentages

# drop 'not known' category
ox.eth <- ox.eth[which(ox.eth$Ethnicity != "refused/Notknown"),]
ox.eth <- ox.eth[complete.cases(ox.eth),]

ox.eth$Applications.perc <- ox.eth$Applications/sum(ox.eth$Applications)*100
ox.eth$Offers.perc <- ox.eth$Offers/sum(ox.eth$Offers)*100

### rename existing categories

ox.eth$eth.group[ox.eth$Ethnicity == "AsainorAsainBritish-Bangaldeshi"] <- "Bangladeshi"
ox.eth$eth.group[ox.eth$Ethnicity == "AsainorAsainBritish-Pakistani"] <- "Pakistani"
ox.eth$eth.group[ox.eth$Ethnicity == "AsainorAsainBritish-Indain"] <- "Indian"
ox.eth$eth.group[ox.eth$Ethnicity == "Chniese"] <- "Chinese"
ox.eth$eth.group[ox.eth$Ethnicity == "BalckorBalckBritsih-Carbibean"] <- "Black Caribbean"
ox.eth$eth.group[ox.eth$Ethnicity == "BalckorBalckBritsih-African"] <- "Black African"
ox.eth$eth.group[ox.eth$Ethnicity ==  "OtherBalckbackground"] <- "Other Black"
ox.eth$eth.group[ox.eth$Ethnicity ==  "Mxied-White&Asain"] <- "Mixed Asian/White"
ox.eth$eth.group[ox.eth$Ethnicity ==  "Mxied-White&BalckAfrican"] <- "Mixed Black African/White"
ox.eth$eth.group[ox.eth$Ethnicity ==  "Mxied-White&BalckCarbibean"] <- "Mixed Black Caribbean/White"
ox.eth$eth.group[ox.eth$Ethnicity ==  "OtherMxiedbackground"] <- "Other Mixed"
ox.eth$eth.group[ox.eth$Ethnicity ==  "OtherAsainbackground" ] <- "Other Asian"
ox.eth$eth.group[ox.eth$Ethnicity ==  "White" ] <- "White"
ox.eth$eth.group[ox.eth$Ethnicity ==  "Arab" ] <- "Arab"

#ox.eth$eth.group[ox.eth$Ethnicity ==  "OtherEthnicbackground" ] <- "Other"

ox.eth$type <- "Detailed"

### Generate new broader categories

ox.eth$eth.broad <- NA
ox.eth$eth.broad[which(ox.eth$eth.group == "White")] <- "White"
ox.eth$eth.broad[which(ox.eth$eth.group == "Chinese")] <- "Chinese"
ox.eth$eth.broad[which(ox.eth$eth.group %in% c("Black African","Other Black",
                                               "Black Caribbean"))] <- "Black"
ox.eth$eth.broad[which(ox.eth$eth.group %in% c("Mixed Asian/White","Mixed Black African/White",
                                               "Mixed Black Caribbean/White","Other Mixed"))] <- "Mixed"
ox.eth$eth.broad[which(ox.eth$eth.group %in% c("Bangladeshi","Pakistani",
                                               "Indian","Other Asian"))] <- "Asian"

ox.eth.broad <- ox.eth %>% group_by(eth.broad)  %>% summarise(sum(Applications), sum(Applications.perc), sum(Offers), sum(Offers.perc))
ox.eth.broad <- ox.eth.broad[complete.cases(ox.eth.broad),]

names(ox.eth.broad) <- c("eth.group","Applications","Applications.perc","Offers","Offers.perc")
ox.eth.broad$type <- "Broad"

### Append

ox.eth <- ox.eth[c("eth.group","Applications","Applications.perc","Offers","Offers.perc","type")]

ox.eth.clean <- rbind(ox.eth,ox.eth.broad)
ox.eth.clean <- ox.eth.clean[complete.cases(ox.eth.clean),]

names(ox.eth.clean)[1] <- "Ethnic group"

### 1.1.3 Merge dataframes

eth.clean <- left_join(ox.eth.clean,census.eth.clean, by = c("Ethnic group","type"))
eth.clean <- eth.clean[complete.cases(eth.clean),]

### Make representativeness measures

eth.clean$Offers.rep <- eth.clean$Offers.perc/eth.clean$`Population percentage`
eth.clean$Applications.rep <- eth.clean$Applications.perc/eth.clean$`Population percentage`

### Make offer success measures

eth.clean$success <- eth.clean$Offers/eth.clean$Applications*100

### subset by type of ethnic breakdown

eth.det.clean <- eth.clean[which(eth.clean$type == "Detailed"),]
eth.broad.clean <- eth.clean[which(eth.clean$type == "Broad"),]

### make long

eth.det.clean.1 <- eth.det.clean[c("Ethnic group","Applications.rep")]
names(eth.det.clean.1)[2] <- "Representation Index"
eth.det.clean.1$type2 <- "Applications"

eth.det.clean.2 <- eth.det.clean[c("Ethnic group","Offers.rep")]
names(eth.det.clean.2)[2] <- "Representation Index"
eth.det.clean.2$type2 <- "Offers"

eth.det.clean.long <- rbind(eth.det.clean.1, eth.det.clean.2)


eth.broad.clean.1 <- eth.broad.clean[c("Ethnic group","Applications.rep")]
names(eth.broad.clean.1)[2] <- "Representation Index"
eth.broad.clean.1$type2 <- "Applications"

eth.broad.clean.2 <- eth.broad.clean[c("Ethnic group","Offers.rep")]
names(eth.broad.clean.2)[2] <- "Representation Index"
eth.broad.clean.2$type2 <- "Offers"

eth.broad.clean.long <- rbind(eth.broad.clean.1, eth.broad.clean.2)

### 1.2 Region

### 1.2.1 ONS census data data

ons.reg$Age.13 <- gsub(ons.reg$Age.13,pattern = ",", replace = "")
ons.reg$Age.13 <- as.numeric(ons.reg$Age.13)
names(ons.reg)[1] <- "Region"

ons.reg$Region[which(ons.reg$Region == "Yorkshire and The Humber")] <- "Yorkshire/Humber"

# as percentage
ons.reg$pop.perc <- ons.reg$Age.13/sum(ons.reg$Age.13)*100

### 1.2.2 Oxford admissions data

ox.reg$Region[ox.reg$Region == "NorthWest"] <- "North West"
ox.reg$Region[ox.reg$Region == "NorthEast"] <- "North East"
ox.reg$Region[ox.reg$Region == "GreaterLondon"] <- "London"
ox.reg$Region[ox.reg$Region == "YorkshireandtheHumber"] <- "Yorkshire/Humber"
ox.reg$Region[ox.reg$Region == "EastMdialnds"] <- "East Midlands"
ox.reg$Region[ox.reg$Region == "WestMdialnds"] <- "West Midlands"
ox.reg$Region[ox.reg$Region == "Eastern"] <- "East"
ox.reg$Region[ox.reg$Region == "SouthEast"] <- "South East"
ox.reg$Region[ox.reg$Region == "SouthWest"] <- "South West"
ox.reg$Region[ox.reg$Region == "Waels"] <- "Wales"
ox.reg$Region[ox.reg$Region == "NorthernIrealnd"] <- "Northern Ireland"
ox.reg$Region[ox.reg$Region == "Scotalnd"] <- "Scotland"

# as percentage
ox.reg$Applications.perc <- ox.reg$Applications/sum(ox.reg$Applications)*100 
ox.reg$Offers.perc <- ox.reg$Offers/sum(ox.reg$Offers)*100 

### 1.2.3 Merging data

reg <- merge(ons.reg,ox.reg, by = "Region")

# make index

reg$Offers.rep <- reg$Offers.perc/reg$pop.perc
reg$Applications.rep <- reg$Applications.perc/reg$pop.perc

# Make success rate

reg$success <- reg$Offers/reg$Applications*100
reg.clean <- reg

# make long

reg.long <- gather(data = reg,key = "Index", value = "Score", 8:9)
reg.long$Index <- gsub(reg.long$Index, pattern = ".rep", replacement = "")
reg.clean.long <- reg.long

### 2. Plot
############################################################

### 2.1 Ethnicity

### Applications / offers

# Detailed ethnicitiesx

eth.det.clean.long$`Ethnic group` <- factor(eth.det.clean.long$`Ethnic group`, 
                                            levels = unique(eth.det.clean.long$`Ethnic group`[order(eth.det.clean.long$`Representation Index`)]))
ggplot(eth.det.clean.long ,aes(x = eth.det.clean.long$`Ethnic group`, y = eth.det.clean.long$`Representation Index`,
                               fill = eth.det.clean.long$type2)) + geom_col(position = "dodge") +
  xlab("Ethnic Group (Detailed)") + ylab("Index of Representation") + 
  coord_flip()

# Broad ethnicities

eth.broad.clean.long$`Ethnic group` <- factor(eth.broad.clean.long$`Ethnic group`, 
                                            levels = unique(eth.broad.clean.long$`Ethnic group`[order(eth.broad.clean.long$`Representation Index`)]))
ggplot(eth.broad.clean.long ,aes(x = eth.broad.clean.long$`Ethnic group`, y = eth.broad.clean.long$`Representation Index`,
                               fill = eth.broad.clean.long$type2)) + geom_col(position = "dodge") +
  xlab("Ethnic Group (Broad)") + ylab("Index of Representation") + 
  coord_flip()

### Offer success

# detailed

eth.det.clean$`Ethnic group` <- factor(eth.det.clean$`Ethnic group`, 
                                            levels = unique(eth.det.clean$`Ethnic group`[order(eth.det.clean$success)]))
ggplot(eth.det.clean ,aes(x = eth.det.clean$`Ethnic group`, y = eth.det.clean$success)) + geom_col() +
  xlab("Ethnic Group (Detailed)") + ylab("Success rate (%)") + geom_col(fill='#00BFC4') + 
  coord_flip()

# broad

eth.broad.clean$`Ethnic group` <- factor(eth.broad.clean$`Ethnic group`, 
                                       levels = unique(eth.broad.clean$`Ethnic group`[order(eth.broad.clean$success)]))
ggplot(eth.broad.clean ,aes(x = eth.broad.clean$`Ethnic group`, y = eth.broad.clean$success)) + geom_col() +
  xlab("Ethnic Group (Broad)") + ylab("Success rate (%)") + geom_col(fill='#00BFC4') + 
  coord_flip()

### 2.2 Region

### applications / offers
reg.clean.long$Region <- factor(reg.clean.long$Region, 
                                       levels = unique(reg.clean.long$Region[order(reg.clean.long$Score)]))
ggplot(reg.clean.long ,aes(x = Region, y = Score, fill = Index)) + geom_col(position = "dodge") +
  xlab("Region") + ylab("Index of Representation") +
  coord_flip()

### Success rates
reg.clean$Region <- factor(reg.clean$Region, 
                                levels = unique(reg.clean$Region[order(reg.clean$success)]))
ggplot(reg.clean ,aes(x = Region, y = success)) + geom_col(fill='#00BFC4') +
  xlab("Region") + ylab("Success rate (%)") +
  coord_flip()


### 3. Output for Shiny App
############################################################

write.csv(eth.broad.clean, file = "eth_broad_clean.csv")
write.csv(eth.broad.clean.long, file = "eth_broad_clean_long.csv")
write.csv(eth.det.clean, file = "eth_det_clean.csv")
write.csv(eth.det.clean.long, file = "eth_det_clean_long.csv")
write.csv(reg.clean, file = "reg_clean.csv")
write.csv(reg.clean.long, file = "reg_clean_long.csv")
