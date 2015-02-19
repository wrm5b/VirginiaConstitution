# 1) Get data

## A) Download 1900 US Census data from NGHIS.org

### Source: 1900 Census: Population, Housing, Agriculture & Manufacturing Data [US, States & Counties]
### The College of William and Mary and the Minnesota Population Center. School Attendance Boundary Information System (SABINS): Version 1.0. Minneapolis, MN: University of Minnesota 2011.
### "The 3 datasets in this group provide population, housing, agriculture and manufacturing data from the
### 1900 Census for the US, states (including territories) and--in 2 datasets--counties."
### Digital source: ICPSR Study 2896, Parts 20, 21 and 50. (Updates in either the ICPSR or NHGIS data may have produced incomplete correspondence.)
### Original ICPSR data requires an institutional subscription.

session.info <- sessionInfo()

if(!file.exists("nhgis0006_ds31_1900_county.csv")){
        temp <- tempfile()
        fileUrl <- "https://data2.nhgis.org/extracts/111780/6/nhgis0006_csv.zip"
        download.file(fileUrl, temp)
        dateDownloaded1 <- date()

        unzip(temp)
        unlink(temp)
}

data <- read.csv("nhgis0006_ds31_1900_county.csv", skip=1)
str(data)
 
### Subset Virginia data and clean

va <- data[which(data$State.Name == "Virginia"), ]
va$GIS.Join.Match.Code <- NULL
va$Data.File.Year <- NULL
va$State.Name <- NULL
va$State.Code <- NULL

names(va)[names(va)=="Total"] <- "population"
names(va)[names(va)=="Total.1"] <- "urban.population"
names(va)[names(va)=="Total.2"] <- "voting.population"
names(va)[names(va)=="Capital.invested.in.manufacturing"] <- "manufacturing.capital"

### Transform data

va$negro.population <- va$Negro....Male + va$Negro....Female
va$percent.negro <- (va$negro.population/va$population) * 100
va$percent.farmland <- ((va$Land.in.farms/640)/va$Land.surface.area) * 100
va$percent.illiterate.whites <- (va$White....Illiterate/(va$White....Literate + va$White....Illiterate)) * 100
va$population.density <- va$population/va$Land.surface.area

### Create a factor variable for whether the locality has any urban population

va$urban.center <- as.factor(ifelse(va$Urban==0, "no", "yes"))
va$capital.farmland <- va$manufacturing.capital/va$percent.farmland
### Create a factor variable for whether the locality is a city or a county
va$locality.type <- as.factor(ifelse(va$County.Code>5000, "city", "county"))

## B) Scrape data on the 1900 presidential election from politicalgraveyard.com

### Source: Lawrence Kestenbaum, The Political Graveyard, 14 Dec. 2014. Archived on the Internet Archive.
### "The original source was the Biographical Directory of the U.S. Congress (1989), 
### but I have also obtained information from many other biographical compilations, 
### from state legislative manuals, the Congressional Directory, newspaper obituaries, 
### library vertical files, and the hundreds of genealogists and political historians 
### who have shared their research with me." I will include a bibliography one of these days."
#### Email to ask about that particular data?

library(XML)

election_url <- "http://www.politicalgraveyard.com/geo/VA/ofc/pr1900-counties.html"
html <- htmlTreeParse(election_url, useInternalNodes=T)
election_1900 <- xpathSApply(html, "//li[a]", xmlValue)

election_dem <- election_1900[7]
election_dem <- strsplit(gsub("(\n- )([[:upper:]])|(: )|( - )","~\\2",election_dem), "~")
election_dem <- unlist(election_dem)
election_dem <- election_dem[2:84]
election_dem <- gsub("(\nCounty)|( County)|", "", election_dem)
election_dem <- gsub("(\n)", " ", election_dem)
election_dem <- gsub(" city city", "city", election_dem)
dem <- rep(c("Democrat"), each = 83)

election_rep <- election_1900[8]
election_rep <- strsplit(gsub("(\n- )([[:upper:]])|(: )|( - )","~\\2",election_rep), "~")
election_rep <- unlist(election_rep)
election_rep <- election_rep[2:35]
election_rep <- gsub("(\nCounty)|( County)|( $)", "", election_rep)
election_rep <- gsub("(\n)", " ", election_rep)
election_rep[34] <- gsub("( )", "", election_rep[34])
election_rep <- gsub(" city city", "city", election_rep)
rep <- rep(c("Republican"), each=34)

election_party <- rbind(cbind(election_dem, dem), cbind(election_rep, rep))
colnames(election_party) <- c("county", "election.1900")
election_party <- gsub("city", "City", election_party)
election_party <- gsub(" and", " And", election_party)
election_party <- gsub(" of", " Of", election_party)
election_party <- gsub("ManchesterCity", "Manchester City", election_party)
election_party <- rbind(election_party, c("Matthews", "Republican"))

va1 <- merge(va, election_party, by.x="County.Name", by.y="county", all.x=T)

## Scrape Convention delegate data from New River Notes (confirmed by VHS source),
## maintained by the Graycon Count, Virginia Heritage Foundation, Inc. Archived on the Internet Archive.

library(rvest)

delegate_election <- html("http://www.newrivernotes.com/historical_20th_1901_virginia_constitutional_conv_directory.htm")
date.scraped <- date()

delegate_table <- delegate_election %>% html_nodes("table") %>% .[[1]] %>% html_table()

delegate_table$County <- gsub("King and Queen", "King And Queen", delegate_table$County)
delegate_table$County <- gsub("Isle of Wight", "Isle Of Wight", delegate_table$County)
delegate_table$County <- gsub("Washington", "Washington and Bristol City", delegate_table$County)
delegate_table$party <- ifelse(grepl("(Rep)", delegate_table$Representatives), "Republican", "Democrat")
delegate_table$party[grep("(Ind)", delegate_table$Representatives)] <- "Independent"

### Split out counties using " and " in the County column

delegate_county <- strsplit(as.character(delegate_table[,1]), "( and )|(, )")
delegates1 <- data.frame(col1= unlist(delegate_county), col2= rep(delegate_table[,2], sapply(delegate_county, length)), col3= rep(delegate_table$party, sapply(delegate_county, length)))

### Clean data in preparation for merge, especially county names

colnames(delegates1) <- c("county", "delegates", "party")
delegates1$county <- gsub("Accomac", "Accomack", delegates1$county)
delegates1$county[11] <- "Staunton City"
delegates1$county[5] <- "Alexandria"
delegates1$county[33] <- "Roanoke"
delegates1$county[28] <- "Powhatan"
delegates1$county <- gsub("Williamsburg", "Williamsburg City", delegates1$county)
delegates1$county <- gsub("Winchester", "Winchester City", delegates1$county)
delegates1$county <- gsub("Radford", "Radford City", delegates1$county)
delegates1$county <- gsub("Portsmouth", "Portsmouth City", delegates1$county)
delegates1$county <- gsub("Petersburg", "Petersburg City", delegates1$county)
delegates1$county <- gsub("Norfolk County", "Norfolk", delegates1$county)
delegates1$county <- gsub("Newport News", "Newport News City", delegates1$county)
delegates1$county <- gsub("Greenville", "Greensville", delegates1$county)
delegates1$county <- gsub("Manchester", "Manchester City", delegates1$county)
delegates1$county <- gsub("Fredericksburg", "Fredericksburg City", delegates1$county)
delegates1$county <- gsub("Danville", "Danville City", delegates1$county)
delegates1$county <- gsub("Charlottesville", "Charlottesville City", delegates1$county)
delegates1$county <- gsub("Buena Vista", "Buena Vista City", delegates1$county)
delegates1$delegates <- as.character(delegates1$delegates)
delegates1 <- rbind(delegates1, c("Fairfax", "R. W. Moore", "Democrat"))
delegates1 <- rbind(delegates1, c("Matthews", "NA", "Democrat"))

dups <- which(duplicated(delegates1$county))
delegates2 <- delegates1[-dups, ]
      
va2 <- merge(va1, delegates2, by.x="County.Name", by.y="county", all.x=T)


## C) Manually enter Covnention referendum results from Richmond Dispatch, Volume 1900, Number 15342, 7 June 1900
## (http://virginiachronicle.com/cgi-bin/virginia?a=d&d=RD19000607&e=-------en-20--1--txt-txIN-------#)
### (Not exact same as mcdanel/Dispatch, but their sums may be off)

counties <- c("Accomack", "Albemarle", "Alexandria", "Alleghany", "Amelia", "Amherst",
              "Appomattox", "Augusta", "Bath", "Bedford", "Bland", "Botetourt", "Brunswick",
              "Buchanan", "Buckingham", "Campbell", "Carroll", "Caroline", "Charles City",
              "Charlotte", "Chesterfield", "Clarke", "Craig", "Culpeper", "Cumberland",
              "Dinwiddie", "Elizabeth City", "Essex", "Fairfax", "Fauquier", "Floyd",
              "Fluvanna", "Franklin", "Frederick", "Giles", "Gloucester", "Goochland", "Grayson",
              "Greene", "Greensville", "Halifax", "Hanover", "Henrico", "Henry", 
              "Highland", "Isle Of Wight", "James City", "King George", "King And Queen",
              "King William", "Lancaster", "Lee", "Loudoun", "Louisa", "Lunenburg",
              "Madison", "Mathews", "Mecklenburg", "Middlesex", "Montgomery", "Nansemond", 
              "Nelson", "New Kent", "Norfolk", "Northampton", "Northumberland", "Nottoway", "Orange",
              "Page", "Patrick", "Pittsylvania", "Powhatan", "Princess Anne", 
              "Prince Edward", "Prince George", "Prince William", "Pulaski", "Rappahannock",
              "Richmond", "Roanoke", "Rockbridge", "Rockingham", "Russell", "Scott",
              "Shenandoah", "Smyth", "Spotsylvania", "Southampton", "Stafford",
              "Surry", "Sussex", "Tazewell", "Warren", "Warwick", "Washington",
              "Westmoreland", "Wise", "Wythe", "York", "Alexandria City", "Bristol City", 
              "Buena Vista City", "Charlottesville City", "Danville City", "Fredericksburg City",
              "Lynchburg City", "Manchester City", "Newport News City", "Norfolk City",
              "Petersburg City", "Portsmouth City", "Radford City", "Richmond City",
              "Roanoke City", "Staunton City", "Williamsburg City", "Winchester City")
convention.for <- as.numeric(c(892, 1659, 79, 118, 415, 901, 502, 1029, 153, 1516, 254, 667, 935, 12, 395,
                    880, 313, 621, 74, 612, 854, 381, 194, 1035, 212, 478, 463, 333, 466,
                    1102, 286, 401, 863, 168, 410, 577, 304, 338, 269, 544, 1406, 834,
                    1101, 652, 95, 849, 225, 164, 319, 365, 374, 226, 750, 833, 453, 478,
                    225, 1128, 324, 654, 638, 1050, 135, 968, 717, 377, 939,518, 
                    335, 172, 1366, 352, 230, 647, 167, 449, 495, 247, 158, 541, 818,
                    1118, 330, 243, 569, 476, 275, 1750, 213, 357, 532, 475, 311, 128,
                    614, 297, 329, 486, 220, 686, 512, 139, 625, 1266, 524, 1386, 414,
                    2427, 4763, 871, 1365, 343, 5072, 2392, 639, 191, 574))
convention.against <- as.numeric(c(556, 800, 432, 127, 562, 561, 67, 618, 181, 1013, 294, 958,
                        842, 157, 328, 853, 1309, 657, 326, 136, 575, 313, 141, 466,
                        138, 357, 571, 358, 627, 471, 1029, 443, 491, 261, 229, 483, 
                        605, 1044, 218, 248, 719, 749, 399, 565, 109, 242, 159, 503,
                        473, 581, 642, 737, 610, 826, 280, 194, 144, 1478, 473, 985, 703,
                        525, 353, 2503, 434, 698, 252, 368, 349, 802, 676, 565,
                        335, 583, 112, 298, 894, 198, 337, 498, 862, 968, 540, 528, 1093,
                        901, 494, 738, 430, 365, 238, 1220, 49, 145, 1878, 588, 405, 523,
                        140, 615, 286, 120, 133, 140, 264, 821, 119, 369, 586, 239,
                        170, 114, 781, 640, 198, 85, 345))

convention <- cbind.data.frame(counties, convention.for, convention.against)

## D) Manually enter area as determined by: West (bordering/west of the Blue Ridge), East (along the Atlantic and in the Tidwater basin), Central

area <- c("East", "Central", "East", "East", "West", "Central", "Central", "Central", "West", "West", "Central", "West", "West", "West", "Central", "West", "Central", "West", 
"Central", "East", "West", "East", "Central", "Central", "Central", "Central", "West", "Central", "Central", "Central", "West", "Central", "East", "East", "East", "Central", 
"West", "Central", "Central", "West", "Central", "West", "East", "Central", "West", "Central", "Central", "Central", "Central", "Central", "Central", "West", "East", 
"East", "East", "East", "East", "East", "West", "Central", "Central", "Central", "Central", "Central", "Central", "East", "Central", "East", "West", "East", "Central", "East", 
"East", "East", "East", "East", "East", "Central", "Central", "Central", "Central", "Central", "Central", "East", "Central", "Central", "East", "East", "East", "West", "West", 
"Central", "East", "Central", "West", "West", "West", "West", "West", "West", "West", "West", "East", "Central", "East", "West", "East", "East", "West", "Central", "East", "West", 
"East", "East", "West", "West", "West", "East")

## E) Create final working dataset and cache as new data set

dat1 <- subset(va2, select = c(County.Name, locality.type, population, population.density, urban.center, negro.population, percent.negro, voting.population, percent.illiterate.whites, manufacturing.capital, percent.farmland, capital.farmland, Land.surface.area, election.1900, delegates, party))
dat1$County.Name <- gsub("Matthews", "Mathews", dat1$County.Name)

Dat <- merge(dat1, convention, by.x = "County.Name", by.y= "counties", all.x=T)
Dat$Convention <- Dat$convention.for - Dat$convention.against
Dat$area <- area
Dat$Convention.outcome <- ifelse(Dat$Convention > 0, "for", "against")

write.csv(Dat, file = "1900Counties3.csv")

# 2) Exploratory analysis

## Use cache, if possible

Dat <- read.csv("1900Counties3.csv", header=T)

### Remove Dickenson County for referendum analysis, in which "the polls were note opened at many of the precincts" and the vote that was cast was "very light-so light that no return was made to the Secretary of the Commonwealth." (Richmond Dispatch, 7 June 1900).

dat <- Dat[complete.cases(Dat$Convention),]
    
## A) Summary information

dat.str <- str(dat)
summary <- summary(dat)
mean.percent.illiterate.whites <- mean(dat$percent.illiterate.whites)
table.1900.election <- table(va2$election.1900)
for.convention <- sum(dat$convention.for, na.rm=T)
against.convention <- sum(dat$convention.against, na.rm=T)
county.convention <- sum(dat$Convention[(dat$locality.type %in% "county")], na.rm=T)
county <- subset(dat, dat$locality.type %in% "county")
counties.for <- length(county$Convention[county$Convention > 0])
counties.against <- length(county$Convention[county$Convention < 0])
city.convention <- sum(dat$Convention[(dat$locality.type %in% "city")], na.rm=T)
convention.referendum.result <- sum(dat$Convention, na.rm=T)
table.party <- table(dat$party)

## B) Exploratory plots w/ Convention[Use for, against -> create difference for dependent variable]

pairs(Convention~locality.type+population + population.density+urban.center+negro.population+percent.negro+percent.illiterate.whites
      +voting.population + manufacturing.capital + percent.farmland + capital.farmland +
          election.1900, dat, main="Exploratory Analysis")

source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
datMat <- dist(dat)
hclustering <- hclust(datMat)
myplclust(hclustering)

# 3) Convention regression: maybe no longer probit. use vote differential. Area???
## logs?

Convention.regression <- lm(Convention ~ population.density + urban.center + percent.negro + 
                      percent.illiterate.whites + percent.farmland + manufacturing.capital + capital.farmland, x = TRUE, data = dat)

summary(Convention.regression)

## Probit Convention regression

Convention.probit <- glm(Convention.outcome ~ I(area == "West") + I(area == "East") + population.density + urban.center + percentblack + 
                      percent.illiterate.whites + percent.farmland + manufacturing.capital + capital.farmland, x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Convention.probit)

# Marginal Effects

library(erer)
maBina(w = Convention.probit)

#######

# 4) Create graphics

library(maptools)
library(ggplot2)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(foreign)
library(extrafont)
font_import()
loadfonts()
library(rgeos)
        
## A) Download geographical data from NHGIS.org. 2000 Tiger Line +
### "These boundaries are derived from the U.S. Census Bureau's 2008 TIGER/Line 
### files, or they have been conflated to align with 2008 TIGER/Line features as
### consistently as possible.
#### Check to be sure using 2008 files?

if(!file.exists("nhgis0001_shape.zip")){
    temp2 <- tempfile()
    fileUrl2 <- "https://data2.nhgis.org/extracts/111780/1/nhgis0001_shape.zip"
    download.file(fileUrl2, temp)
    dateDownloaded2 <- date()
    
    unzip(temp2)
    unlink(temp2)
}

## B) Read in GIS data

usdbf <- read.dbf("US_county_1900_conflated.dbf")

us <- readOGR(dsn = ".", "US_county_1900_conflated")
attributes <- c("NHGISNAM", "STATENAM")
newNames <- c("County.Name", "state")

us_subset <- us[,attributes]
names(us_subset) <- newNames
data_name <- "US"
assign(data_name, spTransform(us_subset, CRS("+proj=longlat")))

save(list=c(data_name), file=paste("USCounty.RData", sep="/"))


## C) Subset Virginia counties

USva <- US[US$state %in% "Virginia", ]

### Alexandria City and County are conflated in the NHGIS data. Combine the two with sums or weighted means as possible.

map.data <- Dat[-4,]
alexandria <- Dat[3:4,]
map.data[3,]$population <- sum(alexandria$population)
map.data[3,]$negro.population <- sum(alexandria$negro.population)
map.data[3,]$percent.negro <- map.data[3,]$negro.population / map.data[3,]$population
map.data[3,]$voting.population <- sum(alexandria$votingpopulation)
map.data[3,]$percent.illiterate.whites <- (alexandria[1,]$percent.illiterate.whites 
                                           + (alexandria[2,]$percent.illiterate.whites * 
                                                  (alexandria[1,]$voting.population/alexandria[2,]$voting.population))) / 2
map.data[3,]$manufacturing.capital <- sum(alexandria$manufacturing.capital)
map.data[3,]$convention.for <- sum(alexandria$convention.for)
map.data[3,]$convention.against <- sum(alexandria$convention.against)
map.data[3,]$Convention <- sum(alexandria$Convention)

### Add the relevant data

USva$party = map.data$party
USva$area = map.data$area
USva$election.1900 = map.data$election.1900
USva$Convention = map.data$Convention
USva$Convention.outcome = map.data$Convention.outcome

## D1) Plot 1900 voting map using base plot

USva1 <- subset(USva, Convention=="for" & election1900=="Democrat")
USva2 <- subset(USva, Convention=="for" & election1900=="Republican")
USva3 <- subset(USva, Convention=="against" & election1900=="Republican")
USva4 <- subset(USva, Convention=="against" & election1900=="Democrat")

pdf("County voting map, 1900 referendum.pdf", family="Times New Roman", width=4, height=4)

plot(USva)
plot(USva1, add=TRUE, col = "navy")
plot(USva2, add=TRUE, col = "darkred")
plot(USva3, add=TRUE, col = "red")
plot(USva4, add=TRUE, col = "cornflowerblue")

title(main="County voting map, \n1900 referendum and presidential election", cex=0.3)
labels <- c("For Convention and Democrat", "For Convention but Republican", "Against Convention and Republican", "Against Convention but Democrat")
label_cols <- c("navy", "darkred", "red", "cornflowerblue")
legend("topleft", NULL, labels, fill=label_cols, density, bty="n", cex = 0.48)

dev.off()

## D2) Plot with ggplot2

USva$percent.illiterate.whites = map.data$percent.illiterate.whites
USva$population.density = map.data$population.density
USva$urban.center = map.data$urban.center
USva$party = map.data$party
USva$population = map.data$population
USva$percent.negro = map.data$percent.negro
USva$percent.farmland = map.data$percent.farmland
USva$manufacturing.capital = map.data$manufacturing.capital
USva$sq.mi = map.data$sq.mi
USva$capital.farmland = map.data$capital.farmland
USva$locality.type = map.data$locality.type

### Fortify the map

USva.f <- fortify(USva, region = "County.Name")
USva.f <- merge(USva.f, USva@data, by.x = "id", by.y = "County.Name")

## Plot 1900 voting data using ggplot2: need to fix for negative values on alpha = Convention: Have blue and red. Try using green as alpha?

map1900 <- ggplot(USva.f, aes(long, lat, group = group, alpha = Convention.outcome, fill = election.1900)) +
    geom_polygon() + coord_equal() + labs(fill = "Presidential election", alpha = "Constitutional convention") +
    ggtitle ("1900 voting map, \npresidential election and convention referendum")

Palette1900 <- c("navy", "darkred")

map1900 + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + 
    scale_fill_manual(values=Palette1900, breaks = c("Democrat", "Republican")) +
    scale_alpha_manual(values=c(1, 0.65, 0), breaks = c("for", "against")) +
    theme_bw(base_family = "Times New Roman", base_size = 12) + 
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
          panel.grid.minor=element_blank())

ggsave("Voting Map, 1900.pdf", scale = 1, dpi = 400)

### Darker constituencies indicate resistance to the norm

## Plot 1900 Constitutional Convention Referendum results: alpha based on the spread of the vote

### Make all Convention vote tally positive

USva.f$Convention <- abs(USva.f$Convention)

referendum.map <- ggplot(USva.f, aes(long, lat, group = group, alpha = Convention, fill = Convention.outcome)) +
    geom_polygon() + coord_equal() + labs(fill = "Constitutional Convention referendum", alpha = "Convention vote tally") +
    ggtitle ("1900 voting map, \n Constitutional convention referendum")

Palette1900a <- c("darkred", "navy")

referendum.map + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + 
    scale_fill_manual(values=Palette1900a, breaks = c("for", "against")) +
    scale_alpha(range = c(0.45, 1)) +
    theme_bw(base_family = "Times New Roman", base_size = 12) + 
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
          panel.grid.minor=element_blank())

ggsave("Constitutional Convention, 1900.pdf", scale = 1, dpi = 400)

### Darker areas indicate a more sweeping result.

## Plot area (use 2 federal judicial districs instead?)

areamap <- ggplot(USva.f, aes(long, lat, group = group, fill = area)) +
    geom_polygon() + coord_equal() + labs(fill = "Area") +
    ggtitle ("Regions of Virginia, 1900")

greyPalette <- c("grey50", "grey75", "grey25")

areamap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=greyPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Regions of Virginia, 1900.pdf", scale = 1, dpi = 400)

## Plot white illiteracy: more in south AND WEST

whiteilliteracymap <- ggplot(USva.f, aes(long, lat, group = group, fill = percent.illiterate.whites)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent illiterate whites") +
    ggtitle ("Illiterate white voters\nas a proportion of white population, 1900")

whiteilliteracymap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Illiterate white voters as a proportion of white population, 1900.pdf", scale = 1, dpi = 400)

## Plot population density

populationdensitymap <- ggplot(USva.f, aes(long, lat, group = group, fill = population.density)) +
    geom_polygon() + coord_equal() + labs(fill = "Population density") +
    ggtitle ("Population density, 1900")

populationdensitymap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey75", high = "grey25", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Population density, 1900.pdf", scale = 1, dpi = 400)

## Plot urbanicity  -- not very useful score

urbanicitymap <- ggplot(USva.f, aes(long, lat, group = group, fill = urbanicity)) +
    geom_polygon() + coord_equal() + labs(fill = "Urbanicity score") +
    ggtitle ("Urbanicity, 1900")

urbanicitymap + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot percent black -- solid west --> east 

percentnegromap <- ggplot(USva.f, aes(long, lat, group = group, fill = percent.negro)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent black") +
    ggtitle ("Percent of population comprised of black persons, 1900")

percentnegromap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Percent black, 1900.pdf", scale = 1, dpi = 400)

## Plot restored gov -- unhelpful, too small sample  

## restoredgovmap <- ggplot(USva.f, aes(long, lat, group = group, fill = restoredgov)) +
    ##geom_polygon() + coord_equal() + labs(fill = "restoredgov") +
    ##ggtitle ("restoredgov, 1900")

## restoredgovmap + scale_fill_manual(values=cbPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot urban (maybe incorporated?) centers --  nicely random
### Something's off... it's because urban.center captures if they counted any of the population as urban. clearly unhelpful.
### See below for better

##urbancentermap <- ggplot(USva.f, aes(long, lat, group = group, fill = urban.center)) +
    geom_polygon() + coord_equal() + labs(fill = "Incorporated city") +
    ggtitle ("Surrounding or bordering an incorporated city, 1900")

##binaryPalette1 <- c("grey50", "black")

##urbancentermap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=binaryPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

##ggsave("Incorporated cities, 1900.pdf", scale = 1, dpi = 400)

## Plot incorporated cities

citiesmap <- ggplot(USva.f, aes(long, lat, group = group, fill = locality.type)) +
    geom_polygon() + coord_equal() + labs(fill = "Incorporated city") +
    ggtitle ("Incorporated cities, 1900")

binaryPalette <- c("black", "grey50")

citiesmap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=binaryPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Incorporated cities, VA 1900.pdf", scale = 1, dpi = 400)

## Plot population -- 

populationmap <- ggplot(USva.f, aes(long, lat, group = group, fill = population)) +
    geom_polygon() + coord_equal() + labs(fill = "population") +
    ggtitle ("population, 1900")

populationmap + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

## Plot party --  west

partymap <- ggplot(USva.f, aes(long, lat, group = group, fill = party)) +
    geom_polygon() + coord_equal() + labs(fill = "Party") +
    ggtitle ("Party of delegate elected to the 1901-02 Convention")

partymap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=greyPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Delegate party, 1900.pdf", scale = 1, dpi = 400)

## Plot manufacturingcapital -- a bit too random

manufacturingcapitalmap <- ggplot(USva.f, aes(long, lat, group = group, fill = manufacturing.capital)) +
    geom_polygon() + coord_equal() + labs(fill = "manufacturingcapital") +
    ggtitle ("manufacturingcapital, 1900")

manufacturingcapitalmap + scale_fill_gradient(low = "grey", high = "black") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot percentfarmland -- sorta central = higher

percentfarmlandmap <- ggplot(USva.f, aes(long, lat, group = group, fill = percent.farmland)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent farmland") +
    ggtitle ("Percent farmland, 1900")

percentfarmlandmap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Percent farmland, 1900.pdf", scale = 1, dpi = 400)

## Plot capitalfarmland -- a bit too random

capitalfarmlandmap <- ggplot(USva.f, aes(long, lat, group = group, fill = capital.farmland)) +
    geom_polygon() + coord_equal() + labs(fill = "capitalfarmland") +
    ggtitle ("capitalfarmland, 1900")

capitalfarmlandmap + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))
