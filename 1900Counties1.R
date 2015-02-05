# 1) Summary Information and formatting for probit regression

dat <- read.csv("1900Counties2.csv", header=TRUE)

object.size(dat)
dim(dat)
names(dat)
head(dat)
summary(dat)
colMeans(dat)
mean(dat$percentilliteratewhites)
str(dat)

# Probit Convention regression

Conventionprobit <- glm(Convention ~ I(area == "West") + I(area == "East") + populationdensity + urbancenter + percentblack + 
                      percentilliteratewhites + percentfarmland + manufacturingcapital + capitalfarmland, x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Conventionprobit)

# Marginal Effects

library(erer)
maBina(w = Conventionprobit)

## 2) Graphics

library(maptools)
library(ggplot2)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(foreign)
library(extrafont)
# font_import()
# loadfonts()
# library(rgeos)

## Read in GIS data and subset out VA counties

usdbf <- read.dbf("US_county_1900_conflated.dbf")

us <- readOGR(dsn = ".", "US_county_1900_conflated")
attributes <- c("NHGISNAM", "STATENAM")
newNames <- c("county", "state")

us_subset <- us[,attributes]
names(us_subset) <- newNames
data_name <- "US"
assign(data_name, spTransform(us_subset, CRS("+proj=longlat")))

save(list=c(data_name), file=paste("USCounty.RData", sep="/"))

USva <- US[US$state %in% "Virginia", ]

USva$party = dat$party
USva$election1900 = dat$election1900
USva$Convention = dat$Convention

## Plot 1900 voting data using base plot

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

## Plot with ggplot2

USva$urbanicity = dat$urbanicity
USva$area = dat$area
USva$percentilliteratewhites = dat$percentilliteratewhites
USva$populationdensity = dat$populationdensity
USva$party = dat$party
USva$restoredgov = dat$restoredgov
USva$population = dat$population
USva$percentblack = dat$percentblack
USva$percentfarmland = dat$percentfarmland
USva$manufacturingcapital = dat$manufacturingcapital
USva$urbancenter = dat$urbancenter
USva$sqmi = dat$sqmi
USva$capitalfarmland = dat$capitalfarmland

USva$map = 

USva.f <- fortify(USva, region = "county")
USva.f <- merge(USva.f, USva@data, by.x = "id", by.y = "county")

## Plot 1900 voting data using ggplot2

map1900 <- ggplot(USva.f, aes(long, lat, group = group, alpha = Convention, fill = election1900)) +
    geom_polygon() + coord_equal() + labs(fill = "Presidential election", alpha = "Constitutional convention") +
    ggtitle ("1900 county voting map, \npresidential election and convention referendum")

Palette1900 <- c("white", "navy", "darkred")

map1900 + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=Palette1900, breaks = c("Democrat", "Republican")) + scale_alpha_manual(values = c(1, 1, 0.65), breaks = c("for", "against")) + theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("County Voting Map, 1900.pdf", scale = 1, dpi = 400)

## Plot area (use 2 federal judicial districs instead?)

areamap <- ggplot(USva.f, aes(long, lat, group = group, fill = area)) +
    geom_polygon() + coord_equal() + labs(fill = "Area") +
    ggtitle ("Regions of Virginia, 1900")

greyPalette <- c("white", "grey50", "grey75", "grey25" )

areamap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=greyPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Regions of Virginia, 1900.pdf", scale = 1, dpi = 400)

## Plot white illiteracy

whiteilliteracymap <- ggplot(USva.f, aes(long, lat, group = group, fill = percentilliteratewhites)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent illiterate whites") +
    ggtitle ("Illiterate white voters\nas a proportion of white population, 1900")

whiteilliteracymap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Illiterate white voters as a proportion of white population, 1900.pdf", scale = 1, dpi = 400)

## Plot population density

populationdensitymap <- ggplot(USva.f, aes(long, lat, group = group, fill = populationdensity)) +
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

percentblackmap <- ggplot(USva.f, aes(long, lat, group = group, fill = percentblack)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent black") +
    ggtitle ("Percent of population comprised of black persons, 1900")

percentblackmap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Percent black, 1900.pdf", scale = 1, dpi = 400)

## Plot restored gov -- unhelpful, too small sample  

restoredgovmap <- ggplot(USva.f, aes(long, lat, group = group, fill = restoredgov)) +
    geom_polygon() + coord_equal() + labs(fill = "restoredgov") +
    ggtitle ("restoredgov, 1900")

restoredgovmap + scale_fill_manual(values=cbPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot urban (maybe incorporated?) centers --  nicely random

urbancentermap <- ggplot(USva.f, aes(long, lat, group = group, fill = urbancenter)) +
    geom_polygon() + coord_equal() + labs(fill = "Incorporated city") +
    ggtitle ("Surrounding or bordering an incorporated city, 1900")

binaryPalette <- c("white", "grey50", "black")

urbancentermap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=binaryPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Incorporated cities, 1900.pdf", scale = 1, dpi = 400)

## Plot population -- 

populationmap <- ggplot(USva.f, aes(long, lat, group = group, fill = population)) +
    geom_polygon() + coord_equal() + labs(fill = "population") +
    ggtitle ("population, 1900")

populationmap + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot party --  west

partymap <- ggplot(USva.f, aes(long, lat, group = group, fill = party)) +
    geom_polygon() + coord_equal() + labs(fill = "Party") +
    ggtitle ("Party of delegate elected to the 1901-02 Convention")

partymap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_manual(values=greyPalette) +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Delegate party, 1900.pdf", scale = 1, dpi = 400)

## Plot manufacturingcapital -- a bit too random

manufacturingcapitalmap <- ggplot(USva.f, aes(long, lat, group = group, fill = manufacturingcapital)) +
    geom_polygon() + coord_equal() + labs(fill = "manufacturingcapital") +
    ggtitle ("manufacturingcapital, 1900")

manufacturingcapitalmap + scale_fill_gradient(low = "grey", high = "black") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

## Plot percentfarmland -- sorta central = higher

percentfarmlandmap <- ggplot(USva.f, aes(long, lat, group = group, fill = percentfarmland)) +
    geom_polygon() + coord_equal() + labs(fill = "Percent farmland") +
    ggtitle ("Percent farmland, 1900")

percentfarmlandmap + scale_x_continuous(breaks=NULL, name="") + scale_y_continuous(breaks=NULL, name="") + scale_fill_gradient(low = "grey60", high = "grey30", space = "Lab", na.value = "white", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor=element_blank())

ggsave("Percent farmland, 1900.pdf", scale = 1, dpi = 400)

## Plot capitalfarmland -- a bit too random

capitalfarmlandmap <- ggplot(USva.f, aes(long, lat, group = group, fill = capitalfarmland)) +
    geom_polygon() + coord_equal() + labs(fill = "capitalfarmland") +
    ggtitle ("capitalfarmland, 1900")

capitalfarmlandmap + scale_fill_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar") +  theme_bw(base_family = "Times New Roman", base_size = 12) + theme(plot.title = element_text(hjust = 0.5))
