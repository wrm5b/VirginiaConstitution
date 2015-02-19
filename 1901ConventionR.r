# Summary Information and formatting

## fileUrl <- "...mapserver.lib.virginia.edu" 

## Get delegate info from newrivernotes
install.packages("rvest")
library(rvest)

delegate_election <- html("http://www.newrivernotes.com/historical_20th_1901_virginia_constitutional_conv_directory.htm")

delegate_bio <- delegate_election %>% html_nodes(".box4-content") %>% html_text()
delegate_bio <- strsplit(gsub("(\n)([[:upper:]])|\n\n","~\\2",delegate_bio), "~")
delegate_bio <- unlist(delegate_bio)
delegate_bio <- delegate_bio[4:103]

representing <- 
occupation <-
party <- 
uva <-
    
delegates <- cbind(representing, occupation, party, uva)

## Alternative using dplyr

library(dplyr)

mydat <- read.csv("1901Convention2.csv")
select(mydat, )
filter(mydat, o1 == "1", a2 == 1)
arrange(mydat, desc(percentwhitesrepresentingilliter))
mutate(mydat, urbanicity = population/percentfarmlandrepresenting)

by_area <- group_by(mydat, area)
summarize(by_area, 
          count = n(),
          avg_illiteracy = mean(percentwhitesrepresentingilliter))
    
    
    
dat <- read.csv("1901Convention.csv", header=TRUE, colClasses=c("Oath"="factor", "Franchise"="factor", "Sccamend"="factor", "Submissionnew"="factor", "Submissionprior"="factor", "Ordination"="factor", "area"="factor", "a1"="factor", "a2"="factor", "a3"="factor", "democrat"="factor", "occupation"="factor", "o1"="factor", 
                    "o2"="factor", "o3"="factor", "o4"="factor", "o5"="factor", "contestedelection"="factor", "uva"="factor", "urbancenter"="factor", "u1"="factor", "u2"="factor", "representingforconvention"="factor"))
## Convert factors to logicals?
    object.size(dat)
    dim(dat)
    names(dat)
    head(dat)
    summary(dat)
    colMeans(dat)
    mean(dat$percentwhitesrepresentingilliter)
    str(dat)


### mlogit Franchise: NNET
library(nnet)
dat$Franchise <- factor(dat$Franchise)
dat$Franchise1 <- relevel(dat$Franchise, ref = 3)
Franchisemlogit <-  multinom(Franchise1 ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 2) + I(occupation == 3) + I(occupation == 4) + factor(contestedelection) + factor(uva) + populationdensity + factor(urbancenter) + percentblackrepresenting + 
                                 percentwhitesrepresentingilliter + percentfarmlandrepresenting + manufacturingcapitalrepresenting + capitalfarmland + factor(representingforconvention), data = dat)

summary(Franchisemlogit)



### A) mlogit Franchise: mlogit package
library(mlogit)
dat1 <- mlogit.data(dat, varying = NULL, choice = "Franchise", shape = "wide")


Franchisemlogit <-  mlogit(Franchise ~ 0 | a1 +  a3 + o1 + o2 + o3 + o4 + contestedelection + uva + populationdensity + urbancenter + percentblackrepresenting + 
                               percentwhitesrepresentingilliter + percentfarmlandrepresenting + representingforconvention, data = dat1, reflevel = 3)

Franchisemlogit3 <-  mlogit(Franchise ~ 0 | a1, data = dat1, reflevel = 3)
    
# 0: intercept, same as stata

summary(Franchisemlogit)

summary(Franchisemlogit3)

# Marginal Effects

## 1) Change factors to integers
d <- dat1
    d$a1 = as.integer(d$a1)
    d$a3 = as.integer(d$a3)
    d$o1 = as.integer(d$o1)
    d$o2 = as.integer(d$o2)
    d$o3 = as.integer(d$o3)
    d$o4 = as.integer(d$o4)
    d$contestedelection = as.integer(d$contestedelection)
    d$uva = as.integer(d$uva)
    d$urbancenter = as.integer(d$urbancenter)
    d$representingforconvention = as.integer(d$representingforconvention)

d1 <- with(d, data.frame(a1 = mean(a1),
                           a3 = mean(a3),
                           o1 = mean(o1),
                           o2 = mean(o2),
                           o3 = mean(o3),
                           o4 = mean(o4),
                           contestedelection = mean(contestedelection, na.rm = TRUE),
                           uva = mean(uva),
                           populationdensity = mean(populationdensity),
                           urbancenter = mean(urbancenter),
                           percentblackrepresenting = mean(percentblackrepresenting),
                           percentwhitesrepresentingilliter = mean(percentwhitesrepresentingilliter),
                           percentfarmlandrepresenting = mean(percentfarmlandrepresenting),
                           representingforconvention = mean(representingforconvention)))

d.a <- with (d, data.frame(a1 = mean(a1)))

## 2) Duplicate matrix 3 times (the number of alternatives)
d2 <- rbind(d1, d1, d1)

d.a2 <- rbind(d.a, d.a, d.a)

effects(Franchisemlogit, covariate = c("a1","a3", "o1", "o2", "o3", "o4", "contestedelection",
                                       "uva", "populationdensity", "urbancenter", "percentblackrepresenting",
                                       "percentwhitesrepresentingilliter", "percentfarmlandrepresenting",
                                        "representingforconvention"), data = d2)

### Need predictions? trying to figure out STATA's formula

Predict <- apply(fitted(Franchisemlogit, outcome = FALSE), 2, mean)
head(Predict)
Predict <- matrix(Predict, ncol = 3, byrow = TRUE)
Predict[,1]

## Obtain dydx()

coef(Franchisemlogit)["1:percentwhitesrepresentingilliter"]/Predict[,2]
coef(Franchisemlogit)["1:percentblackrepresenting"]/Predict[,2]
coef(Franchisemlogit)["1:populationdensity"]/Predict[,2]


### The problem was the missing values in contestedelection


# Franchise mlogit: STATA copy

Franchisemlogit2 <-  mlogit(Franchise ~ 0 | (a1) +  (a3) + (o1) + (o4) + contestedelection + (urbancenter) + populationdensity + percentblackrepresenting + 
                               percentwhitesrepresentingilliter, data = dat1, reflevel = 3)

summary(Franchisemlogit2)

# Marginal effects of the MODEL; still want dydx*

effects(Franchisemlogit2, covariate = c("a1","a3", "o1", "o4", "contestedelection", "urbancenter", "populationdensity", "percentblackrepresenting",
                                        "percentwhitesrepresentingilliter"), data = d2)


Predict2 <- apply(fitted(Franchisemlogit2, outcome = FALSE), 2, mean)
head(Predict2)
Predict2 <- matrix(Predict2, ncol = 3, byrow = TRUE)
Predict2[,1]

coef(Franchisemlogit2)["1:percentwhitesrepresentingilliter"]/Predict2[,2]
coef(Franchisemlogit2)["1:percentblackrepresenting"]/Predict2[,2]
coef(Franchisemlogit2)["1:populationdensity"]/Predict2[,2]




effects(Franchisemlogit3, covariate = "a1", data = d.a2)

coef(Franchisemlogit3)["a1"]


### B) mlogit SCC Amendmentment: mlogit package
library(mlogit)
datScc <- mlogit.data(dat, varying = NULL, choice = "Sccamend", shape = "wide")


Sccamendmlogit <-  mlogit(Sccamend ~ 0 | a1 +  a3 + o1 + o2 + o3 + o4 + contestedelection + uva + populationdensity + urbancenter + percentblackrepresenting + 
                               percentwhitesrepresentingilliter + percentfarmlandrepresenting + representingforconvention, data = datScc, reflevel = 1)

Sccamendemlogit3 <-  mlogit(Sccamend ~ 0 | a1, data = datScc, reflevel = 1)

### 0: intercept, same as stata

summary(Sccamendmlogit)

summary(Sccamendmlogit3)

# Marginal Effects

### 1) Change factors to integers
dScc <- datScc
    dScc$a1 = as.integer(dScc$a1)
    dScc$a3 = as.integer(dScc$a3)
    dScc$o1 = as.integer(dScc$o1)
    dScc$o2 = as.integer(dScc$o2)
    dScc$o3 = as.integer(dScc$o3)
    dScc$o4 = as.integer(dScc$o4)
    dScc$contestedelection = as.integer(dScc$contestedelection)
    dScc$uva = as.integer(dScc$uva)
    dScc$urbancenter = as.integer(dScc$urbancenter)
    dScc$representingforconvention = as.integer(dScc$representingforconvention)

d1Scc <- with(dScc, data.frame(a1 = mean(a1),
                         a3 = mean(a3),
                         o1 = mean(o1),
                         o2 = mean(o2),
                         o3 = mean(o3),
                         o4 = mean(o4),
                         contestedelection = mean(contestedelection, na.rm = TRUE),
                         uva = mean(uva),
                         populationdensity = mean(populationdensity),
                         urbancenter = mean(urbancenter),
                         percentblackrepresenting = mean(percentblackrepresenting),
                         percentwhitesrepresentingilliter = mean(percentwhitesrepresentingilliter),
                         percentfarmlandrepresenting = mean(percentfarmlandrepresenting),
                         representingforconvention = mean(representingforconvention)))

d.aScc <- with (dScc, data.frame(a1 = mean(a1)))

### 2) Duplicate matrix 3 times (the number of alternatives)
d2Scc <- rbind(d1Scc, d1Scc, d1Scc)

d.a2Scc <- rbind(d.aScc, d.aScc, d.aScc)

effects(Sccamendmlogit, covariate = c("a1","a3", "o1", "o2", "o3", "o4", "contestedelection",
                                       "uva", "populationdensity", "urbancenter", "percentblackrepresenting",
                                       "percentwhitesrepresentingilliter", "percentfarmlandrepresenting",
                                       "representingforconvention"), data = d2Scc)


# Sccamend mlogit: STATA copy

Sccamendmlogit2 <-  mlogit(Sccamend ~ 0 | (a1) +  (a3) + (o1) + (o4) + contestedelection + (urbancenter) + populationdensity + percentblackrepresenting + 
                                percentwhitesrepresentingilliter, data = datScc, reflevel = 1)

summary(Franchisemlogit2)

# Marginal effects of the MODEL; still want dydx*

effects(Franchisemlogit2, covariate = c("a1","a3", "o1", "o4", "contestedelection", "urbancenter", "populationdensity", "percentblackrepresenting",
                                        "percentwhitesrepresentingilliter"), data = d2Scc)

effects(Franchisemlogit3, covariate = "a1", data = d.a2Scc)



### 3) mlogit Submissionnew

library(mlogit)
datSubmissionnew <- mlogit.data(dat, varying = NULL, choice = "Submissionnew", shape = "wide")


Submissionnewmlogit <-  mlogit(Submissionnew ~ 0 | a1 +  a3 + o1 + o2 + o3 + o4 + contestedelection + uva + populationdensity + urbancenter + percentblackrepresenting + 
                              percentwhitesrepresentingilliter + percentfarmlandrepresenting + representingforconvention, data = datSubmissionnew, reflevel = 3)

Submissionnewmlogit3 <-  mlogit(Submissionnew ~ 0 | a1, data = datSubmissionnew, reflevel = 3)

### 0: intercept, same as stata

summary(Submissionnewmlogit)

summary(Submissionnewmlogit3)

# Marginal Effects

### 1) Change factors to integers
dSubmissionnew <- datSubmissionnew
    dSubmissionnew$a1 = as.integer(dSubmissionnew$a1)
    dSubmissionnew$a3 = as.integer(dSubmissionnew$a3)
    dSubmissionnew$o1 = as.integer(dSubmissionnew$o1)
    dSubmissionnew$o2 = as.integer(dSubmissionnew$o2)
    dSubmissionnew$o3 = as.integer(dSubmissionnew$o3)
    dSubmissionnew$o4 = as.integer(dSubmissionnew$o4)
    dSubmissionnew$contestedelection = as.integer(dSubmissionnew$contestedelection)
    dSubmissionnew$uva = as.integer(dSubmissionnew$uva)
    dSubmissionnew$urbancenter = as.integer(dSubmissionnew$urbancenter)
    dSubmissionnew$representingforconvention = as.integer(dSubmissionnew$representingforconvention)

d1Submissionnew <- with(dSubmissionnewc, data.frame(a1 = mean(a1),
                               a3 = mean(a3),
                               o1 = mean(o1),
                               o2 = mean(o2),
                               o3 = mean(o3),
                               o4 = mean(o4),
                               contestedelection = mean(contestedelection, na.rm = TRUE),
                               uva = mean(uva),
                               populationdensity = mean(populationdensity),
                               urbancenter = mean(urbancenter),
                               percentblackrepresenting = mean(percentblackrepresenting),
                               percentwhitesrepresentingilliter = mean(percentwhitesrepresentingilliter),
                               percentfarmlandrepresenting = mean(percentfarmlandrepresenting),
                               representingforconvention = mean(representingforconvention)))

d.aSubmissionnew <- with (dSubmissionnew, data.frame(a1 = mean(a1)))

### 2) Duplicate matrix 3 times (the number of alternatives)
d2Submissionnew <- rbind(d1Submissionnew, d1Submissionnew, d1Submissionnew)

d.a2Submissionnew <- rbind(d.aSubmissionnew, d.aSubmissionnew, d.aSubmissionnew)

effects(Submissionnewmlogit, covariate = c("a1","a3", "o1", "o2", "o3", "o4", "contestedelection",
                                     "uva", "populationdensity", "urbancenter", "percentblackrepresenting",
                                     "percentwhitesrepresentingilliter", "percentfarmlandrepresenting",
                                     "representingforconvention"), data = d2Submissionnew)


# Sccamend mlogit: STATA copy

Submissionnewmlogit2 <-  mlogit(Submissionnew ~ 0 | (a1) +  (a3) + (o1) + (o4) + contestedelection + (urbancenter) + populationdensity + percentblackrepresenting + 
                               percentwhitesrepresentingilliter, data = datSubmissionnew, reflevel = 3)

summary(Submissionnewmlogit2)

# Marginal effects of the MODEL; still want dydx*

effects(Submissionnewmlogit2, covariate = c("a1","a3", "o1", "o4", "contestedelection", "urbancenter", "populationdensity", "percentblackrepresenting",
                                        "percentwhitesrepresentingilliter"), data = d2Submissionnew)

effects(Submissionnewmlogit3, covariate = "a1", data = d.a2Submissionnew)
