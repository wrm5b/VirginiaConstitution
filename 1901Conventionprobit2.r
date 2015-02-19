# Summary Information and formatting for probit regression

## fileUrl <- "...mapserver.lib.virginia.edu" 
## use scrape?
## download.file(fileUrl, destfile <- "./1900Census.csv") 
## Format data (see excel formulae)


## Use log10's?


dat <- read.csv("1901Convention.csv", header=TRUE)
    object.size(dat)
    dim(dat)
    names(dat)
    head(dat)
    summary(dat)
    colMeans(dat)
    mean(dat$percentwhitesrepresentingilliter)
    str(dat)

# Probit Oath regression

Oathprobit <- glm(Oathprobit ~ I(area == "West") + I(area == "East") + I(occupation == "lawyer") + I(occupation == "") + I(occupation == 3) + I(occupation == 4) + factor(contestedelection) + factor(uva) + populationdensity + factor(urbancenter) + percentblackrepresenting + 
                                 percentwhitesrepresentingilliter + percentfarmlandrepresenting + manufacturingcapitalrepresenting + capitalfarmland + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Oathprobit)

# Marginal Effects
library(erer)
maBina(w = Oathprobit)

# Probit Oath regression: emphasis on white illiteracy

Oathprobit1 <- glm(Oath ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 2) + I(occupation == 3) + I(occupation == 4) + factor(contestedelection) + populationdensity + factor(urbancenter) + percentblackrepresenting + 
                      percentwhitesrepresentingilliter + capitalfarmland + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Oathprobit1)

# Marginal Effects
library(erer)
maBina(w = Oathprobit1)

# Probit Submissionprior regression

Submissionpriorprobit <- glm(Submissionprior ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 2) + I(occupation == 3) + I(occupation == 4) + factor(contestedelection) + factor(uva) + populationdensity + factor(urbancenter) + percentblackrepresenting + 
                      percentwhitesrepresentingilliter + percentfarmlandrepresenting + capitalfarmland + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Submissionpriorprobit)

# Marginal Effects
library(erer)
maBina(w = Submissionpriorprobit)

# Probit Submissionprior regression: emphasis on white illiteracy

Submissionpriorprobit1 <- glm(Submissionprior ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 4) + factor(urbancenter) + percentwhitesrepresentingilliter + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Submissionpriorprobit1)

# Marginal Effects
library(erer)
maBina(w = Submissionpriorprobit1)

# Probit Submissionnew regression

Submissionnewprobit <- glm(Submissionnew ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 2) + I(occupation == 3) + I(occupation == 4) + factor(urbancenter) + percentblackrepresenting + 
                                 percentwhitesrepresentingilliter + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Submissionnewprobit)

# Marginal Effects
library(erer)
maBina(w = Submissionnewprobit)

# Probit Submissionnew regression: emphasis on white illiteracy

Submissionnewprobit1 <- glm(Submissionnew ~ I(occupation == 1) + I(occupation == 4) + factor(urbancenter) + percentwhitesrepresentingilliter + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Submissionnewprobit1)

# Marginal Effects
library(erer)
maBina(w = Submissionnewprobit1)

# Probit Ordination regression

Ordinationprobit <- glm(Ordination ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 2) + I(occupation == 3) + I(occupation == 4) + factor(contestedelection) + factor(uva) + populationrepresenting + populationdensity + factor(urbancenter) + percentblackrepresenting + 
                            percentwhitesrepresentingilliter + percentfarmlandrepresenting + capitalfarmland + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Ordinationprobit)

# Marginal Effects
library(erer)
maBina(w = Ordinationprobit)

# Probit Ordination regression: emphasis on white illiteracy

Ordinationprobit1 <- glm(Ordination ~ I(area == 1) + I(area == 3) + I(occupation == 1) + I(occupation == 4) + factor(urbancenter) + percentwhitesrepresentingilliter + factor(representingforconvention), x = TRUE, family = binomial (link = "probit"), data = dat)

summary(Ordinationprobit1)

# Marginal Effects
library(erer)
maBina(w = Ordinationprobit1)