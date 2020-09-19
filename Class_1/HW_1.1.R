
# Git hub test

install.packages("rafalib")
library(rafalib)

install.packages("swirl")
library(swirl)
swirl()
vector1<- as.vector(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
x = c(2.23,3.45,1.87,2.11,7.33,18.34,19.23)
mean_x<- mean(x)

prob_3<- c(1:25)
install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename<- "femaleMiceWeights.csv" 

download(url = url, destfile = filename)

library(tidyverse)
data_1<- read_csv("femaleMiceWeights.csv")

data_1$Bodyweight[11]

length(data_1$Diet)
mean(data_1$Bodyweight[13:24])

sample(data_1$Bodyweight[13:24],1)

library(dplyr)

dat <-read.csv("femaleMiceWeights.csv")

library(dplyr)
controls <- filter(dat, Diet=="chow")

controls <- select(controls, Bodyweight)
unlist(controls)

controls <- dat %>% 
  filter(Diet =="chow") %>% 
  select(Bodyweight) %>% 
  unlist

library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)
mamsleep <- read.csv("msleep_ggplot2.csv")
class(mamsleep)
mamsleep_prime<- mamsleep %>%
  filter(order == "Primates") %>%
  select(sleep_total)

class(mamsleep_prime$sleep_total)  
mean(mamsleep_prime$sleep_total)

unlist(mansleep_prime) %>% 
  mean(mansleep_prime)
mamsleep_prime <-mamsleep %>%
  filter(order == "primates") %>% 
  sumarize(sleep_total)


library(downloader)
url <-"https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist(read.csv(filename))

population<- read.csv("femaleControlsPopulation.csv")
mean(population$Bodyweight)
set.seed(1)
mean(sample(population$Bodyweight, 5))
set.seed(1)
abs(mean(population$Bodyweight) - mean(sample(population$Bodyweight,5)))


set.seed(5)
abs(mean(population$Bodyweight) - mean(sample(population$Bodyweight,5)))


set.seed(1)
n<- 1000
result<- vector("numeric", n)
for(a in 1:n){
  result[a]<- mean(sample(population$Bodyweight, 50))
}

true_mean<- mean(population$Bodyweight)
mean(result > true_mean)
mean(result < true_mean)
true_mean +1
true_mean -1
1-mean(result > (true_mean-1) & result < (true_mean+1))

install.packages("gapminder")
library(gapminder)
data("gapminder")
head(gapminder)
gapminder

library(dplyr)
gap_1952 <- gapminder %>% 
  filter(year == 1952)

mean(gap_1952$lifeExp <=60) - mean(gap_1952$lifeExp <=40)

mean(gap_1952$lifeExp >= 40 & gap_1952$lifeExp <= 60)

prop = function(q) {
  mean(x <=q)
}
prop(40)

qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <=q))
plot(ecdf(x))

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url,destfile=filename)
x <- unlist( read.csv(filename))

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages5)
hist(averages50)
10/17

mean(averages50>=23 & averages50<=25)

pnorm((23-23.9)/0.43)
pnorm((25-23.9)/0.43) - pnorm((23-23.9)/0.43)

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename)

dat <- na.omit( dat)
library(dplyr)
controls <- filter(dat, Diet=="chow")

controls <- select(controls, Bodyweight)
unlist(controls)
library(dplyr)
controls <- filter(dat, Diet=="chow", Sex=="M")
controls <- select(x, Bodyweight, Sex)
unlist(controls)
mean(controls$Bodyweight)

library(rafalib)
popsd(controls$Bodyweight)

set.seed(1)
mean(sample(controls$Bodyweight, 25))

library(dplyr)
y <- filter(dat, Diet=="hf", Sex=="M")
y <- select(y, Bodyweight, Sex)
unlist(y)
mean(y$Bodyweight)

library(rafalib)
popsd(y$Bodyweight)

set.seed(1)
mean(sample(y$Bodyweight, 25))

set.seed(1)
abs(mean(controls$Bodyweight) - mean(y$Bodyweight))
set.seed(1)
abs(sd(controls$Bodyweight) - sd(y$Bodyweight)) 
1.158662 - 3.884116



set.seed(1)
abs((mean(controls$Bodyweight) - mean(y$Bodyweight)) - (popsd(controls$Bodyweight) - popsd(y$Bodyweight)))

abs(sd(controls$Bodyweight) - sd(y$Bodyweight)) 

library(dplyr)
x <- filter(dat, Diet=="chow", Sex=="M")
x <- select(x, Bodyweight, Sex)
unlist(x)
mean(x$Bodyweight)

library(rafalib)
popsd(x$Bodyweight)

set.seed(1)
mean(sample(x$Bodyweight, 25))

library(rafalib)
abs((mean(controls$Bodyweight) - mean(y$Bodyweight)) - (popsd(controls$Bodyweight) - popsd(y$Bodyweight)))

(34.84793-30.96381) - (35.8036 - 30.5196)


set.seed(2)
x2k <- mean(sample(controls$Bodyweight, 25))

set.seed(2)
y2k <- mean(sample(y$Bodyweight, 25))

X <- popsd(controls$Bodyweight)

Y <- popsd(y$Bodyweight)

abs(y2k - x2k) 
abs(Y-X)

#########################

set.seed(2)
Y_ty<- mean(sample(y$Bodyweight, 25))

set.seed(2)
X_ty<- mean(sample(x$Bodyweight, 25))

set.seed(2)
x_ty<- popsd(x$Bodyweight)

set.seed(2)
y_ty<- popsd(y$Bodyweight)


(y_ty - x_ty)
(Y_ty - X_ty)











