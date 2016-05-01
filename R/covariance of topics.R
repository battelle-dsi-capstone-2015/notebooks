setwd("/home/matt/Datasets/Battelle Data/Analyses")
source("/home/matt/Projects/Programming/R shit/Dirichlet generation and estimation/dirichlet.R")

abstracts <- read.csv("battelle-trial-with-year-prefix.csv",header = FALSE)
years <- substr(abstracts[,1],1,4)
names(years) <- c('year')

Y <- as.character(2005:2015)
correct.years = sapply(years,function(year) as.logical(length(intersect(year,Y))))
abstracts <- abstracts[correct.years,]


#########################################################
# 40 TOPICS
#########################################################
data40 <- read.csv("battelle-trial-topic-mixtures-40.csv")
data40 <- data40[,1:40]
names(data40) <- substr(names(data40),6,100)
names(data40) <- sub('\\.\\.',':',names(data40))
names(data40) <- gsub('\\.\\.',',',names(data40))
names(data40) <- gsub('\\.','',names(data40))
data40$year <- years
data40 <- data40[correct.years,]

alpha40 = alpha.estimate(data40[,1:40])
alpha40
# cov40 <- expected.cov(alpha40)
# var40 <- expected.var(alpha40)
# var.products <- outer(var40,var40)
# cor40 <- cov40/sqrt(var.products)
cov40 <- cov(data40[,1:40])
var40 <- apply(data40[,1:40],2,var)
var.products <- outer(var40,var40)
cor40 <- cov40/sqrt(var.products)

cor.by.year = array(dim = c(11,40,40))
dim(cor.by.year)
for(i in 1:length(Y)){
    cor.by.year[i,,] = cor(data40[data40[['year']] == Y[i],1:40])
}

cor.sd40 = sapply(1:length(alpha40), 
                  function(i) sapply(1:length(alpha40), 
                                     function(j) sd(cor.by.year[,i,j])))

interest40 = array(dim = c(11,40,40))
for(i in 1:length(Y)){
    interest40[i,,] = (cor.by.year[i,,] - cor40)/cor.sd40
}

par(mfrow= c(3,4))
par(mar = c(2,2,2,2))
apply(interest40, 1, function(X) hist(X))

threshold = 3.9
interesting.indices = apply(interest40,c(2,3),function(v) max(v)-min(v) > threshold)
sum(interesting.indices,na.rm = TRUE)/(2)

indices40 = which(interesting.indices,arr.ind = TRUE)
indices40 = apply(indices40, 1, sort)
indices40 = t(indices40)
indices40 = unique.array(indices40,MARGIN = 1)
indices40

interesting.graphs40 = apply(indices40,1,function(x) interest40[,x[1],x[2]])
plot.new()
par(mfrow = c(3,3))
par(mar = c(2,2,2,2))
for(i in 1:dim(indices40)[1]){
    plot(x = Y, y = interesting.graphs40[,i],type = 'l',main = paste(names(data40)[indices40[i,1]],
                                                                     names(data40)[indices40[i,2]]),
         ylim = c(-1*threshold,threshold), ylab = "Normalized correlation")
}



#########################################################
# 60 TOPICS
#########################################################
data60 <- read.csv("battelle-trial-topic-mixtures-60.csv")
data60 <- data60[,1:60]
names(data60) <- substr(names(data60),6,100)
names(data60) <- sub('\\.\\.',':',names(data60))
names(data60) <- gsub('\\.\\.',',',names(data60))
names(data60) <- gsub('\\.','',names(data60))
data60$year <- years
data60 <- data60[correct.years,]

alpha60 = alpha.estimate(data60[,1:60])
alpha60
# cov60 <- expected.cov(alpha60)
# var60 <- expected.var(alpha60)
# var.products <- outer(var60,var60)
# cor60 <- cov60/sqrt(var.products)
cov60 <- cov(data60[,1:60])
var60 <- apply(data60[,1:60],2,var)
var.products <- outer(var60,var60)
cor60 <- cov60/sqrt(var.products)

cor.by.year = array(dim = c(11,60,60))
dim(cor.by.year)
for(i in 1:length(Y)){
    cor.by.year[i,,] = cor(data60[data60[['year']] == Y[i],1:60])
}

cor.sd60 = sapply(1:length(alpha60), 
                  function(i) sapply(1:length(alpha60), 
                                     function(j) sd(cor.by.year[,i,j])))

interest60 = array(dim = c(11,60,60))
for(i in 1:length(Y)){
    interest60[i,,] = (cor.by.year[i,,] - cor60)/cor.sd60
}

plot.new()
par(mfrow= c(3,4))
par(mar = c(2,2,2,2))
apply(interest60, 1, function(X) hist(X))

threshold = 3.9
interesting.indices = apply(interest60,c(2,3),function(v) max(v)-min(v) > threshold)
sum(interesting.indices,na.rm = TRUE)/(2)

indices60 = which(interesting.indices,arr.ind = TRUE)
indices60 = apply(indices60, 1, sort)
indices60 = t(indices60)
indices60 = unique.array(indices60,MARGIN = 1)
indices60

interesting.graphs60 = apply(indices60,1,function(x) interest60[,x[1],x[2]])
plot.new()
par(mfrow = c(3,3))
par(mar = c(2,2,2,2))
for(i in 1:dim(indices60)[1]){
    plot(x = Y, y = interesting.graphs60[,i],type = 'l',main = paste(names(data60)[indices60[i,1]],
                                                                     names(data60)[indices60[i,2]]),
         ylim = c(-1*threshold,threshold), ylab = "Normalized correlation")
}



#########################################################
# 80 TOPICS
#########################################################
data80 <- read.csv("battelle-trial-topic-mixtures-80.csv")
data80 <- data80[,1:80]
names(data80) <- substr(names(data80),6,100)
names(data80) <- sub('\\.\\.',':',names(data80))
names(data80) <- gsub('\\.\\.',',',names(data80))
names(data80) <- gsub('\\.','',names(data80))
data80$year <- years
data80 <- data80[correct.years,]

alpha80 = alpha.estimate(data80[,1:80])
alpha80
# cov80 <- expected.cov(alpha80)
# var80 <- expected.var(alpha80)
# var.products <- outer(var80,var80)
# cor80 <- cov80/sqrt(var.products)
cov80 <- cov(data80[,1:80])
var80 <- apply(data80[,1:80],2,var)
var.products <- outer(var80,var80)
cor80 <- cov80/sqrt(var.products)

cor.by.year = array(dim = c(11,80,80))
dim(cor.by.year)
for(i in 1:length(Y)){
    cor.by.year[i,,] = cor(data80[data80[['year']] == Y[i],1:80])
}

cor.sd80 = sapply(1:length(alpha80), 
                  function(i) sapply(1:length(alpha80), 
                                     function(j) sd(cor.by.year[,i,j])))

interest80 = array(dim = c(11,80,80))
for(i in 1:length(Y)){
    interest80[i,,] = (cor.by.year[i,,] - cor80)/cor.sd80
}

plot.new()
par(mfrow= c(3,4))
par(mar = c(2,2,2,2))
apply(interest80, 1, function(X) hist(X))

threshold = 3.9
interesting.indices = apply(interest80,c(2,3),function(v) max(v)-min(v) > threshold)
sum(interesting.indices,na.rm = TRUE)/(2)

indices80 = which(interesting.indices,arr.ind = TRUE)
indices80 = apply(indices80, 1, sort)
indices80 = t(indices80)
indices80 = unique.array(indices80,MARGIN = 1)
indices80

interesting.graphs80 = apply(indices80,1,function(x) interest80[,x[1],x[2]])
plot.new()
par(mfrow = c(3,3))
par(mar = c(2,2,2,2))
for(i in 1:dim(indices80)[1]){
    plot(x = Y, y = interesting.graphs80[,i],type = 'l',main = paste(names(data80)[indices80[i,1]],
                                                                     names(data80)[indices80[i,2]]),
         ylim = c(-1*threshold,threshold), ylab = "Normalized correlation")
}


#########################################################
# INTERESTING PAPERS
#########################################################
mean.products <- sapply(1:length(alpha40), 
    function(i){sapply(1:length(alpha40), 
                          function(j){
                              mean(data40[,i]*data40[,j])
                          })})
i = 28
j = 16
papers.28.16 <- data40[,i]*data40[,j] - mean.products[i,j]
abstracts.28.16 <- years[['V1']][which(papers.28.16 > .18)]
class(abstracts.28.16)
abstracts.28.16 = as.character(abstracts.28.16)
abstracts.28.16 = droplevels.data.frame(abstracts28.16)
abstracts.28.16
