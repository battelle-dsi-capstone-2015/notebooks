source('~/Projects/Programming/R shit/Dirichlet generation and estimation/dirichlet.R')
library(cubature)
library(gtools)

alpha = c(1,2,3,4)/2

sample = rdirichlet(alpha, 10000)

plot(sample[,1], sample[,2], cex  = .2)

# Test that this thing came from dir(alpha[1], alpha[2], sum(other alphas))

a1 <-  alpha[1]
a2 <-  alpha[2]
a3 <-  sum(alpha) - a1 - a2
C = gamma(a1+a2+a3)/(gamma(a1)*gamma(a2)*gamma(a3))


Dir.2D <- function(x1,x2,a1,a2,a3,C){
    value = as.numeric(x1+x2 < 1)
    if(value == 1){
        value = C*(x1^(a1-1))*(x2^(a2-1))*((1- x1 - x2)^(a3-1))
    }
    return(value)
}


discrete.dirichlet.prob.2D <- function(alpha1,alpha2,alpha3, divisions, subdivisions){
    bigdelta = 1/divisions
    littledelta = bigdelta/subdivisions
    
    bins = cbind(rep(1:divisions, divisions),rep(1:divisions, each = divisions))
    coords.x = (bins[,1] - 1)*bigdelta
    coords.y = (bins[,2] - 1)*bigdelta
    
    integrals =  numeric(length(coords.x))
    C = gamma(a1+a2+a3)/(gamma(a1)*gamma(a2)*gamma(a3))
    
    for(i in 1:length(integrals)){
        start.x = coords.x[i]
        start.y = coords.y[i]
        end.x = start.x + bigdelta
        end.y = start.y + bigdelta
        x = start.x + (1:subdivisions)*littledelta - littledelta/2
        y = start.y + (1:subdivisions)*littledelta - littledelta/2
        # double integrate on square
        # NOTE: we could way improve this by using qbeta after a transformation,
        # for the inner dimension, and integrate() for the outer. To be continued...
        if(startx + starty > 1){
            integrals[i] = 0
        } else {
            integrals[i] =  littledelta*sum(sapply(x, function(x1){
                            littledelta*sum(sapply(y, function(x2) Dir.2D(x1,x2,a1,a2,a3,C)))}))
        }
    }
    return(cbind(bins,integrals))
}



############
# bin up all the points
a1 = 4
a2 = .2
a3 = 1
sample = rdirichlet(c(a1,a2,a3), 10000)
probs = discrete.dirichlet.prob.2D(a1,a2,a3,xy.min = .05,divisions = 10,subdivisions = 20)
#probs = probs/sum(probs)

counts = document.bin.count.2D(sample,divisions = 10,xy.min = .05)
sum(probs)
sum(counts/nrow(sample))
chisq.test(x = counts, p = probs)
length(counts)
length(probs)
s
counts
