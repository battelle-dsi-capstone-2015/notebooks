# Functions to generate a random sample from a Dirichlet, and to estimate means, variances, and alphas from a Dirichlet sample.

rdirichlet <- function(alpha,n){
    samples = apply(matrix((1:n),ncol = 1),1,
                    function(i){
                        x = rgamma(length(alpha),alpha)
                        x = x/(sum(x))
                    })
    return(t(samples))
}

alpha.estimate <- function(data){
    asum = double(ncol(data))
    means = apply(data,2,mean)
    for(i in 1:ncol(data)){
        meansq = mean(data[,i]^2)
        asum[i] = (means[i]-meansq)/(meansq-means[i]^2)
    }
    alpha = apply(matrix(1:ncol(data),ncol = 1),1, 
                  function(i) mean(means[i]*asum))
}

expected.var <- function(alpha){
    sum <- sum(alpha)
    sapply(alpha,function(a) a*(sum-a)/((sum^2)*(sum+1)))
    
}

expected.cov <- function(alpha){
    sum <- sum(alpha)
    cov = sapply(1:length(alpha), 
           function(i){sapply(1:length(alpha), 
                        function(j){
                                    -1*alpha[i]*alpha[j]/((sum^2)*(sum+1))
                                    })})
    diag(cov) = expected.var(alpha)
    return(cov)
}

expected.value <- function(alpha){
    alpha/sum(alpha)
}
