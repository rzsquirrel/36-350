generate_data = function(n, p) {
    return(list(covariates=matrix(rnorm(n*p), nrow=n, ncol=p), responses=rnorm(n)))
}

model_select = function(covariates, responses, cutoff) {
    lm1 = lm(responses ~ covariates)
    pvals = summary(lm1)$coefficients[,4]
    pvals = pvals[2:length(pvals)]
    if(sum(pvals < cutoff) == 0) {
        return(c())
    }
    
    lm2 = lm(responses ~ covariates[,pvals < cutoff])
    pvals = summary(lm2)$coefficients[,4]
    return(pvals)
}