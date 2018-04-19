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
    return(pvals[2:length(pvals)])
}

run_simulation = function(n_trials, n, p, cutoff, savefile) {
    pvals = vector()
    for(i in 1:n_trials) {
        data = generate_data(n, p)
        pvals = append(pvals, model_select(data$covariates, data$responses, cutoff))
    }
    #hist(pvals)
    # save the pvals
    saveRDS(pvals, file=savefile)
}

make_plot = function(datapath) {
    pvals = c()
    pvals = readRDS(file=datapath)
    hist(pvals)
}

param_grid = expand.grid(c(100, 1000, 10000), c(10, 20, 50))
apply(param_grid, 1, function(row) run_simulation(20, row[1], row[2], 0.05, paste('file_n',row[1],'_p',row[2],'.rds',sep='')))
apply(param_grid, 1, function(row) make_plot(paste('file_n',row[1],'_p',row[2],'.rds',sep='')))

