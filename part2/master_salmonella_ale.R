# based on Lunn 2013, p.116

salmonella_data <- list(y = structure(.Data = c(15,21,29,16,18,21,
                                                16,26,33,27,41,60,
                                                33,38,41,20,27,42),
                                      .Dim = c(6, 3)),
                        x = c(0, 10, 33, 100, 333, 1000), 
                        n = 6,
                        m = 3)

win_path <- "C:\\Users\\conf55\\AppData\\Local\\Microsoft\\AppV\\Client\\Integration\\C030A42B-56AC-4641-B729-18F77627813E\\Root\\VFS\\AppVPackageDrive\\Apps\\WinBUGS14"


n_chains<- 3

init_func <-  function() list(y_pred=rpois(6,1), alpha=rnorm(1,0,10), beta = rnorm(1,0,10), 
                                              gamma=rnorm(1,0,10))
inits <- list(init_func(), init_func(), init_func())

bugs_out  <- bugs(salmonella_data,  parameters.to.save = c("alpha", "beta", "gamma" ,"y_pred"),
                  model.file="master_salmonella.txt", n.iter = 100000,
                  inits = inits, bugs.directory=win_path)

# we can get some summary from the output by just printing it to the screen
bugs_out

plot(bugs_out)

# by converting to an mcmc.list object, we can use functions from the coda package to examine samples.
salmonella_mcmc_l <- as.mcmc.list(bugs_out)

summary(salmonella_mcmc_l)

plot(salmonella_mcmc_l)

acfplot(salmonella_mcmc_l)


# we can also combine these elements into a single matrix
salmonella_mcmc_matrix <- as.matrix(salmonella_mcmc_l)

plot(salmonella_mcmc_matrix[,5])

# we can get summary information by apply functions to each column
par_means  <- apply(salmonella_mcmc_matrix, 2, mean)

par_quants <- apply(salmonella_mcmc_matrix, 2, quantile)

# which indexes hold the predictions (as opposed to parameters)
par_means
pred_indexes <- 5:length(par_means)
pred_died_m <- par_means[pred_indexes]

# plot some predictions
plot(salmonella_data$x, pred_died_m, type="l", ylim=c(0,80))
points(salmonella_data$x, salmonella_data$y[,1],pch=19)
points(salmonella_data$x, salmonella_data$y[,2],pch=19)
points(salmonella_data$x, salmonella_data$y[,3],pch=19)
points(salmonella_data$x, par_quants["25%",][pred_indexes], type="l", lty=3)
points(salmonella_data$x, par_quants["75%",][pred_indexes], type="l", lty=3)
