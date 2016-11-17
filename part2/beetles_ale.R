
# install relevant packages
install.packages("glmx")
install.packages("R2WinBUGS")

# glmx has the beetles dataset included
library(glmx)
library(R2WinBUGS)
data("BeetleMortality")

# look at the data
BeetleMortality

?bugs

input_data <- as.list(BeetleMortality)

input_data["N"] <- dim(BeetleMortality)[1]

win_path <- "C:\\Users\\conf55\\AppData\\Local\\Microsoft\\AppV\\Client\\Integration\\C030A42B-56AC-4641-B729-18F77627813E\\Root\\VFS\\AppVPackageDrive\\Apps\\WinBUGS14"

bugs_out <- bugs(data=input_data,              # a named list of input elements in the bugs model file
                 inits=NULL,                   # let winbugs generate starting values
                 parameters.to.save=c("beta", "pred_died"), # what we want winbugs to save
                 model.file="beetles.txt",     # model file (must be located in this directory, or give path)
                 n.chains=3,                   # number of independent chains we want
                 n.iter=2000,                  # number of total iterations
                 # VvVvV important to get the path to the executable right! VvVvV
                 #bugs.directory= "//soton.ac.uk/ude/PersonalFiles/Users/jdh4g10/mydocuments/WinBUGS14"
                 bugs.directory=win_path
                 )

# 

# we can get some summary from the output by just printing it to the screen
bugs_out

plot(bugs_out)

# by converting to an mcmc.list object, we can use functions from the coda package to examine samples.
beetles_mcmc_l <- as.mcmc.list(bugs_out)

summary(beetles_mcmc_l)

plot(beetles_mcmc_l)

acfplot(beetles_mcmc_l)


# mcmc.list is a list with an element for each change.
str()

# we can also combine these elements into a single matrix
beetles_mcmc_matrix <- as.matrix(beetles_mcmc_l)

plot(beetles_mcmc_matrix[,5])

# we can get summary information by apply functions to each column
par_means  <- apply(beetles_mcmc_matrix, 2, mean)
?quantile
par_quants <- apply(beetles_mcmc_matrix, 2, quantile)

# which indexes hold the predictions (as opposed to parameters)
par_means
pred_indexes <- 4:length(par_means)
pred_died_m <- par_means[pred_indexes]

# plot some predictions
plot(BeetleMortality$dose, pred_died_m, type="l", ylim=c(0,80))
points(BeetleMortality$dose, BeetleMortality$died,pch=19)
points(BeetleMortality$dose, par_quants["25%",][pred_indexes], type="l", lty=3)
points(BeetleMortality$dose, par_quants["75%",][pred_indexes], type="l", lty=3)

# try plotting different quantiles
# HINT par_quants <- apply(beetles_mcmc_matrix, 2, quantile, probs= "your quantiles here")
par_quants <- apply(beetles_mcmc_matrix, 2, quantile, probs= c(0.10,0.90))
plot(BeetleMortality$dose, pred_died_m, type="l", ylim=c(0,80))
points(BeetleMortality$dose, BeetleMortality$died,pch=19)
points(BeetleMortality$dose, par_quants["10%",][pred_indexes], type="l", lty=3)
points(BeetleMortality$dose, par_quants["90%",][pred_indexes], type="l", lty=3)

# also try the different models discussed earlier ( quadratic logistic, cloglog)




