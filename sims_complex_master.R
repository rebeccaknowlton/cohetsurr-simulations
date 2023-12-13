
#only touch this!!!

setting = 1

#####
num.sim <- 100
library(quantreg)
source("boot.var.R")
#source("het.test.R")
source("two.step.est.R")
source("complex.heterogeneity.R")
# Set up sim parameters
n <- 2000
grid.size <- 4
df <- data.frame(matrix(nrow = num.sim, ncol = grid.size * grid.size))
empty.sim <- list(
  "delta" = df,
  "delta.s" = df,
  "R.s" = df,
  "delta.bias" = df,
  "delta.s.bias" = df,
  "R.s.bias" = df,
  "delta.var" = df,
  "delta.s.var" = df,
  "R.s.var" = df,
  "pvals" = rep(NA, num.sim)
)

# function to get coverage from output
get.coverage <- function(list.output) {
  delta.lower <- list.output$delta - 1.96 * sqrt(list.output$delta.var)
  delta.upper <- list.output$delta + 1.96 * sqrt(list.output$delta.var)
  delta.true <- list.output$delta - list.output$delta.bias
  delta.covg <- colMeans((delta.true >= delta.lower) & (delta.true <= delta.upper))

  delta.s.lower <- list.output$delta.s - 1.96 * sqrt(list.output$delta.s.var)
  delta.s.upper <- list.output$delta.s + 1.96 * sqrt(list.output$delta.s.var)
  delta.s.true <- list.output$delta.s - list.output$delta.s.bias
  delta.s.covg <- colMeans((delta.s.true >= delta.s.lower) & (delta.s.true <= delta.s.upper))

  R.s.lower <- list.output$R.s - 1.96 * sqrt(list.output$R.s.var)
  R.s.upper <- list.output$R.s + 1.96 * sqrt(list.output$R.s.var)
  R.s.true <- list.output$R.s - list.output$.R.s.bias
  R.s.covg <- colMeans((R.s.true >= R.s.lower) & (R.s.true <= R.s.upper))
  return("delta.covg" = list(delta.covg,
                             "delta.s.covg" = delta.s.covg,
                             "R.s.covg" = R.s.covg))
}

# function to make latex tables
get.latex.tables <- function(list.output, sim.num) {
  delta.table <- matrix(ncol = 16, nrow = 6)
  rownames(delta.table) <- c("Truth", "Estimate", "Bias", "ESE", "ASE", "Coverage")
  colnames(delta.table) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9",
                             "W10", "W11", "W12", "W13", "W14", "W15", "W16")
  delta.s.table <- delta.table
  R.s.table <- delta.table

  delta.table[1,] <- colMeans(list.output$delta - list.output$delta.bias)
  delta.table[2,] <- colMeans(list.output$delta)
  delta.table[3,] <- colMeans(list.output$delta.bias.)
  delta.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$delta)))
  delta.table[5,] <- colMeans(sqrt(list.output$delta.var))
  delta.table[6,] <- get.coverage(list.output)[[1]]

  delta.s.table[1,] <- colMeans(list.output$delta.s - list.output$delta.s.bias)
  delta.s.table[2,] <- colMeans(list.output$delta.s)
  delta.s.table[3,] <- colMeans(list.output$delta.s.bias)
  delta.s.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$delta.s)))
  delta.s.table[5,] <- colMeans(sqrt(list.output$delta.s.var))
  delta.s.table[6,] <- get.coverage(list.output)[[2]]

  R.s.table[1,] <- colMeans(list.output$R.s - list.output$R.s.bias)
  R.s.table[2,] <- colMeans(list.output$R.s)
  R.s.table[3,] <- colMeans(list.output$R.s.bias)
  R.s.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$R.s)))
  R.s.table[5,] <- colMeans(sqrt(list.output$R.s.var))
  R.s.table[6,] <- get.coverage(list.output)[[3]]

  latex.table(delta.table, paste0("simres_", sim.num, "_delta"), caption = "", dcolumn = T, dec = 3)
  latex.table(delta.s.table, paste0("simres_", sim.num, "_delta.s"), caption = "", dcolumn = T, dec = 3)
  latex.table(R.s.table, paste0("simres_", sim.num, "_R.s"), caption = "", dcolumn = T, dec = 3)

  #index for w, boundary, middle,middle, boundary, average
  index.want = c(1,7,10,16,17)
  delta.table= abs(delta.table)
 delta.table.small = cbind(delta.table, apply(delta.table, 1, mean))[-c(1,2),]
 delta.table.small = format(round(delta.table.small[,index.want] ,3),nsmall=3)
 delta.s.table = abs(delta.s.table)
 delta.s.table.small = cbind(delta.s.table, apply(delta.s.table, 1, mean))[-c(1,2),]
 delta.s.table.small = format(round(delta.s.table.small[,index.want] ,3),nsmall=3)
 R.s.table = abs(R.s.table)
  R.s.table.small = cbind(R.s.table, apply(R.s.table, 1, mean))[-c(1,2),]
 R.s.table.small = format(round(R.s.table.small[,index.want] ,3),nsmall=3)

  #need to append -- as a placeholder for two stage
  placeholder = matrix("--",nrow = dim(R.s.table.small)[1], ncol = dim(R.s.table.small)[2])
  latex.table(cbind(delta.table.small,placeholder), paste0("simressmall_", sim.num, "_delta"), caption = "", dcolumn = T)
  latex.table(cbind(delta.s.table.small,placeholder), paste0("simressmall_", sim.num, "_delta.s"), caption = "", dcolumn = T)
  latex.table(cbind(R.s.table.small,placeholder), paste0("simressmall_", sim.num, "_R.s"), caption = "", dcolumn = T)
}

get.parameters = function(setting){
	if(setting ==1){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.intercept = 2
		s.treat = 3
		sd.s = 3
		beta0 = 2
		beta1 = 3
		beta2 = 3
		beta3 = 2
		beta4 = c(2,1)
		beta5 = 0.33*c(0.5,0.75) #from LP: The multiplication factor is how I made the power not be 1
		sd.y = 6
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.intercept" = s.intercept, "s.treat" = s.treat, "sd.s" = sd.s,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}
	if(setting ==2){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.mean.control = 2
		s.mean.treat = 4
		s.sd.control = 1
		s.sd.treat = 3

		beta0 = 2
		beta1 = 3
		beta2 = 3
		beta3 = 2
		beta4 = c(2,1)
		beta5 = 0.05*c(0.5,0.75)
		sd.y = 6
		beta6 = 0.04 #0.1
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.mean.control" = s.mean.control, "s.mean.treat" = s.mean.treat, "s.sd.control" = s.sd.control, "s.sd.treat" = s.sd.treat,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y, "beta6"=beta6))
	}
	if(setting ==3){
		s.treat1 = 2
		s.treat2 = 2
		s.control1 = 0
		s.control2 = 2
		mean.s.1 = 6
  		mean.s.0 = 3
  		sd.s.1 = 4
  		sd.s.0 = 1
  		treat.effect.y = 0.5
  		treat.effect.s = 12
  		int.effect = 3

		beta0 = 1
		beta1 = 2
		beta2 = 1.2
		beta3 = 2*2.5
		beta4 = 2*0.5
		beta5 = 2*0.01
		sd.y = 2

		return(list("s.treat1" = s.treat1, "s.treat2" = s.treat2, "s.control1" = s.control1, "s.control2" = s.control2,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y, "mean.s.1" = mean.s.1, "mean.s.0" = mean.s.0, "treat.effect.y"=treat.effect.y, "treat.effect.s"=treat.effect.s,"int.effect" = int.effect, "sd.s.1" = sd.s.1, "sd.s.0" = sd.s.0))
	}
	if(setting ==4){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.intercept = 2
		s.treat = 3
		sd.s = 3
		beta0 = 2
		beta1 = 3
		beta2 = 3
		beta3 = 2
		beta4 = c(2,1)
		beta5 = c(0,0)
		sd.y = 6
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.intercept" = s.intercept, "s.treat" = s.treat, "sd.s" = sd.s,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}


}

get.truth = function(setting, grid){
	if(setting ==1){
		params = get.parameters(setting=1)
		delta.s = params$beta1+params$beta3*params$s.intercept + grid%*%params$beta5
		delta = params$beta1+(params$beta2+params$beta3)*(params$s.intercept + params$s.treat) - (params$beta2)*(params$s.intercept) + grid%*%params$beta5
		R = 1-delta.s/delta
		return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
	}
	if(setting ==2){
		params = get.parameters(setting=2)
		delta.s = params$beta1+params$beta3*params$s.mean.control + grid%*%params$beta5 + params$beta6*params$s.mean.control*grid[,2]
		delta = params$beta1+(params$beta2+params$beta3)*(params$s.mean.treat) - (params$beta2)*(params$s.mean.control) + grid%*%params$beta5 + params$beta6*(2*params$s.mean.treat - params$s.mean.control)*grid[,2]
		R = 1-delta.s/delta
		return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
	}
	if(setting == 3){
		params = get.parameters(setting=3)
		delta <- params$treat.effect.y + params$beta2 * log(grid[,1]) + params$beta3 * (grid[,2]^2) + params$beta4 * exp(params$beta5*grid[,2]*grid[,1]) + params$treat.effect.s*params$mean.s.1 - params$treat.effect.s*params$mean.s.0 + params$int.effect*params$mean.s.1
  	delta.s <- params$treat.effect.y + params$int.effect*params$mean.s.0+ params$beta2 * log(grid[,1]) + params$beta3 * (grid[,2]^2) + params$beta4 * exp(params$beta5*grid[,2]*grid[,1])
   	R = 1-delta.s/delta
		return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
}
if(setting ==4){
		params = get.parameters(setting=4)
		delta.s = params$beta1+params$beta3*params$s.intercept + grid%*%params$beta5
		delta = params$beta1+(params$beta2+params$beta3)*(params$s.intercept + params$s.treat) - (params$beta2)*(params$s.intercept) + grid%*%params$beta5
		R = 1-delta.s/delta
		return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
	}

}

gen.data = function(n,setting){
	params = get.parameters(setting=setting)
	if(setting == 1) {
		data.temp <- data.frame(A = rbinom(n, 1 ,0.5),
                          W1 = runif(n, params$mean.w1, params$sd.w2),
                          W2 = runif(n, params$mean.w2, params$sd.w2))

  	data.temp$S <- params$s.intercept + params$s.treat * data.temp$A + rnorm(n, 0, params$sd.s)

  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +  cbind(data.temp$W1*data.temp$A, data.temp$W2*data.temp$A)%*%params$beta5 + rnorm(n, 0, params$sd.y)
    	return(data.temp)
    }
    if(setting == 2){
    	data.temp <- data.frame(A = rbinom(n, 1 ,0.5),
                           W1 = runif(n, params$mean.w1, params$sd.w2),
                          W2 = runif(n, params$mean.w2, params$sd.w2))

  		data.temp$S =vector(length=n)
  		data.temp$S[data.temp$A==1]= rnorm(sum(data.temp$A==1), params$s.mean.treat , params$s.sd.treat )
  		data.temp$S[data.temp$A==0]= rnorm(sum(data.temp$A==0), params$s.mean.control, params$s.mean.control)

		data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +  cbind(data.temp$W1*data.temp$A, data.temp$W2*data.temp$A)%*%params$beta5 + params$beta6 * data.temp$W2 * data.temp$S +
   params$beta6 * data.temp$W2 * data.temp$S * data.temp$A + rnorm(n, 0, params$sd.y)
    	return(data.temp)
    }
    if(setting == 3) {
    	  data.temp = data.frame(A = rbinom(n, 1 ,0.5),
                         W1 = rgamma(n, params$s.treat1, params$s.treat2),
                         W2 = rnorm(n, params$s.control1, params$s.control2))

  data.temp$S =vector(length=n)
  data.temp$S[data.temp$A==1]= rnorm(sum(data.temp$A==1), params$mean.s.1, params$sd.s.1)
  data.temp$S[data.temp$A==0]= rnorm(sum(data.temp$A==0), params$mean.s.0, params$sd.s.0)


  data.temp$Y = params$beta0 + params$treat.effect.y * data.temp$A + params$treat.effect.s*data.temp$S  + params$int.effect*data.temp$S*data.temp$A + params$beta1 * log(data.temp$W1) + data.temp$W2^2 + params$beta2 * log(data.temp$W1) * data.temp$A + params$beta3 * (data.temp$W2^2) * data.temp$A + params$beta4 * exp(params$beta5*data.temp$W2*data.temp$W1) * data.temp$A + exp(rnorm(n, 0, params$sd.y))
  return(data.temp)
    }
    if(setting == 4) {
		data.temp <- data.frame(A = rbinom(n, 1 ,0.5),
                          W1 = runif(n, params$mean.w1, params$sd.w2),
                          W2 = runif(n, params$mean.w2, params$sd.w2))

  	data.temp$S <- params$s.intercept + params$s.treat * data.temp$A + rnorm(n, 0, params$sd.s)

  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +  cbind(data.temp$W1*data.temp$A, data.temp$W2*data.temp$A)%*%params$beta5 + rnorm(n, 0, params$sd.y)
    	return(data.temp)
    }

}

##########################################################################
##### Set the setting, just change it and run it, code not repeated ######
##########################################################################

# Setting 1 = models correctly specified
# Setting 2 = models slightly wrong
# Setting 3 = models very wrong ##
# Setting 4 = models correctly specified, no heterogeneity




#make W grid
#HAVE TO SET A SEED HERE SO THAT ALL PARALLEL SCRIPTS USE SAME GRID
set.seed(1)


params = get.parameters(setting=setting)
if(setting == 1 | setting == 2 | setting == 4){
	w1.large <- runif(10000, params$mean.w1, params$sd.w2)
	w2.large <- runif(10000, params$mean.w2, params$sd.w2)
}
if(setting == 3){
	w1.large = rgamma(10000, params$s.treat1, params$s.treat2)
	w2.large = rnorm(10000, params$s.control1, params$s.control2)
	}

w1.grd <- seq(quantile(w1.large, 0.2), quantile(w1.large, 0.8), length = grid.size)
w2.grd <- seq(quantile(w2.large, 0.2), quantile(w2.large, 0.8), length = grid.size)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
	colnames(w1w2.grid) <- c("W1", "w2")


#THIS IS WHAT MAKES EACH PARALLEL VERSION DIFFERENT
set.seed(parallel.num*100)

outputfile = c()
#pfile = c()
for (i in 1:num.sim) {
	start_time <- Sys.time()

    data.temp = gen.data(n=n, setting =setting)
  func.output <- complex.heterogeneity(y = data.temp$Y,
                                       s = data.temp$S,
                                       a = data.temp$A,
                                       W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2),
                                       type = "two step",
                                       variance = TRUE,
                                       test = FALSE,
                                       W.grid = w1w2.grid)
  #outputfile <- rbind(outputfile,func.output$return.grid)
  outputfile <- rbind(outputfile, func.output)
  #pfile = c(pfile, func.output$pval)

 write.table(outputfile, paste("outputfile", setting, "_",parallel.num,".txt", sep=""), quote = FALSE, row.names = FALSE)
 #write.table(pfile, paste("pfile", setting, "_",parallel.num,".txt", sep=""), quote = FALSE, row.names = FALSE)

  print(i)
  end_time <- Sys.time()
  print(end_time-start_time)
}





