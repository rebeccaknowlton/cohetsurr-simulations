
#only touch this!!!

setting = 4
threshold= 0.70 
n <- 1000 #sample size for each group

#####
num.sim <- 50
library(quantreg)
library(mvtnorm)
library(groc)
source("boot.var.R")
source("het.test.R")
source("two.step.est.R")
source("parametric.est.R")
source("complex.heterogeneity.R")
# Set up sim parameters
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
  "delta.lower" = df,
  "delta.upper" = df,
    "delta.s.lower" = df,
  "delta.s.upper" = df,
    "R.s.lower" = df,
  "R.s.upper" = df,
  "pval.threshold" = df,
  "pvals" = rep(NA, num.sim)
)


# function to get coverage from output
get.coverage <- function(list.output) {
  delta.lower <- list.output$delta.lower
  delta.upper <- list.output$delta.upper
  delta.true <- list.output$delta - list.output$delta.bias
  delta.covg <- colMeans((delta.true >= delta.lower) & (delta.true <= delta.upper), na.rm=TRUE)

  delta.s.lower <- list.output$delta.s.lower
  delta.s.upper <- list.output$delta.s.upper
  delta.s.true <- list.output$delta.s - list.output$delta.s.bias
  delta.s.covg <- colMeans((delta.s.true >= delta.s.lower) & (delta.s.true <= delta.s.upper), na.rm=TRUE)

  R.s.lower <- list.output$R.s.lower
  R.s.upper <- list.output$R.s.upper
  R.s.true <- list.output$R.s - list.output$R.s.bias
  R.s.covg <- colMeans((R.s.true >= R.s.lower) & (R.s.true <= R.s.upper), na.rm=TRUE)
  return("delta.covg" = list(delta.covg,
                             "delta.s.covg" = delta.s.covg,
                             "R.s.covg" = R.s.covg))
}

# function to make latex tables
get.latex.tables <- function(list.output, list.output.2 = NULL, sim.num) {
  delta.table <- matrix(ncol = 16, nrow = 6)
  rownames(delta.table) <- c("Truth", "Estimate", "Bias", "ESE", "ASE", "Coverage")
  colnames(delta.table) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9",
                             "W10", "W11", "W12", "W13", "W14", "W15", "W16")
  delta.s.table <- delta.table
  R.s.table <- delta.table

  delta.table[1,] <- colMeans(list.output$delta - list.output$delta.bias, na.rm=TRUE)
  delta.table[2,] <- colMeans(list.output$delta, na.rm=TRUE)
  delta.table[3,] <- colMeans(list.output$delta.bias, na.rm=TRUE)
  delta.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$delta), na.rm=TRUE))
  delta.table[5,] <- colMeans(sqrt(list.output$delta.var), na.rm=TRUE)
  delta.table[6,] <- get.coverage(list.output)[[1]]

  delta.s.table[1,] <- colMeans(list.output$delta.s - list.output$delta.s.bias, na.rm=TRUE)
  delta.s.table[2,] <- colMeans(list.output$delta.s, na.rm=TRUE)
  delta.s.table[3,] <- colMeans(list.output$delta.s.bias, na.rm=TRUE)
  delta.s.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$delta.s), na.rm=TRUE))
  delta.s.table[5,] <- colMeans(sqrt(list.output$delta.s.var), na.rm=TRUE)
  delta.s.table[6,] <- get.coverage(list.output)[[2]]

  R.s.table[1,] <- colMeans(list.output$R.s - list.output$R.s.bias, na.rm=TRUE)
  R.s.table[2,] <- colMeans(list.output$R.s, na.rm=TRUE)
  R.s.table[3,] <- colMeans(list.output$R.s.bias, na.rm=TRUE)
  R.s.table[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output$R.s), na.rm=TRUE))
  R.s.table[5,] <- colMeans(sqrt(list.output$R.s.var), na.rm=TRUE)
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

#######
if(!is.null(list.output.2)){
delta.table.two.step <- matrix(ncol = 16, nrow = 6)
  rownames(delta.table.two.step) <- c("Truth", "Estimate", "Bias", "ESE", "ASE", "Coverage")
  colnames(delta.table.two.step) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9",
                             "W10", "W11", "W12", "W13", "W14", "W15", "W16")
  delta.s.table.two.step <- delta.table.two.step
  R.s.table.two.step <- delta.table.two.step
 delta.table.two.step[1,] <- colMeans(list.output.2$delta - list.output.2$delta.bias, na.rm=TRUE)
  delta.table.two.step[2,] <- colMeans(list.output.2$delta, na.rm=TRUE)
  delta.table.two.step[3,] <- colMeans(list.output.2$delta.bias, na.rm=TRUE)
  delta.table.two.step[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output.2$delta), na.rm=TRUE))
  delta.table.two.step[5,] <- colMeans(sqrt(list.output.2$delta.var), na.rm=TRUE)
  delta.table.two.step[6,] <- get.coverage(list.output.2)[[1]]

  delta.s.table.two.step[1,] <- colMeans(list.output.2$delta.s - list.output.2$delta.s.bias, na.rm=TRUE)
  delta.s.table.two.step[2,] <- colMeans(list.output.2$delta.s, na.rm=TRUE)
  delta.s.table.two.step[3,] <- colMeans(list.output.2$delta.s.bias, na.rm=TRUE)
  delta.s.table.two.step[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output.2$delta.s), na.rm=TRUE))
  delta.s.table.two.step[5,] <- colMeans(sqrt(list.output.2$delta.s.var), na.rm=TRUE)
  delta.s.table.two.step[6,] <- get.coverage(list.output.2)[[2]]

  R.s.table.two.step[1,] <- colMeans(list.output.2$R.s - list.output.2$R.s.bias, na.rm=TRUE)
  R.s.table.two.step[2,] <- colMeans(list.output.2$R.s, na.rm=TRUE)
  R.s.table.two.step[3,] <- colMeans(list.output.2$R.s.bias, na.rm=TRUE)
  R.s.table.two.step[4,] <- sqrt(matrixStats::colVars(as.matrix(list.output.2$R.s), na.rm=TRUE))
  R.s.table.two.step[5,] <- colMeans(sqrt(list.output.2$R.s.var), na.rm=TRUE)
  R.s.table.two.step[6,] <- get.coverage(list.output.2)[[3]]

  latex.table(delta.table.two.step, paste0("simres_", sim.num, "_delta"), caption = "", dcolumn = T, dec = 3)
  latex.table(delta.s.table.two.step, paste0("simres_", sim.num, "_delta.s"), caption = "", dcolumn = T, dec = 3)
  latex.table(R.s.table.two.step, paste0("simres_", sim.num, "_R.s"), caption = "", dcolumn = T, dec = 3)

  #index for w, boundary, middle,middle, boundary, average
  index.want = c(1,7,10,16,17)
  delta.table.two.step= abs(delta.table.two.step)
 delta.table.two.step.small = cbind(delta.table.two.step, apply(delta.table.two.step, 1, mean))[-c(1,2),]
 delta.table.two.step.small = format(round(delta.table.two.step.small[,index.want] ,3),nsmall=3)
 delta.s.table.two.step = abs(delta.s.table.two.step)
 delta.s.table.two.step.small = cbind(delta.s.table.two.step, apply(delta.s.table.two.step, 1, mean))[-c(1,2),]
 delta.s.table.two.step.small = format(round(delta.s.table.two.step.small[,index.want] ,3),nsmall=3)
 R.s.table.two.step = abs(R.s.table.two.step)
  R.s.table.two.step.small = cbind(R.s.table.two.step, apply(R.s.table.two.step, 1, mean))[-c(1,2),]
 R.s.table.two.step.small = format(round(R.s.table.two.step.small[,index.want] ,3),nsmall=3)
}
else {delta.table.two.step.small = delta.s.table.two.step.small = R.s.table.two.step.small = matrix("--",nrow = dim(R.s.table.small)[1], ncol = dim(R.s.table.small)[2])}
######
print(cbind(delta.table.small,delta.table.two.step.small))
print(cbind(delta.s.table.small,delta.s.table.two.step.small))
print(cbind(R.s.table.small,R.s.table.two.step.small))
  latex.table(cbind(delta.table.small,delta.table.two.step.small), paste0("simressmall_", sim.num, "_delta"), caption = "", dcolumn = T)
  latex.table(cbind(delta.s.table.small,delta.s.table.two.step.small), paste0("simressmall_", sim.num, "_delta.s"), caption = "", dcolumn = T)
  latex.table(cbind(R.s.table.small,R.s.table.two.step.small), paste0("simressmall_", sim.num, "_R.s"), caption = "", dcolumn = T)
}

get.parameters = function(setting){
	if(setting ==1){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.mean.control = 2
		s.mean.treat = 4
		s.sd.control = 2
		s.sd.treat = 3
		beta0 = 2
		beta1 = 0.1
		beta2 = 7
		beta3 = 3
		beta4 = c(0.5,0.25)
		beta5 = .38*c(0.5,0.75) #from LP: The multiplication factor is how I made the power not be 1
		sd.y = 6
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.mean.control" = s.mean.control, "s.mean.treat" = s.mean.treat, "s.sd.control" = s.sd.control, "s.sd.treat" = s.sd.treat,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}
	if(setting ==2){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.mean.control = 2
		s.mean.treat = 4
		s.sd.control = 2
		s.sd.treat = 3
		beta0 = 2
		beta1 = 0.1
		beta2 = 4.12
		beta3 = 3
		beta4 = c(0.5,0.25)
		beta5 = c(0,0)
		sd.y = 3
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.mean.control" = s.mean.control, "s.mean.treat" = s.mean.treat, "s.sd.control" = s.sd.control, "s.sd.treat" = s.sd.treat,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}
	if(setting ==3){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.mean.control = 1.45
		s.mean.treat = 1.7
		s.sd.control = 0.1
		s.sd.treat = 0.2
		beta0 = 2
		beta1 = 0.1
		beta2 = 12
		beta3 = 6
		beta4 = c(1,1)
		beta5 = c(4,5)
		sd.y = 6
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.mean.control" = s.mean.control, "s.mean.treat" = s.mean.treat, "s.sd.control" = s.sd.control, "s.sd.treat" = s.sd.treat,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}
	if(setting ==4){
		mean.w1 = 2
		sd.w1 = 6
		mean.w2 = 3
		sd.w2 = 13
		s.mean.control = 1.45
		s.mean.treat = 1.7
		s.sd.control = 0.1
		s.sd.treat = 0.2
		beta0 = 2
		beta1 = 3
		beta2 = 4
		beta3 = 5
		beta4 = c(1,1)
		beta5 = c(0,0)
		sd.y = 6
		return(list("mean.w1" = mean.w1, "sd.w1" = sd.w1, "mean.w2" = mean.w2, "sd.w2" = sd.w2, "s.mean.control" = s.mean.control, "s.mean.treat" = s.mean.treat, "s.sd.control" = s.sd.control, "s.sd.treat" = s.sd.treat,"beta0"=beta0,"beta1"=beta1, "beta2"=beta2,"beta3"=beta3, "beta4"=beta4, "beta5"=beta5, "sd.y"=sd.y))
	}




}


get.truth = function(setting, grid){
  if(setting ==1){
    params = get.parameters(setting=1)
    delta.s = params$beta1+params$beta3*params$s.mean.control + grid%*%params$beta5
    delta = params$beta1+(params$beta2+params$beta3)*(params$s.mean.treat) - (params$beta2)*(params$s.mean.control) + grid%*%params$beta5
    R = 1-delta.s/delta
    return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
  }
  if(setting ==2){
    params = get.parameters(setting=2)
    delta.s = params$beta1+params$beta3*params$s.mean.control+ grid%*%params$beta5
    delta = params$beta1+(params$beta2+params$beta3)*(params$s.mean.treat) - (params$beta2)*(params$s.mean.control) + grid%*%params$beta5
    R = 1-delta.s/delta
    return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
  }
  if(setting ==3){
    params = get.parameters(setting=3)
    delta.s = params$beta1 + params$beta3*(exp(params$s.mean.control + 0.5*params$s.sd.control^2)) + grid%*%params$beta5
    # calculate E(s^2) for the treat and control groups based on lognormal distribution
    e.s1.2 = (exp(params$s.mean.treat + 0.5*params$s.sd.treat^2))^2 + (exp(params$s.sd.treat^2)-1)*exp(2*params$s.mean.treat + params$s.sd.treat^2)
    e.s0.2 = (exp(params$s.mean.control + 0.5*params$s.sd.control^2))^2 + (exp(params$s.sd.control^2)-1)*exp(2*params$s.mean.control + params$s.sd.control^2)
    delta = params$beta1 + params$beta2*(e.s1.2 - e.s0.2) + params$beta3*(exp(params$s.mean.treat + 0.5*params$s.sd.treat^2)) + grid%*%params$beta5
    R = 1-delta.s/delta
    return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
  }
  
  if(setting ==4){
    params = get.parameters(setting=4)
    delta.s = params$beta1 + params$beta3*(exp(params$s.mean.control + 0.5*params$s.sd.control^2)) + grid%*%params$beta5
    # calculate E(s^2) for the treat and control groups based on lognormal distribution
    e.s1.2 = (exp(params$s.mean.treat + 0.5*params$s.sd.treat^2))^2 + (exp(params$s.sd.treat^2)-1)*exp(2*params$s.mean.treat + params$s.sd.treat^2)
    e.s0.2 = (exp(params$s.mean.control + 0.5*params$s.sd.control^2))^2 + (exp(params$s.sd.control^2)-1)*exp(2*params$s.mean.control + params$s.sd.control^2)
    delta = params$beta1 + params$beta2*(e.s1.2 - e.s0.2) + params$beta3*(exp(params$s.mean.treat + 0.5*params$s.sd.treat^2)) + grid%*%params$beta5
    R = 1-delta.s/delta
    return(list("delta.s" = delta.s, "delta" = delta, "R" = R))
  }
  
}


gen.data = function(n,setting){
	params = get.parameters(setting=setting)
	if(setting == 1) {
		data.temp <- data.frame(A = c(rep(1,n),rep(0,n)),
                          W1 = runif(n*2, params$mean.w1, params$sd.w1),
                          W2 = runif(n*2, params$mean.w2, params$sd.w2))

   	data.temp$S =vector(length=n*2)
  		data.temp$S[data.temp$A==1]= rnorm(sum(data.temp$A==1), params$s.mean.treat , params$s.sd.treat )
  		data.temp$S[data.temp$A==0]= rnorm(sum(data.temp$A==0), params$s.mean.control, params$s.mean.control)


  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +  cbind(data.temp$W1*data.temp$A, data.temp$W2*data.temp$A)%*%params$beta5 + rnorm(n*2, 0, params$sd.y)
    	return(data.temp)
    }
       if(setting == 2) {
		data.temp <- data.frame(A = c(rep(1,n),rep(0,n)),
                          W1 = runif(n*2, params$mean.w1, params$sd.w1),
                          W2 = runif(n*2, params$mean.w2, params$sd.w2))

  		data.temp$S =vector(length=n*2)
  		data.temp$S[data.temp$A==1]= rnorm(sum(data.temp$A==1), params$s.mean.treat , params$s.sd.treat )
  		data.temp$S[data.temp$A==0]= rnorm(sum(data.temp$A==0), params$s.mean.control, params$s.mean.control)


  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +  cbind(data.temp$W1*data.temp$A, data.temp$W2*data.temp$A)%*%params$beta5 + rnorm(n*2, 0, params$sd.y)
    	return(data.temp)
    }
if(setting == 3) {
		data.temp <- data.frame(A = c(rep(1,n),rep(0,n)),
                          W1 = runif(n*2, params$mean.w1, params$sd.w1),
                          W2 = runif(n*2, params$mean.w2, params$sd.w2))

   	data.temp$S =vector(length=n*2)
  		data.temp$S[data.temp$A==1]= exp(rnorm(sum(data.temp$A==1), params$s.mean.treat , params$s.sd.treat ))
  		data.temp$S[data.temp$A==0]= exp(rnorm(sum(data.temp$A==0), params$s.mean.control, params$s.sd.control))


  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S^2 + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  +
    	cbind(data.temp$W1 * data.temp$A,data.temp$W2 * data.temp$A)%*%params$beta5   + rnorm(n*2, 0, params$sd.y) 
    	return(data.temp)
    }
    if(setting == 4) {
		data.temp <- data.frame(A = c(rep(1,n),rep(0,n)),
                          W1 = runif(n*2, params$mean.w1, params$sd.w1),
                          W2 = runif(n*2, params$mean.w2, params$sd.w2))

   	data.temp$S =vector(length=n*2)
  		data.temp$S[data.temp$A==1]= exp(rnorm(sum(data.temp$A==1), params$s.mean.treat , params$s.sd.treat ))
  		data.temp$S[data.temp$A==0]= exp(rnorm(sum(data.temp$A==0), params$s.mean.control, params$s.sd.control))


  	data.temp$Y <- params$beta0 + params$beta1 * data.temp$A + params$beta2 * data.temp$S^2 + params$beta3 * data.temp$A * data.temp$S +
    	cbind(data.temp$W1,data.temp$W2)%*%params$beta4  + rnorm(n*2, 0, params$sd.y) 

  
    	return(data.temp)
    }


}

##########################################################################
##### Set the setting, just change it and run it, code not repeated ######
##########################################################################

# Setting 1 = models correctly specified + yes hetero
# Setting 2 = models correctly specified + no hetero
# Setting 3 = models wrong + yes hetero
# Setting 4 = models wrong +no hetero




#make W grid
#HAVE TO SET A SEED HERE SO THAT ALL PARALLEL SCRIPTS USE SAME GRID
set.seed(1)


params = get.parameters(setting=setting)
if(setting == 1 | setting == 2 | setting == 3 | setting == 4){
	w1.large <- runif(10000, params$mean.w1, params$sd.w1)
	w2.large <- runif(10000, params$mean.w2, params$sd.w2)
}
w1.grd <- seq(quantile(w1.large, 0.2), quantile(w1.large, 0.8), length = grid.size)
w2.grd <- seq(quantile(w2.large, 0.2), quantile(w2.large, 0.8), length = grid.size)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
	colnames(w1w2.grid) <- c("W1", "w2")

if(setting ==2){
	h.0=   0.1726482
	h.1 =  0.1818023
	h.3 = 0.3461025*(n^(-0.01)) #undersmooth this one only
}

if(setting ==1){
	h.0=   1.292377*(n^(-0.05))
	h.1 = 1.198731*(n^(-0.05))
	h.3 = 0.3752841*(n^(-0.05)) 
}

if(setting ==4){
	h.0=    0.4805655*1.5 
	h.1 = 0.4014512 *1.5
	h.3 =  0.1294852*(n^(-0.1)) 
}

if(setting ==3){
	h.0=    17.55111 *(n^(-0.05))
	h.1 = 15.31706*(n^(-0.05))
	h.3 = 0.08621875 *(n^(-0.05))
}

#THIS IS WHAT MAKES EACH PARALLEL VERSION DIFFERENT
set.seed(parallel.num*100)

outputfile = c()
pfile = c()
for (i in 1:num.sim) {
	start_time <- Sys.time()

    data.temp = gen.data(n=n, setting =setting)
  func.output <- complex.heterogeneity(y = data.temp$Y,
                                       s = data.temp$S,
                                       a = data.temp$A,
                                       W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2),
                                       type = "both",
                                       variance = TRUE,
                                       test = TRUE,
                                       W.grid = w1w2.grid, threshold=threshold, h.0=h.0,h.1=h.1,h.3=h.3)
  outputfile <- rbind(outputfile,func.output$return.grid)
  pfile = rbind(pfile, func.output$pval)

 write.table(outputfile, paste("outputfile", setting, "_011824",parallel.num,".txt", sep=""), quote = FALSE, row.names = FALSE)
 write.table(pfile, paste("pfile", setting, "_011824_",parallel.num,".txt", sep=""), quote = FALSE, row.names = FALSE)

  print(i)
  end_time <- Sys.time()
  print(end_time-start_time)
}




