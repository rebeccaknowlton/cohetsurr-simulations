# also need to run functions in the sims_complex_master file

kernel <- function(x, h) { return(dnorm(x / h)) }

mse.BW <- function(data, h)
{	folds = 10
	mse.vec = vector(length=folds)
	num.observ = dim(data)[1]
	ind = sample(1:folds, num.observ, replace = TRUE)
		for(k in 1:folds) {
		data.tra = data[ind != k,]
		data.val = data[ind == k,]
		temp = c()
		for(kk in 1:dim(data.val)[1]){
			u = data.val$U.hat[kk]
			temp = c(temp, sum(kernel(data.tra$U.hat - u, h) * data.val$Y[kk] / sum(kernel(data.tra$U.hat - u, h))) - data.val$Y[kk])
		}
		mse.vec[k] = mean(temp^2)
	}
return(mean(mse.vec))
}


optimize.mse.BW = function(data){
	opt = optimize(f = mse.BW, data=data,lower =bw.nrd(data$U.hat)/10, upper = bw.nrd(data$U.hat)*10)
 return(opt$minimum)
}


mse.BW.s <- function(data, h, h.u)
{	folds = 10
	mse.vec = vector(length=folds)
	num.observ = dim(data)[1]
	ind = sample(1:folds, num.observ, replace = TRUE)
		for(k in 1:folds) {
		data.tra = data[ind != k,]
		data.val = data[ind == k,]
		temp = c()
		for(kk in 1:dim(data.val)[1]){
			u = data.val$U.hat[kk]
			s = data.val$S[kk]
			pred.y = sum(kernel(data.tra$U.hat - u , h.u) * kernel(data.tra$S  - s, h) * data.tra$Y / sum(kernel(data.tra$U.hat- u, h.u) * kernel(data.tra$S - s, h)))
			temp = c(temp, pred.y - data.val$Y[kk])
		}
		mse.vec[k] = mean(temp^2)
	}
return(mean(mse.vec))
	}

optimize.mse.BW.s = function(data, h.u){
opt = optimize(f = mse.BW.s, data=data,lower =bw.nrd(data$S)/10, upper = bw.nrd(data$S)*10, h.u=h.u)
 return(opt$minimum)

}


#generate big dataset to get bandwidth
setting = 4
grid.size = 4
n = 1000
params = get.parameters(setting=setting)
set.seed(1)

if(setting == 1 | setting == 2 | setting == 3| setting == 4){
	w1.large <- runif(10000, params$mean.w1, params$sd.w1)
	w2.large <- runif(10000, params$mean.w2, params$sd.w2)
}

w1.grd <- seq(quantile(w1.large, 0.2), quantile(w1.large, 0.8), length = grid.size)
w2.grd <- seq(quantile(w2.large, 0.2), quantile(w2.large, 0.8), length = grid.size)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
colnames(w1w2.grid) <- c("W1", "W2")
W.grid = w1w2.grid

# expand W.grid
W.grid.expand <- expand.grid(split(W.grid, rep(1:ncol(W.grid), each = nrow(W.grid))))

data.temp = gen.data(n=n, setting =setting)
y = data.temp$Y
s = data.temp$S
a = data.temp$A
W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2)


W.mat.control <- W.mat[a==0,]
W.mat.treat <- W.mat[a==1,]
covariates.control <- split(W.mat.control, rep(1:ncol(W.mat.control), each = nrow(W.mat.control)))
covariates.treat <- split(W.mat.treat, rep(1:ncol(W.mat.treat), each = nrow(W.mat.treat)))
data.control <- cbind(data.frame(Y = y[a==0], S = s[a==0]), covariates.control)
data.treat <- cbind(data.frame(Y = y[a==1], S = s[a==1]), covariates.treat)
data.control.true=data.control
data.treat.true=data.treat

#create dataframe for combined data
covariates.all <- split(W.mat, rep(1:ncol(W.mat), each = nrow(W.mat)))
num.cov <- length(covariates.all)
data.all <- cbind(data.frame(Y = y, S = s, A = a), covariates.all)
for (i in 1:num.cov) {
  names(data.all)[3+i] <- paste0("W",i)
}


names(W.grid.expand) <- names(data.control)[3:ncol(data.control)]

working.model.control <- lm(Y ~ ., data = data.control)
working.model.treat <- lm(Y ~ ., data = data.treat)
  
data.control.pred = data.control
data.control.pred$S = mean(data.all$S) 
data.treat.pred = data.treat
data.treat.pred$S = mean(data.all$S) 
data.control$U.hat <- predict(working.model.treat, newdata= data.control.pred) - predict(working.model.control, newdata= data.control.pred) 
data.treat$U.hat <- predict(working.model.treat, newdata= data.treat.pred) - predict(working.model.control, newdata= data.treat.pred)
W.grid.expand$S = mean(data.all$S)
W.grid.expand$U.hat <- predict(working.model.treat, newdata= W.grid.expand) - predict(working.model.control, newdata= W.grid.expand)

set.seed(1)


h.0 = optimize.mse.BW(data=data.control)

h.1= optimize.mse.BW(data=data.treat)

h.3 = optimize.mse.BW.s(data=data.treat, h.u = h.1)

h.0
h.1
h.3

