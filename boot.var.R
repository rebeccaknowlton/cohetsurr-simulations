#' boot.var
#'
#' @param data.control subset of the data that was in the control group
#' @param data.treat subset of the data that was in the treatment group
#' @param W.grid.expand expanded grid containing rows for each possible combination of the covariates
#' @param type either "model" or "two step"
#'
#' @return bootsrapped variance estimates for delta, delta.s, and R.s
#'
#' @examples
boot.var <- function(data.control, data.treat, W.grid.expand, type, test=FALSE, data.all = NULL, num.cov=NULL, results.for.test = NULL, threshold) {
  num.boot <- 200
  boot.delta.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.delta.s.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.R.s.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
	
  boot.delta.est.two.step <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.delta.s.est.two.step <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.R.s.est.two.step <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))

    boot.R.d.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot)) 
  boot.R.d.est.two.step <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))

  for (j in 1:num.boot) {
  		boot.data.control <- data.control[sample(1:nrow(data.control), nrow(data.control), replace = TRUE),]
      boot.data.treat <- data.treat[sample(1:nrow(data.treat), nrow(data.treat), replace = TRUE),]
      if (type == "model") {
        boot.estimate <- parametric.est(boot.data.control, boot.data.treat, W.grid.expand)
        boot.delta.est[,j] <- boot.estimate$delta
      	boot.delta.s.est[,j] <- boot.estimate$delta.s
      	boot.R.s.est[,j] <- boot.estimate$R.s
         }
      if (type == "two step") {
        boot.estimate.two.step <- two.step.est(boot.data.control, boot.data.treat, W.grid.expand)
        boot.delta.est.two.step[,j] <- boot.estimate.two.step$delta.two.step
      	boot.delta.s.est.two.step[,j] <- boot.estimate.two.step$delta.s.two.step
      	boot.R.s.est.two.step[,j] <- boot.estimate.two.step$R.s.two.step
         }
      if (type == "both"){
      	boot.estimate <- parametric.est(boot.data.control, boot.data.treat, W.grid.expand)
      	boot.delta.est[,j] <- boot.estimate$delta
      	boot.delta.s.est[,j] <- boot.estimate$delta.s
      	boot.R.s.est[,j] <- boot.estimate$R.s

      	boot.estimate.two.step <- two.step.est(boot.data.control, boot.data.treat, W.grid.expand)
      	  	boot.delta.est.two.step[,j] <- boot.estimate.two.step$delta.two.step
      	boot.delta.s.est.two.step[,j] <- boot.estimate.two.step$delta.s.two.step
      	boot.R.s.est.two.step[,j] <- boot.estimate.two.step$R.s.two.step

      }
        if(test) {
        	if(type == "two step" | type == "both"){
   	  tau.boot.two.step = mean(boot.estimate.two.step$R.s.two.step)
	  boot.R.d.est.two.step[,j]  = boot.estimate.two.step$R.s.two.step - tau.boot.two.step
	
}
 	if(type == "model" | type == "both"){
 	  tau.boot = mean(boot.estimate$R.s)
	  boot.R.d.est[,j]  = boot.estimate$R.s - tau.boot
}
        }
    }
      my.grid = c()
      if(type == "model" | type == "both"){
        my.grid <- cbind(apply(as.matrix(boot.delta.est), 1, mad)^2,
                   apply(as.matrix(boot.delta.s.est), 1, mad)^2,
                   apply(as.matrix(boot.R.s.est), 1, mad)^2, 
                   matrixStats::rowQuantiles(as.matrix(boot.delta.est), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.est), probs=0.975),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.s.est), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.s.est), probs=0.975),									   matrixStats::rowQuantiles(as.matrix(boot.R.s.est), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.R.s.est), probs=0.975),
                   apply(as.matrix(boot.R.s.est), 1, function(x) mean(x<=threshold)))
  		colnames(my.grid) <- c("delta.var", "delta.s.var", "R.s.var", "delta.lower", "delta.upper", "delta.s.lower", "delta.s.upper", "R.s.lower", "R.s.upper","pval.threshold")
  		}
  		
		if(type == "two step" | type == "both") {
			my.grid.two.step <- cbind(apply(as.matrix(boot.delta.est.two.step), 1, mad)^2,
                   apply(as.matrix(boot.delta.s.est.two.step), 1, mad)^2,
                   apply(as.matrix(boot.R.s.est.two.step), 1, mad)^2,
                   matrixStats::rowQuantiles(as.matrix(boot.delta.est.two.step), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.est.two.step), probs=0.975),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.s.est.two.step), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.delta.s.est.two.step), probs=0.975),
                   matrixStats::rowQuantiles(as.matrix(boot.R.s.est.two.step), probs=0.025),
                   matrixStats::rowQuantiles(as.matrix(boot.R.s.est.two.step), probs=0.975),
apply(as.matrix(boot.R.s.est.two.step), 1, function(x) mean(x<threshold)))
  		colnames(my.grid.two.step) <- c("delta.var.two.step", "delta.s.var.two.step", "R.s.var.two.step","delta.lower.two.step", "delta.upper.two.step","delta.s.lower.two.step", "delta.s.upper.two.step","R.s.lower.two.step", "R.s.upper.two.step","pval.threshold.two.step")
		my.grid = cbind(my.grid, my.grid.two.step)
		}
  if(!test) {return(list("my.grid"=my.grid))}
  if(test) {
  	pval=c()
  	band.critical = c()
  	if(type == "model" | type == "both") {
  		pval = het.test(data.all, num.cov)
  		R.d.var = apply(as.matrix(boot.R.d.est), 1, sd)^2
  		std.R = (results.for.test$R.s - mean(results.for.test$R.s))/sqrt(R.d.var)
		t.statistic = max(abs(std.R))
		print(paste("Statistic for parametric omnibus:", t.statistic))
		aa = cor(t(boot.R.d.est))
		undernull = rmvnorm(1000, rep(0, length(results.for.test$R.s)), aa)
		dist = apply(undernull, 1, function(x) max(abs(x)))
		print(paste("critical value, parametric:", quantile(dist, 0.95)))
	    pval = c(pval,mean(dist >= t.statistic))
	    mad.r = apply(as.matrix(boot.R.s.est), 1, mad)
	    band.critical = c(band.critical, quantile(apply(abs(boot.R.s.est-results.for.test$R.s)/mad.r, 2, max), 0.95))
	    
	 }
   	if(type == "two step" | type == "both") {
  		R.d.var.two.step = apply(as.matrix(boot.R.d.est.two.step), 1, mad)^2
  		std.R = (results.for.test$R.s.two.step - mean(results.for.test$R.s.two.step))/sqrt(R.d.var.two.step)
		t.statistic = max(abs(std.R))
		print(paste("Statistic for two-stage omnibus:", t.statistic))
		
		#aa = covRob(t(boot.R.d.est.two.step), corr=TRUE,estim="mcd")$cov
		#aa.two.step = covRob(t(boot.R.d.est.two.step), corr=TRUE,estim="mcd")$cov
		aa.two.step = cor(t(boot.R.d.est.two.step))
		undernull = rmvnorm(1000, rep(0, length(results.for.test$R.s.two.step)), aa.two.step)
		dist.two.step = apply(undernull, 1, function(x) max(abs(x)))
		print(paste("critical value, two-stage omnibus:", quantile(dist.two.step, 0.95)))
	    pval = c(pval,mean(dist.two.step >= t.statistic))
	    mad.r = apply(as.matrix(boot.R.s.est.two.step), 1, mad)
	    band.critical = c(band.critical, quantile(apply(abs(boot.R.s.est.two.step-results.for.test$R.s.two.step)/mad.r, 2, max), 0.95))
	    }
		    }
	    
  	return(list("my.grid" = my.grid, "pval" = pval, "band.critical"=band.critical))
  	}
  
  

  

