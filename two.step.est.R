#' two.step.est
#'
#' @param data.control
#' @param data.treat
#' @param W.grid.expand
#'
#' @return
#'
#' @examples
two.step.est <- function(data.control, data.treat, W.grid.expand.function,h.0,h.1,h.3) {

  names(W.grid.expand.function) <- names(data.control)[3:ncol(data.control)]

   working.model.control <- lm(Y ~ ., data = data.control)
  working.model.treat <- lm(Y ~ ., data = data.treat)

  
  data.control.pred = data.control
  data.control.pred$S = mean(c(data.control$S,data.treat$S))
  data.treat.pred = data.treat
  data.treat.pred$S = mean(c(data.control$S,data.treat$S))
  data.control$U.hat <- predict(working.model.treat, newdata= data.control.pred) - predict(working.model.control, newdata= data.control.pred) 
  data.treat$U.hat <- predict(working.model.treat, newdata= data.treat.pred) - predict(working.model.control, newdata= data.treat.pred)
  W.grid.expand.function$S = mean(c(data.control$S,data.treat$S))
 W.grid.expand.function$U.hat <- predict(working.model.treat, newdata= W.grid.expand.function) - predict(working.model.control, newdata= W.grid.expand.function)
 
W.grid.expand.function = W.grid.expand.function[,-which((names(W.grid.expand.function) == "S"))]

  # kernel smoothing
  kernel <- function(x, h) { return(dnorm(x / h)) }
    
   get.delta <- function(u) {
    m.1 <- sum(kernel(data.treat$U.hat - u, h.1) * data.treat$Y / sum(kernel(data.treat$U.hat - u, h.1)))
    m.0 <- sum(kernel(data.control$U.hat - u, h.0) * data.control$Y / sum(kernel(data.control$U.hat - u, h.0)))
    return(m.1 - m.0)
  }

  get.delta.s <- function(u) {
    mus <- rep(NA, nrow(data.control))
    kernels <- rep(NA, nrow(data.control))
    for (i in 1:nrow(data.control)) {
      mus[i] <- sum(kernel(data.treat$U.hat - u, h.1) * kernel(data.treat$S - data.control$S[i], h.3) * data.treat$Y / sum(kernel(data.treat$U.hat - u, h.1) * kernel(data.treat$S - data.control$S[i], h.3)))
      kernels[i] <- kernel(data.control$U.hat[i] - u, h.0)
    }
    if (sum(is.na(mus))!=0) {
      ind = which(is.na(mus))
      sub.nona = cbind(data.control$S, mus)[-ind,]
      for (i in ind) {
        mat <- cbind(abs(sub.nona[,1] - data.control$S[i]), sub.nona[,2])
        mmm <- which(mat[, 1] == min(mat[, 1]))[1]
        mus[i] <- sub.nona[mmm,2]
      }
    }
    m.10 <- sum(mus * kernels) / sum(kernels)
    m.0 <- sum(kernel(data.control$U.hat - u, h.0) * data.control$Y / sum(kernel(data.control$U.hat - u, h.0)))
    return(m.10-m.0)
  }

  my.grid <- W.grid.expand.function

  my.grid$delta.two.step <- unlist(lapply(my.grid$U.hat, get.delta))
  my.grid$delta.s.two.step <- unlist(lapply(my.grid$U.hat, get.delta.s))
  my.grid$R.s.two.step <- 1 - (my.grid$delta.s.two.step / my.grid$delta.two.step)

  return(my.grid)

}
