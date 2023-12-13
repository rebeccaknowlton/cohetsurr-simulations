#' two.step.est
#'
#' @param data.control
#' @param data.treat
#' @param W.grid.expand
#'
#' @return
#'
#' @examples
two.step.est <- function(data.control, data.treat, W.grid.expand) {
  control.nosurr <- subset(data.control, select = -c(S))
  treat.nosurr <- subset(data.treat, select = -c(S))
  names(W.grid.expand) <- names(data.control)[3:ncol(data.control)]

  working.model.control <- lm(Y ~ ., data = control.nosurr)
  working.model.treat <- lm(Y ~ ., data = treat.nosurr)

  data.control$U.hat <- predict(working.model.treat, data.control) - predict(working.model.control, data.control)
  data.treat$U.hat <- predict(working.model.treat, data.treat) - predict(working.model.control, data.treat)
  W.grid.expand$U.hat <- predict(working.model.treat, W.grid.expand) - predict(working.model.control, W.grid.expand)

  # kernel smoothing
  kernel <- function(x, h) { return(dnorm(x / h) / h) }

  h.0 <- bw.nrd(data.control$U.hat)*length(data.control$U.hat)^(-0.2)
  h.1 <- bw.nrd(data.treat$U.hat)*length(data.treat$U.hat)^(-0.2)
  h.2 <- bw.nrd(data.control$S) * length(data.control$S)^(-0.2)
  h.3 <- bw.nrd(data.treat$S) * length(data.treat$S)^(-0.2)

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
      for (i in ind) {
        mat <- cbind(abs(data.control$S - data.control$S[i]), mus)
        mmm <- which(mat[, 1] == min(mat[-i, 1]))[1]
        mus[i] <- mus[mmm]
      }
    }
    m.10 <- sum(mus * kernels) / sum(kernels)
    m.0 <- sum(kernel(data.control$U.hat - u, h.0) * data.control$Y / sum(kernel(data.control$U.hat - u, h.0)))
    return(m.10-m.0)
  }

  my.grid <- W.grid.expand

  my.grid$delta <- unlist(lapply(my.grid$U.hat, get.delta))
  my.grid$delta.s <- unlist(lapply(my.grid$U.hat, get.delta.s))
  my.grid$R.s <- 1 - (my.grid$delta.s / my.grid$delta)

  return(my.grid)

}
