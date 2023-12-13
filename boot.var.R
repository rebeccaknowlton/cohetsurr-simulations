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
boot.var <- function(data.control, data.treat, W.grid.expand, type) {
  num.boot <- 200
  boot.delta.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.delta.s.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))
  boot.R.s.est <- data.frame(matrix(nrow = nrow(W.grid.expand), ncol = num.boot))

  for (j in 1:num.boot) {
      boot.data.control <- data.control[sample(1:nrow(data.control), nrow(data.control), replace = TRUE),]
      boot.data.treat <- data.treat[sample(1:nrow(data.treat), nrow(data.treat), replace = TRUE),]
      if (type == "model") {
        boot.estimate <- parametric.est(boot.data.control, boot.data.treat, W.grid.expand)
      } else if (type == "two step") {
        boot.estimate <- two.step.est(boot.data.control, boot.data.treat, W.grid.expand)
      }
      boot.delta.est[,j] <- boot.estimate$delta
      boot.delta.s.est[,j] <- boot.estimate$delta.s
      boot.R.s.est[,j] <- boot.estimate$R.s
  }
  my.grid <- cbind(matrixStats::rowVars(as.matrix(boot.delta.est)),
                   matrixStats::rowVars(as.matrix(boot.delta.s.est)),
                   matrixStats::rowVars(as.matrix(boot.R.s.est)))
  colnames(my.grid) <- c("delta.var", "delta.s.var", "R.s.var")
  return(my.grid)
}
