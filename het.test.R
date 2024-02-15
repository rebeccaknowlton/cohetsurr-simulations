#' Title
#'
#' @param data.all
#' @param type
#' @param num.cov
#'
#' @return
#' @export
#'
#' @examples
het.test <-  function(data.all, num.cov) {
   model1.form <- "Y ~ . + A*S"
    for (i in 1:num.cov) {
      model1.form <- paste0(model1.form, " + A*W", i)
    }
    model1 <- lm(model1.form, data.all)
    model2 <- lm(Y ~ . + A*S, data.all)

    pval = anova(model1, model2)[2,6]
  
  return(pval)
}
