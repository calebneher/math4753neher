# myboot2: bootstrap confidence interval function
# iter  = number of bootstrap samples
# x     = data vector
# fun   = statistic to compute (mean, var, median, etc.)
# alpha = significance level (0.05 → 95% CI)
# cx    = text size for CI labels
# ...   = extra arguments passed to hist()
#' @export
myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, ...) {

  n <- length(x)

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)

  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Bootstrap Statistics\nalpha=", alpha,
                            " iter=", iter, sep=""),
               ...)

  mat <- matrix(x, nr = n, nc = 1)
  pte <- apply(mat, 2, fun)

  abline(v = pte, lwd = 3, col = "black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep=""), col="red", cex=cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")"), col="red", cex=cx)
  text(pte, max(para$density)/2, round(pte, 2), cex=cx)

  invisible(list(ci = ci, fun = fun, x = x))
}
