#' Converting Among Effect Sizes
#'
#'
#' Confidence interval based on computation
#' https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d
#' @name from_ci
#' @param ci_low lower limit
#' @param ci_hig  upper limit
#' @param n sample size
#' @param ci confidence interva
#'
#' @return value
#'
#' @export
#'
sd_from_ci <-  function(ci_low,
                        ci_hig,
                        n =  n1 + n2,
                        n1 = 5,
                        n2 = 5,
                        ci = .95,
                        verbose = TRUE) {
  se <- se_from_ci(ci_low, ci_hig, n, ci, verbose = TRUE)
  sd <- sd_from_se(se, n, verbose = TRUE)

  if (verbose)
    print(round(c(
      ci_low = ci_low,
      ci_hig = ci_hig,
      n = n ,
      se = se,
      sd = sd
    ), 2))

  invisible(sd)
}

#' @rdname from_ci
#'
#' @export
sd_from_se <- function(se,
                       n =  n1 + n2,
                       n1 = 5,
                       n2 = 5,
                       verbose = FALSE) {
  sd = se * sqrt(n)
  if (verbose)
    print(round(c(
      se = se,  n = n, sd = sd
    ), 2))

  invisible(sd)
}
#' @rdname from_ci
#'
#' @export
se_from_sd <- function(sd,
                       n =  n1 + n2,
                       n1 = 5,
                       n2 = 5,
                       verbose = TRUE){

  se = sd / sqrt(n)

  if (verbose)
    print(round(c(
    sd = sd ,  n = n,  se = se
    ), 2))

  invisible(se)
}


#' @rdname from_ci
#'
#' @export
se_from_ci <-
  function(ci_low,
           ci_hig,
           n =  n1 + n2,
           n1 = 5,
           n2 = 5,
           ci = .95,
           verbose = TRUE) {
    ci_hig_quantil <-   (1 + ci) / 2 # quantiles of t
    me <- abs((ci_hig - ci_low) / 2) # xbar <-  ci_hig - me
    q_t <- stats::qt(p = ci_hig_quantil, df = n - 1)
    se <-  me / q_t

    if (verbose)
      print(round(c(
        ci_low = ci_low,
        ci_hig = ci_hig,
        n = n ,
        me = me,
        se = se
      ), 2))

    invisible(se)

  }


#' @rdname from_ci
#'
#' @export
se_from_d <- function(d, n1, n2) {
  n  = n1 * n2 / (n1 + n2)
  nu = 2 * (n1 + n2 - 2)

  se <-  sqrt(n + (d ^ 2) / nu)
  print(round(c(
    se = se,
    d = d,
    lw = d - se,
    up = d + se
  ), 2))


  se
}
#' @rdname from_ci
#'
#' @export
#'
ci_from_d <- function(d_obs,
                      n1,
                      n2) {
  ### input: observed d and sample sizes n1 n2


  ### computing scale factor n and degrees of freedom
  n  = n1 * n2 / (n1 + n2)
  nu = 2 * (n1 + n2 - 2)

  ### a suitable grid 'ds' for a grid search
  ### based on
  var_est <- n ^ -1 + d_obs ^ 2 / 2 / nu
  ds <-
    seq(d_obs - 4 * var_est ^ 0.5, d_obs + 4 * var_est ^ 0.5, var_est ^ 0.5 /
          10 ^ 4)


  ### boundaries based on limits of t-distributions with ncp parameter
  ### for which the observed d will be in the 2.5% left or right tail
  upper <-
    min(ds[which(pt(d_obs * sqrt(n), nu, ds * sqrt(n)) < 0.025)]) * sqrt(n)    # t-distribution boundary
  # scaled boundary
  lower <-
    max(ds[which(pt(d_obs * sqrt(n), nu, ds * sqrt(n)) > 0.975)]) * sqrt(n)
  lower / sqrt(n)
  c(low = lower / sqrt(n), hig =  upper / sqrt(n))
}





# Asymptotic estimates of standard errors (se)
#
# Nakagawa, S., & Cuthill, I. C. (2007). Effect size, confidence interval and
# statistical significance: A practical guide for biologists.
# Biological Reviews, 82(4), 591–605.
#https://doi.org/10.1111/j.1469-185X.2007.00027.x
#
# @param g Hedges’ g
# @param n1,n2 sample size
#
# @return  standard errors
#
# @examples
# se_from_g <- function(g, n1, n2) {
#   sqrt((n1 + n2) / (n1 * n2) + g ^ 2 / (2 * (n1 + n2 + 2)))
#
# }
# se_from_ci <- function(lower, upper, ci = .95) {
#   dif <- abs(upper - lower)
#   if (ci == .95)
#     dif / 3.92
#   else if (ci == .90)
#     dif / 3.29
#   else if (ci == .99)
#     dif / 5.15
#   else
#     stop()
# }
#
#
# sd_from_se <- function(se, n1, n2) {
#   se / sqrt(n1 ^ -1 + n2 ^ -1)
# }
#
# d_from_md <- function(md, sd, n1, n2) {
#   var_x1 <- (se * sqrt(n1)) ^ 2
#   var_x2 <- (se * sqrt(n2)) ^ 2
#   s <-   sqrt((var_x1 + var_x2) / 2)
#   md / s
# }
