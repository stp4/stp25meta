#' ecs
#'
#' @name ecs
#' @param ...
#'
#' @return string
#' @export
#'
#' @examples
#'
#'
#' require(esc)
#' tmp <- data.frame(
#'   author = c(
#'     "Smith 2000",
#'     "Smith 2010 2",
#'     "Smith 2012",
#'     "Muller 2000",
#'     "Meir 2010 2",
#'     "Bold 2012"
#'   ),
#'   n = c(250, 200, 210, NA, NA, NA),
#'   treat = c(NA, NA, NA, 50, 60, 50),
#'   cntrl = c(NA, NA, NA, 45, 70, 40),
#'
#'   coefficient = c(NA, NA, NA, 0.4, 0.2, 0.6),
#'   se = c(NA, NA, NA, .15, .1, .2),
#'
#'   tvalue = c(3.3, 2.9, 2.3, NA, NA, NA)
#' )
#'
#' tmp
#'
#' effect_sizes2 <-
#'   function(data,
#'            ...,
#'            fun,
#'            es.type = c("d", "g", "or", "logit",
#'                        "r", "f", "eta", "cox.or", "cox.log")) {
#'
#'     data$my_order<- seq_along(data)
#'     es.type <- match.arg(es.type)
#'    # if (substr(fun, 1, 4) != "esc_")
#'    #   fun <- paste0("esc_", fun)
#'     params <- match.call(expand.dots = FALSE)$...
#'
#'
#'
#'    split_data <- split_data_in_methodes(data, fun)
#'   rslt_eff <-   calc_effect(data, es.type)
#'
#'   combime_rslt(rslt_eff )
#'
#'
#'   }
#'
#' effect_sizes(
#'   tmp,
#'   t = tvalue,
#'   totaln = n,
#'   study = author,
#'   fun = "esc_t"
#' )
#'
ecs <- function(...){

  "Hallo Welt!"
}



#' @rdname ecs
#' @description
#' A short description...
#'
#' Effect Size Computation for Meta Analysis
#' https://strengejacke.github.io/esc
esc_B_CI <- function(b,
                     ci_low = NULL,
                     ci_hig = NULL,
                     n1 = 5,
                     n2 = 5,
                     n = n1 + n2,
                     se = NULL,
                     sd = NULL,
                     md = b,
                     es.type = "g",
                     verbose = FALSE) {
  if (!is.null(se)) {
    sd <- sd_from_se(se, n = n, verbose = verbose)
  } else if (!is.null(ci_low)) {
    sd <-  sd_from_ci(ci_low, ci_hig, n = n, verbose = verbose)
  }
  else{
    rslt <- NULL
    cat("\n weis nicht was ich machen soll??\n\n")
    sd <- b
  }
  if (!is.null(sd)) {
    rslt <- esc::esc_B(
      b = b,
      sdy = sd,
      grp1n = n1,
      grp2n = n2,
      es.type = es.type
    )
    data.frame(TE = rslt[["es"]], seTE = rslt[["se"]])

  } else
    data.frame(TE = NA, seTE = NA)


}

#' @rdname ecs
esc_d <-
  function(d,
           n1,
           n2,
           se = NULL,
           ci_low = NULL,
           ci_up = NULL,
           n = n1 + n2) {
    if (is.null(se)) {
      if (is.null(ci_low)) {
        se <- se_from_d(d, n1, n2)
      }
      else{
        se <- se_from_ci(ci_low, ci_up, n = n)
      }
    }
    data.frame(TE = esc::hedges_g(d = d, totaln = n), seTE = se)
  }

#esc_d(measures$d, measures$n1, measures$n2)


