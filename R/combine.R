#' Combining Means and SD by groups
#'
#' @param m1,m2 Mittelwerte
#' @param sd1,sd2 Standartabweichung
#' @param n1,n2 Stichprobe
#' @returns vector(m, sd, n)
#'
#' @export
#' @name combining
#'
#' @examples
#'
#' combining(56, 50,2.3, 5.1,3	, 19)
#' combining(55.2, 50.4,2.2,	5.2,3	, 19)
#' combining(53.4,  49.3, 9.1,	8.7, 13,	26)
#' combining( 53.5, 49.4, 8.6,	8.4,13,	26)
#' set.seed(1)
#'
#' n <- 20
#' dat <- data.frame(x = c(rnorm(n, 50, 5), rnorm(n, 40, 8)),
#'                   group = gl(2, n, labels = c("Control", "Treat")))[-c(1:5), ]
#'
#' my_m <- function(x)
#'   round(c(m = mean(x), sd = sd(x), n = length(x)), 1)
#' aggregate(x ~ group, dat, FUN = my_m)
#'
#' my_m(dat$x)
#'
#'
#' round(combining(51.1, 40.0, 4.7, 6.1, 15, 20), 1)
combining <- function(...) {
  UseMethod("combining")
}


#' @rdname combining
#' @export
combining.default <- function(m1,
                      m2,
                      sd1 = 1,
                      sd2 = 1,
                      n1 = 5,
                      n2 = 5) {
  c(
    m = combining_mean(m1, m2, n1, n2),
    sd = combining_sd(m1, m2, sd1, sd2, n1, n2),
    n = n1 + n2
  )

}

# combining.data.frame<- function(x, ...){
#
# }



#' @rdname combining
#' @export
combining_mean <- function(x1, x2, n1 = 5, n2 = 5) {
  (n1 * x1 + n2 * x2) / (n1 + n2)
}


#' @rdname combining
#' @export
combining_sd <- function(m1,
                         m2,
                         sd1,
                         sd2,
                         n1 = 5,
                         n2 = 5) {
  n <- n1 + n2 - 1
  nn <- n1 * n2 / (n1 + n2)
  s1 <- sd1 ^ 2
  s2 <- sd2 ^ 2

  x <- (n1 - 1) * s1 + (n2 - 1) * s2 + nn * (m1 ^ 2 - 2 * m1 * m2 + m2 ^2)
 sqrt( x / n)
}


#' @param x data.frame (2 Rows)
#'
#' @param n Names  of Number
#' @param m Names  of Estimated mean
#' @param sd Names  of Standard deviation
#' @param ...
#'
#' @rdname combining
#' @export
#' @examples
#'  dat <-
#' data.frame(
#'   #author= c("A", "B"),
#'   cn = c(134, 38),
#'   cm = c(5.96, 5.34),
#'   csd = c(4.24, 4),
#'
#'   tn = c(113, 35),
#'   tm = c(6.82, 6.12),
#'   tsd = c(4.72, 4.32)
#'
#' )
#'
#'
#' combining(dat)
#'
combining.data.frame <- function(x,
                                 n =  c("n", "cn",  "tn",  "cn",   "tn",   "cn",   "tn"),
                                 m =  c("m", "cm",  "tm",  "tm1",  "tm2",  "cm1",  "cm2"),
                                 sd = c("sd","csd", "tsd", "tsd1", "tsd2", "csd1", "csd2"),
                                 ...) {
  rslt <- NULL
  for (i in seq_len(length(n))) {
    xn <- x[[n[i]]]
    xm <- x[[m[i]]]
    xsd <- x[[sd[i]]]

    if (!is.null(xn) & !is.null(xm) & !is.null(xsd)) {
      rst <-  combining(
        m1 = xm[1],
        m2 = xm[2],
        sd1 = xsd[1],
        sd2 = xsd[2],
        n1 = xn[1],
        n2 = xn[2]
      )
      rst <- signif(rst, 5)
      names(rst) <- c(m[i], sd[i], n[i])
      rslt <- c(rslt, rst[setdiff(names(rst), names(rslt))])
    }
  }

  dplyr::bind_rows(x, rslt)
}
