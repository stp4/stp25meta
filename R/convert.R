#' Converting Among Effect Sizes
#'
#' Quelle:
#'  Sutton seite 29 formel 2.16
#'  calculating missing SDs
#' Morris, Scott B., und Richard P. DeShon. „Combining Effect Size Estimates in Meta-Analysis with Repeated Measures and Independent-Groups Designs.“ Psychological Methods, Bd. 7, Nr. 1, 2002, S. 105–25.
#' DOI.org (Crossref), https://doi.org/10.1037/1082-989X.7.1.105.
#'
#' https://training.cochrane.org/handbook/current/chapter-06
#'
#' @export
#'
convert <- function(...) {
  UseMethod("convert")
}


#' Helper
#' @noRd
tretment_effect <- function(tm, cm, md) {
  if (is.na(md))
    tm - cm
  else
    md
}

#' Helper
#' @noRd
var_treatment <- function(cn, tn, s) {
  s ^ 2 * ((1 / tn) + (1 / cn))
}
#' Sutton seite29 formel 2.16
#' Helper
#' @noRd
pooled_sd <- function(cn, tn, csd, tsd, sd) {
  if (is.na(sd))
    sqrt(((tn - 1) * tsd ^ 2 + (cn - 1) * csd ^ 2) / (tn + cn - 2))
  else
    sd
}

#' Helper
#' @noRd
sd_change <- function(sd1, sd2,  r = NA) {
  if (is.na(r))
    sd1
  else
    sqrt(sd1 ^ 2 + sd2 ^ 2 + (2 * r * sd1 * sd2))
}

#' Helper
#' @noRd
sd_se <- function(sd, se,  n) {
  if(!is.na(sd)) sd
  else if( !is.na(se)) se * sqrt(n)
  else NA
}


#' @param cn Number of observations in control group.
#' @param cm Estimated mean in control group.
#' @param csd,cse Standard deviation in control group.
#'
#' @param tn Number of observations in experimental group.
#' @param tm Estimated mean in experimental group.
#' @param tsdtse, Standard deviation in experimental group.
#'
#' @param cm1,csd1,cse1,cm2,csd2,cse2 control group
#' @param tm1,tsd1,tse1,tm2tsd2,tse2,, experimental group.
#' @param md,sd,se Effecte
#' @param r,d,g,f standardisierte Effecte
#' @param digits digits
#' @param ... not used
#' @rdname convert
#' @description
#'
#' change from baseline (also called a change score)
#' mean difference (or difference in means)
#' @export
#'
#' @return Vector
#'
convert.default <- function(cn = 5,
                            cm = NA,
                            csd = NA,
                            cse =NA,

                            cm1 = NA,
                            csd1 = NA,
                            cse1 = NA,
                            cm2 = NA,
                            csd2 = NA,
                            cse2 = NA,

                            tn = 5,
                            tm = NA,
                            tsd = NA,
                            tse = NA,

                            tm1 = NA,
                            tsd1 = NA,
                            tse1 = NA,
                            tm2 = NA,
                            tsd2 = NA,
                            tse2 = NA,



                            r = NA,
                            md = NA,
                            sd = NA,
                            se = NA,

                            d  = NA,
                            g = NA,
                            f = NA,

                            digits = 2,
                            ...) {
rslt <- NA
type <- NA
n <- tn + cn

# Convert SE to SD
csd <- sd_se(csd, cse, n)
csd <- sd_se(csd, cse, n)
csd1 <- sd_se(csd1, cse1, n)
csd2 <- sd_se(csd2, cse2, n)
tsd <- sd_se(tsd, tse, n)
tsd1 <- sd_se(tsd1, tse1, n)
tsd2 <- sd_se(tsd2, tse2, n)
sd <- sd_se(sd, se, n)

if(!is.na(md)) type <- 1


if (!is.na(cm) & !is.na(tm) &
    !is.na(csd) & !is.na(tsd)) {
  s <- pooled_sd(cn, tn, csd, tsd, sd)
  if (is.na(type))  type <- 2
  rslt <-
    c(
      cn = cn,
      cm = cm,
      csd = csd,
      tn = tn,
      tm = tm,
      tsd = tsd,
      md = tretment_effect(tm, cm, md),
      sd = s,
    #  var = var_treatment(cn, tn, s),
      se =se,
      type = type
    )
}
else if (!is.na(cm1) & !is.na(csd1) &
      !is.na(tm1) & !is.na(tsd1) &
      !is.na(cm2) & !is.na(csd2) &
      !is.na(tm2) & !is.na(tsd2)) {


    cm <- cm2 - cm1
    tm <- tm2 - tm1
    if (is.na(csd) & is.na(tsd)) {
       if( is.na(type)) type<- 4
      csd = sd_change(csd1, csd2, r)
      tsd = sd_change(tsd1, tsd2, r)
    }
    if( is.na(type)) type<- 3
    s <- pooled_sd(cn, tn, csd, tsd, sd)
    rslt <-
      c(
        cn = cn,
        cm = cm,
        csd = csd,
        tn = tn,
        tm = tm,
        tsd = tsd,
        md = tretment_effect(tm, cm, md),
        sd = s,
      #  var = var_treatment(cn, tn, s),
        se =se,
        type = type
      )

  }
  round(rslt, digits)

}



#' @rdname convert
#'
#' @export
#'
#' @return data.frame
#'
convert.data.frame <- function(x, digits = 3, ...) {
  if (is.null(x$author))
    x$author <-
      paste("study", seq_len(nrow(x)))
  if (is.null(x$year))
    x$year <- NA

  param <- sapply(x, is.numeric)
  rslt <-
    t(apply(x[param], 1,
            function(args) {
      args["digits"] = digits
      do.call("convert", as.list(args))

    }))

 rslt <-  cbind(x[c("author", "year")], as.data.frame(rslt))
 rslt$type <- factor(rslt$type, 1:5, c("Exact (MD + SD)",
                                      "Exact (Differenzen)",
                                      "Aproximation von MD",
                                      "Aproximation von MD + SD",
                                      "Warnung eventuel Falsche Werte"
                                      ))
rslt
}
