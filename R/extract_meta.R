#' Extract Meta
#'
#' @description  meta_text.meta: Ausgabe von print.meta als tabelle
#' @export
#'
#' @examples
#' # library(meta)
#' data(Olkin1995)
#' head(Olkin1995)
#' m1 <- metabin(ev.exp, n.exp, ev.cont, n.cont,
#'               data = Olkin1995, subset = c(41, 47, 51, 59),
#'               studlab = paste(author, year),
#'               method = "Inverse")
#' print(m1, digits = 1, digits.Q = 1)
#' Tbll(m1)
meta_text  <-
  function(x,
           digits = 2,

           ...) {

    # Workaround da die orginale print.meta Funktion keinen export erlaubt
    rslt <-
      capture.output(
        meta:::print.meta(
          x,
          digits = digits,
          digits.pval = 3,
          digits.Q = 1 ,
          digits.pval.Q = 3,
          digits.I2 = 0,
          digits.tau2 = 2,
          digits.H = 2,
            digits.tau=2,
          ...
        )
      )
 #cat("\nwas kommt: ")
   # model <- grepl("Common effect", rslt)
    model.pos <- which(grepl("Random effects", rslt))


    ans <- paste(rslt[1],   rslt[3],  sep="\n")

         model <- strsplit(rslt[model.pos] , " +")[[1]][-c(1:4)]
        header <-  unlist(strsplit(rslt[model.pos-1], " +"))[-1]
        model<- c(  model[1],  paste0(model[2],model[3]), model[4], model[5])

        q <-  grepl("Test of heterogeneity", rslt)

          q <-  2  + which(q)
          q <-  rslt[q]
          q <-  unlist(strsplit(q, " +"))
          q <- paste0("Q(", q[3], ") = ", q[2], ", p = ", q[4])



ans <- paste(ans,  "Random effects model:",
            paste(header,  "=", model, collapse= ", "),
            "\n",
            rslt[8],
            rslt[9],
           "\n",
            rslt[11], q ,
            "\n",


            rslt[15],
            rslt[16],
            rslt[17],
            rslt[18],

             sep="\n"

             )



 return(ans)




  }



#meta_text(m.gen) |> Text()
