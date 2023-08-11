

#' Utbua regression jofnu
#'
#' @param p fjoldi parametra X
#' @param upper x eda X, defult er upper = FALSE
#'
#' @return
#' @export
#'
#' @examples
#' marglida(2, upper = T)
#' marglida()
marglida <- function(p=0 , upper = F){

  y = "y"
  x = "x"

  if(upper){
   x <-  toupper(x)
   y <- toupper(y)
  }


  poly_n <- paste0("\\beta_1",x,"_{i1} + ", "\\beta_1",x,"_{i2} +...+","\\beta_1",x,"_{ip} + ")

  if(p > 0){
  poly_n <- sapply(0:(p-1) , function(p) paste0("\\beta_",p+1, x,"_{i", p+1,"} + "))
  }

  cat("$",y,"_i = \\beta_0 +", poly_n ,"\\epsilon_i$", sep = "")


}



