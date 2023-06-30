
#' Listar kennsludaga
#'
#' @param upphafs_dagur Dagsettning sem kennsla hefs
#' @param vikudagar_nr Vikudagar sem kennt er t.d. ef kennt er a man og mid
#' vikudagar_nr = c(1, 3)
#' @param fj_vikur Fjoldi vikna i namskeidi
#'
#' @return Lista med kennslu dogum
#' @export
#'
#' @examples
#' finna_kennslutimabil(upphafs_dagur = "2023-12-01", vikudagar_nr = 1, fj_vikur = 3)

finna_kennslutimabil <- function(upphafs_dagur,
                                 vikudagar_nr = c(1, 3),
                                 fj_vikur = 12) {
  upphafs_dagur <- lubridate::as_date(upphafs_dagur)
  vikudagar <- function(upphafs_dagur, vikudagar_nr) {
    purrr::map(vikudagar_nr, \(nr) upphafs_dagur + lubridate::days(nr) - 1) |>
      purrr::list_c()
  }

  vikur <- 0:(fj_vikur - 1)
  purrr::map(vikur,
      \(nr) vikudagar(upphafs_dagur, vikudagar_nr) + lubridate::weeks(nr)) |>
    purrr::list_c()
}


#' Birtir vikudag og manud
#'
#' @param dagar dagsettning "2023-12-01"
#'
#' @return Birtir vikudag og manud
#' @export
#'
#' @examples
#' dagar <- c("2023-12-01", "2023-12-08", "2023-12-15")
#' formata_daga(dagar)
formata_daga <- function(dagar){
  paste0(lubridate::wday(dagar,label = T)|>
         stringr::str_remove_all("[:punct:]"),", ",
         lubridate::day(dagar), ". ",
         lubridate::month(dagar, label = T),"."
  )

}


