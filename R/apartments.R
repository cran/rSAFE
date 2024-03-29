#' Apartments data
#'
#' Datasets \code{apartments} and \code{apartmentsTest} are artificial,
#' generated from the same model.
#' Structure of the dataset is copied from real dataset from \code{PBImisc} package,
#' but they were generated in a way to mimic effect of Anscombe quartet for complex black box models.
#'
#' \itemize{
#' \item m2.price - price per square meter
#' \item surface - apartment area in square meters
#' \item no.rooms - number of rooms (correlated with surface)
#' \item district - district in which apartment is located, factor with 10 levels
#' (Bemowo, Bielany, Mokotow, Ochota, Praga, Srodmiescie, Ursus, Ursynow, Wola, Zoliborz)
#' \item floor - floor
#' \item construction.year - construction year
#' }
#'
#' @aliases apartmentsTest
#' @docType data
#' @keywords apartments
#' @name apartments
#' @usage data(apartments)
#' @format a data frame with 1000 rows and 6 columns
NULL
