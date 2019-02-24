# Before roxygenise() save data frame with:
# save(df, file = "Jahn_CellReports_2018.RData")
#
#' Data from the publication of Jahn et al., CellReports, 2018
#' 
#' The dataset contains protein abundances of the Synechocystis sp. 
#' PCC6803 proteome. Protein abundance was determined using shotgun 
#' mass spectrometry. The data set also contains pathway information according to
#' the cyanobase hierarchical annotation:
#' 
#' \itemize{
#'   \item protein - protein ID
#'   \item condition - combination from light and CO2
#'   \item light - light intensity in umol photons / m2 * s
#'   \item co2_concentration - CO2 concentration in % vol
#'   \item mean_intensity - mean MS1 ion intensity
#'   \item mean_mass_fraction_norm - normalized mean mass fraction of protein
#'   \item sd_intensity - standard deviation from mean
#'   \item Process - functional annotation 1st level
#'   \item Pathway - functional annotation 2nd level
#'   \item Protein - functional annotation 3rd level
#'   \item Process.abbr - abbreviated Process
#'   \item Pathway.abbr - abbreviated Pathway
#' }
#' @docType data
#' @usage data(Jahn_CellReports_2018)
#' @format A data frame with 19790 rows and 12 variables
#' @source \url{https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31485-2}
"Jahn_CellReports_2018"
