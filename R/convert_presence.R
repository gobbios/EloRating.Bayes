#' convert date-based presence to interaction-based presence
#'
#'
#' @param presence a data.frame with presence/absence data, which
#'                 needs to include a date column (YYYY-MM-DD) as
#'                 indicated by \code{date_col=}
#' @param interaction_dates vector of dates or character (YYYY-MM-DD) on
#'                          which interactions were observed
#' @param date_col character, column name for the date column in
#'                 \code{presence}. Default is \code{"Date"}.
#'
#' @return a data frame with presence data
#' @export
#'
#' @examples
#' data("advpres")
#' presence <- advpres
#' nrow(presence) # covering 33 days
#' # 15 interactions:
#' idates <- sort(sample(presence$Date[1:20], 15, replace = TRUE))
#' # convert
#' presence_new <- convert_presence(presence, idates)
#' nrow(presence_new) # 15 rows
#' presence_new

convert_presence <- function(presence, interaction_dates, date_col = "Date") {
  if (!date_col %in% colnames(presence)) {
    stop("didn't find date column in presence data", call. = FALSE)
  }

  sel <- sapply(interaction_dates, function(x)(which(presence[, date_col] == x)))

  out <- presence[sel, ]
  rownames(out) <- NULL
  out
}
