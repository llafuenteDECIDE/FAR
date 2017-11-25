#' fars_read function
#' read a csv file given a route to the file.

#'  @param filename an string indicating the complete route or relative route (from the working directory) to the file. The file can be compressed.

#'  @return  a table_df that contents the data of csv. If the file doesn?t exist returns an error.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df

#'  @examples
#'   fars_read (accident_2013.csv.bz2) supposing accident_2013.csv.bz2 in the working directory
#'   fars_read (/data/accident_2014.csv.bz2) supposing accident_2014.csv.bz2 in a data folder inside the working directory

#'  @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'  make_filename function
#'  given a year, makes the complete name of the "accident" filename. It has the form "accident_year.csv.bz2"
#'  where year is substituted by /code{"year"}

#'  @param year an integer or string indicating the year to which we want to obtain the filename

#'  @return  an string that represents the complete name of the filename.

#'  @examples
#'  make_filename(2013)   returns accident_2013.csv.bz2
#'  make_filename(2014)   returns accident_2014.csv.bz2


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years fuction
#' given a list of years,  extracts for each  year the MONTH column
#'  and a year column of the accident data of that year.

#'@param years a vector or list of integers representing the years

#'@return a list of dataframes of the same length of /code {"year"}.
#'Each element of the list refers to a year of the list and si a dataframe that
#' includes the MONTH column of the accident data of the year and a column with the year.
#' If there is no data of the year the element of the list will be null.


#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"

#' @examples
#' fars_read_years(c(2013, 2014))

#'  @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years function
#' given a list of years gives the number of accidents by month of each of the years

#' @param years a vector or list of ints or strings  representing years

#' @return a table which variables are the years on list and an unique
#' file that indicates the number of accidents by month of this year

#' @examples fars_summarize_years(c(2013, 2014, 2015))

#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr n


#'  @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state function

#' given an state and a year plots the coordenates of the accidents in
#' this state an year

#' @param state.num the number of the state for which we want
#' to draw the accident's coordinates.
#' @param year an integer indicating the year for which we want to draw the accident?s coordinates

#' @return a plot with all accident's coordinates drawed of the /code{state.num}
#' and /code{year} or null if the /code{state.num} does not exist

#' @importFrom dplyr filter
#' @importFrom maps  map
#' @importFrom graphics points

#'@examples fars_map_state(1,2013)

#'  @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
