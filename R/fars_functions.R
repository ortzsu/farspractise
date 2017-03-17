#' Reading data from the Fatality Analysis Reporting System

#' The fars_read function reads data file into a dplyr data table.
#' If the data file does not exist in the working directory, this function throws an error.
#'
#' @param filename A character string giving a filename.
#'
#' @return The function reads the given file and creates a dplyr data table from it.
#' If file with the given name does not exist, the function returns an error message.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generating filenames
#'
#' The "make_filename" function produces "accident_year.csv.bz2"-type filename,
#' where "year" is given by the user.
#'
#' @param year an integer
#'
#' @return The function generates a filename with the year number given by the user.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Generating a list of tables
#'
#' The "fars_read_years" function creates a list of dplyr data tables
#' where each data table represents a year provided by the user,
#' every data tables contain two columns (month, year)
#' and the number of rows is defined by the number of observations.
#'
#' @param years A list of integers representing years.
#'
#' @return A list with the same number of data tables as the number of years provided by the user
#' if the data of those years are available. For the years not represented,
#' the function sends warning message(s).
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2013:2015)
#'
#' @importFrom dplyr mutate select
#'
#' @export
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

#' Generating tables showing observations by months and years
#'
#' The "fars_summarize_years" function summarizes the number of observations by months for the years provided by the user.
#' The results are presented in a tidyr data table.
#'
#' @param years A list of integers representing years.
#'
#' @return The function creates a tidyr data table, in which columns represent years and
#' rows show the months of observations.
#' For the years not represented, the function sends warning message(s).
#'
#' @examples
#' fars_summarize_years(2013:2015)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plotting the observations
#'
#' The "fars_map_state" function plots the observations of fatal accidents on the map of the
#' provided state in the year defined by the user.
#'
#' @param state.num An integer representing a state in the USA.
#' @param year An integer meaning the year of observation.
#'
#' @return The function plots the map of the american state chosen by the user.
#' Black dots represent the place of observed fatal accidents.
#' If user define an invalid state number or year for which data are not available,
#' the function returns an error. If in a given state/ year pair there are no observed accidents,
#' the function returns the message "no  accidents to plot".
#'
#' @examples
#' fars_map_state(39,2013)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export

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
