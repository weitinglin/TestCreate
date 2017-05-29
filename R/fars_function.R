# Basic function
# Function for visualization of the US National Highway Traffic Safety Administrations dataset ---------------------------------------------
# Authors:

#' Load the csv data into R as tbl_df
#'
#' @description  This function will use the readr::read_csv to load the .csv file into R
#' and turn the data into tbl_df data structure
#'
#' @param filename str, the input file's name
#' @return this function will return a tbl_df data , if the filename not exist in current directory, show error message
#' @examples
#' \dontrun{
#' fars_read(filename="./data/accident_2003.csv.bz2")
#' }
#' @importFrom  readr read_csv
#' @importFrom  dplyr tbl_df
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}


#' Generate the file name with the given the request year
#'
#' @description  This function will generate the file name according to the given years and will be used by another function
#'
#'
#' @param year a integer or string, represented the given year
#' @return this function will return the file name according to the input year
#' @examples
#' \dontrun{make_filename(year=2012)
#' }
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}



#' Get all accidents happend according to months within certain year
#'
#'@description This function will first generate the files name according to the input year, then loadding the generated filename,
#'select the month and years column to output
#'
#' @param years the years want to query the accident , and can be a vector
#' @return a list with data.frame elements, this function will return the tbl_df  with month and year column within certain years
#' if the input years didnt contain in the dataset, show error message.
#' @examples
#' \dontun{
#' fars_read_years(years=2013)
#' }
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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



#' Summarize the accidence number happended according to months within certain year
#'
#' @description  This function will first load the data accoding tot years input
#'
#' @param years a vectors contained integer indicated the years we want to query the accidennt happend
#' @return a data.frame, according to the years with the information of the accidents
#' @examples
#' \dontrun{
#' fars_summarize_years(years = c(2013,2014))
#' }
#' @importFrom dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}


#' Visualization fo the accident happening in certain state during a specific year
#'
#'  @description  This function will visualize the traffic accidents happend during certain year  within certain state into the geographic view
#'  @param state.num a integer, the  code number of state
#'  @param year a integer, indicated the year's  of the traffic accidents
#'  @return a graphical object, this function will return a plot with the accident happening at certain state with certain year,
#'  if the STATE number is not exist, show error message, if the choosen state have no accident happended, show errors
#'  @examples
#'  \dontrun{
#'  fars_map_state(state.num = 1, year = 2014)
#'  }
#'  @importFrom  dplyr filter
#'  @importFrom maps map
#'  @importFrom graphics points
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


#' Create the Phenodata for affy input
#'
#' @description  This function will create the phenodata which used in loading cel files
#'
#' @param experiment.set vector, store the experiment set in vector
#' @return this function will return a tbl_df data , if the filename not exist in current directory, show error message
#' @examples
#' \dontrun{
#' create_phenodata(experiment.set=c("control_1","control_2","case_1","case_2"))
#' }
#' @export

create_phenodata <- function(experiment.set){
    set <- experiment.set
    phenodata.set   <- matrix ( rep ( set, 2) , ncol = 2 )
    phenodata.set   <- as.data.frame ( phenodata.set, stringsAsFactors=FALSE)
    colnames ( phenodata.set )   <- c ( "Name" , "FileName" )
    phenodata.set$experiment.set <- experiment.set
    return(phenodata.set)
}
