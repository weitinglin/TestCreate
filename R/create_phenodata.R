# Basic function


#' Create the Phenodata for affy input
#'
#' @description  This function will create the phenodata which used in loading cel files
#'
#' @param experiment.set vector, store the experiment set in vector
#' @param data.path str, the input raw cell directory path
#' @return this function will return a tbl_df data , if the filename not exist in current directory, show error message
#' @examples create_phenodata(experiment.set=c("control_1","control_2","case_1","case_2"), data.path='./cel_data_dir')
#' @export

create_phenodata <- function(experiment.set, data.path){
    set<- list.files (data.path, pattern=".CEL.gz")
    phenodata.set   <- matrix ( rep ( set, 2) , ncol = 2 )
    phenodata.set   <- as.data.frame ( phenodata.set )
    colnames ( phenodata.set )   <- c ( "Name" , "FileName" )
    phenodata.set$experiment.set <- experiment.set
    return(phenodata.set)
}
