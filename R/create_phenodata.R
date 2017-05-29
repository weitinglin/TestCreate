# Basic function

# load the raw cel data from path, add the experiment design and output phenodata data.frame
create_phenodata <- function(experiment.set, data.path){
    set<- list.files (data.path, pattern=".CEL.gz")
    phenodata.set   <- matrix ( rep ( set, 2) , ncol = 2 )
    phenodata.set   <- as.data.frame ( phenodata.set )
    colnames ( phenodata.set )   <- c ( "Name" , "FileName" )
    phenodata.set$experiment.set <- experiment.set
    return(phenodata.set)
}
