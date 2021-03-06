pollutantmean <- function(directory,pollutant,id=1:332) {
    
    ## get the files from a given directory into a list
    files.list <- list.files(directory,full.names=TRUE); ## Note this does not
    ## take into account any sub-directories present in the directory
    
    ## get all the values from each file into a numeric vector
    
    ## initialize the numeric vector
    pollutantvec <- vector()
    for ( i in id)
    {
        ## create a data frame out of the file
        df = read.csv(files.list[i],header=TRUE)
        tempvec <- df[,pollutant]
        
        ## clean up NA values
        bad <- is.na(tempvec)
        tempvec <- tempvec[!bad]
        
        ## append the column values to the numeric vector
        pollutantvec <- append(pollutantvec,tempvec)
    }
    
    ## get the mean
    mean(pollutantvec)
}

complete <- function(directory, id = 1:332)
{
    ## get the files from a given directory into a list
    files.list <- list.files(directory,full.names=TRUE); ## Note this does not
    ## take into account any sub-directories present in the directory
    
    # loop throug the files in the directory
    outputdf <- data.frame("id"=numeric(),"noobs"=numeric())
    for(i in id)
    {
        ## create a data frame out of the file
        df = read.csv(files.list[i],header=TRUE)
        good <- complete.cases(df)
        numgoodcases <- sum(good == TRUE)
        outputvec <- c(i,numgoodcases)
        outputdf <- rbind(outputdf,outputvec)
    }
    names(outputdf)[1]="id"
    names(outputdf)[2]="noob"
    outputdf
}

corr <- function (directory, threshold = 0)
{
    ## get the files from a given directory into a list
    files.list <- list.files(directory,full.names=TRUE); ## Note this does not
    ## take into account any sub-directories present in the directory
    
    # loop throug the files in the directory
    correlationvector <- vector()
    op <- vector()
    for(i in 1:length(files.list))
    {
        # find out whether we should consider the csv file with the current id
        # if so, get the nitrate and sulfate columns as vectors and add to the 
        # global sulfate and nitrate vectors
        df = read.csv(files.list[i],header=TRUE)
        good <-  complete.cases(df)
        if(sum(good) > threshold) { 
            completecases <- df[good,]
            sulfatevector <- completecases[,"sulfate"]
            nitratevector <- completecases[,"nitrate"]
            correlationvector <- append(correlationvector,cor(sulfatevector,nitratevector))
        }
    }
    correlationvector
}
