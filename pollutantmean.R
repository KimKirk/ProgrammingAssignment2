##created by Kimberley Kirk 6-4-2017

##find mean air pollutant across various weather monitors
pollutantmean <- function(directory, pollutant, id) {
    ##set working directory to where the "specdata" folder is
    setwd(file.path("C:", "Users", "Michael", "Desktop", "Inbox", "Career Change", "Data Science", "R Programming", directory))

    ##find all csv files in current working directory
    csv_files_list = list.files(pattern="*.csv")

    ##apply read.csv function across list of all found csv files
    all_data <- do.call(rbind, lapply(csv_files_list, function(x) read.csv(x, colClasses= c("Date", "numeric", "numeric", "integer"), comment.char="" )))
    
    ##use monitor id and pollutant to get subset of the data, returns a vector
    all_data_subset <- all_data[all_data$ID %in% id, pollutant]

    ##remove all NA values and save to new data frame
    all_data_subset_no_NA <- all_data_subset[!is.na(all_data_subset)]

    ##find the mean of values across monitor ids for that given pollutant
    sum(all_data_subset_no_NA)/length(all_data_subset_no_NA)
    
}