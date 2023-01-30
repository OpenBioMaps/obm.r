#' mark_transform Function
#'
#' Import a csv file and transform to Mark input file
#' ./mark_transform.R --file foo.csv [--sep , --quote \' ... ]
#'
#' @param file A CSV or Excel file path
#' @param data Optional data.frame instead of input file
#' @param csv.sep CSV separator string. Default is autodetect
#' @param csv.quote CSV quote string. Default is autodetect
#' @param write_output_files Write all outputs to separated files
#' @param na.values Replace specified characters to NA, read.csv->na.strings
#' @param na.drop Skip error evaluation of lines where control-column value is empty
#' @export
#' @examples mark_transform('maculinea_2021.07.19.csv')

mark_transform <- function(file=NULL, data=NULL, csv.sep=NULL, csv.quote=NULL, write_output_files=T, na.values="", na.drop=F) {
    # default values 
    csv.file <- file
    
    #species_name column exist in the file?

    if ( !is.null(csv.file) ) {
        file_type <- getExtension(csv.file)
    } else if ( !is.null(data) ) {
        file_type <- NULL
    }

    if (file_type == '.csv') {
        L <- readLines(csv.file, n = 1)
        numfields <- count.fields(textConnection(L), sep = ";")
        if (numfields == 1) {
            csv.sep <- ","
            csv.quote <- "'"
        } else {
            csv.sep <- ";"
            csv.quote <- '"'
        }

        if (!is.null(na.values)) {
            # na_values can be a vector!!
            csv.data <- read.csv2(csv.file, header=T, sep=csv.sep, quote=csv.quote, na.strings = na.values)
        } else {
            csv.data <- read.csv2(csv.file, header=T, sep=csv.sep, quote=csv.quote)
        }
    } else if (file_type == '.xls' || file_type == '.xlsx') {
        #return ("Processing xlsx files is not supported any more due to the complicated dependencies of xlsx package")
        csv.data <- xlsx::read.xlsx(csv.file, sheetIndex = 1)
    } else if ( is.null(file_type) && !is.null(data)) {
        csv.data <- data
    } else {
        return ("Data source must be specified: a file or data.frame")
    }

    names <- colnames(csv.data)
    x <- grep('[_. ]NO|NO[_. ]|NU', names, ignore.case ="True")
    w <- setdiff(names,names[x])
    columns <- c(names[x],w)
    print(c(names[x],w))
    if (length(x)>0) {
        m <- paste("Hit enter to choose [",columns[1],"]")
    } else {
        m <- paste("Hit a number");
    }
    A <- readline(prompt=paste("\nWhich column is the individual ID column?\n", m, ""))
    if (A == '') {
        A <- 1 # if, hit enter, use the first 
    } else {
        x <- grep("^\\d+$",A,perl=T)
        if (!length(x)) {
            A <- grep(A,columns)
            if (!length(A)) {
                cat ("Valid ID column should be choosed!")
                return(0)
            }
        }
    }
    names <- columns[-as.numeric(A)]
    id_col <- columns[as.numeric(A)]

    x <- grep('date|datum', names, ignore.case ="True")
    w <- setdiff(names,names[x])
    columns <- c(names[x],w)
    print(c(names[x],w))
    if (length(x)>0) {
        m <- paste("Hit enter to choose [",columns[1],"]")
    } else {
        m <- paste("Hit a number");
    }
    A <- readline(prompt=paste("\nWhich column is the observation date column?\n", m, ""))
    if (A == '') {
        A <- 1 # if, hit enter, use the first
    } else {
        x <- grep("^\\d+$",A,perl=T)
        if (!length(x)) {
            A <- grep(A,columns)
            if (!length(A)) {
                cat ("Valid DATE column should be choosed!")
                return(0)
            }
        }
    }
    names <- columns[-as.numeric(A)]
    date_col <- columns[as.numeric(A)]

    x <- grep('species|faj', names, ignore.case ="True")
    w <- setdiff(names,names[x])
    columns <- c(names[x],w)
    print(c(names[x],w))
    if (length(x)>0) {
        m <- paste("Hit 1 to choose [",columns[1],"]")
    } else {
        m <- 'hit enter if no species name column'
    }
    A <- readline(prompt=paste("\nWhich column is the species column?\n", m, ""))
    if (A != '') {
        x <- grep("^\\d+$",A,perl=T)
        if (!length(x)) {
            A <- grep(A,columns)
            if (!length(A)) {
                cat ("Invalid species name column selected!")
                return(0)
            }
        }

        species_name_col <- columns[as.numeric(A)]
        names <- columns[-as.numeric(A)]
    } else {
        species_name_col <- NULL
    }

    x <- grep('gender|sex|ivar', names, ignore.case ="True")
    w <- setdiff(names,names[x])
    columns <- c(names[x],w)
    print(c(names[x],w))
    if (length(x)>0) {
        m <- paste("Hit 1 to choose [",columns[1],"]")
    } else {
        m <- 'Hit enter if no sex column'
    }
    A <- readline(prompt=paste("\nWhich column is the sex column?\n", m, ""))
    if (A != '') {
        x <- grep("^\\d+$",A,perl=T)
        if (!length(x)) {
            A <- grep(A,columns)
            if (!length(A)) {
                cat ("Invalid sex column selected!")
                return(0)
            }
        }
        sex_col <- columns[as.numeric(A)]
        names <- columns[-as.numeric(A)]
    } else {
        sex_col <- NULL
    }

    x <- grep('comment|megj', names, ignore.case ="True")
    w <- setdiff(names,names[x])
    columns <- c(names[x],w)
    print(c(names[x],w))
    if (length(x)>0) {
        m <- paste("Hit 1 to choose [",columns[1],"]")
    } else {
        m <- 'Hit enter if no comment column'
    }
    A <- readline(prompt=paste("\nWhich column is the comment column?\n", m, ""))
    if (A != '') {
        x <- grep("^\\d+$",A,perl=T)
        if (!length(x)) {
            A <- grep(A,columns)
            if (!length(A)) {
                cat ("Invalid comment column selected!")
                return(0)
            }
        }
        comment_col <- columns[as.numeric(A)]
        names <- columns[-as.numeric(A)]
    } else {
        comment_col <- NULL
    }

    # 1. Single or multi species?
    # 2. If multi species, split by species and process all species separately
    x_mark_transform(csv.data,id_col,date_col,species_name_col,sex_col,comment_col,write_output_files,na.drop=F)
}

#' getExtension Function
#'
#' This is a helper function
#' @param file is a file name
#' @keywords file-extension

getExtension <- function(file) { 
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(paste('.',ex[-1],sep=''))
}

#' x_mark_transform Function
#'
#' This is a helper function
#' @param data (data.frame)
#' @param id_col (character string)
#' @param date_col (character string)
#' @param species_name_col (character string or NULL)
#' @param sex_col (character string or NULL)
#' @param comment_col (character string or NULL)
#' @param write_output_files (T/F)
#' @param drop na? - skip error evaluation of lines where control-column value is empty
#' @keywords Mark transform error-check

x_mark_transform <- function(data,id_col,date_col,species_name_col=NULL,sex_col=NULL,comment_col=NULL,write_output_files=T,na.drop=F) {

    results <- list()
    errors <- c()
    write("",file="errors.txt",append=F)

    # 1. Collect dates
    #unique_dates <- sort(as.Date(unique(data[,date_col]), format = "%d/%m/%Y"))
    unique_dates = sort(unique(as.Date(data[,date_col])))

    # 2. Collect individual ids
    unique_id    <- sort(as.integer(unique(data[,id_col])))

    # 3. If no species name, add a species_name column with value: species1
    control_cols <- character()
    if (is.null(species_name_col)) {
        data$species_name <- 'species1'
        species_name_col <- 'species_name'
    } else {
        control_cols <- species_name_col
    }
    female <- ''
    male <- ''
    if (is.null(sex_col)) {
        data$sex <- 'sex'
        sex_col_levels <- c('sex')
    } else {
        control_cols <- c(control_cols,sex_col)
        sex_col_levels <- levels(sex_col)
        if (length(levels(sex_col)) > 2) {
            print(paste0("There are more two levels of sex: ", paste(levels(sex), collapse=", ")))
        }
        for (i in sex_col_levels) {
            if (grepl("^(f|n)", sex_col_levels[i], ignore.case = T)) {
                female <- sex_col_levels[i]
            } else if (grepl("^(m|h)", sex_col_levels[i], ignore.case = T)) {
                male <- sex_col_levels[i]
            }
        }
    }
    control_cols <- unique(control_cols[control_cols != ""])

    # 4. Validate each [1] if it not the first [1]:
    #    Is this the same species, same sex, same age?
    #    If not, maka a warning line
    for (i in unique_id) {
        control_data <- data[data[,id_col] == i,]
        if (nrow(control_data) > 1)
        {
            for (control_col in control_cols) {
                compared_levels = unique(control_data[,control_col])
                if (length(compared_levels) > 1) {
                    for (j in compared_levels) {
                        if (is.na(j)) {
                            if (na.drop) {
                                next
                            } else {
                                error <- as.numeric(rownames(control_data[is.na(control_data[,control_col]),]))
                            }
                        } else {
                            x <- rownames(control_data[control_data[,control_col] == j,])
                            x <- x[ !x == 'NA']
                            error <- as.numeric(x)
                        }

                        if (length(error) > 1) {
                            error <- paste(error,collapse=", ")
                        } 
                        if (write_output_files) {
                            write(paste0("Error at ID: ", i ,". The value of ", control_col, " in row " , error, " is ", j), file = "errors.txt", append = TRUE)
                        } else {
                            errors <- append(errors,paste0("Error at ID: ", i ,". The value of ", control_col, " in row " , error, " is ", j))
                        }
                    }
                }
            }
        }
    }


    old_data <- data
    for (s in unique(old_data[,species_name_col])) {
        data <- old_data[old_data[,species_name_col] == s,]
        unique_id    = sort(as.integer(unique(data[,id_col])))
        m <- matrix(0, ncol = length(unique_dates), nrow = length(unique_id))
        output.df = data.frame(m)
        colnames(output.df) <- as.character(unique_dates)
        for (i in 1:length(unique_id)){
            control_data <- data[data[,id_col] == unique_id[i],]
            for (j in 1:length(unique_dates)) {
                if (sum(as.Date(control_data[,date_col]) == unique_dates[j], na.rm = TRUE) > 0) {
                    output.df[i, j] = 1
                }
            }
        }
    #}
    # Mark kódolás megcsinálása
    for (i in 1:nrow(output.df)) {
        output.df$mark_code[i] <- 0
        control_data = data[data[,id_col] == unique_id[i],]
       
        if (length(sex_col_levels) == 2 ) {
            if (sum(control_data[,sex_col] == female) > sum(control_data[,sex_col] == male)) {
                output.df$mark_code[i] = paste(paste(output.df[i,-ncol(output.df)], collapse = ""), " ", " ", 0, " ", 1, ";", sep = "")
            }
            else {
                output.df$mark_code[i] = paste(paste(output.df[i,-ncol(output.df)], collapse = ""), " ", " ", 1, " ", 0, ";", sep = "")
            }
        } else {
            output.df$mark_code[i] = paste(paste(output.df[i,-ncol(output.df)], collapse = ""), ";", sep = "")
        }
    }

    # Add original id columns
    for(i in 1:length(unique_id)) {
        output.df$original_id[i] = unique_id[i]
    }
    # _results tagok az ellenőrzést segítő táblák, mehetnek csv-be
    results[[paste0(s, "_results")]] = output.df
    # Lista _input tagjait kell kiírtani .inp file-ba
    results[[paste0(s, "_input")]] = output.df$mark_code
    }
    if (write_output_files) {
        # Outputok kimentése
        for (i in names(results)) {
            if (class(results[[i]]) == "data.frame") {
                write.csv(results[[i]], file = paste0(i, ".csv"), row.names = FALSE)
            } else {
                write(results[[i]], file = paste0(i, ".inp"))
            }
        }
    } else {
        results$errors <- errors
        return(results)
    }

    # End of function
}

