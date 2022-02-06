#' Init Function
#'
#' This function is initiating an OBM connection.
#' @param project Which project?
#' @param url project server domain DEFAULT is openbiomaps.org
#' @param scope vector of required scopes. DEFAULT is ok usually
#' @param verbose print some messages
#' @param api_version API version
#' @keywords init
#' @export
#' @examples
#' connect to a database on the default server (openbiomaps.org)
#' obm_init(project='dead_animals')
#' connect on the local server intance to the butterfly database project
#' obm_init('http://localhost/biomaps','butterflies')

obm_init <- function (project='',url='openbiomaps.org',scope=c(),verbose=F,api_version=2.3) {
        
    return_val <- TRUE
    domain <- ''

    OBM <<- new.env()
    OBM$shared_link <- ''

    # get server url
    if (url=='') {
        url <- readline(prompt="Enter project url (e.g. openbiomaps.org): ")
        if (url=='') {
            url <- 'openbiomaps.org'
        }
    }
    # set some default value
    if (!grepl('https?://',url)) {
        url <- paste('http://',url,sep='')
    }
    init_url <- paste(url,'/v',api_version,'/','pds.php',sep='')
    if (verbose==T) {
        message('Init url: ',init_url)
    }

    # get project
    h <- httr::POST(init_url,body=list(scope='get_project',value='get_project_list'),encode='form')
    if (httr::status_code(h) != 200) {
        return(paste("http error:",httr::status_code(h) ))
    }
    h.content <- httr::content(h,'text')
    h.json <- jsonlite::fromJSON( h.content )

    if (h.json$status=='success') {

        h.cl <- structure(list(data = h.json$data), class = "obm_class")
        if (project=='') {
            for (i in 1:nrow(h.cl$data)) {
                print(h.cl$data$project_table[i])
            }
            project <- readline(prompt="Enter project name: ")
        }
        for (i in 1:nrow(h.cl$data)) {
            if (project == h.cl$data$project_table[i]) {
                domain <- h.cl$data$project_url[i]
            }
        }
    } else {
        if (exists('message',h.json)) {
            print(h.json$message)
        }
        else if (exists('data',h.json)) {
            print(h.json$data)
        }
    }

    protocol <- gsub('(https?)://.*','\\1',domain)
    server <- gsub('(https?://)(.*)(/projects/)(.*)', '\\2', domain)
    OBM$server <- server

    OBM$pds_url <- paste(domain,'/v',api_version,'/pds.php',sep='')
    OBM$token_url <- paste(protocol,'://',server,'/oauth/token.php',sep='')
    if (verbose==T) {
        message('PDS url: ',OBM$pds_url)
        message('Token url: ',OBM$token_url)
    }

    s <- httr::GET(OBM$token_url)
    if (httr::status_code(s) == 404 ) {
        message("The token url is not valid! ", OBM$token_url)
        verbose <- TRUE
        return_val <- FALSE
    }
    OBM$project <- project
    if(length(scope) > 0) {
        OBM$scope <- scope
    } else {
        # default scopes
        OBM$scope <- c('get_form','get_profile','get_data','get_specieslist','get_history','set_rules','get_report','put_data','get_tables','pg_user','use_repo')
    }

    # default client_id
    OBM$client_id <- 'R'

    # return init variables
    if (verbose==T) {
        ls(OBM)
    }
    return(return_val)
}

#' Auth Function
#'
#' This function allows you to connect to and OBM server.
#' @param username Your OBM username (email)
#' @param password Your password
#' @param scope vector for OAuth2 scopes 
#' @param client_id Default is R
#' @param url OAuth2 token url obm_init() provide it
#' @param verbose print some messages
#' @param paranoid hide password while typing (on Linux)
#' @keywords auth
#' @export
#' @examples
#' obm_auth()
#' token <- obm_auth('foo@google.com','abc123')

obm_auth <- function (username='',password='',scope=OBM$scope,client_id=OBM$client_id,url=OBM$token_url,verbose=F,paranoid=T) {
    if ( exists('token', envir=OBM ,inherits=F) & exists('time', envir=OBM ,inherits=F) & (username=='' & password=='')) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e)  && e < timestamp) {
            if (verbose) {
                message("Token expired, trying to refresh...")
                return(FALSE)
            }
            # expired
            obm_refresh_token(verbose=verbose)
        } else {
            if (verbose) {
                message("Token is valid until:",as.POSIXlt(OBM$time+OBM$token$expires_in,origin="1970-01-01"))
            }
        }
    } else {
        if ( username=='' ) {
            username <- readline(prompt="Enter username: ")
        } 
        if ( password=='' ) {
            if (paranoid==T) {
                password <- get_password()
            } else {
                password <- readline(prompt="Enter password: ")
            }
        }
        scope <- paste(scope, collapse = ' ')
        h <- httr::POST(url,body=list(grant_type='password', username=username, password=password, client_id=client_id, scope=scope))
        if (httr::status_code(h)==401) {
            message('authentication failed!')
            return(FALSE)
        }
        z <- Sys.time()
        j <- httr::content(h, "parsed", "application/json")
        if (verbose) {
            print(j)
        }
        if (exists('access_token',j)) {
            OBM$token <- j
            OBM$time <- unclass(z)
        } else {
            if ( exists('token', envir=OBM, inherits=F) & !is.null(OBM$token) ) {
                rm(list=c('token'),envir=OBM)
            }
            if ( exists('time', envir=OBM, inherits=F)  & !is.null(OBM$time)) {
                rm(list=c('time'),envir=OBM)
            }
            message("Authentication failed.")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' Connect by shared link function
#'
#' This function allows you to connect to an OBM server with a shared link
#' It using client_credentials authentication, so it is returning an access_token
#' Return an oauth token
#' @param an url link
#' @keywords connect auth shared link
#' @export
#' @examples
#' obm_connect()
#' token <- obm_connect('abcdefghikl123456789')

obm_connect <- function (link='',verbose=F) {

    if ( link=='' ) {
        link <- readline(prompt="Paste shared link: ")
    } 

    h <- httr::POST(OBM$pds_url,body=list(shared_link=link, scope='shared_link'))
    if (httr::status_code(h)==401) {
        message('authorization failed!')
        return(FALSE)
    }
    if (httr::status_code(h) != 200) {
        message("http error:",httr::status_code(h) )
        return(FALSE)
    }

    if (httr::http_type(h) == 'application/json') {
        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )
    } else {
        h.json <- httr::content(h)
    }

    if (h.json$status=='success') {
        
        z <- Sys.time()
        OBM$token <- h.json$data
        OBM$time <- unclass(z)
        OBM$shared_link <- link

    } else {
        if (exists('message',h.json)) {
            return(h.json$message)
        }
        else if (exists('data',h.json)) {
            return(h.json$data)
        }
    }
}



#' unix like password function
#' used in obm_auth()
#' 
get_password <- function() {
    cat("Password: ")
    system("stty -echo")
    a <- readline()
    system("stty echo")
    cat("\n")
    return(a)
}

#' Get Function
#'
#' This function allows you to get data from an OpenBioMaps server.
#' @param scope Which scope? e.g. get_data 
#' @param condition A text condition based on column in your table
#' @param token obm_init() provide it
#' @param url obm_init() provide it
#' @param table optional table from project
#' @keywords get
#' @export
#' @examples
#' get data rows from the main table from 39980 to 39988
#' data <- obm_get('get_data','39980:39988')
#' get rows from the main table where column 'species' is 'Parus palustris'
#' data <- obm_get('get_data','species=Parus palustris')
#' get all data from the default/main table
#' data <- obm_get('get_data','*')
#' get data from a non-default table
#' obm_get('get_data','*',table='additional_data')
#'
#' get list of available forms
#' data <- obm_get('get_form_list')
#' get data of a form
#' data <- obm_get('get_form_data',73)
#' perform strored query 'last' is a custom label
#' obm_get('get_report','last')
#' get list of available tables in the project
#' obm_get('get_tables')

obm_get <- function (scope='',condition=NULL,token=OBM$token,url=OBM$pds_url,table=OBM$project) {
    if (scope=='') {
        return ("usage: obm_get(scope,...)")
    }
    if ( exists('token', envir=OBM, inherits=F) & exists('time', envir=OBM, inherits=F) ) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e) && e < timestamp) {
            # expired
            obm_refresh_token()
        }
    }
    if (scope == 'get_form_list') {
        scope = 'get_form'
        value = 'get_form_list'
    }
    if (scope == 'get_form_data') {
        scope = 'get_form'
    }

    h <- httr::POST(url,
                    body=list(access_token=token$access_token, scope=scope, value=condition, table=table, shared_link=OBM$shared_link),
                    encode='form')
    if (httr::status_code(h) != 200) {
        if (httr::status_code(h) == 403) {
            message( "Resource access denied" )
        } 
        else if (httr::status_code(h) == 500) {
            message( "Server error" )
        }
    }

    # however it sent as JSON, it is better to parse as text 
    # h.list <- httr::content(h, "parsed", "application/json")

    if (httr::http_type(h) == 'application/json') {
        #h.json <- httr::content(h) # kellemetlen list formában jön vissza...
        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )
    } else {
        # automatikus feldolgozás valamivé...
        h.json <- httr::content(h)
    }

    #if (typeof(h.list)=='list') {
        if (h.json$status=='success') {
            #h.df <- do.call("rbind", h.list$data)
            #class(h.df) <- "obm_class"
            h.cl <- structure(list(data = h.json$data), class = "obm_class")
            return(h.cl$data)
        } else {
            if (exists('message',h.json)) {
                return(h.json$message)
            }
            else if (exists('data',h.json)) {
                return(h.json$data)
            }
        }
    #} else {
    #    h.list
    #}
}

# offline edit 
# Read form data
# form_data <- obm_get('get_form_data',n)
# Create obm_class data_frame object
# obm_data <- as.obm_class(data.frame)
# obm_data$form_data <- form_data
# obm_data <- obm_edit(obm_data)
#       x <- edit(obm_data$data)
#       x <- validate(x)
#               ...
# save(obm_data,file=obm_data_form_id.df)
# load(file=obm_data_form_id.df)

#' as. class Function
#'
#' This class function creates an obm_class
#' @param x data.frame
#' @keywords as obm_class
#' @export
#' @examples
#' as.obm_class(DF)
as.obm_class <- function(x) {
    return(structure(list(data = x), class = "obm_class"))
}

#' obm_form offline form fill function
#'
#' Offline data editor
#' @param x obm_class
#' @keywords fill form
#' @export
#' @examples
#' data.frame <- obm_fill_form(obm_class)
obm_fill_form <- function(x) {
    d.f <- edit(x$data)
    # results <- validate(df,x$form_data)
    # print(results)
    return(d.f)
}

#' summary class Function
#'
#' This class function creates a standard summary
#' @param obm_class S3 data object
#' @keywords summary
#' @export
#' @examples
#' summary(obm.data)
summary.obm_class <- function(x) {
    cat(paste('','Columns','\t','Rows',sep="\t"))
    cat("\n")
    cat(paste('',ncol(x$data),'\t',nrow(x$data),sep="\t"))
    cat("\n")
    cat("\n")
    cat(paste('','Column names',sep="\t"))
    cat("\n")
    print(sort(colnames(x$data)))
}

#' as.data.frame class Function
#'
#' This class function extract data frame from obm_class S3 object
#' @param obm_class S3 class object
#' @keywords as.data.frame
#' @export
#' @examples
#' as.data.frame(obm.data)
as.data.frame.obm_class <- function(x) {
    return(x$data)
}

#' Put Function
#'
#' This function allows put data into an OpenBioMaps server.
#' @param scope currently put_data supported
#' @param form_header database column names vector, if missing default is the full list from the form
#' @param data_file a csv file with header row
#' @param media_file a media file to attach
#' @param form_id the form's id
#' @param form_data JSON array of data
#' @param soft_error JSON array of 'Yes' strings (or translations of it) to skip soft error messages
#' @param token OBM$token
#' @param pds_url OBM$pds_url
#' @param data_table OBM$project
#' @keywords put
#' @export
#' @examples
#' using own list of columns
#'   obm_get('get_form_list')
#'   form <- obm_get('get_form_data',57)
#'   columns <- unlist(form[,'column'])
#'   t <- obm_put('put_data',columns[1:3],form_id=57,data_file='~/teszt2.csv')
#'
#' using default columns list:
#'   t <- obm_put(scope='put_data',form_id=57,csv_file='~/teszt2.csv')
#'
#' JSON upload
#'   data <- matrix(c(c("Tringa totanus",'egyed',"AWBO",'10','POINT(47.1 21.3)'),c("Tringa flavipes",'egyed',"BYWO",'2','POINT(47.3 21.4)')),ncol=5,nrow=2,byrow=T)
#'   #colnames(data)<-c("species","nume","place","no","geom")
#'   t <- obm_put(scope='put_data',form_id=57,form_data=as.data.frame(data),form_header=c('faj','szamossag','hely','egyedszam'))
#' 
#' with attached file
#'   data <- matrix(c(c("Tringa totanus",'egyed',"AWBO",'10','szamok.odt'),c("Tringa flavipes",'egyed',"BYWO",'2','a.pdf')),ncol=5,nrow=2,byrow=T)
#'   #colnames(data)<-c("species","nume","place","no",'Attach')
#'   t <- obm_put(scope='put_data',form_id=57,form_data=as.data.frame(data),form_header=c('faj','szamossag','hely','egyedszam','obm_files_id'),media_file=c('~/szamok.odt','~/a.pdf'))
#'
obm_put <- function (scope=NULL,form_header=NULL,data_file=NULL,media_file=NULL,form_id='',form_data='',soft_error='',token=OBM$token,pds_url=OBM$pds_url,data_table=OBM$project) {
    if ( is.null(scope) ) {
        return ("usage: obm_get(scope...)")
    }
    if ( exists('token', envir=OBM, inherits=F) & exists('time', envir=OBM, inherits=F) ) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e) && e < timestamp) {
            # expired
            obm_refresh_token()
        }
    }

    # create json from data.frame - api expect JSON array as api_form_data
    soft_error <- jsonlite::toJSON(soft_error)

    data_attachment <- 0
    media_attachment <- 0
    
    if (!is.null( data_file )) {
        data_attachment <- 1
    } 
    
    if (!is.null (media_file)) {
        media_attachment <- 1
    }

    if (data_attachment==1) {

        form_data <- jsonlite::toJSON(form_data)
        if (!is.null(form_header) && is.vector(form_header)) {
            form_header <- jsonlite::toJSON(form_header)
        }

        # only one attached file
        h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token,scope=scope,form_id=form_id,header=form_header,data=form_data,soft_error=soft_error,table=data_table, 
                              file=httr::upload_file(data_file)),
                    encode="multipart")

    } else if (media_attachment==1) {

        files <- list()
        for ( i in media_file ) {
            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope=scope, table=data_table, attached_files=httr::upload_file(i)),
                    encode="multipart")
            j <- httr::content(h, "parsed", "application/json")
            if (j$status == "success") {
                # uploaded file reference name
                files[[i]] <- unlist(j$data)
            }
        }
        if (!is.null(form_header) && is.vector(form_header)) {
            if ( !length(which(form_header=='obm_files_id')) ) {
                message ("obm_files_id column should be exists in header names")
            } else {
                obm_files_id_idx <- which(form_header=='obm_files_id')
            }
        } else {
            if ( !length(which(colnames(form_data)=='obm_files_id')) ) {
                message ("obm_files_id column should be exists in header names")
            } else {
                obm_files_id_idx <- which(colnames(form_data)=='obm_files_id')
            }
        }

        # file names to file name index in each obm_files_id cells
        for ( j in 1:nrow(form_data)) {
            # file names in obm_files_id cell
            s <- unlist(strsplit(form_data[,obm_files_id_idx][j],','))
            
            n <- 1
            for ( i in files ) {
                name <- names(files)[n]
                w <- which(s==name)
                if (length(w)) {
                    s[w] <- i
                }
                n <- n+1
            }
            form_data[,obm_files_id_idx][j] <- paste(s,collapse=',')
        }
        form_data <- jsonlite::toJSON(form_data)

        if (!is.null(form_header) && is.vector(form_header)) {
            form_header <- jsonlite::toJSON(form_header)
        }

        h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope=scope, table=data_table, form_id=form_id, header=form_header, data=form_data),
                    encode="form")
    
    } else {

        form_data <- jsonlite::toJSON(form_data)
        if (!is.null(form_header) && is.vector(form_header)) {
            form_header <- jsonlite::toJSON(form_header)
        }

        h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope=scope, table=data_table, form_id=form_id, header=form_header, data=form_data),
                    encode="form")
    }

    if (httr::status_code(h) != 200) {
        return(paste("http error:",httr::status_code(h),h ))
    }

    h.list <- httr::content(h, "parsed", "application/json")
    h.list
}

#obm_put(scope='put_data',form_id=1,form_header_names=columns,api_form_data=as.data.frame(data))

#' Set Function
#'
#' Experimental function!
#' This function allows you to set rules for obm_get.
#' @param scope Which scope? e.g. set_join 
#' @param condition A text condition based on column in your table
#' @param token obm_init() provide it
#' @param url obm_init() provide it
#' @keywords set
#' @export
#' @examples
#' automatically join tables
#' data <- obm_set('set_join',c('dead_animals','dead_animals_history'))

obm_set <- function (scope='',condition='',token=OBM$token,url=OBM$pds_url) {
    if (scope=='' || condition == '') {
        return ("usage: obm_set(scope,condition,...)")
    }
    if ( exists('token', envir=OBM, inherits=F) & exists('time', envir=OBM, inherits=F) ) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e) && e < timestamp) {
            # expired
            obm_refresh_token()
        }
    }
    h <- httr::POST(url,body=list(access_token=token$access_token,scope=scope,value=condition),encode='form')
    if (httr::status_code(h) != 200) {
        return(paste("http error:",httr::status_code(h) ))
    }
    h.list <- httr::content(h, "parsed", "application/json")
    if (typeof(h.list)=='list') {
        do.call("rbind", h.list)
    } else {
        h.list
    }
}


#' Auth Function
#'
#' This function allows you to refresh your OAuth2 token. It is usually a hidden function
#' @param refresh token
#' @param url token url: obm_init() provide it
#' @param client_id, default is R
#' @param verbose, default is FALSE
#' @keywords refresh
#' @export
#' @examples
#' obm_refresh_token(token)

obm_refresh_token <- function(token=OBM$token$refresh_token,url=OBM$token_url,client_id='R',verbose=F) {
    h <- httr::POST(url,body=list(grant_type='refresh_token',refresh_token=token,client_id=client_id))
    j <- httr::content(h, "parsed", "application/json")
    if (exists('access_token',j)) {
        OBM$token <- j
        z <- Sys.time()
        OBM$time <- unclass(z)
        if (verbose) {
            print(j)
        }
    } else {
        if ( exists('token', envir=OBM, inherits=F) & !is.null(OBM$token)) {
            rm(list=c('token'),envir=OBM)
        }
        if ( exists('time', envir=OBM, inherits=F) & !is.null(OBM$time)) {
            rm(list=c('time'),envir=OBM)
        }
        message("Authentication disconnected.")
        if (verbose) {
            print(j)
        }
    }
}

#' SQL Interface
#'
#' It is a simple SQL Query interface function
#' @param sqlcmd
#' username most probably automatically set by create_pg_user module
#' password most probably automatically set by create_pg_user module
#' paranoid password promt type
#' postgres server port, default is 5432
#' database remote database, default is gisdata
#' @keywords postgres
#' @export
#' @examples
#' obm_sql_query("SELECT DATE_PART('day', enddate::timestamp - startdate::timestamp) AS days FROM nestboxes WHERE enddate IS NOT NULL AND startdate IS NOT NULL ORDER BY days")

obm_sql_query <- function(sqlcmd,username='',password='',paranoid=T,port=5432,database='gisdata') {


    if (exists('sqluser',OBM)) {
        username <- OBM$sqluser
    }
    if (exists('sqlpasswd',OBM)) {
        password <- OBM$sqlpasswd
    }

    if (username=='' & password=='') {
        h <- httr::POST(OBM$pds_url,body=list(access_token=OBM$token$access_token,scope='pg_user',value='1',table=OBM$project),encode='form')
        if (httr::status_code(h) != 200) {
            return(paste("http error:",httr::status_code(h) ))
        }

        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )

        if (h.json$status=='success') {
            h.cl <- structure(list(data = h.json$data), class = "obm_class")
            if (exists('username',h.cl$data)) {
                username <- h.cl$data$username
            } else if (exists('usern',h.cl$data)) {
                username <- h.cl$data$usern
                password <- h.cl$data$passw    
            }
            
        } else {
            if (exists('message',h.json)) {
                return(h.json$message)
            }
            else if (exists('data',h.json)) {
                return(h.json$data)
            }
        }
    }

    if ( username=='' ) {
        username <- readline(prompt="Enter username: ")
    } 
    if ( password=='' ) {
        if (paranoid==T) {
            password <- get_password()
        } else {
            password <- readline(prompt="Enter password: ")
        }
    }
    OBM$sqluser <- username 
    OBM$sqlpasswd <- password

    drv <- RPostgreSQL:::PostgreSQL()
    #drv <- DBI::dbDriver("PostgreSQL")
    con <- DBI::dbConnect(drv, dbname = database,
                     host = OBM$server,port = port,
                     user = username, password = password)
    df_postgres_result <- RPostgreSQL::dbGetQuery(con,sqlcmd)
    RPostgreSQL::dbDisconnect(con)
    RPostgreSQL::dbUnloadDriver(drv)
    return(df_postgres_result)
}

#' Helper Function
#'
#' This function allows put data into a repozitorium.
#' @param n replication 
#' @keywords random text
#' @export
randtext <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

#' Repozitorium Function
#'
#' This function allows put data into a repozitorium.
#' @param scope get or put 
#' @param params list which contains parameters for repozitorium
#' @keywords repozitorium
#' @export
#' @examples
#'
#' Getting server conf
#'      obm_repo('get',params=list(server_conf=1))
#'
#' Set the default server/project-repo for each of the following operations 
#'    - default is 0
#'    - set possible id's from server_conf query above
#'      obm_repo('set',params=list(REPO=x))
#'      obm_repo('set',params=list(REPO=x, PARENT=xxx))
#'
#' Listing dataverse      
#'      obm_repo('get',params=list(type='dataverse',contents=1))
#'      obm_repo('get',params=list(type='dataverse'))
#'
#' Getting content of the named dataverse
#'      obm_repo('get',params=list(id='DINPI'))
#'
#' Get JSON Representation of a Dataset
#'      res <- obm_repo('get',params=list(type='datasets',persistentUrl='https://doi.org/xxx/xxx/xxx'))
#'      res <- obm_repo('get',params=list(type='datasets',id=xxx))
#'      repo_summary(res)
#'
#' Get versions of dataset
#'      obm_repo('get',params=list(type='datasets',id=42,version=''))
#'      obm_repo('get',params=list(type='datasets',id=42,version=':draft'))
#'
#' Get files of dataset
#'      obm_repo('get',params=list(type='datasets',id=42,files='',version=''))
#'
#' Get a file
#'      res<-obm_repo('get',params=list(type='datafile',id=83))
#'      res<-obm_repo('get',params=list(type='datafile',id=83,version=':draft'))
#'
#' Create a dataverse
#'      res <- obm_repo('put',params=list(type='dataverse'))
#'      repo_summary(res)
#'
#' Create a dateset
#'      res <- obm_repo('put',params=list(type='datasets',dataverse=''))
#'      repo_summary(res)
#'
#' Add file to dataset (referenced by id or persistentUrl)
#'      res <- obm_repo('put',params=list(type='datafile',file='...',id= | persistentUrl=))
#'      repo_summary(res)
#'
#' Add object as file to dataset (referenced by id or persistentUrl)
#' - automatically convert data object to JSON
#' - returning with the last file's state
#'      res <- obm_repo('put',params=list(type='datafile', id= | persistentUrl=, data=list(results=res.list,init_params=init.df)))
#'      repo_summary(res)
#'
#' Delete file
#'      res <- obm_repo('delete',params=list(type='datafile',id=...,PARENT_DATAVERSE=...))
#'
#' Set settings
#'      res <- obm_repo('set',params=list(type='dataset',id=...))
obm_repo <- function (scope=NULL,token=OBM$token,pds_url=OBM$pds_url,data_table=OBM$project,params=NULL) {

    if ( is.null(scope) ) {
        return ("usage: obm_repo(put|get,...)")
    }

    if ( exists('token', envir=OBM, inherits=F) & exists('time', envir=OBM, inherits=F) ) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e) && e < timestamp) {
            # expired
            obm_refresh_token()
        }
    }


    if (!is.null(OBM$default_repo)) {
        params$REPO = OBM$default_repo
    }
    if (!is.null(OBM$parent_dataverse)) {
        params$PARENT_DATAVERSE = OBM$parent_dataverse
    }

    # Upload/create processes
    if (scope == 'put') {
        
        data_file <- NULL
        
        # Upload files
        if (params$type == 'datafile') {
            tmp_dir <- NULL
            if (!is.null(params$file)) {
                data_file <- params$file     # vector of files
                params <- within(params, rm(file))
            } else {
                if (!is.null(params$data)) {
                    tmp_dir <- paste("obm_temp_dir-",randtext(1),sep='')
                    dir.create(tmp_dir)
                    setwd(tmp_dir)
                    if (!is.null(params$currentSession)) {
                        # Create files from ws-data
                        save.image(file='currentSession.RData')
                        data_file <- 'currentSession.RData'
                    }

                    for ( i in 1:length(params$data) ) {
                        x <- rjson::toJSON(params$data[[i]])
                        writeLines(x, paste('o_',names(params$data)[i],'.json',sep=''), useBytes=T)
                        data_file <- c(data_file, paste(tmp_dir,'/','o_',names(params$data)[i],'.json',sep=''))
                    }
                    setwd("..")
                } else {
                    warning('params$file or params$data should be given if you would like to upload something')
                }
            }
            
            # Data from local files / choose and upload files
            if (!is.null( data_file )) {

                # Check file exist in the dataset for automatic replacing
                k <- obm_repo('get',params=list(type='datasets',id=params$id))
                dataset_state <- k$data$versionState
                existing_files <- k$data$files[[1]]$label
                for (i in 1:length(existing_files)) {
                    if (existing_files[i] == basename(data_file)) {
                        if (dataset_state == 'DRAFT') {
                            res <- obm_repo('get',params=list(type='datafile',id=k$data$files[[1]]$dataFile$id[i]))
                        } else {
                            params$replace_file <- k$data$files[[1]]$dataFile$id[i]
                        }
                    }
                }

                params <- rjson::toJSON(params)
                # Add files to the datasets
                # upload file
                for ( i in data_file ) {
                    h <- httr::POST(pds_url,
                            body=list(access_token=token$access_token, scope='use_repo', params=params, method='put', data_files=httr::upload_file(i)),
                            encode="multipart")
                    
                    if (httr::status_code(h) != 200) {
                        return(paste("http error:",httr::status_code(h),h ))
                    }
                    
                    j <- httr::content(h, "parsed", "application/json")
                    
                    if (j$status == "success") {
                        # Processing response....?
                        z <- j$data
                    } else {
                        return( j )
                    }
                }
                # return last file data
                return( z )

            }
            if (!is.null(tmp_dir)) {
                unlink(tmp_dir, recursive=TRUE)
            }
        # Create a dataverse
        } else if (params$type == 'dataverse') {
            if (is.null(params$metadata)) {
                m <- list(  
                        "Name"='',
                        "Alias"='',
                        "ContactEmail0"='',
                        "ContactEmail1"='',
                        "Affiliation"='',
                        "Description"='',
                        "DataverseType"='')
 
                message( "You must fill the follwing metadata attributes:" )
                cat( "  ", rownames(as.data.frame(unlist(m))), "\n\n" )

                m$Name <- readline(prompt="Enter name: ")
                m$Alias <- readline(prompt="Enter alias name: ")
                m$ContactEmail0 <- readline(prompt="Enter contact's email-1: ")
                m$ContactEmail1 <- readline(prompt="Enter contact's email-2: ")
                m$Affiliation <- readline(prompt="Enter author's affiliation: ")
                m$Description <- readline(prompt="Enter description: ")

                s <- list('DEPARTMENT','JOURNALS','LABORATORY','ORGANIZATIONS_INSTITUTIONS','RESEARCHERS','RESEARCH_GROUP','RESEARCH_PROJECTS','TEACHING_COURSES','UNCATEGORIZED');

                print ( unlist(s) )
                m$DataverseType <- readline(prompt="Enter dataverse type: ")

                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope='use_repo', params=params, method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                return(paste("http error:",httr::status_code(h),h ))
            }
            
            j <- httr::content(h, "parsed", "application/json")
            
            if (j$status == "success") {
                return ( j$data )

                # Processing response ?
                # response can be lightweighted by the request type, e.g.
                # request: dataset by id
                # response: resp <- obm_repo(...)
                # names(resp$data$files[[1]][1,])
                # [1] "description"      "label"            "restricted"       "version"         
                # [5] "datasetVersionId" "categories"       "dataFile"
                # nrow(ki$data$files[[1]])
                # 21
            } else {
                return ( j )
            }


        # Create datasets
        } else if (params$type == 'datasets') {
            if (is.null(params$metadata)) {
                m <- list(  
                        "Title"='',
                        "AuthorName"='',
                        "AuthorAffiliation"='',
                        "ContactName"='',
                        "ContactEmail"='',
                        "Description"='',
                        "Subject"='')

                message( "You must fill the follwing metadata attributes:" )
                cat( "  ", rownames(as.data.frame(unlist(m))), "\n\n" )

                m$Title <- readline(prompt="Enter title: ")
                m$AuthorName <- readline(prompt="Enter author's name: ")
                m$AuthorAffiliation <- readline(prompt="Enter author's affiliation: ")
                m$ContactName <- readline(prompt="Enter contact's name: ")
                m$ContactEmail <- readline(prompt="Enter contact's email: ")
                m$Description <- readline(prompt="Enter description: ")

                #s <- list('Agricultural Sciences',
                #    'Arts and Humanities',
                #    'Astronomy and Astrophysics',
                #    'Business and Management',
                #    'Chemistry',
                #    'Computer and Information Science',
                #    'Earth and Environmental Sciences',
                #    'Engineering',
                #    'Law',
                #    'Mathematical Sciences',
                #    'Medicine, Health and Life Sciences',
                #    'Physics',
                #    'Social Sciences',
                #    'Other')
                # Shortened list
                s <- list('Agricultural Sciences',
                    'Chemistry',
                    'Computer and Information Science',
                    'Earth and Environmental Sciences',
                    'Mathematical Sciences',
                    'Medicine, Health and Life Sciences',
                    'Social Sciences',
                    'Other')


                print ( unlist(s) )
                read <- strsplit(readline(prompt="Enter subjects (e.g. 10 11): "), "\\s", perl=TRUE)
                m$Subject <- s[as.numeric(read[[1]])]
                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope='use_repo', params=params, method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                return(paste("http error:",httr::status_code(h),h ))
            }
            
            j <- httr::content(h, "parsed", "application/json")
            
            if (j$status == "success") {
                return( j$data )

                # Processing response
                # response can be lightweighted by the request type, e.g.
                # request: dataset by id
                # response: resp <- obm_repo(...)
                # names(resp$data$files[[1]][1,])
                # [1] "description"      "label"            "restricted"       "version"         
                # [5] "datasetVersionId" "categories"       "dataFile"
                # nrow(ki$data$files[[1]])
                # 21
            } else {
                return ( j )
            }
        }
    }
    else if (scope == 'get') {
        # Getting files/info

        p <- params
        params <- rjson::toJSON(params)

        h <- httr::POST(pds_url,
                body=list(access_token=token$access_token, scope='use_repo', params=params, method='get'),
                encode="form")

        print (h)

        j <- try(httr::content(h),silent=T)
        if (inherits(j, "try-error")) {
            j <- httr::content(h,"text")
        }
        if ( class(j) == 'raw' ) {
            jk <- httr::headers(h)
            return(list(header=jk$`content-disposition`,data=j))
        } else {
            if (!is.null(p$type) && p$type == 'datafile') {
                jk <- httr::headers(h)
                return(list(header=jk$`content-disposition`,data=j))
            }
            j <- httr::content(h, "parsed", "application/json")
        }

        return(j)
        if (j$status == "success") {
            # metadata uploded, get PID
            #z <- jsonlite::fromJSON(j$data)
            # dataset
            # response can be lightweighted by the request type, e.g.
            # response$data$metadataBlocks$citation$fields[[1]]
            # Dataset title: response$data$metadataBlocks$citation$fields[[1]]$value[[1]]
            # Dataset author: response$data$metadataBlocks$citation$fields[[1]]$value[[2]]$authorName$value
            # Dataset author affiliation: response$data$metadataBlocks$citation$fields[[1]]$value[[2]]$authorAffiliation$value
            # Dataset files: response$data$files
            return( j$data )
        } else {
            return ( j )
        } } 
    else if (scope == 'delete') {

        if (params$type == 'dataverse') {
            message('Are you sure you want to delete your dataverse?\nYou cannot undelete this dataverse. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        if (params$type == 'datasets') {
            message('Are you sure you want to delete this dataset and all of its files?\nYou cannot undelete this dataset. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        if (params$type == 'datafile') {
            message('The file will be deleted after you continue. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        params <- rjson::toJSON(params)

        h <- httr::POST(pds_url,
                body=list(access_token=token$access_token, scope='use_repo', params=params, method='delete'),
                encode="form")

        j <- httr::content(h)

        return(j)
    }
    else if (scope == 'set') {

        if (!is.null(params$REPO)) {
            OBM$default_repo <- params$REPO
        }
        if (!is.null(params$PARENT)) {
            OBM$parent_dataverse <- params$PARENT
            params$PARENT_DATAVERSE = params$PARENT
        }

        api_call <- NULL
        if (!is.null(params$publish)) {
            message("Are you sure you want to publish this dataverse?\nOnce you do so it must remain published. (Yes | No)")
            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
            api_call <- 1
        }

        if (!is.null(api_call)) {
            params <- rjson::toJSON(params)

            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, scope='use_repo', params=params, method='set'),
                    encode="form")

            j <- httr::content(h)

            return(j)
        }
    }
}

#' repo_summary Function
#'
#' This is repozitorium helper function 
#' Creating readable labels, notes and summary of repo output
#' @param x repo output object
#' @keywords label summary
#' @export
repo_summary <- function(x=NULL) {

    r <- unlist(x)

    return(r)
}


