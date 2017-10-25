#' Init Function
#'
#' This function allows you to initiate an OBM connection.
#' @param project Which project?
#' @param url project server domain DEFAULT is openbiomaps.org
#' @param verbose print some messages
#' @keywords init
#' @export
#' @examples
#' obm_init('dead_animals')
#' obm_init('butterflies','http://localhost/biomaps')

obm_init <- function (project='',url='openbiomaps.org',scope=c(),verbose=F) {
        
    return_val <- TRUE

    OBM <<- new.env()

    if (project=='') {
        project <- readline(prompt="Enter project name: ")
    }

    if (url=='openbiomaps.org') {
        url <- readline(prompt="Enter project url (openbiomaps.org): ")
        if (url=='') {
            url <- 'openbiomaps.org'
        }
    }
    # set some default value
    if (!grepl('https?://',url)) {
        url <- paste('http://',url,sep='')
    }
    OBM$token_url <- paste(url,'/oauth/token.php',sep='')
    OBM$pds_url <- paste(url,'/projects/',project,'/pds.php',sep='')
    s <- httr::GET(OBM$token_url)
    if (httr::status_code(s) == 404 ) {
        print("The url is not valid!")
        print(OBM$token_url)
        verbose <- TRUE
        return_val <- FALSE
    }
    OBM$project <- project
    if(length(scope) > 0) {
        OBM$scope <- scope
    } else {
        # default scopes
        #OBM$scope <- c('get_form_data','get_form_list','get_profile','get_data','get_history','push_data','update_profile')
        OBM$scope <- c('get_form_data','get_form_list','get_profile','get_data','get_history','set_rules','get_report','put_data')
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
#' @param scope vector OAuth2 scopes scecified in the server DEFAULTS are: get_form_data get_form_list get_profile get_data get_history
#' @param client_id Default is R
#' @param url OAuth2 token url obm_init() provide it
#' @param verbose print some messages
#' @param paranoid hide password while typing (on Linux)
#' @keywords auth
#' @export
#' @examples
#' obm_auth()
#' token <- obm_auth('foo@google.com','abc123')

obm_auth <- function (username='',password='',scope=OBM$scope,client_id=OBM$client_id,url=OBM$token_url,verbose=F,paranoid=F) {
    if ( exists('token', envir=OBM ,inherits=F) & exists('time', envir=OBM ,inherits=F) & (username=='' & password=='')) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e)  && e < timestamp) {
            if (verbose) {
                print("Token expired, trying to refresh...")
                return(FALSE)
            }
            # expired
            obm_refresh_token(verbose=verbose)
        } else {
            if (verbose) {
                print(paste("Token is valid until:",as.POSIXlt(OBM$time+OBM$token$expires_in,origin="1970-01-01")))
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
        h <- httr::POST(url,body=list(grant_type='password',username=username,password=password,client_id=client_id,scope=scope))
        if (httr::status_code(h)==401) {
            print('authentication failed!')
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
            print("Authentication failed.")
            return(FALSE)
        }
    }
    return(TRUE)
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
#' data <- obm_get('get_data','39980:39988')
#' data <- obm_get('get_data','faj=Parus palustris')
#' get list of available forms
#' data <- obm_get('get_form_list',0)
#' get data of a form
#' data <- obm_get('get_form_data',73)
#' perform strored query 'last' is a custom label
#' obm_get('get_report','last')

obm_get <- function (scope='',condition='',token=OBM$token,url=OBM$pds_url,table=OBM$project) {
    if (scope=='' || condition == '') {
        return ("usage: obm_get(scope,condition,...)")
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
    h <- httr::POST(url,body=list(access_token=token$access_token,scope=scope,value=condition,table=table),encode='form')
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

#' Put Function
#'
#' This function allows put data into an OpenBioMaps server.
#' @param scope currently put_data supported
#' @param form_header_names database column names vector, if missing default is the full list from the form
#' @param csv_file a csv file with header row
#' @param form_id the form's id
#' @param api_form_data JSON array of data
#' @param token OBM$token
#' @param url OBM$url
#' @param table OBM$table
#' @keywords put
#' @export
#' @examples
#' using own list of columns
#'   obm_get('get_form_list',0)
#'   form <- obm_get('get_form_data',57)
#'   columns <- unlist(form[,'column'])
#'   t <- obm_put('put_data',columns[1:3],form_id=57,data_file='~/teszt2.csv')
#'
#' using default columns list:
#'   t <- obm_put(scope='put_data',form_id=57,csv_file='~/teszt2.csv')
#'
#' JSON upload
#'   data  <- matrix(c(c("Tringa totanus",'egyed',"AWBO",'10'),c("Tringa flavipes",'egyed',"BYWO",'2')),ncol=4,nrow=2,byrow=T)
#'   #colnames(data)<-c("species","nume","place","no")
#'   t <- obm_put(scope='put_data',form_id=57,api_form_data=as.data.frame(data),form_header_names=c('faj','szamossag','hely','egyedszam'))
#' 
#' with attached file
#'   data  <- matrix(c(c("Tringa totanus",'egyed',"AWBO",'10','szamok.odt'),c("Tringa flavipes",'egyed',"BYWO",'2','a.pdf')),ncol=5,nrow=2,byrow=T)
#'   #colnames(data)<-c("species","nume","place","no",'Attach')
#'   t <- obm_put(scope='put_data',form_id=57,api_form_data=as.data.frame(data),form_header_names=c('faj','szamossag','hely','egyedszam','obm_files_id'),media_file=c('~/szamok.odt','~/a.pdf'))
#'
obm_put <- function (scope=NULL,form_header_names=NULL,data_file=NULL,media_file=NULL,form_id='',api_form_data='',token=OBM$token,url=OBM$pds_url,table=OBM$project) {
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
    api_form_data <- jsonlite::toJSON(api_form_data)

    data_attachment <- 0
    media_attachment <- 0
    
    if (!is.null( data_file )) {
        data_attachment <- 1
    } 
    
    if (!is.null (media_file)) {
        media_attachment <- 1
        if ( !length(which(form_header_names=='obm_files_id')) ) {
            print ("obm_files_id column should be exists and filled with the file names!")
        }
    }

    if (!is.null(form_header_names) && is.vector(form_header_names)) {
        form_header_names <- jsonlite::toJSON(form_header_names)
    }


    if (data_attachment==1) {
        # only one attached file
        h <- httr::POST(url,
                    body=list(access_token=token$access_token,scope=scope,put_api_form=form_id,value=form_header_names,api_form_data=api_form_data,table=table, 
                              file=upload_file(data_file)),
                    encode="multipart")

    } else if (media_attachment==1) {

        for ( i in media_file ) {
            h <- httr::POST(url,
                    body=list(access_token=token$access_token, scope=scope, table=table, attached_files=upload_file(i)),
                    encode="multipart")
        }
        
        h <- httr::POST(url,
                    body=list(access_token=token$access_token, scope=scope, table=table, put_api_form=form_id, value=form_header_names, api_form_data=api_form_data),
                    encode="multipart")
    
    } else {
        h <- httr::POST(url,
                    body=list(access_token=token$access_token, scope=scope, table=table, put_api_form=form_id, value=form_header_names, api_form_data=api_form_data),
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
#' @param token
#' url obm_init() provide it
#' client_id default is R
#' verbose
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
        print("Authentication disconnected.")
        if (verbose) {
            print(j)
        }
    }
}

