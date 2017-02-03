#' Init Function
#'
#' This function allows you to initiate an OBM connection.
#' @param project Which project?
#' @param domain project server domain DEFAULT is openbiomaps.org
#' @keywords init
#' @export
#' @examples
#' OBM_init('dead_animals')
#' OBM_init('butterflies','http://localhost/biomaps')

OBM_init <- function (project,domain='openbiomaps.org',scope=c()) {
        
    OBM <<- new.env()
    # set some default value
    if (!grepl('https?://',domain)) {
        domain <- paste('http://',domain,sep='')
    }
    OBM$token_url <- paste(domain,'/oauth/token.php',sep='')
    OBM$pds_url <- paste(domain,'/projects/',project,'/pds.php',sep='')
    if(length(scope) > 0) {
        OBM$scope <- scope
    } else {
        # default scopes
        #OBM$scope <- c('get_form_data','get_form_list','get_profile','get_data','get_history','push_data','update_profile')
        OBM$scope <- c('get_form_data','get_form_list','get_profile','get_data','get_history')
    }
    # default client_id
    OBM$client_id <- 'R'
}

#' Auth Function
#'
#' This function allows you to connect to and OBM server.
#' @param username Your OBM username (email)
#' @param password Your password
#' @param scope vector OAuth2 scopes scecified in the server DEFAULTS are: get_form_data get_form_list get_profile get_data get_history
#' @param client_id Default is R
#' @param url OAuth2 token url OBM_init() provide it
#' @param verbose print some messages
#' @keywords auth
#' @export
#' @examples
#' OBM_auth()
#' token <- OBM_auth('banm@vocs.unideb.hu','12345')

OBM_auth <- function (username='',password='',scope=OBM$scope,client_id=OBM$client_id,url=OBM$token_url,verbose=F) {
    if ( exists('token', envir=OBM) & exists('time', envir=OBM) & (username=='' & password=='')) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e)  && e < timestamp) {
            if (verbose) {
                print("Token expired, trying to refresh...")
            }
            # expired
            OBM_refresh_token(verbose=verbose)
        }
    } else {
        if (username=='' || password=='') {
            username <- readline(prompt="Enter username: ")
            password <- readline(prompt="Enter password: ")
        }
        scope <- paste(scope, collapse = ' ')
        h <- httr::POST(url,body=list(grant_type='password',username=username,password=password,client_id=client_id,scope=scope))
        z <- Sys.time()
        j <- httr::content(h, "parsed", "application/json")
        if (verbose) {
            print(j)
        }
        if (exists('access_token',j)) {
            OBM$token <- j
            OBM$time <- unclass(z)
        } else {
            if ( exists('token', envir=OBM) & !is.null(OBM$token) ) {
                rm(list=c('token'),envir=OBM)
            }
            if ( exists('time', envir=OBM)  & !is.null(OBM$time)) {
                rm(list=c('time'),envir=OBM)
            }
            print("Authentication failed.")
        }
    }
}

#' Get Function
#'
#' This function allows you to get data from an OpenBioMaps server.
#' @param scope Which scope? e.g. get_data 
#' @param condition A text condition based on column in your table
#' @param token OBM_init() provide it
#' @param url OBM_init() provide it
#' @keywords get
#' @export
#' @examples
#' data <- OBM_get('get_data','39980:39988')
#' data <- OBM_get('get_data','faj=Parus palustris')

OBM_get <- function (scope='',condition='',token=OBM$token,url=OBM$pds_url) {
    if (scope=='' || condition == '') {
        return ("usage: OBM_get(scope,condition,...)")
    }
    if ( exists('token', envir=OBM) & exists('time', envir=OBM) ) {
        # auto refresh token 
        z <- Sys.time()
        timestamp <- unclass(z)
        e <- OBM$time + OBM$token$expires_in
        if (length(e) && e < timestamp) {
            # expired
            OBM_refresh_token()
        }
    }
    h <- httr::POST(url,body=list(access_token=token$access_token,scope=scope,value=condition),encode='form')
    h.list <- httr::content(h, "parsed", "application/json")
    do.call("rbind", h.list)
}

#' Auth Function
#'
#' This function allows you to refresh your OAuth2 token. It is usually a hidden function
#' @param token
#' url OBM_init() provide it
#' client_id default is R
#' verbose
#' @keywords refresh
#' @export
#' @examples
#' OBM_refresh_token(token)

OBM_refresh_token <- function(token=OBM$token$refresh_token,url=OBM$token_url,client_id='R',verbose=F) {
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
        if ( exists('token', envir=OBM) & !is.null(OBM$token)) {
            rm(list=c('token'),envir=OBM)
        }
        if ( exists('time', envir=OBM) & !is.null(OBM$time)) {
            rm(list=c('time'),envir=OBM)
        }
        print("Authentication disconnected.")
        if (verbose) {
            print(j)
        }
    }
}

