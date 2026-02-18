#' Initialise connection to an OpenBioMaps server
#'
#' This function is initiating an OBM connection.
#' @param project Which project you would like to connect? It has a short name.
#' @param url project server's web url DEFAULT is https://openbiomaps.org
#' @param api_version API version. 3.0 or higher enables the new REST and GraphQL API.
#' @param scope vector of required scopes. DEFAULT is ok usually
#' @param verbose print verbose feedback messages - default is FALSE
#' @keywords initiate openbiomaps connect
#' @return boolean
#' @examples
#' \donttest{
#' # connect with API v3 to the dead_animals project on the default openbiomaps.org server
#' obm_init(project='dead_animals')
#' # connect to a database on the default server (openbiomaps.org) with API v2.3
#' obm_init(project='dead_animals', api_version=2.3)
#' # connect on the local server instance to the butterfly database project
#' obm_init(url='http://localhost/biomaps',project='butterflies')
#' }
#' @export
obm_init <- function (project='',
                      url='openbiomaps.org',
                      scope=c(),
                      verbose=FALSE,
                      api_version=3) {

    return_val <- TRUE
    domain <- ''

    set_obm("shared_link", '')
    set_obm("api_version", api_version)

    # get server url
    if (url=='') {
        url <- readline(prompt="Enter project url (e.g. openbiomaps.org): ")
        if (url=='') {
            url <- 'openbiomaps.org'
        }
    }
    # set some default value
    if (!grepl('https?://',url)) {
        url <- paste('https://',url,sep='')
    }

    if (api_version >= 3) {
        # API v3 Logic
        init_url <- paste(url,'/server-api/v3/projects',sep='')
        if (verbose) {
            message('Init url: ',init_url)
        }

        # get project list
        h <- httr::GET(init_url, httr::add_headers(`Accept-Language` = "hu")) # Default to hu or make param? Using hu as per user req example

        if (httr::status_code(h) != 200) {
            warning(paste("http error: ",httr::status_code(h) ))
            return( FALSE )
        }

        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )

        # h.json is a data.frame directly in v3 response example
        projects_df <- h.json

        if (verbose) {
             message('Connected using API v3')
        }

        if (project=='') {
            message("Available project are:\n")
            for (i in 1:nrow(projects_df)) {
                message(projects_df$project_table[i]," (",projects_df$name[i],")")
            }
            message("\n")
            project <- readline(prompt="Enter project name: ")
            project <- gsub('(\\w+)\\s+.*','\\1',project,perl=TRUE)
        }

        selected_project <- projects_df[projects_df$project_table == project, ]

        if (nrow(selected_project) == 0) {
            warning(paste("Project ",project," does not exist! Choose a valid project name."))
            return( FALSE )
        }

        domain <- selected_project$domain[1]

        # Set OBM variables for v3
        # Base URL for project API: {domain}/api/v3/
        # But per user request: {server-url}/projects/{project}/api/v3/
        # The structure in json domain example is https://openbiomaps.org/projects/public_nestbox_data
        # So we can append /api/v3/ to the domain from the response.

        set_obm("server", gsub('(https?://)([^/]*)(/projects/)?(.*)?', '\\2', domain))
        set_obm("pds_url", paste(domain,'/api/v3/',sep=''))

        # Using project-specific token endpoint as in v2
        set_obm("token_url", paste(domain, '/oauth/token.php', sep=''))

        if (verbose) {
            message('API base url: ',get_obm("pds_url"))
            message('Token url: ',get_obm("token_url"))
        }

    } else {
        # API v2 Logic
        init_url <- paste(url,'/v',api_version,'/','pds.php',sep='')
        if (verbose) {
            message('Init url: ',init_url)
        }

        # get project
        h <- httr::POST(init_url,body=list(scope='get_project',value='get_project_list'),encode='form')
        if (httr::status_code(h) != 200) {
            warning(paste("http error: ",httr::status_code(h) ))
            return( FALSE )
        }
        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )

        if (h.json$status=='success') {

            message('Connected to: ',init_url)
            h.cl <- structure(list(data = h.json$data), class = "obm_class")
            if (project=='') {
                message("Available project are:\n")
                for (i in 1:nrow(h.cl$data)) {
                    message(h.cl$data$project_table[i]," (",h.cl$data$project_description[i],")")
                }
                message("\n")
                project <- readline(prompt="Enter project name: ")
                project <- gsub('(\\w+)\\s+.*','\\1',project,perl=TRUE)

            }
            for (i in 1:nrow(h.cl$data)) {
                if (project == h.cl$data$project_table[i]) {
                    domain <- h.cl$data$project_url[i]
                }
            }
        } else {
            if ('message' %in% names(h.json)) {
                warning(h.json$message)
            }
            else if ('data' %in% names(h.json)) {
                warning(h.json$data)
            }
        }
        if (domain == '') {
            warning(
                    paste(
                          "Project ",
                          project,
                          "does not exists! Choose a valid project name.")
            )
            return( FALSE )
        }

        protocol <- gsub('(https?)://.*','\\1',domain)
        server <- gsub('(https?://)([^/]*)(/projects/)?(.*)?', '\\2', domain)
        set_obm("server", server)

        set_obm("pds_url", paste0(sub('/$', '', domain), '/v', api_version, '/pds.php'))
        set_obm("token_url", paste(domain,'oauth/token.php',sep=''))
        if (verbose) {
            message('PDS url: ',get_obm("pds_url"))
            message('Token url: ',get_obm("token_url"))
        }
    } # End API v2

    s <- httr::GET(get_obm("token_url"))
    if (httr::status_code(s) == 404 ) {
        message("The token url is not valid! ", get_obm("token_url"))
        verbose <- TRUE
        return_val <- FALSE
    }
    set_obm("project", project)
    if(length(scope) > 0) {
        set_obm("scope", scope)
    } else {
        # default scopes
        set_obm("scope", c('get_form','get_profile','get_data','get_specieslist',
                       'get_history','set_rules','get_report','put_data',
                       'get_tables','pg_user','use_repo','computation','tracklog'))
    }

    # default client_id
    set_obm("client_id", 'R')

    # return init variables
    if (verbose) {
        ls(.obm_env)
    }
    return(return_val)
}

#' Log in to an OpenBioMaps server
#'
#' This function allows you to connect to and OBM server.
#' @param username Your OBM username (email)
#' @param password Your password
#' @param verbose print verbose feedback messages - default is FALSE
#' @param paranoid hide password while typing (on Linux) - default is TRUE
#' @keywords authentication login
#' @return true if successful and false if not
#' @examples
#' \dontrun{
#' # default interactive authentication
#' obm_auth()
#' 
#' # authentication with username and password
#' obm_auth('foo@google.com','abc123')
#' }
#' @export
obm_auth <- function (username='',
                      password='',
                      verbose=FALSE,
                      paranoid=TRUE)
{

    url   <- get_obm("token_url")
    if (is.null(url)) {
        warning("OBM environment not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    token_time <- get_obm("time")
    token <- get_obm("token")
    scope    = get_obm("scope")
    client_id = get_obm("client_id", "R")

    # --- TOKEN REFRESH LOGiC ---
    if (!is.null(token) && !is.null(token_time) && username == '' && password == '') {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
            if (verbose) message("Token expired, refreshing...")
            obm_refresh_token(verbose = verbose)
            return(TRUE)
        } else {
            if (verbose) {
              expiry_str <- as.POSIXlt(expiry, origin = "1970-01-01")
              message("Token valid until: ", expiry_str)
            }
            return(TRUE)
        }
    }

    # --- INTERACTIVE AUTH ---
    if (username == '') {
        username <- readline(prompt = "Enter username (email address): ")
    }

    if (password == '') {
        if (paranoid) {
            password <- get_password()
        } else {
            password <- readline(prompt = "Enter password: ")
        }
    }

    if (is.null(scope)) scope <- "get_data"
    scope_str <- paste(scope, collapse = ' ')

    h <- httr::POST(
                    url,
                    body=list(
                              grant_type='password', 
                              remember_me=TRUE, 
                              username=username, 
                              password=password, 
                              client_id=client_id, 
                              scope=scope_str),
                    encode = "form"
    )
    if (httr::status_code(h)==401) {
        message('Authentication failed! Invalid credentials.')
        return(FALSE)
    }
        
    j <- httr::content(h, "parsed", "application/json")
    if (verbose) {
            message(j)
    }
    
    if (!is.null(j$access_token)) {
        set_obm("token", j)
        set_obm("time", unclass(Sys.time()))
        if (verbose) message("Authentication successful.")
        return(TRUE)
    } else {
        rm(list = c("token", "time"), envir = obm_env())
        message("Authentication failed (no access token returned).")
        return(FALSE)      
    }
}

#' Connect to a server with a shared link
#'
#' This function allows to connect to an OBM server with a shared link
#' It using client_credentials authentication, so it is returning an access_token
#' Return an oauth token
#' @param link an url link
#' @param verbose print verbose feedback messages - default is FALSE
#' @keywords connect auth shared link
#' @return boolean
#' @examples
#' \donttest{
#' # Interactive connect
#' obm_connect()
#' # Non interactive connect
#' token <- obm_connect('abcdefghikl123456789')
#'}
#' @export
obm_connect <- function (link='',
                         verbose=FALSE) {

    url   <- get_obm("pds_url")
    if (is.null(url)) {
        warning("OBM environment not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if ( link=='' ) {
        link <- readline(prompt="Paste shared link: ")
    }

    h <- httr::POST(
                    url,
                    body=list(
                              shared_link=link, 
                              scope='shared_link'
                              )
    )
    if (httr::status_code(h)==401) {
        message('Authorization failed!')
        return(FALSE)
    }
    if (httr::status_code(h) != 200) {
        message("http error: ",httr::status_code(h) )
        return(FALSE)
    }

    if (httr::http_type(h) == 'application/json') {
        h.content <- httr::content(h,'text')
        h.json <- jsonlite::fromJSON( h.content )
    } else {
        h.json <- httr::content(h)
    }

    if (h.json$status=='success') {

        set_obm("token", h.json$data)
        set_obm("time", unclass(Sys.time()))
        set_obm("shared_link", link)
        return(TRUE)

    } else {
        if (exists('message',h.json)) {
            warning(h.json$message)
            return(FALSE)
        }
        else if (exists('data',h.json)) {
            warning(h.json$data)
            return(FALSE)
        }
    }
}

#' Password reader helper function
#'
#' used in obm_auth()
#' Not intended for direct user use.
#'
#' @keywords internal
get_password <- function() {
    cat("Password: ")
    system("stty -echo")
    a <- readline()
    system("stty echo")
    cat("\n")
    return(a)
}

#' obm_parse_control helper function
#'
#' Helper function to parse control string (e.g. "limit=100:0")
#' Not intended for direct user use.
#' @keywords internal
obm_parse_control <- function(control_string) {
  res <- list(limit = 100, offset = 0)
  if (!is.null(control_string) && control_string != '*' && control_string != '') {
    # Check for limit=LIMIT:OFFSET format
    if (grepl("limit=", control_string)) {
       parts <- strsplit(control_string, "limit=")[[1]][2]
       parts <- strsplit(parts, ":")[[1]]
       res$limit <- as.integer(parts[1])
       if (length(parts) > 1) {
         res$offset <- as.integer(parts[2])
       }
    }
  }
  return(res)
}

#' obm_parse_filters helper function
#'
#' Helper to construct GraphQL filter from condition list
#' Not intended for direct user use.
#' @keywords internal
#' @importFrom stats setNames
obm_parse_filters <- function(condition) {
    if (is.null(condition)) return(NULL)

    # Simple key-value pairs are assumed to be "equals"
    # Logic for ranges like "39980:39988" needs to be handled

    filters <- list()
    for (key in names(condition)) {
        val <- condition[[key]]

        # Check for range: "min:max" (only if string and contains :)
        if (is.character(val) && length(val) == 1 && grepl("^\\d+:\\d+$", val)) {
            parts <- strsplit(val, ":")[[1]]
            # Assuming numeric ID range for now if it looks like numbers
            filters[[length(filters) + 1]] <- list(
                AND = list(
                    setNames(list(list(greater_than_or_equals = as.numeric(parts[1]))), key),
                    setNames(list(list(less_than_or_equals = as.numeric(parts[2]))), key)
                )
            )
        } else if (is.list(val)) {
            # If value is already a list, use it as the operator block
            filter_item <- list()
            filter_item[[key]] <- val
            filters[[length(filters) + 1]] <- filter_item
        } else {
            # Standard equals
            filter_item <- list()
            filter_item[[key]] <- list(equals = val)
            filters[[length(filters) + 1]] <- filter_item
        }
    }

    if (length(filters) == 0) return(NULL)

    # Wrap in AND if multiple conditions
    if (length(filters) > 1) {
        return(list(AND = filters))
    } else {
        return(filters[[1]]) # Single filter object
    }
}

#' obm_get_graphql helper function
#'
#' Helper to execute GraphQL query
#' Not intended for direct user use.
#' @keywords internal
obm_get_graphql <- function(scope, 
                            control_condition, 
                            condition, 
                            token, 
                            url, 
                            table, 
                            retry = TRUE) {

        # Schema and Table defaults
        # The user recently changed default db_schema to "public" but we should
        # allow it to be overridden or default to the project name.
        db_schema <- "public"
        data_table <- table
        explicit_columns <- NULL

        if (is.list(condition)) {
            if ("schema" %in% names(condition)) {
                db_schema <- condition$schema
                condition$schema <- NULL
            }
            if ("table" %in% names(condition)) {
                data_table <- condition$table
                condition$table <- NULL
            }
            if ("fields" %in% names(condition)) {
                # If it is *, we query all fields
                if (is.character(condition$fields) && length(condition$fields) == 1 && condition$fields == "*") {

                    # query the table structure
                    tables_info <- obm_get(
                        'get_tables',
                        condition = list(schema = db_schema, table = data_table)
                    )

                    mandatory_fields <- c("obm_id", "obm_uploading_id")
                    explicit_columns <- vapply(tables_info$fields, function(x) x$name, character(1))
                    explicit_columns <- unique(c(mandatory_fields, explicit_columns))

                } else if (is.list(condition$fields)) {
                    # If user passed fields as a result of get_tables, it might be a list of lists
                    explicit_columns <- vapply(condition$fields, function(x) x$name, character(1))
                } else {
                    explicit_columns <- condition$fields
                }
                condition$fields <- NULL
            }
        }

        # Parse control for limit/offset
        p_control <- obm_parse_control(control_condition)

        # Parse filters
        graphql_filters <- obm_parse_filters(condition)

        # Fields: if control_condition is '*' or null, we want all fields.
        # But GraphQL demands explicit fields.

        if (!is.null(explicit_columns)) {
            fields_block <- paste(explicit_columns, collapse = "\n")
        } else {
            # We need to fetch table columns first.
            # Call /v3/data-tables/{schema}/{dataTable} to get columns

            # Construct URL for table details
            table_details_url <- paste0(url, "data-tables/", db_schema, "/", data_table)

            h_td <- httr::GET(table_details_url, httr::add_headers(Authorization = token$access_token))
            if (httr::status_code(h_td) == 200) {
                td_content <- httr::content(h_td, "parsed")

                columns <- tryCatch({
                     vapply(td_content$columns, function(x) x$name, character(1))
                }, error = function(e) {
                    # Fallback fields if metadata fails
                    c("obm_id")
                })

             # Construct query
             if (length(columns) == 0 || columns == "") {
                 warning("Could not retrieve table columns. Defaulting to 'obm_id'.")
                 columns <- c("obm_id")
             }
             fields_block <- paste(columns, collapse = "\n")
        }
    }

    query_str <- sprintf(
            'query { obmDataList(limit: %d, offset: %d%s) { items { %s } } }',
            p_control$limit,
            p_control$offset,
            if (!is.null(graphql_filters)) ", filters: $filters" else "",
            fields_block
        )

        req_body <- list(
            schema = db_schema,
            table_name = data_table
        )

        if (!is.null(graphql_filters)) {
            # We need to define variables in the query string if we use them
            query_str <- sprintf(
                'query GetData($filters: ObmDataFilterInput) { obmDataList(limit: %d, offset: %d, filters: $filters) { items { %s } } }',
                p_control$limit,
                p_control$offset,
                fields_block
            )
            req_body$query <- query_str
            req_body$variables <- list(filters = graphql_filters)
        } else {
            # No variables needed
            req_body$query <- sprintf(
                'query { obmDataList(limit: %d, offset: %d) { items { %s } } }',
                p_control$limit,
                p_control$offset,
                fields_block
            )
        }

        if (scope == "get_public_data") {
            target_url <- paste0(url, "get-public-data")
            h <- httr::POST(target_url,
                       body = req_body,
                       encode = "json")
        } else {
            target_url <- paste0(url, "get-data")
            h <- httr::POST(target_url,
                       body = req_body,
                       encode = "json",
                       httr::add_headers(Authorization = token$access_token))
        }

        if (httr::status_code(h) != 200) {
            return(paste("http error:", httr::status_code(h), httr::content(h, "text", encoding = "UTF-8")))
        }

        resp <- httr::content(h, "parsed")

        # Check for errors in GraphQL response
        if (!is.null(resp$errors)) {
            return(paste("GraphQL Error:", resp$errors[[1]]$message))
        }

        # Extract items
        items <- resp$data$obmDataList$items

        # Convert to data frame
        if (length(items) > 0) {
             # items is list of lists, convert to DF
             # Using jsonlite or manual binding
             df <- jsonlite::fromJSON(jsonlite::toJSON(items))
             return(df)
        } else {
        return(data.frame())
        }

    return(NULL)
}

#' obm_get_rest_v3 helper function
#'
#' Helper to execute REST API v3 queries (non-GraphQL)
#' Not intended for direct user use.
#' @keywords internal
obm_get_rest_v3 <- function(scope, 
                            control_condition, 
                            condition, 
                            token, 
                            url, 
                            table) {
    if (scope == 'get_form') {
        if ("published_form_id" %in% names(condition)) {
            # /v3/forms/{published-form-id}
            target_url <- paste0(url, "forms/", condition$published_form_id)
            if ("language" %in% names(condition)) {
                h <- httr::GET(
                    target_url,
                    httr::add_headers(Authorization = token$access_token),
                    httr::add_headers("Accept-Language" = condition$language)
                )
            } else {
                h <- httr::GET(target_url, httr::add_headers(Authorization = token$access_token))
            }
        } else {
            # /v3/forms
            target_url <- paste0(url, "forms")
            h <- httr::GET(target_url, httr::add_headers(Authorization = token$access_token))
        }

         if (httr::status_code(h) == 200) {
             return(httr::content(h, "parsed"))
         } else {
             return(paste("http error:", httr::status_code(h), httr::content(h, "text", encoding = "UTF-8")))
         }
    } else if (scope == 'get_tables') {
         if (is.list(condition) && "schema" %in% names(condition) && "table" %in% names(condition)) {
            if ("column" %in% names(condition)) {
                # /v3/data-tables/{schema}/{dataTable}/{column}/unique-values?limit=100&offset=0
                target_url <- paste0(url, "data-tables/", condition$schema, "/", condition$table, "/", condition$column, "/unique-values")
                # Parse URL and add query parameters
                parsed_url <- httr::parse_url(target_url)

                # Add optional query parameters
                if (!is.null(condition$limit)) parsed_url$query$limit <- condition$limit
                if (!is.null(condition$offset)) parsed_url$query$offset <- condition$offset

                # Rebuild URL
                full_url <- httr::build_url(parsed_url)
                h <- httr::GET(full_url, httr::add_headers(Authorization = token$access_token))
            } else {
                # /v3/data-tables/{schema}/{dataTable}
                target_url <- paste0(url, "data-tables/", condition$schema, "/", condition$table)
                h <- httr::GET(target_url, httr::add_headers(Authorization = token$access_token))
            }
         } else {
            # /v3/data-tables
            target_url <- paste0(url, "data-tables")
            h <- httr::GET(target_url, httr::add_headers(Authorization = token$access_token))
         }
         if (httr::status_code(h) == 200) {
             return(httr::content(h, "parsed"))
         } else {
             return(paste("http error:", httr::status_code(h), httr::content(h, "text", encoding = "UTF-8")))
         }
    } else {
        return("Scope not supported in v3 REST API")
    }
}

#' Get Information/Data from an OpenBioMaps project
#'
#' This function get data or information from a project.
#' Supported functions (APIv3): 
#'   get_data: Get records from an SQL table or view, 
#'   get_public_data: Get publicly available records from an SQL table or view, 
#'   get_tables: Get list of SQL tables (or column list) and VIEWS in a project, 
#'   get_form_list, get_form_data: Get upload form information
#' For API v3 (api_version >= 3.0), 'get_data' uses GraphQL for powerful filtering and data selection.
#' See the [GraphQL User Guide](https://gitlab.com/openbiomaps/api/obm-project-api/-/blob/main/GraphQLUserGuide.md) for more details on query possibilities.
#'
#' @param scope A supported obm scope, e.g. get_data, get_form_list, get_tables
#' @param condition list - A condition based on column in your table.
#'   - In API v3 (GraphQL): Can include operators like `iequals`, `AND`, `OR`, and special keys:
#'     - `fields`: vector of columns to return
#'     - `schema`: database schema (defaults to 'public')
#'     - `table`: database table (defaults to project name)
#'   - In API v2: Mostly key-value pairs, e.g., list(species = 'Parus palustris')
#' @param control_condition Control condition.
#'   - In API v3: 'limit=LIMIT:OFFSET' format
#'   - In API v2: SQL-like, e.g., 'limit=10:1'
#' @param table optional table from project (fallback if not in condition list)
#' @keywords get data fetch
#' @return a data.frame, list or error message
#' @examples
#' # --- API v3 GraphQL Examples ---
#' \donttest{
#' # get rows where column 'species' is 'Parus palustris' (case-insensitive)
#' data <- obm_get('get_data', condition=list(species = list(iequals = 'Parus palustris')))
#'
#' # get specific fields from a specific table and schema
#' data <- obm_get('get_data',
#'                 condition=list(
#'                   schema = "public",
#'                   table = "dead_animals",
#'                   fields = c("obm_id", "faj", "hely"),
#'                   faj = list(iequals = "asio otus")
#'                 ))
#'
#' # get all fields from the default table
#' data <- obm_get('get_data', 
#'                 condition=list(
#'                   fields = '*',
#'                   faj = list(iequals = "asio otus")
#'                 ))
#'
#' # get 100 rows with offset 0
#' data <- obm_get('get_data', 'limit=100:0')
#'
#' # get list of available forms
#' data <- obm_get('get_form_list')
#'
#' # get list of available tables in the project
#' obm_get('get_tables')
#'
#' # get table details (columns) in v3
#' obm_get('get_tables', condition=list(schema="public", table="dead_animals"))
#' }
#'
#' # --- API v2 Examples ---
#' \donttest{
#' # get data rows from the main table from 39980 to 39988
#' data <- obm_get('get_data',condition=list(obm_id = '39980:39988'))
#'
#' #get rows from the main table where column 'species' is 'Parus palustris'
#' data <- obm_get('get_data',condition=list(species = 'Parus palustris'))
#'
#' #get 100 rows only from filtered query
#' data <- obm_get('get_data','limit=100:0',condition=list(species = 'Parus palustris'))
#'
#' #get all data from the default/main table
#' data <- obm_get('get_data','*')
#'
#' #get data from a non-default table
#' data <- obm_get('get_data','*',table='additional_data')
#' }
#' @export
obm_get <- function (scope = '',
                     control_condition = NULL,
                     condition = NULL,
                     table = get_obm("project")) {

    token <- get_obm("token")
    url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(scope, '')) {
        warning("Usage: obm_get(scope, params = ...)")
        return(FALSE)
    }

    # auto refreshing token
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
            # expired
            try({
                obm_refresh_token()
                token <- get_obm("token")
                }, silent = TRUE)
        }
    }

    # API v3 Routing
    if (!is.null(api_v) && api_v >= 3) {
        if (scope == 'get_public_data') {
            return(obm_get_graphql(scope, control_condition, condition, NULL, url, table))
        } else if (scope == 'get_data') {
            return(obm_get_graphql(scope, control_condition, condition, token, url, table))
        } else {
            if (scope == 'get_form_list' || scope == 'get_form_data') {
                scope = 'get_form'
            }
            return(obm_get_rest_v3(scope, control_condition, condition, token, url, table))
        }
    }

    # API v2.x
    value <- NULL
    if (scope == 'get_form_list') {
        scope = 'get_form'
        value = 'get_form_list'
    }
    if (scope == 'get_form_data') {
        scope = 'get_form'
    }
    if (is.list(condition)) {
        condition <- jsonlite::toJSON(condition, auto_unbox = TRUE)
        if (is.null(control_condition) || control_condition == '') {
            control_condition <- 'filter'
        } else {
            control_condition <- paste0(control_condition, '^filter')
        }
    } else {
            condition <- NULL
    }

    # -d 'value=filter^limit=3:1' -d 'filters={"terepi_nev":"keleti sün"}
    h <- httr::POST(url,
                    body=list(
                              access_token=token$access_token, 
                              scope=scope, 
                              value=control_condition, 
                              filters=condition, 
                              table=table, 
                              shared_link=get_obm("shared_link"),
                    encode='form')
                    )

    sc <- httr::status_code(h)
    if (sc != 200) {
        msg <- switch(
          as.character(sc),
          '403' = "Resource access denied",
          '202' = "Processing failed",
          '204' = "No data returned",
          '400' = "Bad request",
          '500' = "Server error",
          paste("Unexpected HTTP status:", sc)
        )
        warning(msg)
        return(FALSE)
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

    if (h.json$status=='success') {
        #h.df <- do.call("rbind", h.list$data)
        #class(h.df) <- "obm_class"
        h.cl <- structure(list(data = h.json$data), class = "obm_class")
        return(h.cl$data)
    } else {
        if ('message' %in% names(h.json)) {
            return(h.json$message)
        }
        else if ('data' %in% names(h.json)) {
            return(h.json$data)
        }
    }

    invisible(FALSE)
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

#' Internal function to create obm_class from data.frame
#'
#' This internal function creates an obm_class
#' @keywords internal
#' @return structured data
as.obm_class <- function(x) {
    return(structure(list(data = x), class = "obm_class"))
}

#' Offline form fill function
#'
#' Offline data editor, this function is not ready
#'
#' @param x obm_class
#' @keywords fill form
#' @return data.frame
#' @examples
#' \dontrun{
#' data.frame <- obm_fill_form(obm_class)
#' }
#' @importFrom utils edit
#' @export
obm_fill_form <- function(x) {
    d.f <- edit(x$data)
    # results <- validate(df,x$form_data)
    # print(results)
    return(d.f)
}

#' Put data helper function for API v3
#'
#' Internal function to handle API v3 puts
#' Not intended for direct user use.
#' @keywords internal
#' @return json results object or boolean
#' @importFrom stats runif
obm_put_v3 <- function(scope,
                       form_id,
                       form_data,
                       media_file,
                       token,
                       url,
                       verbose=FALSE) {

  if (scope == 'put_data') {
      target_url <- paste0(url, "forms/", form_id)

      # Prepare data
      row_data <- form_data
      if (is.data.frame(form_data)) {
          if (nrow(form_data) > 1) {
              warning("API v3 endpoint only supports single observation upload. Uploading first row.")
          }
          row_data <- as.list(form_data[1, , drop=FALSE])
          # Convert factors to characters and remove dataframe-specific attributes
          row_data <- lapply(row_data, function(x) if(is.factor(x)) as.character(x) else x)
      }

      # Construct valid metadata
      # UUID approximation: R-timestamp-random
      metadata <- list(
          id = paste0("R-", as.integer(unclass(Sys.time())), "-", round(runif(1, 1000, 9999))),
          app_version = "R-client",
          form_version = as.integer(unclass(Sys.time())),
          started_at = as.integer(unclass(Sys.time())),
          finished_at = as.integer(unclass(Sys.time())),
          timezone = 0
      )

      if (is.null(media_file) || length(media_file) == 0) {
          # JSON upload (preferred for non-file data in v3)
          body_list <- list(
              data = row_data,
              metadata = metadata
          )

          h <- httr::POST(target_url,
                          body = body_list,
                          encode = "json",
                          httr::add_headers(Authorization = token$access_token))
      } else {
          # Multipart upload for files
          # In multipart, data and metadata must be stringified JSON
          body_list <- list(
              data = jsonlite::toJSON(row_data, auto_unbox = TRUE),
              metadata = jsonlite::toJSON(metadata, auto_unbox = TRUE)
          )

          # Add files
          for (f in media_file) {
              if (file.exists(f)) {
                  body_list[[length(body_list) + 1]] <- httr::upload_file(f)
              } else {
                  warning("Media file not found: ", f)
              }
          }
          # All file arguments should have the same name "files[]" for the API to process them as an array
          names(body_list)[3:length(body_list)] <- "files[]"

          h <- httr::POST(target_url,
                          body = body_list,
                          encode = "multipart",
                          httr::add_headers(Authorization = token$access_token))
      }

      if (httr::status_code(h) %in% c(200, 201)) {
          return(httr::content(h, "text"))
      } else {
          warning(paste("http error:", httr::status_code(h), httr::content(h, "text", encoding = "UTF-8")))
          return(FALSE)
      }
  }

  warning("Scope not supported in v3")
  return(FALSE)
}

#' Put data unsing obm forms
#'
#' This function allows put data into an OpenBioMaps server.
#' For API v3 (api_version >= 3.0), 'put_data' is supported.
#' Note: In API v3, 'put_data' currently supports single observation upload only.
#'
#' @param scope currently put_data supported.
#' @param form_header database column names vector, if missing default is the full list from the form
#' @param data_file a csv file with header row (API v2)
#' @param media_file a media file to attach
#' @param form_id the form's id
#' @param form_data JSON array or data.frame of data. In v3, only the first row of a data.frame is uploaded.
#' @param soft_error JSON array of 'Yes' strings (or translations of it) to skip soft error messages (API v2)
#' @param data_table a database table name
#' @keywords put upload
#' @return json or boolean
#' @examples
#' \donttest{
#' # Using own list of columns
#' obm_get('get_form_list')
#' form <- obm_get('get_form_data',57)
#' #columns <- unlist(form[,'column'])
#' 
#' #t <- obm_put('put_data',columns[1:3],form_id=57,data_file='~/teszt2.csv')
#'
#' #Using default columns list:
#' #t <- obm_put(scope='put_data',form_id=57,csv_file='~/teszt2.csv')
#'
#' #JSON upload
#' data <- matrix(c(
#'                  c("Tringa totanus",'egyed',"AWBO",'10','POINT(47.1 21.3)'),
#'                  c("Tringa flavipes",'egyed',"BYWO",'2','POINT(47.3 21.4)')),
#'                ncol=5,nrow=2,byrow=TRUE)
#' colnames(data)<-c("species","nume","place","no","geom")
#' t <- obm_put(scope='put_data',form_id=57,form_data=as.data.frame(data),
#'                form_header=c('faj','szamossag','hely','egyedszam'))
#'
#' #With attached file
#' data <- matrix(c(c("Tringa totanus",'egyed',"AWBO",'10','szamok.odt'),
#'                  c("Tringa flavipes",'egyed',"BYWO",'2','a.pdf')),
#'                ncol=5,nrow=2,byrow=TRUE)
#' colnames(data)<-c("species","nume","place","no",'Attach')
#' #t <- obm_put(scope='put_data',form_id=57,form_data=as.data.frame(data),
#' #               form_header=c('faj','szamossag','hely','egyedszam','obm_files_id'),
#' #               media_file=c('~/szamok.odt','~/a.pdf'))
#' }
#' @export
obm_put <- function (scope=NULL,
                     form_header=NULL,
                     data_file=NULL,
                     media_file=NULL,
                     form_id='',
                     form_data='',
                     soft_error='',
                     data_table=NULL) {

    token <- get_obm("token")
    pds_url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(pds_url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(scope, '')) {
        warning("Usage: obm_put(scope, params = ...)")
        return(FALSE)
    }

    # auto refreshing token
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
            # expired
            try({
                obm_refresh_token()
                token <- get_obm("token")
                }, silent = TRUE)
        }
    }

    if (is.null(data_table)) data_table <- get_obm("project")

    # Wrapper for API v3
    if (!is.null(api_v) && api_v >= 3) {
        return(obm_put_v3(scope, form_id, form_data, media_file, token, pds_url))
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
                    body=list(access_token=token$access_token,
                              scope=scope,
                              form_id=form_id,
                              header=form_header,
                              data=form_data,
                              soft_error=soft_error,
                              table=data_table,
                              file=httr::upload_file(data_file)),
                    encode="multipart")

    } else if (media_attachment==1) {

        files <- list()
        for ( i in media_file ) {
            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token, 
                              scope=scope,
                              table=data_table,
                              attached_files=httr::upload_file(i)),
                    encode="multipart")
            j <- httr::content(h, "parsed", "application/json")
            if (j$status == "success") {
                # uploaded file reference name
                files[[i]] <- unlist(j$data)
            }
        }
        if (!is.null(form_header) && is.vector(form_header)) {
            if ( !length(which(form_header=='obm_files_id')) ) {
                warning ("obm_files_id column should be exists in header names")
            } else {
                obm_files_id_idx <- which(form_header=='obm_files_id')
            }
        } else {
            if ( !length(which(colnames(form_data)=='obm_files_id')) ) {
                warning ("obm_files_id column should be exists in header names")
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
                    body=list(access_token=token$access_token,
                              scope=scope,
                              table=data_table,
                              form_id=form_id,
                              header=form_header,
                              data=form_data),
                    encode="form")

    } else {

        form_data <- jsonlite::toJSON(form_data)
        if (!is.null(form_header) && is.vector(form_header)) {
            form_header <- jsonlite::toJSON(form_header)
        }

        h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token,
                              scope=scope,
                              table=data_table,
                              form_id=form_id,
                              header=form_header,
                              data=form_data),
                    encode="form")
    }

    if (httr::status_code(h) != 200) {
        warning(paste("http error:",httr::status_code(h),h ))
        return(FALSE)
    }

    h.list <- httr::content(h, "parsed", "application/json")
    return(h.list)
}

#obm_put(scope='put_data',form_id=1,form_header_names=columns,api_form_data=as.data.frame(data))

#' Set Function
#'
#' Experimental function in APIv2!
#' This function allows you to set rules for obm_get.
#' @param scope Which scope? e.g. set_join
#' @param condition A text condition based on column in your table
#' @keywords set
#' @return list, json or boolean 
#' @examples
#' \donttest{
#' # automatically join tables
#' data <- obm_set('set_join',c('dead_animals','dead_animals_history'))
#' }
#' @export
obm_set <- function (scope='',
                     condition='') {

    token <- get_obm("token")
    pds_url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(pds_url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(scope, '')) {
        warning("Usage: set_obm(scope, params = ...)")
        return(FALSE)
    }

    # auto refreshing token
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
            # expired
            try({
                obm_refresh_token()
                token <- get_obm("token")
                }, silent = TRUE)
        }
    }

    # API v3 check
    if (!is.null(api_v) && api_v >= 3) {
        warning("This function is not available in API v3")
        return(FALSE)
    }

    h <- httr::POST(pds_url,
                    body=list(
                              access_token=token$access_token,
                              scope=scope,
                              value=condition),
                    encode='form')
    if (httr::status_code(h) != 200) {
        warning(paste("http error:",httr::status_code(h) ))
        return(FALSE)
    }
    h.list <- httr::content(h, "parsed", "application/json")
    if (typeof(h.list)=='list') {
        return(do.call("rbind", h.list))
    } else {
        return(h.list)
    }
}

#' Refresh an expired OBM access token
#'
#' This function requests a new access token using the stored refresh token.
#' It should normally be called internally by other OBM functions when
#' the current token has expired.
#'
#' @param token Optional explicit refresh token (defaults to stored one)
#' @param url OAuth2 token endpoint URL (from obm_init())
#' @param client_id OAuth2 client identifier ("R" by default)
#' @param verbose Print HTTP and token details
#' @return Logical TRUE if token refreshed successfully, FALSE otherwise
#' @importFrom utils capture.output
#' @export
obm_refresh_token <- function(token     = NULL,
                              url       = get_obm("token_url"),
                              client_id = get_obm("client_id", "R"),
                              verbose   = FALSE) {

    if (is.null(url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }
    if (is.null(token)) {
        old_token <- get_obm("token")
        if (is.null(old_token$refresh_token)) {
          warning("No refresh token available.")
          return(invisible(FALSE))
        }
        token <- old_token$refresh_token
    }
    h <- httr::POST(
        url,
        body = list(
            grant_type    = "refresh_token",
            refresh_token = token,
            client_id     = client_id
        ),
        encode = "form"
    )

    j <- httr::content(h, "parsed", "application/json")
    if (!is.null(j$access_token)) {
        set_obm("token", j)
        set_obm("time", unclass(Sys.time()))
        if (verbose) {
            message("Access token refreshed successfully.")
        }
        return(TRUE)
    } else {
        # Token érvénytelen, töröljük a régi session-adatokat
        rm(list = c("token", "time"), envir = obm_env())
        warning("Authentication disconnected.")
        if (verbose && length(j)) {
            message(paste(capture.output(print(j)), collapse = "\n"))
        }
        return(FALSE)
    }
}

#' SQL Interface
#'
#' It is a simple SQL Query interface function
#' @param sqlcmd a valid SQL command
#' @param username most probably automatically set by create_pg_user module
#' @param password most probably automatically set by create_pg_user module
#' @param paranoid password prompt type
#' @param port postgres server port, default is 5432
#' @param database remote database name, default is gisdata
#' @keywords postgres
#' @return sql data object or boolean
#' @examples
#' \donttest{
#' obm_sql_query("SELECT DATE_PART('day', enddate::timestamp - startdate::timestamp) AS days 
#'                FROM nestboxes WHERE enddate IS NOT NULL AND startdate IS NOT NULL 
#'                ORDER BY days")
#' }
#' @export
obm_sql_query <- function(sqlcmd,
                          username='',
                          password='',
                          paranoid=TRUE,
                          port=5432,
                          database='gisdata') {

    token <- get_obm("token")
    api_v <- get_obm("api_version", default = 3)

    sqluser <- get_obm("sqluser")
    if (!is.null(sqluser)) {
        username <- sqluser
    }

    sqlpasswd <- get_obm("sqlpasswd")
    if (!is.null(sqlpasswd)) {
        password <- sqlpasswd
    }

    if (!is.null(api_v) && api_v >= 3) {
        warning("obm_sql_query is not supported in API v3")
        return(FALSE)
    }

    if (username=='' & password=='') {
        h <- httr::POST(
                        get_obm("pds_url"),
                        body=list(
                                  access_token=token$access_token,
                                  scope='pg_user',
                                  value='1',
                                  table=get_obm("project")
                                  ),
                        encode='form')
        if (httr::status_code(h) != 200) {
            warning(paste("http error:",httr::status_code(h) ))
            return(FALSE)
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
            if ("message" %in% names(h.json)) {
                warning(h.json$message)
                return(FALSE)
            }
            else if ("data" %in% names(h.json)) {
                message(jsonlite::toJSON(h.json$data, auto_unbox = TRUE, pretty = TRUE))
                return(FALSE)
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
    set_obm("sqluser", username)
    set_obm("sqlpasswd", password)

    drv <- RPostgreSQL::PostgreSQL()
    #drv <- DBI::dbDriver("PostgreSQL")
    con <- DBI::dbConnect(drv, dbname = database,
                     host = get_obm("server"),port = port,
                     user = username, password = password)
    df_postgres_result <- RPostgreSQL::dbGetQuery(con,sqlcmd)
    RPostgreSQL::dbDisconnect(con)
    RPostgreSQL::dbUnloadDriver(drv)
    return(df_postgres_result)
}

#' Random text generator helper function
#'
#' Not intended for direct user use.
#' @return text string 
#' @keywords internal
randtext <- function(n = 5000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    return(paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE)))
}

#' Repozitorium manager function
#'
#' This function allows put data into a Dataverse repozitorium.
#' @param scope get or put
#' @param data_table OBM project table
#' @param params list which contains parameters for repozitorium
#' @keywords repozitorium
#' @return data frame or boolean
#' @examples
#' \donttest{
#' #Getting server conf
#' obm_repo('get',params=list(server_conf=1))
#'
#' #Set the default server/project-repo for each of the following operations
#' #   - default is 0
#' #   - set possible id's from server_conf query above
#' obm_repo('set',params=list(REPO=x))
#' obm_repo('set',params=list(REPO=x, PARENT=xxx))
#'
#' #Listing dataverse
#' obm_repo('get',params=list(type='dataverse',contents=1))
#' obm_repo('get',params=list(type='dataverse'))
#'
#' #Getting content of the named dataverse
#' obm_repo('get',params=list(id='DINPI'))
#'
#' #Get JSON Representation of a Dataset
#' res <- obm_repo('get',params=list(type='datasets',persistentUrl='https://doi.org/xxx/xxx/xxx'))
#' res <- obm_repo('get',params=list(type='datasets',id=xxx))
#' unlist(res)
#'
#' #Get versions of dataset
#' obm_repo('get',params=list(type='datasets',id=42,version=''))
#' obm_repo('get',params=list(type='datasets',id=42,version=':draft'))
#'
#' #Get files of dataset
#' obm_repo('get',params=list(type='datasets',id=42,files='',version=''))
#'
#' #Get a file
#' res<-obm_repo('get',params=list(type='datafile',id=83))
#' res<-obm_repo('get',params=list(type='datafile',id=83,version=':draft'))
#'
#' #Create a dataverse
#' res <- obm_repo('put',params=list(type='dataverse'))
#' unlist(res)
#'
#' #Create a dateset
#' res <- obm_repo('put',params=list(type='datasets',dataverse=''))
#' unlist(res)
#' }
#'
#' #Add file to dataset (referenced by id or persistentUrl)
#' #res <- obm_repo('put',params=list(type='datafile',file='...',id= | persistentUrl=))
#' #unlist(res)
#'
#' #Add object as file to dataset (referenced by id or persistentUrl)
#' #- automatically convert data object to JSON
#' #- returning with the last file's state
#' #res <- obm_repo('put',params=list(type='datafile', id= | persistentUrl=, 
#' #                        data=list(results=res.list,init_params=init.df)))
#' #unlist(res)
#'
#' #Delete file
#' #res <- obm_repo('delete',params=list(type='datafile',id=...,PARENT_DATAVERSE=...))
#'
#' #Set settings
#' #res <- obm_repo('set',params=list(type='dataset',id=...))
#' @export
obm_repo <- function (scope=NULL,
                      data_table=NULL,
                      params=NULL) {

    token <- get_obm("token")
    pds_url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(pds_url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(scope, '')) {
        warning("Usage: obm_repo(scope, params = ...)")
        return(FALSE)
    }

    # --- Token refresh ---
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
          try({
            obm_refresh_token()
            token <- get_obm("token")
          }, silent = TRUE)
        }
    }


    if (is.null(data_table)) data_table <- get_obm("project")

    if (!is.null(get_obm("default_repo"))) {
        params$REPO <- get_obm("default_repo")
    }
    if (!is.null(get_obm("parent_dataverse"))) {
        params$PARENT_DATAVERSE = get_obm("parent_dataverse")
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
                    oldwd <- getwd()
                    on.exit(setwd(oldwd))
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
                            body=list(
                                      access_token=token$access_token,
                                      scope='use_repo',
                                      params=params,
                                      method='put',
                                      data_files=httr::upload_file(i)
                                      ),
                            encode="multipart")

                    if (httr::status_code(h) != 200) {
                        warning(paste("http error:",httr::status_code(h),h ))
                        return(FALSE)
                    }

                    j <- httr::content(h, "parsed", "application/json")

                    if (j$status == "success") {
                        # Processing response....?
                        z <- j$data
                    } else {
                        warning( j )
                        return(FALSE)
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
                message( "  ", rownames(as.data.frame(unlist(m))), "\n" )

                m$Name <- readline(prompt="Enter name: ")
                m$Alias <- readline(prompt="Enter alias name: ")
                m$ContactEmail0 <- readline(prompt="Enter contact's email-1: ")
                m$ContactEmail1 <- readline(prompt="Enter contact's email-2: ")
                m$Affiliation <- readline(prompt="Enter author's affiliation: ")
                m$Description <- readline(prompt="Enter description: ")

                s <- list('DEPARTMENT','JOURNALS','LABORATORY','ORGANIZATIONS_INSTITUTIONS','RESEARCHERS','RESEARCH_GROUP',
                          'RESEARCH_PROJECTS','TEACHING_COURSES','UNCATEGORIZED');

                message ( unlist(s) )
                m$DataverseType <- readline(prompt="Enter dataverse type: ")

                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(
                              access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
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
                warning ( j )
                return(FALSE)
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
                message( "  ", rownames(as.data.frame(unlist(m))), "\n" )

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


                message ( unlist(s) )
                read <- strsplit(readline(prompt="Enter subjects (e.g. 10 11): "), "\\s", perl=TRUE)
                m$Subject <- s[as.numeric(read[[1]])]
                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(
                              access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
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
                warning ( j )
                return(FALSE)
            }
        }
    }
    else if (scope == 'get') {
        # Getting files/info

        p <- params
        params <- rjson::toJSON(params)

        h <- httr::POST(pds_url,
                body=list(access_token=token$access_token,
                          scope='use_repo',
                          params=params,
                          method='get'),
                encode="form")

        #message (h)

        j <- try(httr::content(h),silent=T)
        if (inherits(j, "try-error")) {
            j <- httr::content(h,"text")
        }
        if (inherits(j, 'raw')) {
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
            warning ( j )
            return(FALSE)
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
                body=list(access_token=token$access_token,
                          scope='use_repo',
                          params=params,
                          method='delete'),
                encode="form")

        j <- httr::content(h)

        return(j)
    }
    else if (scope == 'set') {

        if (!is.null(params$REPO)) {
            set_obm("default_repo", params$REPO)
        }
        if (!is.null(params$PARENT)) {
            set_obm("parent_dataverse", params$PARENT)
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
                    body=list(access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='set'),
                    encode="form")

            j <- httr::content(h)

            return(j)
        }
    }
}


#' Computation Function
#'
#' This function allows you to get data from an OpenBioMaps server.
#' @param action post, get-results, get-status
#' @param data_files list of files to sending
#' @param params computation control params
#' @param config_file is The computation_conf.yml file
#' @keywords computation
#' @return data frame or boolean
#' @examples
#' \donttest{
#' # Manage computational packs
#' results <- obm_computation('post','afile.R',params->c())
#' }
#' @export
obm_computation <- function (action='',
                             data_files=NULL,
                             params=NULL,
                             config_file=NULL) {

    token <- get_obm("token")
    url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(action, '')) {
        warning("Usage: obm_computation(action, params = ...)")
        return(FALSE)
    }

    # --- Token refresh ---
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
          try({
            obm_refresh_token()
            token <- get_obm("token")
          }, silent = TRUE)
        }
    }
    if (!is.null(api_v) && api_v >= 3) {
        stop("obm_computation is not supported in API v3")
    }

    params <- rjson::toJSON(params)

    if (action == 'post') {

        if (dir.exists(data_files)) {
            files2zip <- dir(data_files, full.names = TRUE)
            utils::zip(zipfile = 'scripts.zip', files = files2zip)
            data_files <- 'scripts.zip'
        }

        if (typeof(data_files)=='list') {
            #for ( i in data_files ) {
            #}
            utils::zip("scripts.zip", data_files)
            data_files <- 'scripts.zip'
        }

            h <- httr::POST(url,
                    body=list(access_token=token$access_token,
                              scope='computation',
                              params=params,
                              method='post', 
                              data_files=httr::upload_file(data_files),
                              config_file=httr::upload_file(config_file)),
                    encode="multipart")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
            }

            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
    } else if (action == 'get-status') {

        h <- httr::POST(url,
                        body=list(access_token=token$access_token,
                                  scope='computation',
                                  method='get-status',
                                  params=params),
                        encode='form')
        if (httr::status_code(h) != 200) {
            if (httr::status_code(h) == 403) {
                warning( "Resource access denied" )
                return(FALSE)
            }
            else if (httr::status_code(h) == 500) {
                warning( "Server error" )
                return(FALSE)
            }
        } else {
            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
        }
    } else if (action == 'get-results') {

        h <- httr::POST(url,
                        body=list(access_token=token$access_token,
                                  scope='computation',
                                  method='get-results',
                                  params=params),
                        encode='form')
        if (httr::status_code(h) != 200) {
            if (httr::status_code(h) == 403) {
                warning( "Resource access denied" )
                return(FALSE)
            }
            else if (httr::status_code(h) == 500) {
                warning( "Server error" )
                return(FALSE)
            }
        } else {
            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
        }
    } else {
        warning( "Action should be either: post, get-results or get-status")
        return(FALSE)
    }
}
