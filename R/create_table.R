#' create_table Function
#'
#' Create table from csv
#' ./create_table_from_csv.R --file foo.csv [--sep , --quote \' --create-table --project ... --table ... ]
#' Default for quote is "
#' Default for sep is ,
#' Default for create-table is FALSE. If TRUE, SQL output will be CREATE TABLE... instead of ALTER TABLE... 
#' No default for project. If set, table name will be prefixed with this value
#' Default table is the basename of the csv file.
#' @param file csv file for processing
#' @param data instead of input file using a data.frame as input object 
#' @param sep separator character in csv file
#' @param quote quote character of fields in csv file
#' @param createtable output as CREATE TABLE ... or ALTER TABLE ... ADD COLUMN ...
#' @param project prefix for table name
#' @param table the output sql table name
#' @keywords create table
#' @export

create_table <- function(file=NULL, data=NULL, sep=',', quote="'", createtable=FALSE, project=FALSE, table=NULL) {
    # default values 
    csv.sep <- sep
    csv.quote <- quote
    create_table <- createtable
    #project <- ""
    table_name <- table
    csv.file <- file

    if ( !is.null(csv.file) ) {
        file_type <- getExtension(csv.file)
        table_name <- mapply(gsub,file_type,"",csv.file)
        output_file <- paste(table_name,".sql",sep='')
    } 
    else if ( !is.null(data) ) {
        file_type <- NULL
        table_name <- table
        output_file <- paste(table_name,".sql",sep='')
    
    }

    # RUN
    if (project != '') {
        db <- project # set projecttable
        dbtable <- paste(project,tolower(table_name),sep='_')
    } else {
        db <- tolower(table_name)
        dbtable <- tolower(table_name)
    }

    cat("",file=output_file)

    if (create_table) {
        cat(paste("--
    -- OBM database create from csv
    --

    SET statement_timeout = 0;
    SET lock_timeout = 0;
    SET client_encoding = 'UTF8';
    SET standard_conforming_strings = on;
    SET check_function_bodies = false;
    SET client_min_messages = warning;
    SET search_path = public, pg_catalog;
    SET default_tablespace = '';
    SET default_with_oids = false;\n\n",sep=''),file=output_file,append=T)

        cat(paste('CREATE TABLE ',dbtable,"(\n",sep=""),file=output_file,append=T)

        cat(paste("    obm_id integer NOT NULL,
        obm_geometry geometry,
        obm_datum timestamp with time zone DEFAULT now(),
        obm_uploading_id integer,
        obm_validation numeric,
        obm_comments text[],
        obm_modifier_id integer,
        obm_files_id character varying(32),
        CONSTRAINT enforce_dims_obm_geometry CHECK ((st_ndims(obm_geometry) = 2)),
        CONSTRAINT enforce_geotype_obm_geometry CHECK (((((geometrytype(obm_geometry) = 'POINT'::text) OR (geometrytype(obm_geometry) = 'LINE'::text)) OR (geometrytype(obm_geometry) = 'POLYGON'::text)) OR (obm_geometry IS NULL))),
        CONSTRAINT enforce_srid_obm_geometry CHECK ((st_srid(obm_geometry) = 4326))\n);\n"),file=output_file,append=T)


        cat(paste("
    ALTER TABLE ",dbtable," OWNER TO gisadmin;

    --
    -- Name: TABLE ",dbtable,"; Type: COMMENT; Schema: public; Owner: gisadmin
    --

    COMMENT ON TABLE ",dbtable," IS 'user defined table:",Sys.info()['login'],"';

    --
    -- Name: ",dbtable,"_obm_id_seq; Type: SEQUENCE; Schema: public; Owner: gisadmin
    --

    CREATE SEQUENCE ",dbtable,"_obm_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;

    ALTER TABLE ",dbtable,"_obm_id_seq OWNER TO gisadmin;

    --
    -- Name: ",dbtable,"_obm_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: gisadmin
    --

    ALTER SEQUENCE ",dbtable,"_obm_id_seq OWNED BY ",dbtable,".obm_id;

    --
    -- Name: obm_id; Type: DEFAULT; Schema: public; Owner: gisadmin
    --

    ALTER TABLE ONLY ",dbtable," ALTER COLUMN obm_id SET DEFAULT nextval('",dbtable,"_obm_id_seq'::regclass);

    --
    -- Name: ",dbtable,"_pkey; Type: CONSTRAINT; Schema: public; Owner: gisadmin; Tablespace: 
    --

    ALTER TABLE ONLY ",dbtable,"
        ADD CONSTRAINT ",dbtable,"_pkey PRIMARY KEY (obm_id);

    --
    -- Name: obm_uploading_id; Type: FK CONSTRAINT; Schema: public; Owner: gisadmin
    --

    ALTER TABLE ONLY ",dbtable,"
        ADD CONSTRAINT obm_uploading_id FOREIGN KEY (obm_uploading_id) REFERENCES uploadings(id);

    --
    -- Name: ",dbtable,"; Type: ACL; Schema: public; Owner: gisadmin
    --

    REVOKE ALL ON TABLE ",dbtable," FROM PUBLIC;
    REVOKE ALL ON TABLE ",dbtable," FROM gisadmin;
    GRANT ALL ON TABLE ",dbtable," TO gisadmin;
    GRANT ALL ON TABLE ",dbtable," TO ",tolower(db),"_admin;

    --
    -- Name: ",dbtable,"_obm_id_seq; Type: ACL; Schema: public; Owner: gisadmin
    --

    REVOKE ALL ON SEQUENCE ",dbtable,"_obm_id_seq FROM PUBLIC;
    REVOKE ALL ON SEQUENCE ",dbtable,"_obm_id_seq FROM gisadmin;
    GRANT ALL ON SEQUENCE ",dbtable,"_obm_id_seq TO gisadmin;
    GRANT SELECT,USAGE ON SEQUENCE ",dbtable,"_obm_id_seq TO ",tolower(db),"_admin;

    --
    -- OBM add processed columns
    --    \n\n",sep=''),file=output_file,append=T)


    }

    if (file_type == '.csv') {
        csv.data <- read.csv2(csv.file, header=T, sep=csv.sep, quote=csv.quote)
    } else if (file_type == '.xls' || file_type == '.xlsx') {
        #csv.data <- xlsx::read.xlsx(csv.file, sheetIndex = 1)
        return ("Processing xlsx file not supported here due to the complicated dependencies of xlsx package")
    } else if ( is.null(file_type) && !is.null(data)) {
        csv.data <- data
    } else {
        return ("Data source must be specified: a file or data.frame")
    }

    csv.sqlnames <- NULL
    csv.names <- colnames(csv.data)
    for (i in 1:ncol(csv.data)) {
        #csv.coltypes <- append(csv.coltypes,analyse( csv.data[,i],i ))
        sqltype <- analyse( csv.data[,i],csv.names[i],i )
        name <- tolower(gsub("[^A-Za-z0-9]","_",csv.names[i]))
        csv.sqlnames <- append(csv.sqlnames,name)
        cat(paste('ALTER TABLE ',dbtable,' ADD COLUMN ','"',name,'"',' ',sqltype,";\n",sep=""),file=output_file,append=T)
    }

    if (length(unique(csv.sqlnames)) < length(csv.sqlnames)) {
        cat("\nWARNING! Non unique column names!")
        csv.sqlnames[duplicated(csv.sqlnames)]
    }
}

#' getExtension Function
#'
#' This is a helper function
#' @param a file
#' @keywords extension file
#' @export

getExtension <- function(file){ 
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(paste('.',ex[-1],sep=''))
}

#' analyse Function
#'
#' This is a helper function
#' @param column column name
#' @param cn counter
#' @param counter counter
#' @param na drop na?
#' @keywords analyse
#' @export

analyse <- function(col,cn,counter,na.drop=T) {
    type <- class( col )
    flev <- length(levels(as.factor(col)))

    if (type == 'integer') {
        if (min(col,na.rm=T) == 0 && max(col,na.rm=T)==1) {
            type = 'boolean'
            return(type)
        } else {
            if (max(col,na.rm=T) > 32678) {
                return('integer')
            } else {
                return('smallint')
            }
        }
    } else if (type == 'numeric') {
        return('real')
    } else if (type == 'logical' && flev == 2 ) {
            type = 'boolean'
            return(type)
    } 
    
    if (type == 'factor' || type == 'logical') {
        # drop empty cells if na.action=''
        if (na.drop==T) {
            col <- droplevels(as.factor(col[col != ""]))
        }
        f.mod <- tryCatch({
            f.mod <- as.character(as.numeric(levels(col)))
        }, warning = function(war) {
            return(NA)
        })

        if (!anyNA(f.mod) && length(f.mod)) {
            f <- levels(col)
            #if (all(tolower(f)==f.mod)) {
            #    # it is numeric
            #    y <- as.numeric(as.matrix(col))
            #    if (!isTRUE(all(y == floor(y)))) { 
            #        type <- 'real'
            #        return(type)
            #    } else {
            #        type <- 'smallint'
            #        return(type)
            #    }
            #} else {
                y <- as.numeric(as.matrix(col))
                if (!isTRUE(all(y == floor(y)))) { 
                    type <- 'real'
                    return(type)
                } else {

                    if (max(col,na.rm=T) > 32768) {
                        type <- 'integer'
                    } else {
                        type <- 'smallint'
                    }
                    return(type)
                }

                # is not numeric, might be date
                isdate <- tryCatch({
                    isdate <- try(as.Date(col),silent=T)
                    if (!anyNA(isdate)){
                         isdate <- try(as.Date(col,format='%Y.%m.%d',silent=T))
                    }
                }, warning = function(w) {
                    return(NA)
                }, error = function(e){
                    return(NA)
                })

                if (!anyNA(isdate) && class(isdate)=='Date') {
                    type <- 'date'
                    return(type)
                }

                print(paste('Unrecognized factor column: ',cn,sep=''))
                return('text')
            #}
        } else {
            if ( flev == 2 && nchar(levels(as.factor(col))[1])<12 ) {
                print(paste('Probably logical type: ',cn,' (',levels(as.factor(col)),')',sep=''))
            } else if (flev == 0) {
                print(paste('Empty/unknown column: ',cn,sep=''))
                type <- 'text'
                return(type)
            } 
            #min(sapply(as.character(col),nchar))
            col.length <- tryCatch({
                col.length <- max(sapply(as.character(col),nchar),na.rm=T)
            }, error = function(err) {
                print(paste('STRING conversion error: ',err))
                return(256)

            })

            if (col.length<=255) {
                # might be date
                isdate <- tryCatch({
                    isdate <- try(as.Date(col),silent=T)
                    if (!anyNA(isdate)){
                         isdate <- try(as.Date(col,format='%Y.%m.%d',silent=T))
                    }
                }, warning = function(w) {
                    return(NA)
                }, error = function(e){
                    return(NA)
                })

                if (!anyNA(isdate) && class(isdate)=='Date') {
                    type <- 'date'
                    return(type)
                }
                type <- paste('character varying(',col.length,')',sep='')
                return(type)
            } else {
                type <- 'text'
                return(type)
            }
        }
    } else if ( type == 'character') {
        col.length <- tryCatch({
            col.length <- max(sapply(as.character(col),nchar),na.rm=T)
        }, error = function(err) {
            print(paste('STRING conversion error: ',err))
            return(256)
        }) 
        type <- paste('character varying(',col.length,')',sep='')
        return(type)
    }
    return(type)
}
