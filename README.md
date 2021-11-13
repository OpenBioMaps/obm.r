# obm.r
R package for connecting and retreive data from OpenBioMaps servers

## install from github

library("devtools")

install_github('OpenBioMaps/obm.r')

## load library

library(obm)

## usage examples

### initialize connection to dead_animals database on openbiomaps.org. If the database is registered in openbiomaps.org it might be works without the url param
obm_init('dead_animals')

### initialize connection to dead_animals database on a specified server
obm_init('dead_animals','https://somewhere.something')

### initialize connection without parameters
obm_init()

### authenticating without parameters
obm_auth()

### query a range of data from the main table. Returns obm_class
data <- obm_get('get_data','39980:39988')

### query all data from the main table. Returns obm_class
data <- obm_get('get_data','*')

### query all data from an additional data table. Returns obm_class
data <- obm_get('get_data','*',table='buildings')

### query data based on column filter. Returns obm_class
data <- obm_get('get_data','species=Parus palustris')

### get avilable forms 
form_list <- obm_get('get_form_list')

### get form data 
form_data <- obm_get('get_form_data',73)

### offline form fill - THIS FUNCTION IS NOT READY
obm_form_fill(form_data)

### upload data
obm_put(...)

### perform stored query
#### the `last` label points an SQL query which stored on the server. These queries connected with the users. 
#### The users can create and save custom queries with custom labels.
obm_get('get_report','last')

### Create datatable using a csv file
#### creates an sql file which can be used in the obm web interface for creating a new table, or add several new columns to an existing table
create_table(file='foo.csv',[data=, sep=',' , quote="'", create-table=F, project=F, table='']

### repository function
#### If the OBM project has a repository connection, the following function will be available
#### Create dataverse, dataset, add files to dataset
#### Get info about dataverses, datasets, files
#### Publish datasets
#### Delete files, datasets, dataverse
obm_repo('get|put',params=list())
