# obm.r
R package for connecting and retreive data from OpenBioMaps servers

## install from github

library("devtools")

install_github('OpenBioMaps/obm.r')

## load library

library(obm)

## usage examples

### initialize connection to dead_animals database on openbiomaps.org
obm_init('dead_animals')

### initialize connection to dead_animals database on localhost
obm_init('localhost/biomaps','dead_animals')

### initialize connection without parameters
obm_init()

### authenticating without parameters
obm_auth()

### get avilable forms 
form_list <- obm_get('get_form_list')

### get form data 
form_data <- obm_get('get_form_data',73)

### query a range of data from the main table. Returns obm_class
data <- obm_get('get_data','39980:39988')

### query all data from the main table. Returns obm_class
data <- obm_get('get_data','*')

### query data based on column filter. Returns obm_class
data <- obm_get('get_data','faj=Parus palustris')

### offline form fill - THIS FUNCTION IS NOT READY
obm_form_fill(form_data)

### upload data
obm_put(...)

### perform stored query
#### the `last` label points an SQL query which stored on the server. These queries connected with the users. 
#### The users can create and save custom queries with custom labels.
obm_get('get_report','last')
