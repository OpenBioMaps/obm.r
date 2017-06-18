# obm.r
R package for connecting and retreive data from OpenBioMaps servers

## install from github

library("devtools")

install_github('OpenBioMaps/obm.r')

## load library

library(obm)

## usage examples

### init dead_animals on openbiomaps.org
obm_init('dead_animals')

### init dead_animals on local gekko
obm_init('dead_animals','localhost/biomaps')

### authenticating - request token
token <- obm_auth('foobar@gmail.com','pamparampam')

### interactive authentication
token <- OBM_auth()

### refresh token
#### usually auto refreshed
token <- obm_refresh_token(token)

### get avilable forms 
#### it is only a testing code for the mobile app developers...
data <- obm_get('get_form_list',0)

data <- obm_get('get_form_data',73)

### query a range of data from the main table 
data <- obm_get('get_data','39980:39988')

### query data based on column filter
data <- obm_get('get_data','faj=Parus palustris')

### perform stored query
#### the `last` label points an SQL query which stored on the server. These queries connected with the users. 
#### The users can create and save custom queries with custom labels.
obm_get('get_report','last')
