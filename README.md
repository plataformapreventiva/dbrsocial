# db-connection
R package to run queries over the predictivadb database.

In order to get this package running you have to set the following environmental variables:
* PGHOST
* POSTGRES_USER
* POSTGRES_PASSWORD
* PGPORT
* PGDATABASE

## Token
As this is a private repo, in order to install it via devtools you have to
ask your administrator  for the token and add it to the environmental variables
as "GITHUB_PATH".
To set an access token [follow this](https://github.com/settings/tokens/new).

## Installation
devtools::install_github("plataformapreventiva/dbconnection", auth_token=Sys.getenv("GITHUB_PATH"), build_vignettes=TRUE)
