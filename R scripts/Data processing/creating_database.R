library(RSQLite)
library(DBI)

db <- dbConnect(RSQLite::SQLite(), file.path(derived_data, "data.db"))
try(dbRemoveTable(db, "study_data"))
dbCreateTable(conn = db,
             name = "study_data",
             fields = c(wave = "integer", sex = "integer"))
dbListTables(db)
dbDisconnect(db)

