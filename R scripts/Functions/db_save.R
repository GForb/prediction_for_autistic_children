
db_save_study <- function(data, study_name) {
  db <- DBI::dbConnect(RSQLite::SQLite(), file.path(derived_data, "data.db"))
  
  dbDisconnect(db)
}

