db_query <- function(sql_path, ...) {
  
  # Check the type of variable `file`. If it is file, read it. 
  # If it is string, use it directly
  if(file.exists(sql_path)) {
    
    statement <- get_sql(sql_path)
    
  } else if (is.character(sql_path)) {
    
    statement <- sql_path
    
  } else {
    
    stop('no file exists or bad query!')
    
  }
  
  con <- connect_db()
  
  # interpolate parameter
  statement <- DBI::sqlInterpolate(con, statement, ...)
  
  # query
  value <- 
    tryCatch(DBI::dbGetQuery(con, statement) %>% as.data.frame(),
             error = function(cond) {
               DBI::dbDisconnect(con)
               stop('Error in Query')
             })
  
  # close con
  DBI::dbDisconnect(con)
  return(value)
}