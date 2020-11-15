get_sql <- function(filepath){
  con <- file(filepath, "r")
  sql.string <- ""
  
  while ( T )
  {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) { break }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--", line) == T)
    {
      line <- paste(sub("--","/*", line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}