db_manager <- function(conn_nm) {
  
  # PostgreSQL with ODBC set up
  con <- DBI::dbConnect(odbc::odbc(), .conn_nm, bigint = "integer")

  return(con)
}