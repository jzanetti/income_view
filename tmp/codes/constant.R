ONLY_TEST <- FALSE
SAMPLE_SIZE <- XXXXXX


CONN_IR = DBI::dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 18 for SQL Server",
  TrustServerCertificate = "Yes",
  server = "PRTPRDSQL36.stats.govt.nz",
  database = "IDI_Clean_202403",
  Trusted_Connection = "Yes"
)


CONN_ADHOCIR = DBI::dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 18 for SQL Server",
  TrustServerCertificate = "Yes",
  server = "PRTPRDSQL36.stats.govt.nz",
  database = "IDI_Adhoc",
  Trusted_Connection = "Yes"
)
