library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local", app_name = "Betting DS")