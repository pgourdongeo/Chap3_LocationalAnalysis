
list.files("KEEP/AD/")

load("KEEP/AD/outEU_nocoord.RDS")
library(readr)

encod<- guess_encoding("KEEP/AD/outEU_nocoord.RDS")
write.csv2(outEU_nocoord, "OutEU_nocoord_UTF8.csv", fileEncoding = "ascII", row.names = F)
