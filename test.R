library("devtools")
build()
install()

SqlRender::translate(sql = "select * from CDMSYNPUF1K.cdm_source;", targetDialect = "hana")