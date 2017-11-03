library(WQReview)
qw.data <- readNWISodbc(DSN="NWISWI",
                        env.db = "01",
                        qa.db = "03",
                        STAIDS = c("04087088","04087142","430125087540400"),
                        dl.parms="All",
                        parm.group.check=TRUE,
                        begin.date = "2017-01-01",
                        end.date = "2017-10-30",
                        projectCd = NULL)

