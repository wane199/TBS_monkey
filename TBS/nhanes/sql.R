rm(list = ls())
library(DBI)
# Initialize a temporary in memory database and copy a data.frame into it
con <- dbConnect(RSQLite::SQLite(), ":memory:")
data(USArrests)
dbWriteTable(con, "USArrests", USArrests)
dbListTables(con)

# Fetch all query results into a data frame:
dbGetQuery(con, "SELECT * FROM USArrests")

# Or do it in batches
rs <- dbSendQuery(con, "SELECT * FROM USArrests")
d1 <- dbFetch(rs, n = 10) # extract data in chunks of 10 rows
dbHasCompleted(rs)
d2 <- dbFetch(rs, n = -1) # extract all remaining data
dbHasCompleted(rs)
dbClearResult(rs)

# clean up
dbDisconnect(con)


library(DBI)
db <- RSQLite::datasetsDb()
dbListTables(db)

dbReadTable(db, "CO2")
dbGetQuery(db, "SELECT * FROM CO2 WHERE conc < 100")

dbDisconnect(db)



###################
library(sqldf)
a1r <- head(warpbreaks)
a1s <- sqldf("select * from warpbreaks limit 6")
identical(a1r, a1s)

# subset

a2r <- subset(CO2, grepl("^Qn", Plant))
a2s <- sqldf("select * from CO2 where Plant like 'Qn%'")
all.equal(as.data.frame(a2r), a2s)

data(farms, package = "MASS")
a3r <- subset(farms, Manag %in% c("BF", "HF"))
a3s <- sqldf("select * from farms where Manag in ('BF', 'HF')")
row.names(a3r) <- NULL
identical(a3r, a3s)

a4r <- subset(warpbreaks, breaks >= 20 & breaks <= 30)
a4s <- sqldf("select * from warpbreaks where breaks between 20 and 30", 
             row.names = TRUE)
identical(a4r, a4s)

a5r <- subset(farms, Mois == 'M1')
a5s <- sqldf("select * from farms where Mois = 'M1'", row.names = TRUE)
identical(a5r, a5s)

a6r <- subset(farms, Mois == 'M2')
a6s <- sqldf("select * from farms where Mois = 'M2'", row.names = TRUE)
identical(a6r, a6s)

# rbind
a7r <- rbind(a5r, a6r)
a7s <- sqldf("select * from a5s union all select * from a6s")

# sqldf drops the unused levels of Mois but rbind does not; however,
# all data is the same and the other columns are identical
row.names(a7r) <- NULL
identical(a7r[-1], a7s[-1])

# aggregate - avg conc and uptake by Plant and Type
a8r <- aggregate(iris[1:2], iris[5], mean)
a8s <- sqldf('select Species, avg("Sepal.Length") `Sepal.Length`, 
   avg("Sepal.Width") `Sepal.Width` from iris group by Species')
all.equal(a8r, a8s)

# by - avg conc and total uptake by Plant and Type
a9r <- do.call(rbind, by(iris, iris[5], function(x) with(x,
                                                         data.frame(Species = Species[1], 
                                                                    mean.Sepal.Length = mean(Sepal.Length),
                                                                    mean.Sepal.Width = mean(Sepal.Width),
                                                                    mean.Sepal.ratio = mean(Sepal.Length/Sepal.Width)))))
row.names(a9r) <- NULL
a9s <- sqldf('select Species, avg("Sepal.Length") `mean.Sepal.Length`,
	avg("Sepal.Width") `mean.Sepal.Width`, 
	avg("Sepal.Length"/"Sepal.Width") `mean.Sepal.ratio` from iris
	group by Species')
all.equal(a9r, a9s)

# head - top 3 breaks
a10r <- head(warpbreaks[order(warpbreaks$breaks, decreasing = TRUE), ], 3)
a10s <- sqldf("select * from warpbreaks order by breaks desc limit 3")
row.names(a10r) <- NULL
identical(a10r, a10s)

# head - bottom 3 breaks
a11r <- head(warpbreaks[order(warpbreaks$breaks), ], 3)
a11s <- sqldf("select * from warpbreaks order by breaks limit 3")
# attributes(a11r) <- attributes(a11s) <- NULL
row.names(a11r) <- NULL
identical(a11r, a11s)

# ave - rows for which v exceeds its group average where g is group
DF <- data.frame(g = rep(1:2, each = 5), t = rep(1:5, 2), v = 1:10)
a12r <- subset(DF, v > ave(v, g, FUN = mean))
Gavg <- sqldf("select g, avg(v) as avg_v from DF group by g")
a12s <- sqldf("select DF.g, t, v from DF, Gavg where DF.g = Gavg.g and v > avg_v")
row.names(a12r) <- NULL
identical(a12r, a12s)

# same but reduce the two select statements to one using a subquery
a13s <- sqldf("select g, t, v 
from DF d1, (select g as g2, avg(v) as avg_v from DF group by g) 
where d1.g = g2 and v > avg_v")
identical(a12r, a13s)

# same but shorten using natural join
a14s <- sqldf("select g, t, v 
from DF 
natural join (select g, avg(v) as avg_v from DF group by g) 
where v > avg_v")
identical(a12r, a14s)

# table
a15r <- table(warpbreaks$tension, warpbreaks$wool)
a15s <- sqldf("select sum(wool = 'A'), sum(wool = 'B') 
   from warpbreaks group by tension")
all.equal(as.data.frame.matrix(a15r), a15s, check.attributes = FALSE)

# reshape
t.names <- paste("t", unique(as.character(DF$t)), sep = "_")
a16r <- reshape(DF, direction = "wide", timevar = "t", idvar = "g", varying = list(t.names))
a16s <- sqldf("select 
	g, 
	sum((t == 1) * v) t_1, 
	sum((t == 2) * v) t_2, 
	sum((t == 3) * v) t_3, 
	sum((t == 4) * v) t_4, 
	sum((t == 5) * v) t_5 
from DF group by g")
all.equal(a16r, a16s, check.attributes = FALSE)

# order
a17r <- Formaldehyde[order(Formaldehyde$optden, decreasing = TRUE), ]
a17s <- sqldf("select * from Formaldehyde order by optden desc")
row.names(a17r) <- NULL
identical(a17r, a17s)

# centered moving average of length 7
set.seed(1)
DF <- data.frame(x = rnorm(15, 1:15))
s18 <- sqldf("select a.x x, avg(b.x) movavgx from DF a, DF b 
   where a.row_names - b.row_names between -3 and 3 
   group by a.row_names having count(*) = 7 
   order by a.row_names+0", 
             row.names = TRUE)
r18 <- data.frame(x = DF[4:12,], movavgx = rowMeans(embed(DF$x, 7)))
row.names(r18) <- NULL
all.equal(r18, s18)

# merge.  a19r and a19s are same except row order and row names
A <- data.frame(a1 = c(1, 2, 1), a2 = c(2, 3, 3), a3 = c(3, 1, 2))
B <- data.frame(b1 = 1:2, b2 = 2:1)
a19s <- sqldf("select * from A, B")
a19r <- merge(A, B)
Sort <- function(DF) DF[do.call(order, DF),]
all.equal(Sort(a19s), Sort(a19r), check.attributes = FALSE)

# within Date, of the highest quality records list the one closest 
# to noon.  Note use of two sql statements in one call to sqldf.

Lines <- "DeployID Date.Time LocationQuality Latitude Longitude
STM05-1 2005/02/28 17:35 Good -35.562 177.158
STM05-1 2005/02/28 19:44 Good -35.487 177.129
STM05-1 2005/02/28 23:01 Unknown -35.399 177.064
STM05-1 2005/03/01 07:28 Unknown -34.978 177.268
STM05-1 2005/03/01 18:06 Poor -34.799 177.027
STM05-1 2005/03/01 18:47 Poor -34.85 177.059
STM05-2 2005/02/28 12:49 Good -35.928 177.328
STM05-2 2005/02/28 21:23 Poor -35.926 177.314
"

DF <- read.table(textConnection(Lines), skip = 1,  as.is = TRUE,
                 col.names = c("Id", "Date", "Time", "Quality", "Lat", "Long"))

sqldf(c("create temp table DFo as select * from DF order by
  Date DESC, Quality DESC,
  abs(substr(Time, 1, 2) + substr(Time, 4, 2) /60 - 12) DESC",
        "select * from DFo group by Date"))

## Not run: 

# test of file connections with sqldf

# create test .csv file of just 3 records
write.table(head(iris, 3), "iris3.dat", sep = ",", quote = FALSE)

# look at contents of iris3.dat
readLines("iris3.dat")

# set up file connection
iris3 <- file("iris3.dat")
sqldf('select * from iris3 where "Sepal.Width" > 3')

# using a non-default separator
# file.format can be an attribute of file object or an arg passed to sqldf
write.table(head(iris, 3), "iris3.dat", sep = ";", quote = FALSE)
iris3 <- file("iris3.dat")
sqldf('select * from iris3 where "Sepal.Width" > 3', file.format = list(sep = ";"))

# same but pass file.format through attribute of file object
attr(iris3, "file.format") <- list(sep = ";")
sqldf('select * from iris3 where "Sepal.Width" > 3')

# copy file straight to disk without going through R
# and then retrieve portion into R  
sqldf('select * from iris3 where "Sepal.Width" > 3', dbname = tempfile())

### same as previous example except it allows multiple queries against
### the database.  We use iris3 from before.  This time we use an
### in memory SQLite database.

sqldf() # open a connection
sqldf('select * from iris3 where "Sepal.Width" > 3')

# At this point we have an iris3 variable in both
# the R workspace and in the SQLite database so we need to
# explicitly let it know we want the version in the database.
# If we were not to do that it would try to use the R version
# by default and fail since sqldf would prevent it from 
# overwriting the version already in the database to protect
# the user from inadvertent errors.
sqldf('select * from main.iris3 where "Sepal.Width" > 4')
sqldf('select * from main.iris3 where "Sepal_Width" < 4')
sqldf() # close connection

### another way to do this is a mix of sqldf and RSQLite statements
### In that case we need to fetch the connection for use with RSQLite
### and do not have to specifically refer to main since RSQLite can
### only access the database.

con <- sqldf()
# this iris3 refers to the R variable and file
sqldf('select * from iris3 where "Sepal.Width" > 3')
sqldf("select count(*) from iris3")
# these iris3 refer to the database table
dbGetQuery(con, 'select * from iris3 where "Sepal.Width" > 4')
dbGetQuery(con, 'select * from iris3 where "Sepal.Width" < 4')
sqldf()


## End(Not run)

