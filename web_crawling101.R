# practice on yahoo

setwd('C:/Users/Davif Wong/Desktop')

#remove possible 0
string_DD <- function(x='07'){
  as.character(as.integer(x))
}
string_MM <- function(x='07'){
  as.character(as.integer(x)-1)
}
url_time_yahoo <- function(stk_code='0005.HK', start = "2017-01-01", end= "2017-03-31"){
  year_head = substr(start, 1,4);  year_end  = substr(end, 1,4)
  mon_head = string_MM(substr(start, 6,7)); 
  mon_end = string_MM(substr(end, 6,7))
  day_head = string_DD(substr(start, 9,10))
  day_end = string_DD(substr(end, 9,10))
  lst <- c(mon_head, day_head, year_head, mon_end, day_end, year_end)
  #print(lst)
  print( paste(rep('&',6), letters[1:6], rep('=',6), lst, sep='', collapse='') )
  paste(rep('&',6), letters[1:6], rep('=',6), lst, sep='', collapse='')
}

url_yahoo_finance <- function(stock_code='0005.HK', start_prd = "2017-01-01", end_prd= "2017-03-31"){
  yahoo_url_fnt <- 'http://chart.finance.yahoo.com/table.csv?s='
  yahoo_url_tail <- '&g=d&ignore=.csv'
  paste(yahoo_url_fnt, stock_code, url_time_yahoo(stock_code, start_prd, end_prd), yahoo_url_tail, sep='', collapse='')
}

download.file(url_yahoo_finance(), 'testing.csv')
download.file(url_yahoo_finance('0001.HK', "2015-03-01", '2017-03-31'), '0001_1517.csv')
download.file(url_yahoo_finance('0055.HK', "1970-01-01", '2017-03-31'), '0055_long.csv')
download.file(url_yahoo_finance('0620.HK', "2016-02-30", '2017-03-31'), '0620_wrong.csv')
download.file(url_yahoo_finance('0620.HK', "2016-03-30", '2017-03-31'), '0620.csv')

######### XML ####################Don't use xmlTreeParse if url starts with https
install.packages(c('httr','XML','RCurl'))
library(XML);library(RCurl);library(httr)
#Convert to df#
f = system.file("exampleData", "size.xml", package = "XML")
xmlToDataFrame(f, c("integer", "integer", "numeric"))

#read XML as it is #
doc1 <- xmlTreeParse(system.file("exampleData", "mtcars.xml", package="XML"))
rootNode <- xmlRoot(doc1)
xmlName(rootNode)
names(rootNode)

rootNode[[1]] #variables contain variable tag; looks like Var descri for dataset
rootNode[[1]][[1]] # takeout the first element in 'variables'; variable mpg  
rootNode[[1]][[1]][[1]] 

xmlSApply(rootNode[[1]], xmlValue) # now extend to extract multi part of xml file all in once
xml_to_df <- xmlToDataFrame(doc1$doc$file)
xmlSApply(rootNode, xmlValue)

#XML from internet #
doc2 <- htmlTreeParse('http://espnfc.com/club/manchester-united/360/index', 
                      useInternal = TRUE)
teams <- xpathSApply(doc2, "//div[@class='team-name']//span",xmlValue) 

############ JSON: fetch over stat from my git account ###############
install.packages('jsonlite')
library(jsonlite)
json_1 <- fromJSON("https://api.github.com/users/dmtwong/repos")
names(json_1)
json_1$name;json_1$updated_at;json_1$git_url

#Create Json from a default data and then read it as json 
data(mtcars)
json_2 <- toJSON(mtcars, pretty = T)
cat(json_2)

mtcars_2 <- fromJSON(json_2)
typeof(mtcars_2);class(mtcars_2);names(mtcars_2);names(mtcars)
head(mtcars_2)

############ data table revised ###############
## faster at subsetting grouping and updating var compare to data.frame
## If subsetting with 1 dim provide dt[x]; rows will be return 
install.packages('data.table')
library(data.table)
dt <- data.table(x = rnorm(9),
                 y = rep(c('a','b','c'), 3),
                 z = rnorm(9))
tables()
d2 <- data.table(x = rnorm(12),
                 y = rep(c('a','b','c'), 4),
                 z = rnorm(12))
tables()
d2[2,];d2[,2];d2[2] #d2[2] return 2nd row as well
d2[,d2$y=="b"] #same as d2$y=="b"
d2[d2$y=="b"]; d2[d2$y=="b",] 
{
  x=1
  y=2
}
k = {print(10);5}
print(k)
# instead we could pass a list of function on data table obj
dt_2 <- as.data.table(matrix(1:99, nrow=9))
names(dt_2) <- letters[1:11]
dt_2[, list(mean(a), sum(b), length(k))]
d2[,table(y)]

# adding new col
dt_2[, ksquar2:=k^2] #better as no need to create a copy of data frame
#memory efficient if dataset is big
head(dt_2,2)
#but then it will create draw back as referencing the same obj
dt_2_fake_copy <- dt_2
dt_2[,ksquar2:=NA]
head(dt_2,2)
head(dt_2_fake_copy,2)
dt_2_real_copy <- copy(dt_2)
dt_2[,ksquar2:=5]
head(dt_2,2); head(dt_2_real_copy,2)
# perform multiply step function to create var
dt_2[,m:= {
  tmp <- (a+b+c+d+e+f+g+h+i+j+k);
  mean(tmp)
}]
# plyr like opteration
dt_2[, bool_a:={
  a>5
}]
dt_2[, group_sum:=
  sum(a+b+c+d+e+f+g+h+i+j+k), by = !bool_a
]
set.seed(1234)
dt_3 <- data.table(x=sample(letters[1:3], 1E6, T))
dt_3[, .N, by=x]
#recap dt
head(dt)
setkey(dt, y)
dt['b']

dt_4join1 <- data.table(a = rnorm(9),
                       y = rep(c('a','b','c'), 3),
                       b = rnorm(9))
dt_4join2 <- data.table(x = rnorm(3),
                       y = c('a','b','c'),
                       z = rnorm(3))
dt_4join1;dt_4join2
setkey(dt_4join1,y); setkey(dt_4join2,y)
merge(dt_4join1, dt_4join2)

# fast reading: df is created for comparision
big_df <-data.frame(rnorm(1E6), rnorm(1E6))
file <- tempfile()
write.table(big_df, file = file, row.names=F,
            col.names=F, sep ="\t", quote = F)
system.time(fread(file))
system.time(read.table(file, header = F, sep= "\t"))

############ Read MySQL ###############
install.packages('RMySQL')
library(RMySQL)
# assign handle for connection
sql_Db <- dbConnect(MySQL(), user = "genome", 
                    host ="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(sql_Db, 'show databases;')
dbDisconnect(sql_Db) # VERY IMPORTANT

#connecting to specific database (not server only )
database_1 <- dbConnect(MySQL(), user = "genome", db= "hg19",
                    host ="genome-mysql.cse.ucsc.edu")
lst_tables <- dbListTables(database_1)
dbListFields(database_1,"augustusGene") 
#fields/variables that are stored in the table of augustusGene in database hg19

dbGetQuery(database_1, "select count(*) from augustusGene") #standard sql commend

# read the whole table
df_sql_1 <- dbReadTable(database_1, "augustusGene")
head(df_sql_1)

# read subset of table
#Note dbSendQuery send query to database but not yet suck back to comp until fetch 
sql_query_1 <- dbSendQuery(database_1, "select * from augustusGene where bin
                           between 80 and 585")
aff_bin <- fetch(sql_query_1)
names(aff_bin)
summary(aff_bin$bin);table(aff_bin$bin)
# read subset of table
aff_bin2 <- fetch(sql_query_1, 15)
summary(aff_bin2$bin);table(aff_bin2$bin)

dbClearResult(sql_query_1) ## Again, don't forget 
dim(aff_bin); dim(aff_bin2)
dbDisconnect(database_1) ## last but not least
