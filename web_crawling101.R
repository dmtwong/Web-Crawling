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



