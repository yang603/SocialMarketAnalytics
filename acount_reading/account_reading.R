library("RODBC")

options(digits=4)

odbcCloseAll()
ch = odbcConnect("Falcon_3326")


query <- paste("
               SET Transaction Isolation Level Read Uncommitted;
               ")
sqlQuery(ch, query)

# query_account <- paste("
#                   SELECT * FROM tm_allinone.EXTERNAL_ACCOUNT;
#                     ")#change the date here to get the sma data from database
#   
# smadat <- sqlQuery(ch, query_account)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\account_info.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

# query_twitter <- paste("
#                   SELECT * FROM tm_allinone.Z_DIGEST where year(posted_date) >='2013' and year(posted_date) <'2014';
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\twitters_info.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")



# query_twitter <- paste("
#                   SELECT count(distinct posting_account) as num_account, source FROM tm_allinone.Z_DIGEST where year(posted_date)>=2012 and year(posted_date)<=2014 group by source order by num_account DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_2014_num_acount.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct posting_account) as num_account, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2012 group by source order by num_account DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_num_acount.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct posting_account) as num_account, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2013 group by source order by num_account DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2013_num_acount.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct posting_account) as num_account, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2014 group by source order by num_account DESC;
#                        ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2014_num_acount.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)>=2012 and year(posted_date)<=2014 group by source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_2014_num_contents.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2012 group by source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_num_contents.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2013 group by source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2013_num_contents.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2014 group by source order by num_contents DESC;
#                        ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2014_num_contents.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

# query_twitter <- paste("
#                   SELECT posting_account, count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)>=2012 and year(posted_date)<=2014 group by posting_account, source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_2014_num_contents_account.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT posting_account, count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2012 group by posting_account, source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2012_num_contents_account.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT posting_account, count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2013 group by posting_account, source order by num_contents DESC;
#                     ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2013_num_contents_account.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
# 
# query_twitter <- paste("
#                   SELECT posting_account, count(distinct contents) as num_contents, source FROM tm_allinone.Z_DIGEST where year(posted_date)=2014 group by posting_account, source order by num_contents DESC;
#                        ")#change the date here to get the sma data from database
# 
# smadat <- sqlQuery(ch, query_twitter)
# fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\2014_num_contents_account.csv", sep="")
# write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

