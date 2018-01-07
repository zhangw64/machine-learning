library(sqldf)
library(data.table)
DF <- read.csv.sql("ProjectTrainingData.csv", sql = "select * from file order by random() limit 100000")
fwrite(DF,  file ="Sample100000.csv")
# reference: https://stackoverflow.com/questions/22261082/load-a-small-random-sample-from-a-large-csv-file-into-r-data-frame