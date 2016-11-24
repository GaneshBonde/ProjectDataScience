library(recommenderlab)
library(ggplot2)
library(mgcv)
library(XML)

f=xmlParse("C:/personal/Big_Data/SpringBoard/Project/Project 1/movielists_20130821/movielists_20130821/sample.xml")
f2=xmlParse("C:/personal/Big_Data/SpringBoard/Project/Project 1/movielists_20130821/movielists_20130821/imdb-normalised.xml")
f3=xmlParse("C:/personal/Big_Data/SpringBoard/Project/Project 1/movielists_20130821/movielists_20130821/imdb-colisted.graphml")
df=xmlToDataFrame("C:/personal/Big_Data/SpringBoard/Project/Project 1/movielists_20130821/movielists_20130821/sample.xml")

df <- data.frame(id=integer(), key=character(),movieName=character(),
                 stringsAsFactors = FALSE)

df2 <- data.frame(source=integer(), key=character(),target = integer(),edge=double(),
                 stringsAsFactors = FALSE)
Sys.time()
xml_data2 <- xmlToList(f2)
Sys.time()
xml_data4 <- xmlToList(f3)
Sys.time()


count <- 1
count2 <- 1
Sys.time()
for (xml_data2 in 1:length(xml_data4$graph)) {
  if(count < 40286){
  df[count,1] <- xml_data4$graph[count]$node$.attrs[[1]]
  df[count,2] <- xml_data4$graph[count]$node$data$.attrs[[1]]
  df[count,3] <- xml_data4$graph[count]$node$data$text
  }else {
    df2[count2,1] <- xml_data4$graph[count]$edge$.attrs[[1]]
    df2[count2,2] <- xml_data4$graph[count]$edge$.attrs[[2]]
    df2[count2,3] <- xml_data4$graph[count]$edge$.attrs[[1]]
    df2[count2,4] <- xml_data4$graph[count]$edge$data$text
    count2 <- count2 + 1
  }
  print(count)
  count <- count +1
}
Sys.time()

