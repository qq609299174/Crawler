#Save html pages

#Change working directories to where AllHomeLinks.R is and load AllHomeLinks.R

setwd("C:/Users/STG/Desktop/RRRRRRRRRRRRRRRRRRRRRRRRRRR")#TYPE THE DIRECTORIES TO WHERE AllHomeLinks.R IS SAVED
AllHomeLinks <- dget("AllHomeLinks.R")

for (j in 1:length(AllHomeLinks)){
  Link <- AllHomeLinks[j]
  id <- regexpr("/[0-9]*_zpid/$",Link)
  zpid <-gsub("/|_zpid/","",regmatches(Link,id))
  
  DestFolder <- "C:/Users/STG/Desktop/RRRRRRRRRRRRRRRRRRRRRRRRRRR" #TYPE THE DIRECTORIES TO WHERE THE HTML PAGES WILL BE SAVED
  filename <- paste0 ("/",zpid,".html")
  destfile <- paste0(DestFolder, filename)
  download.file(Link, destfile)
}