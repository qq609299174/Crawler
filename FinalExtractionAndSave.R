## Final Submission_v10 ##

#R packages needed to be installed:
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("xml2")
#install.packages("rvest")
#install.packages("stringr")
#install.packages("curl")
#install.packages("ggvis")
#install.packages("knitr")
#install.packages("xtable")
library(plyr)
library(dplyr)
library(reshape2)
library(xml2)
library(rvest)
library(stringr)
library(curl)
library(xtable)
library(ggvis)
library(knitr)
options(digits = 4)


#set working directories
setwd("C:/Users/STG/Desktop/RRRRRRRRRRRRRRRRRRRRRRRRRRR")
AllHomeLinks <-dget("AllHomeLInks.R")

#Num <- length(AllHomeLinks)
StartNum <- 1
EndNum <- length(AllHomeLinks)
ResultFile <- matrix("NA",22,EndNum-StartNum +1)
HomeLinks <- AllHomeLinks[StartNum:EndNum]

for (n in 1:(EndNum-StartNum +1)){
  
  Link <- HomeLinks[n]
  #url <- read_html(Link)
  url <-read_html(curl(Link, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  indListItem <- html_text(html_nodes(x = url, css= "li"))
  
  #1. Zillow_Home_ID
  HomeIDtemp<-grep("Zillow Home ID",indListItem,value=TRUE)
  Zillow_Home_ID <- gsub("Zillow Home ID: ","",HomeIDtemp)
  #Zillow_Home_ID <- as.numeric(ZillowHomeID)
  if (length(Zillow_Home_ID)== 0){Zillow_Home_ID <- NA}
  if (identical(Zillow_Home_ID,"")) {Zillow_Home_ID <- NA}
  
  #2. Current_Price
  CurrentPrice <- html_nodes(x = url, css= ".main-row span") %>% html_text()
  Current_Price <- gsub("    | \\$","",CurrentPrice)[1]
  if (length(Current_Price)== 0){Current_Price <- NA}
  if (identical(Current_Price,"")){Current_Price <- NA}
  
  #3. Price_Each_SQFT
  PriceSQFT <- html_nodes(x = url, css= ".top-facts li") %>% html_text()
  PriceSQFT <- grep("Price/sqft: \\$", PriceSQFT,value= TRUE)
  Price_Each_SQFT <- gsub("Price/sqft: \\$","",PriceSQFT)
  if (length(Price_Each_SQFT)== 0){Price_Each_SQFT <- NA}
  if (identical(Price_Each_SQFT,"")) {Price_Each_SQFT <- NA}
  
  #4. Price_History
  # Extract tables (Price History and Tax History)
  #we need to use a program called phantomjs
  connection <- "zillow_home.js"
  
  #writing lines from the webpage given to the url to a js file
  #code inside the sprintf function (which is a wrapper) is written in Java
  writeLines(sprintf("var page = new WebPage()
                     var fs = require('fs');
                     
                     
                     page.open('%s', function (status) {
                     just_wait();
                     });
                     
                     function just_wait() {
                     setTimeout(function() {
                     fs.write('zillow2.html', page.content, 'w');
                     phantom.exit();
                     }, 3000);
                     }",Link),con=connection)

  
  
  system("./phantomjs zillow_home.js")
  table_Data <- "zillow2.html" %>% read_html() %>% html_nodes(xpath = "//table") %>% html_text()
  
  #4. Price_History
  Price_history <- table_Data[gregexpr("^DateEvent[aA-zZ | 0-9]*",table_Data[1:length(table_Data)]) != -1]
  #Price_history <- gsub("Report(.)*[^(../../../)]","",Price_history)
  datetemp <- unlist(gregexpr("[0-9]{2}/[0-9]+/[0-9]{2}",Price_history))
  r <- 1
  row_details<- character(0)
  rowlength <- length(datetemp)
  if(rowlength > 1){
    for(i in 2 : rowlength){
      row_details[r] <- substr(Price_history,datetemp[i-1],datetemp[i]-1)
      r <- r+1
      if(i == rowlength){
        row_details[r] <- substring(Price_history,datetemp[i])
        row_details[r] <- sub("\\.{3}.*","",row_details[r])
      }
    }
  }else{
    if(rowlength < 1){
      row_details <- NA
    }else{
      rtemp <- substring(Price_history,datetemp)
      row_details <- sub("\\.{3}.*","",rtemp)
    }
  }
  row_details <- gsub("([\\.]|Report).*[\\.$]","",row_details)
  
  
  date <- unlist(regmatches(Price_history,gregexpr("[0-9]{2}/[0-9]+/[0-9]{2}",Price_history)))
  eventtemp <- gsub("../../..|\\$.*", "", row_details)
  event <- gsub("--","",eventtemp)
  pricetemp1 <- unlist(regmatches(Price_history,gregexpr("\\$([0-9]){1,3}((,[0-9]{1,3})+)?",Price_history)))
  pricetemp2 <- gsub("\\$","",pricetemp1)
  row_detailstemp1 <- gsub("More Less ","",row_details)
  row_detailstemp2 <- str_trim(row_detailstemp1)
  agenttemp1 <- regmatches(row_details, regexpr("-[a-zA-Z]*( ?)[a-zA-Z]*", row_detailstemp2))
  
  pricetemp3 <-character(0)
  agenttemp2 <- character(0)
  agenttemp3 <- character(0)
  agent <- character(0)
  price <- numeric(0)
  if(length(date) != 0){
    pricetemp3 <- gsub(",","",pricetemp2)
    agenttemp2 <- gsub("--","",agenttemp1)
    agenttemp3 <- gsub("-","",agenttemp2)
    agent <- gsub("[rR]ecord","",agenttemp3)
    price <- as.numeric(pricetemp3)
  }
  #Retriving the sqft 
  sqtemp <- html_text(html_nodes(x = url, css= ".addr_bbs"))
  sqfttemp <- sqtemp[gregexpr(".*sqft",sqtemp[1:length(sqtemp)]) != -1]
  if(regexpr("[0-9]",sqfttemp) != -1){
    sqft <- gsub(" sqft","",sqfttemp)
    sqft <- gsub(",","",sqft)
    
    sqft <- as.numeric(sqft)
  }else{
    sqft <- NA
  }  
  #retriving the $/sqft
  pricepersqft <- price/sqft
  price_per_sqft <- gsub("(\\.).*","",pricepersqft)
  
  pricehistory <- character(0)
  for(i in 1 : length(date)){
    pricehistory <- c(pricehistory,paste(date[i],event[i],price[i],price_per_sqft[i],agent[i],sep="_"))
  }
  Price_History <- paste(pricehistory,collapse = ";")
  
  #5. Tax_History
  Tax_history <- table_Data[gregexpr("YearProperty[aA-zZ | 0-9]*",table_Data[1:length(table_Data)]) != -1]
  ##row <- gregexpr("[0-9].*?[%|-]",Tax_history)
  temp_year <- unlist(gregexpr("[0-9]{4}",Tax_history))
  if(length(temp_year) != 0){
    year <- unlist(regmatches(Tax_history,gregexpr("[0-9]{4}",Tax_history)))
    #Tax_year <- year[[1]]
    #pricestemp <- gregexpr("\\$[0-9]+((,[0-9]+)+)?",Tax_history)
    taxprice<-unlist(regmatches(Tax_history,gregexpr("\\$[0-9]+((,[0-9]+)+)?",Tax_history)))
    #taxprice <- taxpricetemp[[1]]
    property_tax <- taxprice[seq(1,length(taxprice),by=2)]
    Tax_assessment <- taxprice[seq(2,length(taxprice),by=2)]
    
    #Rows are concatinated to a single element
    tax_history_rows <- character(0)
    for(i in 1:length(year)){
      tax_history_rows <- c(tax_history_rows,paste(year[i],property_tax[i],Tax_assessment[i],sep="_"))
    }
    Tax_History <- paste(tax_history_rows,collapse = ";")
  }else{
    Tax_History <- NA
  }
  
  #6. URL
  URL <- Link
  
  #7. Address
  Address <-html_nodes(x = url, css= ".addr .notranslate") %>% html_text()
  if (length(Address)== 0){Address <- NA}
  if (identical(Address,"")) {Address <- NA}
  
  #8. Zipcode
  temp <- substr(Address,gregexpr(",",Address)[[1]][2],nchar(Address))
  Zipcode <- gsub(", IN | ","",temp)
  #Zipcode <- as.numeric(Zipcode)
  if (length(Zipcode)== 0){Zipcode <- NA}
  if (identical(Zipcode,"")) {Zipcode <- NA}
  
  #9. Neighborhood
  Ntemp <-html_nodes(x = url, css= "#hdp-neighborhood h2") %>% html_text()
  Neighborhood <- gsub("Neighborhood: ","",Ntemp[1])
  if (length(Neighborhood)== 0){Neighborhood <- NA}
  if (identical(Neighborhood,"")) {Neighborhood <- NA}
  
  #10. School
  Stemp <-html_nodes(x = url, css= "#nearbySchools .clearfix , .gs-rating-6") %>% html_text()
  Stemp <- gsub("[\n]","",Stemp) #get rid of "\n"
  Stemp <- gsub("out of 10| mi", "",Stemp)
  Stemp <- str_trim(Stemp)
  #for (j in 1:length(Stemp)){
    # <- regexpr(" ", Stemp[j])
    #if (!is.na(inx) & inx == -1){
     # Stemp <- Stemp <- Stemp[-j]
     # break
   # }
  #}
  temp <-strsplit(Stemp,"[ ]{5,} ")
  School <- paste(sapply(temp,paste,collapse = "_"),collapse =";")
  #gsub("\\s+", "_", gsub("^\\s+|\\s+$", "",S))
  if (length(School)== 0){School <- NA}
  if (identical(School,"")) {School <- NA}
  
  #11.Lot_Size
  LStemp<-grep("Lot:",indListItem,value=TRUE)
  LStemp <- sub("Lot: ","",LStemp)
  if (length(LStemp)== 0){
    Lot_Size <- NA
  } else{
    if (grepl("acres", LStemp)==TRUE){
      num <- as.numeric(sub(" acres","",LStemp))
      numSF <- 43560* num # convert acres to square feet
      Lot_Size <- as.character(numSF)
    }else{
      Lot_Size <- sub(" sqft","",LStemp)
    }
  }
  if (length(Lot_Size)== 0){Lot_Size <- NA}
  if (identical(Lot_Size,"")) {Lot_Size <- NA}
  
  #12. Floor_Size
  FStemp <- grep("Floor size:",indListItem,value= TRUE)
  Floor_Size <- gsub("Floor size: | sqft", "", FStemp)
  #FStemp <- as.numeric(FStemp)
  if (length(Floor_Size)== 0){Floor_Size <- NA}
  if (identical(Floor_Size,"")) {Floor_Size <- NA}
  
  #13. MLS_Number
  MLStemp <- grep("MLS #:",indListItem,value= TRUE)
  MLS_Number <- sub("MLS #: ", "", MLStemp)
  # MLS # might be unavailable:
  if (length(MLS_Number)== 0){MLS_Number <- NA}
  if (identical(MLS_Number,"")) {MLS_Number <- NA}
  
  #14. Parcel_Number
  PNtemp <- grep("Parcel #", indListItem, value=TRUE)
  Parcel_Number <- sub("Parcel #: ","",PNtemp)
  if (length(Parcel_Number)== 0){Parcel_Number <- NA}
  if (identical(Parcel_Number,"")) {Parcel_Number <- NA}
  
  #15. Stories
  Stemp <- grep("Stories:", indListItem, value=TRUE)
  Stories <- sub("Stories: ","",Stemp)
  if (length(Stories)== 0){Stories <- NA}
  if (identical(Stories,"")) {Stories <- NA}
  
  #16. Built_Year
  BYtemp <- grep("Built in", indListItem, value = TRUE)
  Built_Year <- sub("Built in ", "", BYtemp)
  if (length(Built_Year)== 0){Built_Year <- NA}
  if (identical(Built_Year,"")) {Built_Year <- NA}
  
  #17. Last_Remodel_Year
  LRYtemp <- grep("Last remodel year", indListItem,value = TRUE)
  Last_Remodel_Year <- sub("Last remodel year: ","", LRYtemp)
  if (length(Last_Remodel_Year)== 0){Last_Remodel_Year <- NA}
  if (identical(Last_Remodel_Year,"")) {Last_Remodel_Year <- NA}
  
  #18. House_Type
  HTtemp <-html_text(html_nodes(x = url, css= ".top-facts li"))
  House_Type <- grep("Family",HTtemp,value=TRUE)
  if (length(House_Type)== 0){House_Type <- NA}
  if (identical(House_Type,"")) {House_Type <- NA}
  
  #19. Structure_Type
  STtemp <- html_text(html_nodes(x = url, css= ".z-moreless-content+ .z-moreless-content li"))
  STtemp <- grep("Structure type",STtemp,value=TRUE)
  Structure_Type <- sub("Structure type: ","", STtemp)
  if (length(Structure_Type)== 0){Structure_Type <- NA}
  if (identical(Structure_Type,"")) {Structure_Type <- NA}
  
  #20. Bath_Num
  BNtemp <- html_text(html_nodes(x = url, css= ".addr_bbs"))
  BNtemp <- grep("bath", BNtemp,value = TRUE)
  Bath_Num <- gsub(" bath| baths","", BNtemp)
  if (length(Bath_Num)== 0){Bath_Num <- NA}
  if (identical(Bath_Num,"--")){Bath_Num <- NA}
  
  #21. Bed_Num
  BedNtemp <- html_text(html_nodes(x = url, css= ".addr_bbs"))
  BedNtemp <- grep("bed", BedNtemp,value = TRUE)
  Bed_Num <- gsub(" bed| beds","", BedNtemp)
  if (length(Bed_Num)== 0){Bed_Num <- NA}
  if (identical(Bed_Num,"--")){Bed_Num <- NA}
  
  #22. Room_Num
  RNtemp <- grep("Room count",indListItem,value=TRUE)
  Room_Num <- sub("Room count: ","",RNtemp)
  if (length(Room_Num)== 0){Room_Num <- NA}
  if (identical(Room_Num,"--")){Room_Num <- NA}
  
  results <- c(Zillow_Home_ID,Current_Price,Price_Each_SQFT,Price_History,Tax_History,URL,Address,Zipcode,Neighborhood,School, Lot_Size,Floor_Size,MLS_Number,Parcel_Number,Stories,Built_Year,Last_Remodel_Year,House_Type,Structure_Type,Bath_Num,Bed_Num,Room_Num)
  ResultFile[,n] <- results
  print(n)
}
dput(ResultFile,file="ResultFile.R")



###Organize the ResultFile and Save it into a txt file

ResultFile <- t(ResultFile) #transpose matrix

temp <- apply(ResultFile,1,function(x){identical(x,rep("NA",22))}) #remove the rows of "NA"
ResultFile <- ResultFile[-temp,]

colnamesRESULT <- c("Zillow_Home_ID","Current_Price","Price_Each_SQFT","Price_History","Tax_History","URL","Address","Zip_Code","Neighborhood","School","Lot_Size","Floor_Size","MLS_Number","Parcel_Number","Stories","Built_Year","Last_Remodel_Year","House_Type","Structure_Type","Bath_Num","Bed_Num","Room_Num")
colnames(ResultFile)<- colnamesRESULT
RESULT <-data.frame(RESULT)
write.csv(RESULT,"IndianapolisIN.csv")
write.table(RESULT, "Indianapolis.txt", sep="\t")



