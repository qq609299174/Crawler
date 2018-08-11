#Function ExtractLinks is used to extract all the home links from a searching-result page
#the input should be the link of a searching-result page
#This needs to be run before ExtractAllHomeLinks

ExtractLinks <- function(Link){
  #url <- read_html(Link) #page 1
  url <-read_html(curl(Link, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  #Extract all the links from page 1:
  URLtemp <- html_nodes(x=url,css="a") # find all links
  HomeLinkstemp <- URLtemp %>% html_attr("href")# get the url
  HomeLinkstemp <- grep("_zpid/$",HomeLinkstemp,value=TRUE)
  if (length(HomeLinkstemp)== 0){
    AllHomeLinks <- HomeLinkstemp
    return(AllHomeLinks)
  }else{
    
  HomeLinks <- paste0("https://www.zillow.com/",HomeLinkstemp)
  AllHomeLinks<- HomeLinks #AllHomeLinks_Type1 is gonna use to contrain all the home links from home type 1
  
  #If there are more than one result page in the searching result:
  indListItem <- html_text(html_nodes(x = url, css= "li"))
  temp <- which(indListItem  == "Next")
  temp <- temp-1
  totalPageNum <- indListItem[temp] #the page number in the list of result
  
  if (!length(totalPageNum)==0){
  #Continue to extract from page 2 (2_p) to the last page (totalPageNum_p)
  for (j in 2:totalPageNum){
    page <- paste0(j,"_p/")
    NewLink <- paste0(Link, page)
    Newurl <- read_html(NewLink)
    URLtemp <- html_nodes(x=Newurl,css="a") # find all links
    Linkstemp <- URLtemp %>% html_attr("href")# get the url
    Linkstemp <- grep("_zpid/$",Linkstemp,value=TRUE) #filter for home links
    HomeLinks <- paste0("https://www.zillow.com/",Linkstemp)
    AllHomeLinks <-c(AllHomeLinks,HomeLinks)
    }
  }
    return(AllHomeLinks)
  }
}