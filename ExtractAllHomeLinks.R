#Extract All the Home Links _version 3
#Extract home links according to home type
#Need to run function ExtrctLinks first,its input should be the link of a searching-result page

#####################################  Type 1: Lots/Land  #####################################

#https://www.zillow.com/homes/for_sale/Indianapolis-IN/land_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/
#.../land_type/...
#386 homes for sale. 
#Do not need to categorize homes according to home size, 
#because the total number of homes in this house type is smaller than 500

HomeType1 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/land_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T1 <- ExtractLinks(HomeType1)

#####################################  Type 2: Manufactured  #####################################

#https://www.zillow.com/homes/for_sale/Indianapolis-IN/mobile_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/
#.../mobile_type/...
#9 homes for sale, totalPageNum = 1
#Do not need to categorize homes according to home size, 
#because the number of homes is smaller than 500

HomeType2 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/mobile_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T2 <- ExtractLinks(HomeType2)


#####################################  Type3: Townhouse  #####################################

#https://www.zillow.com/homes/for_sale/Indianapolis-IN/townhouse_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/
#195 homes for sale
#Do not need to categorize homes according to home size, 
#because the number of homes is smaller than 500

HomeType3 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/townhouse_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T3 <- ExtractLinks(HomeType3)


#####################################  Type 4: Condos/Co-ops  #####################################
#https://www.zillow.com/homes/for_sale/Indianapolis-IN/condo_type/32149_rid/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/
#3000 homes for sale
#Since the total number of homes is bigger than 500, 
#the homes in this house type need to be categorized according to home size:
#0-1000 sqft: ~418 homes; #2000-max sqft: ~431 homes;
#Between 1000-2000, need to loop in every 100 increase: /1000-1100_size/ ...... /1900-2000_size/

#0-1000 sqft: ~418 homes
HomeType4 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/condo_type/32149_rid/0-1000_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T4 <- ExtractLinks(HomeType4)

#2000-max : ~431 homes
HomeType4 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/condo_type/32149_rid/2000-_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T4 <-c(result_T4,ExtractLinks(HomeType4))

#Between 1000-2000, need to loop in every 100 increase: /1000-1100_size/ ...... /1900-2000_size/
for (j in 1:10){
  HomeType4 <- paste0("https://www.zillow.com/homes/for_sale/Indianapolis-IN/condo_type/32149_rid/",1000+(j-1)*100,"-",1000+j*100,"_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/")
  result_T4 <- c(result_T4,ExtractLinks(HomeType4))
}


#####################################  Type 5: Apartment  #####################################
#806 homes for sale
#Since the number of homes is bigger than 500, 
#the homes in this type need to be categorized according to home size

#0-1500 sqft
HomeType5 <-"https://www.zillow.com/homes/for_sale/Indianapolis-IN/apartment_duplex_type/32149_rid/0-1500_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T5 <- ExtractLinks(HomeType5)

#1500- Max sqft
HomeType5 <-"https://www.zillow.com/homes/for_sale/Indianapolis-IN/apartment_duplex_type/32149_rid/1500-_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T5 <- c(result_T5,ExtractLinks(HomeType5))


#####################################  Type 6: House  #####################################
# 28,134 homes for sale
#Since the number of homes is bigger than 500, 
#the homes in this type need to be categorized according to home size

#There are 637 homes that have "-- sqft",
#so I categorize them according to lot size: 2000-_lot & -2000_lot

#2000-_lot:
HomeType6 <- "https://www.zillow.com/homes/for_sale/Indianapolis-IN/house_type/32149_rid/0-1_size/2000-_lot/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T6 <- ExtractLinks(HomeType6)

#-2000_lot:
HomeType6 <-"https://www.zillow.com/homes/for_sale/Indianapolis-IN/house_type/32149_rid/0-1_size/-2000_lot/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/"
result_T6 <- c(result_T6,ExtractLinks(HomeType6))

#Categorize the rest homes with 20 interval in home size
MaxSize <- 211590340
inc <-seq(1,MaxSize,by=20)
for (j in 1:(length(inc)-1)){
  HomeType6 <- paste0("https://www.zillow.com/homes/for_sale/Indianapolis-IN/house_type/32149_rid/",inc[j],"-",inc[j+1],"_size/globalrelevanceex_sort/39.975015,-85.804253,39.611507,-86.460686_rect/10_zm/0_mmm/1_rs/")
  result_T6 <- c(result_T6,ExtractLinks(HomeType6))
}

#####################################  To Sum up  #####################################

AllHomeLinks <- c(result_T6,result_T5,result_T4,result_T3,result_T2,result_T1)
AllHomeLinks <- unique(AllHomeLinks)
   