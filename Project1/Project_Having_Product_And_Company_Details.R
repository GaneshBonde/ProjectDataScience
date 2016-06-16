#Intializing all the packages needed for this project
library(xlsx)
library(tidyr)
library(dplyr)
library(data.table)
library(EDAWR)
library(iterators)
require(devtools)

#Creating data File object having csv file information.
dataFile <- read.csv("C:\\personal\\Big_Data\\SpringBoard\\refine_original.csv")

#Converting dataFile table matrix form
M4 <- as.matrix(dataFile)

#Creating search criterias to search the company names.
MatchCase1 <- as.matrix(c("[pP][hH]","[aA][kK]","[vV][aA]","[uU][nN]","[pP][sS]$"))
MatchCase2 <- as.matrix(c("philips" ,"akzo" ,"van houten","unilever" ,"philips"))

#Creating function to search a text value and replace it with desired value. If blnBinary 
# is set to TRUE, then binary value 1 would be generated for matching text. Rest all
# the rows would be 0.
matchString <- function(dataOject,MatchCase1,MatchCase2,blnBinary){
  for(k in 1:length(dataOject)) {
      blnCheck=TRUE
      for (i in 1:length(MatchCase1)) {
        if(grepl(MatchCase1[[i]], dataOject[[k]])){
          blnCheck=FALSE
          if(blnBinary){
            dataOject[[k]] <-1
            
            break
          }else{
            dataOject[[k]]<-MatchCase2[[i]]
            break
          }
          
        }else{
          blnCheck=TRUE
        }
      }
    if(blnBinary & blnCheck){
      dataOject[[k]] <-0
    }
  }
  return(dataOject)
}

#Separating the product code and product number into separate columns  
# called product_code and product_number, containing
#the product code and number respectively
product_code <- as.matrix(select(separate(tbl_df(dataFile),Product.code...number,c("product_code","product_number"), sep="-"),product_code))
product_number <- as.matrix(select(separate(tbl_df(dataFile),Product.code...number,c("product_code","product_number"), sep="-"),product_number))

# Cleaning brand names and assigning new column company as object
company <-data.frame(company=as.matrix(lapply(M4[,1],matchString, MatchCase1,MatchCase2, FALSE)))




#Creating matrix having all the search criteria for products
MatchCaseProduct1 <- as.matrix(c("[p]","[v]","[x]","[q]"))
MatchCaseProductName <- as.matrix(c("Smartphone" ,"TV" ,"Laptop","Tablet"))




#Creating product category column having product names with respect to their product code
product_Category <-data.frame(product_Category=as.matrix(lapply(M4[,2],matchString, MatchCaseProduct1,MatchCaseProductName, FALSE)))

#Creating full address column required for geocoding
full_address<- as.matrix(select(unite(as.data.frame(M2),full_address,address,city,country,sep = ","),full_address))

#Creating dummy variables for company column for each company having binary values for 
# corresponding company values in the data
company_philips <- data.frame(company_philips=as.matrix(lapply(M4[,1],matchString, matrix(c(MatchCase1[1],MatchCase1[5]), nrow = 1, byrow = TRUE),NULL, TRUE)))
company_akzo <- data.frame(company_akzo=as.matrix(lapply(M4[,1],matchString, matrix(c(MatchCase1[2]), nrow = 1, byrow = TRUE),NULL, TRUE)))
company_van_houten <- data.frame(company_van_houten=as.matrix(lapply(M4[,1],matchString, matrix(c(MatchCase1[3]), nrow = 1, byrow = TRUE), NULL,TRUE)))
company_unilever <- data.frame(company_unilever=as.matrix(lapply(M4[,1],matchString, matrix(c(MatchCase1[4]), nrow = 1, byrow = TRUE), NULL, TRUE)))

#Creating dummy variables for product column for each product having binary values for 
# corresponding company values in the data
product_smartphone <- data.frame(product_smartphone=as.matrix(lapply(M4[,2],matchString, matrix(c("[p]"), nrow = 1, byrow = TRUE),matrix(c("Smartphone"), nrow = 1, byrow = TRUE), TRUE)))
product_tv <- data.frame(product_tv=as.matrix(lapply(M4[,2],matchString,  matrix(c("[v]"), nrow = 1, byrow = TRUE),matrix(c("TV"), nrow = 1, byrow = TRUE),TRUE)))
product_laptop <- data.frame(product_laptop=as.matrix(lapply(M4[,2],matchString, matrix(c("[x]"), nrow = 1, byrow = TRUE), matrix(c("Laptop"), nrow = 1, byrow = TRUE), TRUE)))
product_tablet <- data.frame(product_tablet=as.matrix(lapply(M4[,2],matchString, matrix(c("[q]"), nrow = 1, byrow = TRUE), matrix(c("Tablet"), nrow = 1, byrow = TRUE), TRUE)))

#Creating name object having names of the people from data
name=M4[,6]

# Printing all the information required after the desired cleaning and manupulation done
#on data.Following columns are being displayed
#Company, product_code, product_number, full_address, name, product_category, company_philips,
#company_akzo, company_van_houten, company_unilever, product_smartphone, product_tv,
#product_laptop,product_tablet
dt4 <- data.table(company,product_code,product_number,full_address,name,product_Category,company_philips,company_akzo, company_van_houten,company_unilever,product_smartphone, product_tv, product_laptop,product_tablet)

#Coerce data frames
dt4 <- data.frame(lapply(dt4, as.character), stringsAsFactors=FALSE)

#Writing all the cleaned data into csv file as desired
write.table(dt4, file = "C:\\personal\\Big_Data\\SpringBoard\\refine_clean.csv", sep = ",", col.names = NA,
            qmethod = c("escape","double"))

