#Intializing all the packages needed for this project
library(data.table)
#install.packages("varhandle")

#Creating data File object having csv file information.
dataFile <- read.csv("C:\\titanic_original.csv", stringsAsFactors=FALSE)

#Converting dataFile table matrix form

titanic_data_table <- as.data.table(dataFile)

#Creating search criterias to search the company names.
match_case_1 <- "S"

# Creating function to search a null value and replace it with desired match value. If 
# the bln_binary is set to TRUE, then binary value 1 would be assigned for data_object's 
# values which are not NULL and rest all the values would be 0.
match_string_for_null <- function(data_Object,match_case_1,bln_binary){
  for(k in 1:length(data_Object)) {
      if( is.na(data_Object[[k]]) || data_Object[[k]]=="NA" || data_Object[[k]]=="" || is.null(data_Object[[k]] )){
          if(bln_binary){
            data_Object[[k]]<-0
            
          }else{
            data_Object[[k]]<-match_case_1
            
          }
      }else{
        if(bln_binary){
          data_Object[[k]]<-1
          
        }
      }
  }
  return(data_Object)
}


# Replacing missing values in embarked column with value S.
# Replacing missing values in age column by mean value of the remaining values 
# present.
# Assigning NA to those who could not make it to life boat in Lifeboat column.
# Creating new column having 1/0 values. 1 would be assigned if cabin number is
# present else 0 would be assigned.
titanic_data_table_cleaned <- transform(titanic_data_table,age = match_string_for_null(age,mean(age,na.rm = TRUE),FALSE),
                 boat= match_string_for_null(titanic_data_table$boat,"NA",FALSE),
                 has_cabin_member=match_string_for_null(titanic_data_table$cabin,"",TRUE),
                 embarked=(match_string_for_null(titanic_data_table$embarked,match_case_1,FALSE))) 


#Second way of doing same thing. To exceute it, you will to uncomment it
# Replacing missing values in embarked column with value S.
#titanic_data_table$embarked <- data.table(match_string_for_null(titanic_data_table$embarked,match_case_1,FALSE))

# Replacing missing values in age column by mean value of the remaining values 
# present.
#titanic_data_table$age <- data.table(match_string_for_null(titanic_data_table$age,mean(titanic_data_table$age,na.rm = TRUE),FALSE))

# Assigning NA to those who could not make it to life boat in Lifeboat column.
#titanic_data_table$boat <- data.table(as.character(match_string_for_null(titanic_data_table$boat,"NA",FALSE),stringsAsFactors = FALSE))

# Creating new column having 1/0 values. 1 would be assigned if cabin number is
# present else 0 would be assigned.
#titanic_data_table$has_cabin_number <- data.table(match_string_for_null(titanic_data_table$cabin,"",TRUE))
#titanic_data_table_cleaned <-titanic_data_table




# Printing all the information required after the desired cleaning and 
# manupulation done on data.Following columns are being displayed
# sex, age, sibsp, parch, ticket, fare, cabin, embarked, boat, body,home.dest, has_cabin_member
dt4 <- data.table(titanic_data_table_cleaned)

#Coerce data frames
dt4 <- data.frame(lapply(dt4, as.character), stringsAsFactors=FALSE)

#Writing all the cleaned data into csv file as desired
write.table(dt4, file = "C:\\personal\\Big_Data\\SpringBoard\\Assignment\\titanic_clean.csv", sep = ",",row.names = FALSE,
            qmethod = c("escape","double"))

