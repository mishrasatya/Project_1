#----------------------------Spark_Funds--------------------------------#

setwd("D:/Upgrad/Case_Study_1_Spark_Funds")
#------------------------------------------------------------------------------------------#
#Table 1.1--Checkpoint 1#
#Loading companies and round2 files to R console#
companies <- read.delim("companies.txt",header = TRUE, sep = "\t",stringsAsFactors = FALSE)
round2 <- read.csv("rounds2.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)

#As company_permalink column in round2 dataframe has mixed cases for the same permalink, hence converting#
#conpamy_permalink column to uppercase, storing it in upper_permalink and then getting the unique values#
#Now calling dplyr package#
library(dplyr)
round2_mutate <- mutate(round2, upper_permalink = toupper(round2$company_permalink))
unique_round2 <- (length(unique(round2_mutate$upper_permalink)))

#using permalink from companies dataframe to find unique companies#
unique_companies <- (length(unique(companies$permalink)))

#Before merging companies and round2 dataframe to make master dataframe we have converted company_permalink#
#and permalink to all caps so that the merging can be done properly#
companies[,1] = toupper(companies[,1]) #Making the permalink to uppercase#
round2[,1] = toupper(round2[,1]) #Making the conpamy_permalink to uppercase#
master_frame <- left_join(round2,companies,by = c("company_permalink" = "permalink")) #Joining round2 and companies#

#-----------------------------------------------------------------------------------------#
#Table 2.1 Checkpoint 2#
#Finding NA in raised amount usd column of master_frame#
number_of_na <- sum(is.na(master_frame$raised_amount_usd))

#Replacing NA with Zero#
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

#-----------------------------------------------------------------------------------------#
#Table 3.1 Checkpoint 3#
#Finding average of funding type#
#This will answer the questions related to average funding amount for venture, angel, seed & private equity#
average_of_funding_types <- summarise(group_by(master_frame, funding_round_type), mean(raised_amount_usd,na.rm = TRUE))

#Converting to millions to find what funding type falls within 5 to 15 million#
#This conversion will easily show what type falls into the 5 to 15 million bucket#
convert_to_millions <- average_of_funding_types$`mean(raised_amount_usd, na.rm = TRUE)`/1000000
average_of_funding_types_in_millions <- cbind(average_of_funding_types, convert_to_millions)

#-----------------------------------------------------------------------------------------#
#Table 4.1 Checkpoint 4#
#We now know that venture type is the one where spark should invest#
#Hence subsetting master_frame to get line items related to only venture fund type#
venture_fund_type <- subset(master_frame, funding_round_type == "venture")

#Grouping and summarizing the venture fund type dataframe on a counrty code level by the basis of fund raised#
fundraised_by_countrycode <- summarise(group_by(venture_fund_type, country_code), sum(raised_amount_usd,na.rm = TRUE))

#After we have summarized venture fund type on a country_code level its time to#
#get the top 9 by fund raised, this is just the raw table#
top_9_raw <- top_n(fundraised_by_countrycode,9)

#Converting to sum of fund raised to millions and billions for better readability#
convertmillion <- top_9_raw$`sum(raised_amount_usd, na.rm = TRUE)`/1000000
convertbillion <- top_9_raw$`sum(raised_amount_usd, na.rm = TRUE)`/1000000000

#Here we are attaching convertmillion & convertbillion to top_9_raw to form a new dataframe called top_9#
top_9 <- cbind(top_9_raw,convertmillion,convertbillion)
arrange(top_9,desc(convertbillion))

#----------------------------------------------------------------------------------------#
#Checkpoint 5#
#Here we are loading the sector mapping file to R console#
sector_mapping_raw <- read.csv("mapping.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Loading tidyr package#
library(tidyr)

#using the gather function we are converting the wide format data to long format data#
sector_mapping_gather <- gather(sector_mapping_raw, Catagory, Catagory_value, Automotive_Sports:Social_Finance_Analytics_Advertising)

#Removing all the row that has a value of 0 in the newly created catagory_value column as row#
#that have 1 is what we require#
sector_mapping_gather <- sector_mapping_gather[!(sector_mapping_gather$Catagory_value == 0),]

#Removing the third column from the dataframe as it dosen't add any value any more#
sector_mapping_gather <- sector_mapping_gather[, -3]

#Using the gsub function, here we are replacing the characters which are typo errors in the mapping master#
#There are a total of four character strings that were replaced#
sector_mapping_gather$category_list <- gsub("0","na",sector_mapping_gather$category_list)
sector_mapping_gather$category_list <- gsub("nanotechnology","Nanotechnology",sector_mapping_gather$category_list)
sector_mapping_gather$category_list <- gsub("natural","Natural",sector_mapping_gather$category_list)
sector_mapping_gather$category_list <- gsub("navigation","Navigation",sector_mapping_gather$category_list)

#Mutating the column category_list to remove multipe category in each row#
#This is done as per the business rule set up by spark#
venture_fund_type <- mutate(venture_fund_type,category_list=sapply(strsplit(venture_fund_type$category_list, split='|', fixed=TRUE),function(x) (x[1])))

#Joining venture_fund_type and sector_mapping_gather for create the final cleansed output dataframe#
Venture_fund_final <- left_join(venture_fund_type,sector_mapping_gather,by = c("category_list" = "category_list"))

#-----------------------------------------------------------------------------------------#
#Table 6.1--Checkpoint 6#

#Using the venture fund final dataframe we are creating a new vector raised_usd_in_millions to store values in millions#
#Then we are combining the vector to venture fund final to form combined raw dataframe#
#After that we are taking the rows that fall in the bucket of 5 to 15 million and creating combined_final#
raised_usd_in_millions <- Venture_fund_final$raised_amount_usd/1000000
combined_raw <- cbind(Venture_fund_final,raised_usd_in_millions)
combined_final <- combined_raw[(combined_raw$raised_usd_in_millions >= 5.0) & (combined_raw$raised_usd_in_millions <= 15.0) & !is.na(combined_raw$raised_usd_in_millions), ]

#Here we are subsetting and creating three data frames , one each for the top three countries#
united_states <- subset(combined_final, country_code == "USA")
great_britain <- subset(combined_final, country_code == "GBR")
india <- subset(combined_final,country_code == "IND")

#To get the total count of investment Question-1#
#Using tally function we are getting the count and then sorting it#
total_countofinvestment <- tally(group_by(combined_final,country_code), sort = TRUE)

#To get the total amount of investment Question-2 #
#Using the summarise and group_by function we are getting the amount on a counrty code level#
total_amtofinvestment <- summarise(group_by(combined_final, country_code),sum(raised_amount_usd))

#For top 3 sectors and count of respective sector Question-3 to Question-8 #
#Using tally function we are getting the count and then sorting it#
sector_name_count_USA <- tally(group_by(united_states,Catagory), sort = TRUE)
sector_name_count_GRB <- tally(group_by(great_britain,Catagory), sort = TRUE)
sector_name_count_IND <- tally(group_by(india,Catagory), sort = TRUE)

#Finding the top two compananies to which investements were rendered for USA, GBR & IND#
#Here for the top company in USA we first summarise and group_by Catagory & name on united_states dataframe#
#From there we subset the dataframe on others catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
first_high_invest_USA <- summarise(group_by(united_states, Catagory,name), sum(raised_amount_usd))
USA_others <- subset(first_high_invest_USA,Catagory == "Others")
USA_others_summary <- summarise(group_by(USA_others,name), sum(`sum(raised_amount_usd)`))
first_high_invest_USA_final <- top_n(USA_others_summary,1)

#Here for the top company in USA we first summarise and group_by Catagory & name on united_states dataframe#
#From there we subset the dataframe on Social_Finance_Analytics_Advertising catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
second_high_invest_USA <- summarise(group_by(united_states, Catagory,name), sum(raised_amount_usd))
USA_SFAA <- subset(second_high_invest_USA,Catagory == "Social_Finance_Analytics_Advertising")
USA_SFAA_summary <- summarise(group_by(USA_SFAA,name), sum(`sum(raised_amount_usd)`))
second_high_invest_USA_final <- top_n(USA_SFAA_summary,1)

#Here for the top company in GBR we first summarise and group_by Catagory & name on great_britain dataframe#
#From there we subset the dataframe on others catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
first_high_invest_GRB <- summarise(group_by(great_britain, Catagory,name), sum(raised_amount_usd))
GRB_others <- subset(first_high_invest_GRB,Catagory == "Others")
GRB_others_summary <- summarise(group_by(GRB_others,name), sum(`sum(raised_amount_usd)`))
first_high_invest_GRB_final <- top_n(GRB_others_summary,1)

#Here for the top company in GBR we first summarise and group_by Catagory & name on great_britain dataframe#
#From there we subset the dataframe on Social_Finance_Analytics_Advertising catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
second_high_invest_GRB <- summarise(group_by(great_britain, Catagory,name), sum(raised_amount_usd))
GRB_SFAA <- subset(second_high_invest_GRB,Catagory == "Social_Finance_Analytics_Advertising")
GRB_SFAA_summary <- summarise(group_by(GRB_SFAA,name), sum(`sum(raised_amount_usd)`))
second_high_invest_GRB_final <- top_n(GRB_SFAA_summary,1)

#Here for the top company in IND we first summarise and group_by Catagory & name on india dataframe#
#From there we subset the dataframe on others catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
first_high_invest_IND <- summarise(group_by(india, Catagory,name), sum(raised_amount_usd))
IND_others <- subset(first_high_invest_IND,Catagory == "Others")
IND_others_summary <- summarise(group_by(IND_others,name), sum(`sum(raised_amount_usd)`))
first_high_invest_IND_final <- top_n(IND_others_summary,1)

#Here for the top company in IND we first summarise and group_by Catagory & name on india dataframe#
#From there we subset the dataframe on Social_Finance_Analytics_Advertising catagory#
#Then we again summarise and group_by on name from the subsetted dataframe to get the total amount given to each company#
#Using the top_n function we find the top most company#
second_high_invest_IND <- summarise(group_by(india, Catagory,name), sum(raised_amount_usd))
IND_SFAA <- subset(second_high_invest_IND,Catagory == "Social_Finance_Analytics_Advertising")
IND_SFAA_summary <- summarise(group_by(IND_SFAA,name), sum(`sum(raised_amount_usd)`))
second_high_invest_IND_final <- top_n(IND_SFAA_summary,1)

#-------------------------------------END OF CODE--------------------------------------#




