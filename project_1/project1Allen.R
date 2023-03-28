# Project 1 R script
# data loading
# link to webpage with data: https://www.lendingclub.com/info/download-data.action
mainData <- read.csv(file = "C:\\Users\\Allen\\Desktop\\Data Science in R\\project1\\LoanStats3a.csv")

# data cleaning
mainData$member_id <- NULL
mainData$url <- NULL
mainData$desc <- NULL
mainData$purpose <- NULL
mainData$title <- NULL
mainData$zip_code <- NULL
mainData$addr_state <- NULL
mainData$mths_since_last_record <- NULL
mainData$initial_list_status <- NULL
mainData$out_prncp_inv <- NULL
mainData$total_pymnt <- NULL
mainData$recoveries <- NULL
mainData$collection_recovery_fee <- NULL
mainData$last_credit_pull_d <- NULL
mainData$collections_12_mths_ex_med <- NULL
mainData$mths_since_last_major_derog <- NULL
mainData$policy_code <- NULL
mainData$application_type <- NULL
mainData$annual_inc_joint <- NULL
mainData$dti_joint <- NULL
mainData$verification_status_joint <- NULL
mainData$acc_now_delinq <- NULL
mainData$tot_coll_amt <- NULL
mainData$tot_cur_bal <- NULL
mainData$open_acc_6m <- NULL
mainData[82:145] <- NULL
mainData[35:80] <- NULL
mainData$percent_laon_rec <- mainData$funded_amnt/mainData$loan_amnt

# imputing(kind of)
mainData$id <- 1:length(mainData$id)

# making of the plots to find interesting data woohoo
plot(mainData$loan_amnt, mainData$installment, main = "Loan AMount vs. Installments", xlab = "Loan Amount", ylab = "Installments")
plot(mainData$grade, mainData$loan_amnt, main = "Loan Amount vs. Grade", xlab = "Grade", ylab = "Loan Amount")
plot(mainData$grade, mainData$funded_amnt, main = "Funded Amount vs. Grade", xlab = "Grade", ylab = "Funded amount")
plot(mainData$loan_amnt, mainData$funded_amnt, main = "Loan Amount vs. Funded Amount", xlab = "Loan Amount", ylab = "Funded Amount")
plot(mainData$grade, mainData$percent_laon_rec, main = "Percent of Loan Received vs. Grade", xlab = "Grade", ylab = "Percent of Loan Received")
plot(mainData$sub_grade, mainData$funded_amnt, main = "Funded Amount vs. Sub Grade", xlab = "Sub Grade", ylab = "Funded Amount")
plot(mainData$verification_status, mainData$funded_amnt, type = "h", main = "Verification Status vs. Funded Amount", xlab = "Verification Status", ylab = "Funded Amount")
plot(mainData$grade, mainData$annual_inc, main = "Annual Income vs. Grade", xlab = "Grade", ylab = "Annual Income")
plot(mainData$loan_status, mainData$dti, main = "Loan Status vs. DTI", xlab = "Loan Status", ylab = "DTI")
plot(mainData$grade, mainData$total_rec_int, main = "Grade vs. Total Interest Recieved", xlab = "Grade", ylab = "Total Interest Recieved")
plot(mainData$sub_grade, mainData$total_rec_int, main = "Sub Grade vs. Total Interest Recieved", xlab = "Sub Grade", ylab = "Total Interest Recieved")

View(mainData)