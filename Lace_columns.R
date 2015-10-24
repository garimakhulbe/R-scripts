columns_to_include <- c("rln","DXCCS_P","DXCCS_2",
                        "DXCCS_3", "DXCCS_4",
                        "DXCCS_5","DXCCS_6","DXCCS_7",
                        "DXCCS_8","DXCCS_9","DXCCS_10",
                        "DXCCS_11","DXCCS_12","DXCCS_13","DXCCS_14","DXCCS_15",
                        "DXCCS_16","DXCCS_17","DXCCS_18",
                        "DXCCS_19","DXCCS_20","DXCCS_21"
                        ,"DXCCS_22","DXCCS_23","DXCCS_24","DXCCS_25","los",
                        "srcroute", "admtdate",
                        "dschdate", "next_admit_date","thirtyday" )

CCS_CODE_COLUMN <- c("DXCCS_2","DXCCS_3", "DXCCS_4",
                     "DXCCS_5","DXCCS_6","DXCCS_7",
                     "DXCCS_8","DXCCS_9","DXCCS_10",
                     "DXCCS_11","DXCCS_12","DXCCS_13","DXCCS_14","DXCCS_15",
                     "DXCCS_16","DXCCS_17","DXCCS_18",
                     "DXCCS_19","DXCCS_20","DXCCS_21"
                     ,"DXCCS_22","DXCCS_23","DXCCS_24","DXCCS_25")



read_oshpd_data <- function(filename, admit_date, discharge_date) {
  data_frame <- read.csv(filename, header=T, na.strings="")
  data_frame[,admit_date] <- as.Date(data_frame[, c(admit_date)], "%m/%d/%Y")
  data_frame[,discharge_date] <- as.Date(data_frame[, c(discharge_date)], "%m/%d/%Y")
  row_order <- order(data_frame$rln, data_frame[,admit_date])
  t <- data_frame[row_order,]
  
  next_admit_date <- c()
  prev_rln_id <- as.character(t[1,"rln"])
  for(i in seq(2,nrow(t))) {
    row <- t[i,]
    rln_id <- as.vector(row[,"rln"])
    a_date <- as.Date(row[,admit_date], "%m-%d-%Y")
    d_date <- row[, discharge_date]
    #print(as.character(rln_id))
    if (rln_id == prev_rln_id) {
      #print(a_date)
      next_admit_date <-c (next_admit_date, as.character(a_date))
    }
    else
      next_admit_date <- c(next_admit_date, NA)
    prev_rln_id <- rln_id
  }
  next_admit_date <- c(next_admit_date, NA)
  t$next_admit_date <- next_admit_date
  t$thirtyday <- as.factor(as.numeric(as.numeric(as.Date
  (t$next_admit_date) - as.Date(t$dschdate)) <= 30))
  
  return(t)
}

new_table <- function(data_frame,columns_to_include, file.name2)
{
  data_frame <- data_frame[,columns_to_include]
  write.csv(data_frame, file.name2)
  
  return(data_frame)
  
}

# This is helper function to find all the comorbidity names for each of ccs codes
# given in the code_vector
# ccs_code_vector (vector) : vector or list of ccs codes
# ccs_to_morbid (data.frame) : A data frame with first column containing the 
# ccs code and second column containing the comorbid name.
find_chf <- function(ccs_code_vector, ccs_to_morbid) {
  morbids <- c()
  for (ccs_code in ccs_code_vector) {
    if (ccs_code %in% ccs_to_morbid[,1]) {
      name <- as.character(ccs_to_morbid[match(ccs_code, ccs_to_morbid[,1]), 2])
      morbids<-c(morbids, name)
      }
    }
  return(morbids)
  }

# This function takes two files:
# patient_data (data.frame) : is the data frame containing the ccs codes , rln, admit date, 
# discharge_date, next_admit_date.
# ccs_to_morbid_file (string) : Is the name of file containing ccs code and comorbidities name. The first
# column should be the ccs code and second column should be the comorbidity name
#
# For each of the row( a single patient), this function takes all the ccs codes and get the corresponding
# comorbidity names. It create columns for each of the comorbidity. If a 
# comorbidity is present for a patient then it marks it as 1.
# After computing all this information it save this information to the files specified
# by the out_file_name.

find_and_make_chf <- function(patient_data, ccs_to_morbid_file, out_file_name) {

  ccs_to_morbid <- read.csv(ccs_to_morbid_file, header=T,quote="") # Reads the ccs to morbid csv file
  
  # find all the comorbid names from ccs_to_morbid and create a new column
  # for each of these in the patient data
  for(new_col in as.character(unique(ccs_to_morbid[,2]))) {
    patient_data[,new_col] <- 0
  }
  
  # Now go over each row in the the patient data
  for (i in seq(nrow(patient_data))) {
    patient = patient_data[i,] # Pick ith row data
    codes <- patient[, CCS_CODE_COLUMN] # CCS_CODE_COLUMN (defined at top) is the name of colums for which we wish to find the comorbid names
    morbids <- find_chf(codes, ccs_to_morbid) # Given list of ccs code find their corresponding mordid name
 #   print (morbids)
    # for each of the morbid names found set the column value to 1
    for (m in morbids){
      patient_data[i, m] <- 1
    }
  }
  # Now write the patient_data to file specified by the out_file_name
  write.csv(patient_data, out_file_name, row.names=F)
  
  # Now return the patient_data which can be used.
  return( patient_data)
  }

create_data_frame_for_lace_script <- function (data_frame){
  lace_data_frame = data.frame(PID=1:nrow(data_frame), admittype=1:nrow(data_frame), 
                               LOS=1:nrow(data_frame),
                               dischargeDT=1:nrow(data_frame), admitDT=1:nrow(data_frame), 
                               thirtyday=1:nrow(data_frame),dementia=1:nrow(data_frame),
                               chf=1:nrow(data_frame),compdiabetes=1:nrow(data_frame),  
                               copd=1:nrow(data_frame), liverdisease=1:nrow(data_frame),
                               cancer=1:nrow(data_frame), renalfailure=1:nrow(data_frame),
                               leukemia=1:nrow(data_frame), myocardial=1:nrow(data_frame), cerebro=1:nrow(data_frame),
                               preipheral=1:nrow(data_frame), uncompdiabetes=1:nrow(data_frame), 
                               hivaids=1:nrow(data_frame))
  
  lace_data_frame$PID <- data_frame$rln
  lace_data_frame$admittype <- data_frame$srcroute
  lace_data_frame$LOS <- data_frame$los
  
  lace_data_frame$dischargeDT <- data_frame$dschdate
  lace_data_frame$admitDT <- data_frame$admtdate
  lace_data_frame$thirtyday <- data_frame$thirtyday
  lace_data_frame$dementia <- data_frame$dementia
  lace_data_frame$chf<- data_frame$chf
  lace_data_frame$compdiabetes<- data_frame$compdiabetes
  lace_data_frame$copd <- data_frame$copd
  lace_data_frame$liverdisease <- data_frame$liverdisease
  lace_data_frame$cancer <- data_frame$cancer
  lace_data_frame$renalfail <- data_frame$renalfailure
  lace_data_frame$leukemia <- data_frame$leukemia
  lace_data_frame$myocardial <- data_frame$myocardial
  lace_data_frame$cerebro <- data_frame$cerebro
  lace_data_frame$peripheral <- data_frame$peripheral
  lace_data_frame$uncompdiabetes <- data_frame$uncompdiabetes
  lace_data_frame$hivaids <- data_frame$hivaids
  return(lace_data_frame)
}

thirtyday_data <- read_oshpd_data("testdata.seqLength12.converted_bothCCSICD9.csv", "admtdate", "dschdate")
morbid_name_data <- find_and_make_chf(thirtyday_data, "ccs_code_comorbid.csv", "temp.csv")
lace_script_data = create_data_frame_for_lace_script(morbid_name_data)
saveRDS(lace_script_data, file="lace_data.rds")
