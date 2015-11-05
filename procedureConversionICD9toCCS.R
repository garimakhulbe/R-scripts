# list of ICD9 to CCS procedure codes
readRDS('final_procedures_list2.rds') -> list
print('read list')
message(nrow(list))

# Creating hash map
map <- new.env(hash=T, parent=emptyenv())
for (i in 1:nrow(list)){
	map[[as.character(list[i,2])]] <- list[i,1]
}

#print(ls(map))
#readRDS('admitWithoutTransferRecords.rds') -> df
readRDS('allyears_oshpd_proccodes.rds') -> df
message('nrow(df) ', nrow(df))
message('ncol(df) ', ncol(df))

proc.columns <- df[,grepl("proc",names(df))]
m = ncol(proc.columns)
message('proc columns: ', m)

for(i in 1:nrow(proc.columns)){
        print(i)
        for(j in 1:m){
        		print(proc.columns[i,j])
                if(!is.na(proc.columns[i,j]))
                {
                        map[[as.character(proc.columns[i,j])]] -> proc.columns[i,j]
                }
                else {
                  break
                }
        }
}

df = df[,!grepl("proc", names(df))]
df = cbind(df, proc.columns)
message('ncol(df) - after conversion ', ncol(df))
message(colnames(df))


saveRDS(df, 'admitWithoutTransferRecords_procConverted.rds')
message('done')
