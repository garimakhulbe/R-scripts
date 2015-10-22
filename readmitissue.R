readRDS('testdata.seqLength12_version2.rds') -> df

#rows which have negative days between admits
which(df$daysBtwAdmits < 0) -> rownum

print(rownum)
print(length(rownum))

index <- c()
j = 1
for (i in 1:length(rownum)) {
  print(i)
  print(j)
  print(rownum[i])
  print(rownum[i]+1)
  index[j] <- rownum[i]
  index[j+1] <- (rownum[i]+1)
  j = j+2
}

print(length(unique(index))
print(length(index))

#take that records and its next record
df[index,c('rln','oshpd_id','admtdate_Date','dschdate_Date','daysBtwAdmits')] -> data


i = 1
while(i< nrow(data)){
  if(data[i,'oshpd_id']==data[i+1,'oshpd_id']){
    data[i,'hospital_ind'] = 1
  }
  else 
  {
    data[i,'hospital_ind'] = 0
  }
  i = i+2
}

print(data[,c('rln','oshpd_id','hospital_ind','admtdate_Date','dschdate_Date','daysBtwAdmits')])

print(table(data$hospital_ind))
