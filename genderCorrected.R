#This scripts corrects the discrepency in the gender attribute (in OSHPD, sex attribute). 
# It takes majority of gender in the patient's records and update the records.
# For example: For a patient, if records have gender as 'M M F F F', it changes all the gender to 'F'. 
# However, if gender attribute has values 'U U U U M' for a patient, it will change all to 'M.

readRDS('ordered_mydata_sex_problem.rds') -> ordered_mydata_sex
data <- ordered_mydata_sex
final = data.frame()

i=1
while(i < nrow(data)){
 print(i)
 tmp = data.frame()
 tmp = rbind(data[i,],tmp)
 rln = data[i,'rln']
 print(rln)
 while(i+1 <= nrow(data) && (data[i+1,'rln'] == rln))
 {
	i = i+1
	tmp = rbind(data[i,],tmp)
 }
 print('-------------------')
 nrow(tmp[which(tmp$sex == '1'),]) -> m
 nrow(tmp[which(tmp$sex == '2'),]) -> f
 print(m)
 print(f)
 if(m>f)
 {
	print('1')
	tmp$sex='1'
 } else if(f>m){
	print('2')
	tmp$sex='2'
 } else if(m==0 & f==0){
	print('3')
	tmp$sex='3'
 }else if(m==f){
	print('4-1')
	tmp$sex='1'
 }
 final = rbind(final,tmp)
 i = i+1
}

saveRDS(final,'sex_corrected_records.rds')
