
readRDS('diagnosisCodes.rds') -> df
print('read file')

print(nrow(df))
print(ncol(df))

readRDS('ccslist.rds') -> list
print('read list')

for(i in 1:nrow(df)){
	print(i)
	for(j in 1:25){
		if(df[i,j]!=""){
			list[which(list$codes==df[i,j]),1] -> df[i,j]
		}
		else {
		  break
		}
	}
}

saveRDS(df,'allyears_css_converted_oshpd.rds')
print('done')
