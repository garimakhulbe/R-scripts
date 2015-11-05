map <- new.env(hash=T, parent=emptyenv())
key <- 'ddd'
map$key <- 4
print(ls(map))
>>[1] "key"




map$key



read.csv('procedures-2.txt', header=FALSE) -> df
final = data.frame();
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
for(i in 1:nrow(df)){
        print(i)
        line = strsplit(df[i,],":")
		pccs = line[[1]][1]
		icd9 = strsplit(line[[1]][2]," ")
		print(length(icd9[[1]]))
		j = 1
        while( j <= length(icd9[[1]])) 
		{
			print(pccs)
			print(icd9[[1]][j])
			cbind(pccs, icd9[[1]][j]) -> row
			rbind(final, row) -> final
		    j = j+1
		}
}

colnames(final)[1] = "ccs"
colnames(final)[2] = "icd9"
saveRDS(final, 'final_procedures_list2.rds')



final = data.frame();
i = 2
line = strsplit(df[i,],":")
pccs = line[[1]][1]
icd9 = strsplit(line[[1]][2]," ")
print(length(icd9[[1]]))
j = 1
while( j <= length(icd9[[1]])) 
{
	print(pccs)
	print(icd9[[1]][j])
	cbind(pccs, icd9[[1]][j]) -> row
	rbind(final, row) -> final
    j = j+1
}
print(final)




readRDS('diagnosisCodes.rds') -> df
print('read file')

print(nrow(df))
print(ncol(df))

readRDS('final_procedures_list.rds') -> list
print('read list')

map <- new.env(hash=T, parent=emptyenv())

for (i in 1:nrow(list)){
	map[[as.character(list[i,2])]] <- list[i,1]
}

print(ls(map))

for(i in 1:nrow(df)){
        print(i)
        for(j in 1:25){
                if(df[i,j]!=""){
                        list[list$codes==df[i,j]),1] -> df[i,j]
                }
                else {
                  break
                }
        }
}


