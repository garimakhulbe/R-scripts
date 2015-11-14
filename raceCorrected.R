#race_grp
args<-commandArgs(trailingOnly = TRUE)
filename <-args[1]
outputFilename <- args[2]
print(filename)

readRDS(filename) -> data
as.character(data$rln) -> data$rln
final = data.frame()
i=1
while(i < nrow(data)){

 print('-------------------')
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
 as.data.frame(table(tmp$racegrp,useNA = c("always"))) -> r
 print(r)
 r[order(-r[,2],na.last=NA),1] ->  p
 print('**')
 if( (1 <= as.integer(as.character(p[1]))) && (as.integer(as.character(p[1])) <= 5) ){
	tmp$racegrp = as.character(p[1])
 } else if( is.finite(p[2]) && !(as.integer(as.character(p[2]))==0||as.integer(as.character(p[2]))==6)) {
	tmp$racegrp = as.character(p[2])
 } else if(is.finite(p[3])) {
	tmp$racegrp = as.character(p[3])
 } else {
	tmp$racegrp = '6'
 }
 final = rbind(final,tmp)
 i = i+1
}

saveRDS(final,outputFilename)
