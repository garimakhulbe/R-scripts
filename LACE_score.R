#R Script to calculate risk score:
#run this script via the following terminal command:
# 	nohup Rscript LACE.R readmit30_chf_ada.RDS readmit30_chf_ada_LACE.RDS thirtyday readmit30_chf_ada_LACE_res.log > readmit30_chf_ada_LACE.log &
#which will score data from readmit30_chf_ada.RDS via the LACE method
#assuming the response variable is in a column titled thirtyday
#data plus 5 extra columns titled L, A, C, E, and LACE containing the method's scores will be saved to readmit30_chf_ada_LACE.RDS
#metrics will be printed to readmit30_chf_ada_LACE_res.log
#errors and any screen messages will be printed to readmit30_chf_ada_LACE.log
#this script calculates the LACE score of the patients provided in argument 1
#these LACE scores are then appended to the input file and saved in argument 2
#the file provided in argument 1 is expected to be in RDS format
#and have the following columns:
# 	PID                 	(unique patient identifier)
# 	admittype           	(where 1 == Emergency)
# 	LOS                 	(length of stay for admission)
# 	dischargeDT         	(time of discharge for an admission)
# 	admitDT             	(time of admission for an admission)
# 	dementia            	(dementia)
# 	compdiabetes        	(diabetes with end organ damage)
# 	copd                	(chronic pulmonary disease)
# 	liverdiesease       	(mild liver or renal disease)
# 	cancer              	(any tumor [including lymphoma or leukemia])
# 	endrenal, renalfailure  (moderate or severe liver or renal disease)
# 	leukemia            	(metastatic solid tumor)
# 	myocardial    	(previous myocardial infarction)
# 	cerebro     	(cerebrovascular disease)
# 	peripheral    	(peripheral vascular disease)
# 	uncompdiabetes    	(diabetes without complications)
# 	hivaids    	(AIDS)
#This script is currently not accounting for the following factors:
# 	connective tissue disease
#This script assumes all patients provided have CHF
#explanation of how to calculate LACE can be found at http://www.besler.com/lace-risk-score/
library(pROC)
file.name   	<- "lace_data.rds" #where the data is located
outputfile.name <- "lace_output.rds" #where the data will be saved with LACE scores appended
responseVar 	<- "thirtyday" #name of column containing ground truth
df.original<-readRDS(file.name)
message('number of rows read from ',file.name,': ',nrow(df.original))
col.list <- c("PID", "admittype", "LOS", "dischargeDT",
          	"admitDT", "dementia","chf", "compdiabetes", "copd",
          	"liverdisease", "cancer",
          	"renalfail", "leukemia", "myocardial",
          	"cerebro", "peripheral", "uncompdiabetes",
          	"hivaids")
indexs<-match(col.list, names(df.original))
print(indexs)
df<-df.original[,indexs]
df$L<-0
df$A<-0
df$C<-0
df$E<-0
df$LACE<-0
message('now have the following columns: ',colnames(df),' and the following rows: ',nrow(df))
#L = Length of Stay
# 	1-->1
# 	2-->2
# 	3-->3
# [4,6]-->4
#[7,13]-->5
#   >13-->7
df$L<-df$LOS
df[df$L>3 & df$L<7,]$L<-4
df[df$L>6 & df$L<14,]$L<-5
df[df$L>13,]$L<-7
print('done with L')
print(summary(df$L))
#A = Acuity of Admission
#   admittype==1-->3
#  	Otherwise-->0
df[df$admittype==1,]$A<-3
print('done with A')
print(summary(df$A))
#C = Comorbidities
# 	myocardial-->1
#    	cerebro-->1
# 	peripheral-->1
#        	chf-->2
# uncompdiabetes-->1
#   compdiabetes-->2
#      	copd -->2
#   liverdisease-->2
#     	cancer-->2
#   	dementia-->3
#  	renalfail-->4
#    	hivaids-->4
#   	leukemia-->6

df$myocardial[is.na(df$myocardial)]<-0
df[df$myocardial==1,]$C<-df[df$myocardial==1,]$C+1
df$cerebro[is.na(df$cerebro)]<-0
df[df$cerebro==1,]$C<-df[df$cerebro==1,]$C+1
df$peripheral[is.na(df$peripheral)]<-0
df[df$peripheral==1,]$C<-df[df$peripheral==1,]$C+1
df$uncompdiabetes[is.na(df$uncompdiabetes)]<-0
df[df$uncompdiabetes==1,]$C<-df[df$uncompdiabetes==1,]$C+1
df$chf[is.na(df$chf)]<-0
df[df$chf==1,]$C<-df[df$chf==1,]$C+2
df$compdiabetes[is.na(df$compdiabetes)]<-0
df[df$compdiabetes==1,]$C<-df[df$compdiabetes==1,]$C+2
df$copd[is.na(df$copd)]<-0
df[df$copd==1,]$C<-df[df$copd==1,]$C+2
df$liverdisease[is.na(df$liverdisease)]<-0
df[df$liverdisease==1,]$C<-df[df$liverdisease==1,]$C+2
df$cancer[is.na(df$cancer)]<-0
df[df$cancer==1,]$C<-df[df$cancer==1,]$C+2
df$dementia[is.na(df$dementia)]<-0
df[df$dementia==1,]$C<-df[df$dementia==1,]$C+3
df$renalfail[is.na(df$renalfail)]<-0
df[df$renalfail==1,]$C<-df[df$renalfail==1,]$C+4
df$hivaids[is.na(df$hivaids)]<-0
df[df$hivaids==1,]$C<-df[df$hivaids==1,]$C+4
df$leukemia[is.na(df$leukemia)]<-0
df[df$leukemia==1,]$C<-df[df$leukemia==1,]$C+6
print('done with C')
print(summary(df$C))
#E = Emergency
#   min(4, count(admittype==1 & dischargeDT<admitDT))
print('working on E')
for(i in 1:nrow(df)) {
  epicID<-df$PID[i]
  admitdt<-df$admitDT[i]
  df.prev<-df[df$PID==epicID & df$dischargeDT<admitdt & df$admittype==1,]
  df$E[i]<-min(4,nrow(df.prev))
}
print('done with E')
print(summary(df$E))
df$LACE<-df$L+df$A+df$C+df$E
print('sum LACE')
print(summary(df$LACE))
#Scores >=10 are considered High Risk
df.final<-cbind(df.original, df$L, df$A, df$C, df$E, df$LACE)
colnames(df.final)<-c(colnames(df.original),'L','A','C','E','LACE')
print("saving")
saveRDS(df.final, file=outputfile.name)
# get the metrics
thresh = 10
tp = length(which(df.final[,names(df.final)==responseVar] == 1 & df.final$LACE >= thresh))
fp = length(which(df.final[,names(df.final)==responseVar] == 0 & df.final$LACE >= thresh))
tn = length(which(df.final[,names(df.final)==responseVar] == 0 & df.final$LACE < thresh))
fn = length(which(df.final[,names(df.final)==responseVar] == 1 & df.final$LACE < thresh))
precision = tp / (tp + fp)
recall = tp / (tp + fn)
accuracy = (tp + tn) / (tp + fp + tn + fn)
auc = 0
res.stats = round(c(precision, recall, accuracy, auc, tp, fp, tn, fn, nrow(df.final)), digits = 4)
res.stats
auc = roc(df.final[,names(df.final)==responseVar], as.numeric(df.final$LACE >= thresh))$auc
res.stats = round(c(precision, recall, accuracy, auc, tp, fp, tn, fn, nrow(df.final)), digits = 4)
# outpt metrics
cat(c('precision', 'recall', 'accuracy', 'auc', 'tp', 'fp', 'tn', 'fn', 'sum'))
cat('\n')
cat(res.stats)

