
mergedRows <- function(df) {

	#takes first from list of records to create a record.
	m <- df[1,]
	#print(paste("df rows:",nrow(df),sep=""))
	#m$Ageyradm <- df[1, 'Ageyradm']
	#m$gender <- df[1, 'sex']

	#take the rln to get all the records for that rln.
	m$rln = df[1,'rln']
	#m$oshpd_id = df[1,'oshpd_id']
	#m$race_grp = df[1,'race_grp']

	# sum up the charge
	m$charge <- colSums(as.matrix(as.numeric(df$charge)))

	# Earliest admit date
	m$admtdate_Date = min(as.character(df$admtdate_Date))

	#lastest discharge date
	m$dschdate_Date = max(as.character(df$dschdate_Date))
	m$next_admit_date <- max(as.character(df$next_admit_date))

	# Earliest poa_p
	m$poa_p <- df[1,'poa_p']

	# Calculate los_adj on the basis of new admit and discharge date.
	m$los_adj <- as.integer(as.Date(m$dschdate_Date) - as.Date(m$admtdate_Date))

	# Latest disposition
	m$disp <- df[nrow(df), 'disp']
	dx <- c()
	pr <- c()
	typcare <- c()
	sevcode <- c()
	srcsite <- c()
	srclicns <- c() 
	srcroute <- c()
	proc_p <- c()
	diag_p <- c()
	#print(colnames(df))

	# combine together all the dx and pr codes in one array.
	# same for typcare, sev_code, source
	i = 1
	while(i <= nrow(df)) {
		dx <-c(dx, df[i, grepl("odiag",names(df))])
		pr <- c(pr, df[i,grepl("oproc",names(df))])
		typcare <- c(typcare, df[i,"otypcare"], df[i,"typcare"])
		sevcode <- c(sevcode, df[i,"osev_code"], df[i,"sev_code"])
		srcsite <- c(srcsite, df[i,"osrcsite"], df[i,"srcsite"])
		srclicns <- c(srclicns, df[i,"osrclicns"], df[i,"srclicns"])
		srcroute <- c(srcroute, df[i,"osrcroute"], df[i,"srcroute"])
		proc_p <- c(proc_p, df[i,"o_proc_p"], df[i,"proc_p"])
		diag_p <- c(diag_p, df[i,"o_diag_p"], df[i,"diag_p"])
		i = i + 1
	}

	dx[dx==""] <- NA
	pr[pr==""] <- NA
	proc_p[proc_p==""] <- NA
	diag_p[diag_p==""] <- NA
	typcare[typcare == ""] <- NA
	sevcode[sevcode==""] <- NA
	srcsite[srcsite==""] <- NA
	srcroute[srcroute==""] <- NA
	srclicns[srclicns==""] <- NA
	dx <- dx[!is.na(dx)]
	pr <- pr[!is.na(pr)]

	dx <- unique(dx)
	pr <- unique(pr)
	typcare <- typcare[!is.na(typcare)]
	sevcode <- sevcode[!is.na(sevcode)]
	srcsite <- srcsite[!is.na(srcsite)]
	srclicns <- srclicns[!is.na(srclicns)]
	srcroute <- srcroute[!is.na(srcroute)]
	proc_p <- proc_p[!is.na(proc_p)]
	diag_p <- diag_p[!is.na(diag_p)]
	
	# latest typcare in typcare attribute in the record and 
	# rest in other typcare. 
	m$typcare = ifelse(length(typcare)>0, typcare[length(typcare)], NA)
	m$otypcare= ifelse(length(typcare)>1,paste(typcare[1:length(typcare)-1], collapse=";"),NA)

	# latest sev_code in sev_code attribute in the record and 
	# rest in other sev_code.
	m$sev_code= ifelse(length(sevcode)>0, sevcode[length(sevcode)],NA)
	m$osev_code=ifelse(length(sevcode)>1, paste(sevcode[1:length(sevcode)-1], collapse=";"), NA)

	# Earliest srcsite in srcsite attribute in the record and 
	# rest in other srcsite.
	m$srcsite= ifelse(length(srcsite)>0,srcsite[1], NA)
	m$osrcsite= ifelse(length(srcsite)>1,paste(srcsite[2:length(srcsite)], collapse=";"),NA)

	# Earliest srcroute in srcroute attribute in the record and 
	# rest in other srcroute.
	m$srcroute= ifelse(length(srcroute)>0,srcroute[1], NA)
	m$osrcroute=ifelse(length(srcroute)>1,paste(srcroute[2:length(srcroute)], collapse=";"),NA)

	# Earliest srclicns in srclicns attribute in the record and 
	# rest in other srclicns.
	m$srclicns= ifelse(length(srclicns)>0,srclicns[1], NA)
	m$osrclicns= ifelse(length(srclicns)>1,paste(srclicns[2:length(srclicns)], collapse=";"),NA)

	# Latest primary procedure in proc_p attribute in the record and 
	# rest in other primary procedures.
	m$proc_p = ifelse(length(proc_p)>0, proc_p[length(proc_p)], NA)
	m$o_proc_p = ifelse(length(proc_p)>1,paste(proc_p[1:length(proc_p)-1], collapse=";"), NA)

	# Latest primary diagnosis in diag_p attribute in the record and 
	# rest in other primary diagnosis.
	m$diag_p = ifelse(length(diag_p)>0, diag_p[length(diag_p)], NA)
	m$o_diag_p = ifelse(length(diag_p)>1,paste(diag_p[1:length(diag_p)-1], collapse=";"), NA)

	# expand row to have 100 columns
	dx <- c(dx, as.data.frame(matrix(NA, ncol=(100-length(dx)), nrow=1)))
	pr <- c(pr, as.data.frame(matrix(NA, ncol=(100-length(pr)), nrow=1)))
	
	dx <- as.data.frame(dx)
	pr <- as.data.frame(pr)
	print(nrow(dx))
	print(nrow(pr))

	#kept the column names same as in dataset.
	for( i in 1:100) {
		colnames(dx)[i]= paste("odiag",i,sep="")
		colnames(pr)[i]= paste("oproc",i,sep="")
	}

	m <- cbind(m[,!grepl("oproc",names(m))],pr)
	m <- cbind(m[,!grepl("odiag",names(m))],dx)
	
	#print(ncol(m))
	#print(colnames(m))
	return(m)
}


process.data <- function(filename){
	#read data
	readRDS(filename) -> data.full
	data <- data.full
	print(nrow(data))
	print(ncol(data))
	
	data <- data[order(data$rln, data$admtdate_Date, data$dschdate_Date),]
	data$rln = as.character(data$rln)
	
	i = 1
	while(i <= nrow(data)) {
		message("i -->",i)
		rln = data[i,"rln"]
		print(rln)
		admtdate_Date = data[i,"admtdate_Date"]
		dschdate_Date = data[i, "dschdate_Date"]
		rows <- which(rln==data$rln & as.Date(data$admtdate_Date) >= as.Date(admtdate_Date) 
									& as.Date(data$admtdate_Date) <= as.Date(dschdate_Date))
		if(length(rows)>1){
			message('working on row ', i, ' with ', length(rows)-1,' conflicts')

			#return single record after merge
			dt <- mergedRows(data[rows,])
			message("merged row length:",nrow(dt))
			#print(ncol(data))

			# This removes all the rows which are combine 
			# and add the merged record to the dataset. 
			lvl.one <- data[1:i,]
			if(length(which((1:i) %in% rows)) > 0) {
			      lvl.one<-lvl.one[-which((1:i) %in% rows),]
			}

			lvl.two <- data[(i+1):nrow(data),]
		    if(length(which((i+1):nrow(data) %in% rows)) > 0) {
		          lvl.two <-lvl.two[-which(((i+1):nrow(data)) %in% rows),]
			}

			data <-rbind(lvl.one, dt, lvl.two)
		} 
		else {
			i = i + 1
		}
	}
	message('removed ',nrow(data.full) - nrow(data),' conflicts')
	return(data)
}

df <- process.data('admitIssue_updated.rds')
saveRDS(df,'merged_Admit.rds')
