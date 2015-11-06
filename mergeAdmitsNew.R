
mergedRows <- function(dt) {

	m <- dt[1,]
	#print(paste("df rows:",nrow(df),sep=""))
	#m$Ageyradm <- df[1, 'Ageyradm']
	#m$gender <- df[1, 'sex']
	m$rln = dt[1,'rln']
	#m$oshpd_id = df[1,'oshpd_id']
	#m$race_grp = df[1,'race_grp']
	m$charge <- dt[,sum(as.numeric(dt$charge))]
	m$admtdate_Date = min(as.character(dt$admtdate_Date))
	m$dschdate_Date = max(as.character(dt$dschdate_Date))
	nrow = nrow(dt)
	m$next_admit_date <- max(as.character(dt$next_admit_date))
	m$poa_p <- dt[1,'poa_p']
	m$los_adj <- as.integer(as.Date(m$dschdate_Date) - as.Date(m$admtdate_Date))
	m$disp <- dt[nrow, 'disp']

	oDxCodes = dt[,grepl('oproc', names(dt))]
	oPrCodes = dt[,grepl('odiag', names(dt))]
	oDxCodes = unique(dxCodes[!is.na(dxCodes)])
	oPrCodes = unique(prCodes[!is.na(prCodes)])
	
	proc_p = dt[,('o_proc_p','proc_p')]
	diag_p = dt[,('o_diag_p','diag_p')]
	typcare = dt[,('otypcare','typcare')]
	sevcode = dt[,('osev_code','sev_code')]
	srcsite = dt[,('srcsite','osrcsite')]
	srcroute = dt[,('srcroute','osrcroute')]
	srclicns = dt[,('srclicns','osrclicns')]

	typcare = typcare[!is.na(typcare)]
	sevcode = sevcode[!is.na(sevcode)]
	srcsite = srcsite[!is.na(srcsite)]
	srclicns = srclicns[!is.na(srclicns)]
	srcroute = srcroute[!is.na(srcroute)]
	proc_p = proc_p[!is.na(proc_p)]
	diag_p = diag_p[!is.na(diag_p)]

	ltypcare = length(typcare)
	m$typcare = ifelse(ltypcare>0, typcare[ltypcare],NA)
	m$otypcare= ifelse(ltypcare>1,paste(typcare[1:ltypcare-1], collapse=";"),NA)

	lsev = length(sevcode)
	m$sev_code= ifelse(lsev>0, sevcode[lsev],NA)
	m$osev_code=ifelse(lsev>1, paste(sevcode[1:lsev-1], collapse=";"), NA)

	lsite = length(srcsite)
	m$srcsite= ifelse(lsite>0,srcsite[1], NA)
	m$osrcsite= ifelse(lsite>1,paste(srcsite[-1], collapse=";"),NA)

	lroute = length(srcroute)
	m$srcroute= ifelse(lroute>0,srcroute[1], NA)
	m$osrcroute=ifelse(lroute>1,paste(srcroute[-1], collapse=";"),NA)

	llicns = length(srclicns)
	m$srclicns= ifelse(llicns>0,srclicns[1], NA)
	m$osrclicns= ifelse(llicns>1,paste(srclicns[-1], collapse=";"),NA)

	lproc = length(proc_p)
	m$proc_p = ifelse(lproc>0, proc_p[lproc], NA)
	m$o_proc_p = ifelse(lproc>1,paste(proc_p[1:lproc-1], collapse=";"), NA)

	ldiag = length(diag_p)
	m$diag_p = ifelse(ldiag>0, diag_p[ldiag])], NA)
	m$o_diag_p = ifelse(ldiag>1,paste(diag_p[1:ldiag-1], collapse=";"), NA)

	m <- cbind(m[,!grepl("oproc",names(m))], oDxCodes)
	m <- cbind(m[,!grepl("odiag",names(m))], oPrCodes)
	
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
	n = nrow(data)
	while(i <= n) {
		message("i -->",i)
		rln = data[i,"rln"]
		message(rln)
		admtdate_Date = data[i,"admtdate_Date"]
		dschdate_Date = data[i, "dschdate_Date"]
		rows <- which(rln==data$rln & as.Date(data$admtdate_Date) >= as.Date(admtdate_Date) 
									& as.Date(data$admtdate_Date) <= as.Date(dschdate_Date))
		if(length(rows)>1){
			dt <- mergedRows(data[rows,])
			message("merged row length:",nrow(dt))
			#print(ncol(data))
			lvl.one <- data[1:i,]
			if(length(which((1:i) %in% rows)) > 0) {
			      lvl.one<-lvl.one[-which((1:i) %in% rows),]
			}

			lvl.two <- data[(i+1):nrow(data),]
		    if(length(which((i+1):nrow(data) %in% rows)) > 0) {
		          lvl.two <-lvl.two[-which(((i+1):nrow(data)) %in% rows),]
			}

			data <-rbind(lvl.one, dt, lvl.two)
			n = nrow(data)
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
