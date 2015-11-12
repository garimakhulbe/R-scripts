#read file
ptm <- proc.time()
args <- commandArgs(trailingOnly = TRUE)
filename = args[1]
outputfilename = args[2]
df = readRDS(filename)
message('file read')

#colnames(df)[which(names(df)=='diag_p')] = "odiag0"
DXCCSvalues = unique(unname(unlist(df[which(names(df) == "odiag1"):which(names(df) == "odiag100")])))
DXCCSvalues = DXCCSvalues[!is.na(DXCCSvalues)]
DXCCSvalues = DXCCSvalues[DXCCSvalues > 0]

# check which DXCCS values are present in which row:
df1 = df[which(names(df) == "odiag1"):which(names(df) == "odiag100")]
df1[is.na(df1)] = 0

m <- sapply(DXCCSvalues, function(x) as.integer(rowSums(df1 == x) > 0L))
# add the result to the original data and set column names:
df <- setNames(cbind(df, m), c(names(df), paste0("DXCCS_", DXCCSvalues)))

message('DX flattened')

#colnames(df)[which(names(df)=='proc_p')] = "oproc0"
PRCCSvalues = unique(unname(unlist(df[which(names(df) == "oproc1"):which(names(df) == "oproc100")])))
PRCCSvalues = PRCCSvalues[!is.na(PRCCSvalues)]
PRCCSvalues = PRCCSvalues[PRCCSvalues > 0]
df2 = df[which(names(df) == "oproc1"):which(names(df) == "oproc100")]
df2[is.na(df2)] = 0

k <- sapply(PRCCSvalues, function(x) as.integer(rowSums(df2 == x) > 0L))
# add the result to the original data and set column names:
df <- setNames(cbind(df, k), c(names(df), paste0("PRCCS_", PRCCSvalues)))
message('PR flattened')
message(nrow(df))
message(ncol(df))
saveRDS(df, outputfilename)
proc.time() - ptm
