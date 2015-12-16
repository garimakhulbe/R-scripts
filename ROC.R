install.packages("ROCR")

pred <- prediction(df$prediction, df$truth)
perf <- performance(pred,"tpr","fpr")


plot(NA, xlim=c(0,1), ylim=c(0,1))

points(pref@x.values[[1]], pref@y.values[[1]])

plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="False Positive Rate(1-specificity)", ylab="True Positive Rate(sensitivity)")
points(perf_rf@x.values[[1]], perf_rf@y.values[[1]],type="l", 
		lty=1, lwd=2, col="green") 
points(perf_svm@x.values[[1]], perf_svm@y.values[[1]],
	  type="l", lty=3, lwd=2, col="orange")
points(perf_gbm@x.values[[1]], perf_gbm@y.values[[1]],
	   type="l", lty=4, lwd=2, col="red")
points(perf_logis@x.values[[1]], perf_logis@y.values[[1]],
	   type="l", lty=5, lwd=2, col="blue")

	   
	   legend(0.4, 0.3 ,c("SVM","Logistic Regression"),lwd=c(2.5,2.5),col=c("orange","blue")) 

