### Summary for GXmean class
summary.GXmean = function(object, trait, ...){
  type = attr(object,"type")
  rep = table(object$rep_number)
  for ( i in 1:length(rep)){
    cat("There are ",rep[i]," ",type,"(s) with ", names(rep[i])," replicate(s)\n",sep = "")
  }
  traitlist = c("Photo_first","Cond_first","Ci.Ca_first","Photo_last","Cond_last","Ci.Ca_last") 
  if( trait %in% traitlist && length(trait)==1 ){
    print(paste("Summary for",trait,":"))
    print(summary(object[,trait]))
    hist(object[,trait], breaks=20,main=paste("Histogram for",trait),xlab = trait)
  }else{
    stop("Please have single input: Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last")
  }
}