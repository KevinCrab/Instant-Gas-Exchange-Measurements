### Summary for GXvalue class
summary.GXvalue = function(object,trait, ...){
  print(paste(nrow(object),"entries"))
  print(paste("leaf number:",names(table(object$leaf))," counts:",table(object$leaf)))
  traitlist = c("Photo_first","Cond_first","Ci.Ca_first","Photo_last","Cond_last","Ci.Ca_last") 
  if( trait %in% traitlist && length(trait)==1 ){
    print(paste("Summary for",trait,":"))
    print(summary(object[,trait]))
    hist(object[,trait], breaks=20,main=paste("Histogram for",trait),xlab = trait)
  }else{
    stop("Please have single input: Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last")
  }
}
