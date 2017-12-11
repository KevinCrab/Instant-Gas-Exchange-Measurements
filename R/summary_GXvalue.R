### Summary for GXvalue class
summary.GXvalue = function(table,trait){
  print(paste(nrow(table),"entries"))
  print(paste("leaf number:",names(table(table$leaf))," counts:",table(table$leaf)))
  traitlist = c("Photo_first","Cond_first","Ci.Ca_first","Photo_last","Cond_last","Ci.Ca_last") 
  if( trait %in% traitlist && length(trait)==1 ){
    print(paste("Summary for",trait,":"))
    print(summary(table[,trait]))
    hist(table[,trait], breaks=20,main=paste("Histogram for",trait),xlab = trait)
  }else{
    warnings("Please have single input: Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last")
  }
}