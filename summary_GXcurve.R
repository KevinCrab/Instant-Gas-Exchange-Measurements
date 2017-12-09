### Summary for GXcurve class
summary.GXcurve = function(table,value_summary = FALSE, check_condition = FALSE){
  #basic data entry property
  print(paste("Measurent count:",length(unique(table$name))))
  print(paste("leaf number:",names(table(table$leaf))," counts:",table(table$leaf)))
  cat("\n### Data set included: ",unique(table$tag),"\n",sep="\n")
  # print out data summary
  if (value_summary){
    print("Summary for Photosynthesis rate:")
    print(summary(table$Photo))
    print("Summary for water conductance:")
    print(summary(table$Cond))
    print("Summary for Ci/Ca:")
    print(summary(table$"Ci/Ca"))
  }
  # also, type to see measurement conditions
  if (check_condition){
    object = readline(prompt="Which condition do you want to look at?\nmeasurement_time/machine/lightsource\nAD.avgtime/flow/par/CO2_mixer/Tblock\n     ")
    print(attr(table,object))
  }
}

### Summary for GXmean class
summary.GXmean = function(table,trait = FALSE){
    print(paste(nrow(table),"entries"))
    print(paste("leaf number:",names(table(table$leaf))," counts:",table(table$leaf)))
    if(trait != FALSE){
  traitlist = c("Photo_first","Cond_first","Ci.Ca_first","Photo_last","Cond_last","Ci.Ca_last") 
  if( trait %in% traitlist && length(trait)==1 ){
    print(paste("Summary for",trait,":"))
    print(summary(table[,trait]))
    hist(table[,trait], breaks=20,main=paste("Histogram for",trait))
  }else{
    warnings("Please have single input: Photo_first/Cond_first/Ci.Ca_first/Photo_last/Cond_last/Ci.Ca_last")
  }
 }
}

  

