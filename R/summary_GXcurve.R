### Summary for GXcurve class
summary.GXcurve = function(table,value_summary = FALSE, check_condition = TRUE){
  #basic data entry property
  print(paste("Measurent count:",length(unique(table$name))))
  print(paste("leaf number:",names(table(table$leaf))," observation counts:",
              table(table$leaf)))
  cat("\n### Data set included: ",unique(table$tag),sep="\n")
  # print out data summary
  if (value_summary){
    print("Summary for Photosynthesis rate:")
    print(summary(table$Photo))
    print("Summary for water conductance:")
    print(summary(table$Cond))
    print("Summary for Ci.Ca:")
    print(summary(table$"Ci.Ca"))
  }
  # also, type to see measurement conditions
  if (check_condition != TRUE){
    conditions = c("measurement_time","machine","lightsource","AD.avgtime",
                   "flow","par","CO2_mixer","Tblock")
    if(check_condition %in% conditions){
      cat("\nLooking at", check_condition,":\n")
      print(attr(table,check_condition))
    }else{
    warnings("please input measurement_time/machine/
             lightsource/AD.avgtime/flow/par/CO2_mixer/Tblock")
    }
  }
}











