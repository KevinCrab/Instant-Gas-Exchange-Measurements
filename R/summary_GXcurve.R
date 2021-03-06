### Summary for GXcurve class
summary.GXcurve = function(object,value_summary = FALSE, check_condition = TRUE, ...){
  #basic data entry property
  print(paste("Measurent count:",length(unique(object$name))))
  print(paste("leaf number:",names(table(object$leaf))," observation counts:",
              table(object$leaf)))
  cat("\n### Data set included: ",unique(object$tag),sep="\n")
  # print out data summary
  if (value_summary){
    print("Summary for Photosynthesis rate:")
    print(summary(object$Photo))
    print("Summary for water conductance:")
    print(summary(object$Cond))
    print("Summary for Ci.Ca:")
    print(summary(object$"Ci.Ca"))
  }
  # also, type to see measurement conditions
  if (check_condition != TRUE){
    conditions = c("measurement_time","machine","lightsource","AD.avgtime",
                   "flow","par","CO2_mixer","Tblock")
    if(check_condition %in% conditions){
      cat("\nLooking at", check_condition,":\n")
      print(attr(object,check_condition))
    }else{
    warnings("please input measurement_time/machine/
             lightsource/AD.avgtime/flow/par/CO2_mixer/Tblock")
    }
  }
}











