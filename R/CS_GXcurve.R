# Combining and Screenig function
CS = function(table, ...){
  UseMethod("CS", table)
}

CS.GXcurve = function(table, ...){
  datalist = list(table, ...)
  out = rbind(table, ...)
  # in gas exchange measurement for each leaf sample, having 57 or 58 recordings in 4 mins period is normal. Any other recording number indicate abnomal behavior. 
  GXrecordinglength = table(table(out$name))
  toobig = GXrecordinglength[which(as.numeric(names((GXrecordinglength))) > 58)]
  toobig_names = names(which(table(out$name)>58))
  toosmall = GXrecordinglength[which(as.numeric(names((GXrecordinglength))) < 57)]
  toosmall_names = names(which(table(out$name)<57))
  #for too many observations, it could be caused by 1. duplicate names (typo during name record) 2. miss name record between two measurements
  countlist = list() 
  fullname = vector()
  for(i in 1:length(datalist)){
    tem = 1
    fullname = c(fullname, datalist[[i]]$name[1])
    for ( j in 2: length(datalist[[i]]$name)){
      if (datalist[[i]]$name[j] != datalist[[i]]$name[j-1]){
        tem = tem + 1
        fullname = c(fullname, datalist[[i]]$name[j])
      }
    }
    countlist[[i]] = tem
  }
  total = 0
  for(k in 1:length(countlist)){
    cat("input",k,"/",unique(datalist[[k]]$tag),":",countlist[[k]],"entries\n")
    total = total + countlist[[k]]
  }  
  cat("overall combining",total,"entries\n\n")
  # duplicatelist is the entries with double measurements
  duplicatelist = names(which(table(fullname)>1))
  # missrecording are the entries that include another measurement right after it but people forgot to record its name in between
  missrecording = toobig_names[!(toobig_names %in% duplicatelist)]
  # print out screening result
  if (length(duplicatelist) != 0){
    print(paste(">>> Walch out! You have",length(duplicatelist),"case(s) in which you have duplicate entry names.","Better check out raw data:"))
    print(duplicatelist)
  }
  if (length(toosmall_names) !=0){
    print(paste(">>> Walch out! You have",length(toosmall_names),"case(s) in which you have too few(",names(toosmall),") gas exchange recordings.","Better check out raw data:"))
    print(toosmall_names)
  }
  if (length(missrecording) !=0){
    print(paste(">>> Walch out! You have",length(missrecording),"case(s) in which you have missing name recording between two seperate measurements","Better check out raw data:"))
    print(missrecording)
  }
  # assign("fullname", fullname, envir = .GlobalEnv)
  attr(out, "measurement_time") <- NULL
  attr(out, "machine") <- NULL
  attr(out, "lightsource") <- NULL
  attr(out, "AD.avgtime") <- NULL
  attr(out, "flow") <- NULL 
  attr(out, "par") <- NULL
  attr(out, "CO2_mixer") <- NULL
  attr(out, "Tblock") <- NULL
  attr(out, "duplicatelist") <- duplicatelist
  attr(out, "toosmall_names") <- toosmall_names
  attr(out, "missrecording") <- missrecording
  return(out)
}


