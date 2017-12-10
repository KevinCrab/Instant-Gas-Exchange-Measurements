# Calculate mean values for each parameter
GetValue = function(table,...){
  UseMethod("GetValue",table)
}

GetValue.GXcurve = function(table, first.minute = TRUE, last.minute = TRUE){
  #set emply vectors
  plot_name <- vector(mode="character", length=0)
  leaf_name <- vector(mode="numeric", length=0)
  name <- vector(mode="character", length=0)
  genotype <- vector(mode="character", length=0)
  range <- vector(mode="character", length=0)
  row <- vector(mode="character", length=0)
  block <- vector(mode="character", length=0)
  if(first.minute){
    Photo_first <- vector(mode="numeric", length=0)
    Cond_first <- vector(mode="numeric", length=0)
    Ci.Ca_first = vector(mode="numeric", length=0)
  }
  if(last.minute){
    Photo_last <- vector(mode="numeric", length=0)
    Cond_last <- vector(mode="numeric", length=0)
    Ci.Ca_last = vector(mode="numeric", length=0)
  }
  nameslist = unique(table$name)
  # get parameters
  for (x in 1:length(nameslist)){
  sub = table[which(table$name == nameslist[x]),]
  plot_name = c(plot_name,sub[1,"plot"])
  leaf_name = c(leaf_name,sub[1,"leaf"])
  name = c(name,sub[1,"name"])
  genotype = c(genotype,sub[1,"genotype"])
  range = c(range,sub[1,"range"])
  row = c(row,sub[1,"row"])
  block = c(block,sub[1,"block"])
  if(first.minute){
  subfirstone = sub[c(1:min(which(sub$FTime > sub$FTime[1] +60))),]
  Photo_first = c(Photo_first, subfirstone$Photo[which.max(subfirstone$Cond)])
  Cond_first = c(Cond_first, max(subfirstone$Cond))
  Ci.Ca_first = c(Ci.Ca_first, subfirstone$`Ci/Ca`[which.max(subfirstone$Cond)])
  }
  if(last.minute){
  sublastone = sub[c(max(which(sub$FTime < sub$FTime[length(sub$FTime)]-60)):length(sub$FTime)),]
  Photo_last = c(Photo_last,mean(sublastone$Photo))
  Cond_last = c(Cond_last, mean(sublastone$Cond))
  Ci.Ca_last = c(Ci.Ca_last,mean(sublastone$`Ci/Ca`))
  }
  cat(x,"getting value for ----",nameslist[x],"\n")
  }
  #combining and assigning new class
  ps = data.frame(plot_name, leaf_name, name,genotype,range,row,block)
  if(first.minute) ps = cbind(ps,Photo_first,Cond_first,Ci.Ca_first)
  if(last.minute) ps = cbind(ps,Photo_last,Cond_last,Ci.Ca_last)
  class(ps) <- c("GXmean", class(ps))
  return(ps)
}

















