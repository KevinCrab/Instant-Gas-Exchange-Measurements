PGMean = function(table, ...){
  UseMethod("PGMean",table)
}

PGMean.GXvalue = function(table, type){
  # find corresponding column 
  if(type == "plot"){
    col = "plot_name"
  }
  if(type == "genotype"){
    col = type
  }
  if(! type %in% c("plot","genotype")){
    stop("Wrong type input. plot/genotype")
  }
  #define function to calculate standard error
  se = function(x){
    return(sd(x)/sqrt(length(x)))
  }
  #use tapply to calculate mean and standard error
  Photo_first = tapply(table$Photo_first, table[,col] , mean, na.rm = TRUE)
  se1 = tapply(table$Photo_first, table[,col] , se)
  Cond_first = tapply(table$Cond_first, table[,col] , mean, na.rm = TRUE)
  se2 = tapply(table$Cond_first, table[,col] , se)
  Ci.Ca_first = tapply(table$Ci.Ca_first, table[,col] , mean, na.rm = TRUE)
  se3 = tapply(table$Ci.Ca_first, table[,col] , se)
  Photo_last = tapply(table$Photo_last, table[,col] , mean, na.rm = TRUE)
  se4 = tapply(table$Photo_last, table[,col] , se)
  Cond_last = tapply(table$Cond_last, table[,col] , mean, na.rm = TRUE)
  se5 = tapply(table$Cond_last, table[,col] , se)
  Ci.Ca_last = tapply(table$Ci.Ca_last, table[,col] , mean, na.rm = TRUE)
  se6 = tapply(table$Ci.Ca_last, table[,col] , se)
  out = data.frame(Photo_first,se1,Cond_first,se2,Ci.Ca_first,se3,Photo_last,se4,
             Cond_last,se5,Ci.Ca_last,se6)
  out$name = rownames(out)
  out$rep_number = table(table[,col])
  #get rid of NA columns (if in the last GetValue function, only one of 
  #first or last minute values was chosen)
  out = out[, colSums(is.na(out)) != nrow(out)]
  rownames(out) = 1:length(out$name)
  # assign class and attributes
  class(out) = c("GXmean",class(out))
  attr(out, "type") = type
  return(out)
}



