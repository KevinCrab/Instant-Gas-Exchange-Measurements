PGMean = function(table,...){
  UseMethod("PGMean",table)
}

PGMean.GXvalue = function(table, type){
  name = vector(mode="character", length=0)
  leaf_replicate_number = vector(mode="numeric", length=0)
  Photo_first = vector(mode="numeric", length=0)
  SE1 = vector(mode="numeric", length=0)
  Cond_first = vector(mode="numeric", length=0)
  SE2 = vector(mode="numeric", length=0)
  Ci.Ca_first = vector(mode="numeric", length=0)
  SE3 = vector(mode="numeric", length=0)
  Photo_last = vector(mode="numeric", length=0)
  SE4 = vector(mode="numeric", length=0)
  Cond_last = vector(mode="numeric", length=0)
  SE5 = vector(mode="numeric", length=0)
  Ci.Ca_last = vector(mode="numeric", length=0)
  SE6 = vector(mode="numeric", length=0)
  
  if(type == "plot"){
    col = "plot_name"
  }
  if(type == "genotype"){
    col = type
  }
  if(! type %in% c("plot","genotype")){
    stop("Wrong type input. plot/genotype")
  }
  
  for (i in 1:length(unique(table[,col]))){
    sub = table[which(table[,col] == unique(table[,col])[i]),]
    name = c(name, sub[,col][1])
    leaf_replicate_number = c(leaf_replicate_number, nrow(sub))
    Photo_first = c(Photo_first, mean(sub$Photo_first))
    SE1 = c(SE1, sd(sub$Photo_first)/sqrt(length(sub$Photo_first)))
    Cond_first = c(Cond_first, mean(sub$Cond_first))
    SE2 = c(SE2, sd(sub$Cond_first)/sqrt(length(sub$Cond_first)))
    Ci.Ca_first = c(Ci.Ca_first, mean(sub$Ci.Ca_first))
    SE3 = c(SE3, sd(sub$Ci.Ca_first)/sqrt(length(sub$Ci.Ca_first)))
    Photo_last = c(Photo_last, mean(sub$Photo_last))
    SE4 = c(SE4, sd(sub$Photo_last)/sqrt(length(sub$Photo_last)))
    Cond_last = c(Cond_last, mean(sub$Cond_last))
    SE5 = c(SE5, sd(sub$Cond_last)/sqrt(length(sub$Cond_last)))
    Ci.Ca_last = c(Ci.Ca_last, mean(sub$Ci.Ca_last))
    SE6 = c(SE6, sd(sub$Ci.Ca_last)/sqrt(length(sub$Ci.Ca_last)))
  }
  
  out = data.frame(name, leaf_replicate_number, Photo_first, SE1, Cond_first, 
                   SE2, Ci.Ca_first, SE3, Photo_last, SE4, Cond_last, SE5,
                   Ci.Ca_last, SE6, stringsAsFactors = FALSE)
  out = out[, colSums(is.na(out)) != nrow(out)]
  
  class(out) = c("GXmean",class(out))
  attr(out, "type") = type
  return(out)
}
