##function for read in data and turn it into GX class
read.GX <- function(filename,leaf_rep,genolist,condition = FALSE){
  table = read.csv(filename,header = FALSE,stringsAsFactors = FALSE,na.strings = c("","NA"))
  #check input
  if(!is.data.frame(table)) stop("Data frame needed.")
  if(!as.numeric(leaf_rep)) stop("leaf replication number must be numeric\nstart from 1")
  #record measurement properties
  measurement.time = table[2,1]
  machine = table[which(table$V1=="Unit="),2]
  lightsource = table[which(table$V1=="LightSource="),2]
  AD.avgtime = table[which(table$V1=="A/D AvgTime="),2]
  flow = table[grep("Flow: Fixed ",table[,2]),2]
  flow = unique(gsub("(\\d{2}:\\d{2}:\\d{2})","",flow))
  flow = gsub("\"\n" ,"",flow)
  flow = gsub("\" ","",flow)
  par = table[grep("Lamp: ParIn",table[,2]),2]
  par = unique(gsub("(\\d{2}:\\d{2}:\\d{2})","",par))
  par = gsub("\"\n" ,"",par)
  par = gsub("\" ","",par)
  CO2 = table[grep("CO2 Mixer",table[,2]),2]
  CO2 = unique(gsub("(\\d{2}:\\d{2}:\\d{2})","",CO2))
  CO2 = gsub("\"\n" ,"",CO2)
  CO2 = gsub("\" ","",CO2)
  Tblock = table[grep("Coolers: Tblock",table[,2]),2]
  Tblock = unique(gsub("(\\d{2}:\\d{2}:\\d{2})","",Tblock))
  Tblock = gsub("\"\n" ,"",Tblock)
  Tblock = gsub("\" ","",Tblock)
  # indicate zero or more than one measurement conditions
  if (condition == TRUE){
  if(length(flow) > 1 | length(par) > 1 | length(CO2) > 1 | length(Tblock) > 1) {
    warning("Multiple measuring conditions. Check data collection conditions.")
    print(c(flow,par,CO2,Tblock))
    }
    if(length(flow) == 0 | length(par) == 0 | length(CO2) == 0 | length(Tblock) == 0){
    warning("Missing measuring conditions. Check data collection conditions.")
    print(c(flow,par,CO2,Tblock))
    }
  }
  #trim uneccessary parameters
  colname = table[which(table$V1 == "Obs"),]
  colnames(table) = colname
  table = table[-c(1:(which(table[,1] == "Obs")+1)),c("Obs","HHMMSS","FTime","Photo","Cond","Ci/Ca")]
  rownames(table)=1:nrow(table)
  #eliminate remarks other than records
  record = grep("(\\d{2}:\\d{2}:\\d{2})[[:space:]][[:digit:]]+",table$HHMMSS)
  remark = which(table$Obs == "Remark=")
  remove = remark[!remark %in% record]
  table = table[-remove,]
  rownames(table)=1:nrow(table)
  #give name to measurement points
  table$HHMMSS = gsub("(\\d{2}:\\d{2}:\\d{2})","",table$HHMMSS)
  table$HHMMSS = gsub("\"","",table$HHMMSS)
  table$HHMMSS = gsub("\n","",table$HHMMSS)
  table$HHMMSS = gsub(" ","",table$HHMMSS)
  table$HHMMSS = gsub("[[:alpha:]]","",table$HHMMSS)
  table$HHMMSS[table$HHMMSS==""] <- "000"
  # give records same length of 3 digits
  digit3 = unique(table$HHMMSS)[grep("^[[:digit:]]{3}$",unique(table$HHMMSS))]
  other = unique(table$HHMMSS)[!unique(table$HHMMSS) %in% digit3]
  if(length(other)>1) {
    cat("check out these names, are they 3 digits? \n",other)
    cat("\nReformatting them into 3 digits...")
    Sys.sleep(2)
    table[grep("^[[:digit:]]{2}$",table$HHMMSS),"HHMMSS"] = paste("0",table[grep("^[[:digit:]]{2}$",table$HHMMSS),"HHMMSS"],sep="")
    table[grep("^[[:digit:]]{1}$",table$HHMMSS),"HHMMSS"] = paste("00",table[grep("^[[:digit:]]{1}$",table$HHMMSS),"HHMMSS"],sep="")
    cat("Done.")
  }
  table$name = table$HHMMSS
  table$plot = table$HHMMSS
  table$leaf = table$HHMMSS
  table$leaf = leaf_rep
  nameslist = which(!(table$HHMMSS=="000"))
  for (row in 1:length(table$Obs)){
    if (table[row,"plot"]=="000"){
      firstline = which.max (  which(nameslist < row)  )
      table[row,"plot"] = table[nameslist[firstline],"plot"]
      table[row,"name"] = paste(table[nameslist[firstline],"plot"],"_",table[nameslist[firstline],"leaf"],sep="")
      }
  }
  remark = which(table$Obs == "Remark=")
  table = table[-remark,]
  table = table[,-c(1,2)]
  rownames(table)=1:nrow(table)
  col.numeric = c(1,2,3,4)
  table[,col.numeric] = apply(table[,col.numeric],2,function(x) as.numeric(x))
  table = table[c(6,7,5,1,2,3,4)]
  table$tag = filename
  #input genotype table and format it
  geno = read.csv(genolist,stringsAsFactors = FALSE)
  geno[,1:ncol(geno)] = apply(geno[,1:ncol(geno)],2,function(x) as.character(x))
  geno$plot = gsub(" ","",geno$plot)
  geno[grep("^[[:digit:]]{2}$",geno$plot),"plot"] = paste("0",geno[grep("^[[:digit:]]{2}$",geno$plot),"plot"],sep="")
  geno[grep("^[[:digit:]]{1}$",geno$plot),"plot"] = paste("00",geno[grep("^[[:digit:]]{1}$",geno$plot),"plot"],sep="")
  #find genotypic and field imformation
  table$genotype = NA
  table$range = NA
  table$row = NA
  table$block = NA
  nameslist = unique(table$name)
  for (x in 1:length(nameslist)){
    # reset starting time
    time_in_seconds = table[which(table$name == nameslist[x]),"FTime"]
    time_in_seconds = time_in_seconds-time_in_seconds[1]
    table[which(table$name == nameslist[x]),"FTime"] = time_in_seconds
    plotname = unique(table[which(table$name==nameslist[x]),"plot"])
    if(length(plotname)>1) warning("You can duplicate names in this dataset!!!")
    table[which(table$name == nameslist[x]),"genotype"] = geno[which(geno$plot == plotname),"name"]
    table[which(table$name == nameslist[x]),"range"] = geno[which(geno$plot == plotname),"range"]
    table[which(table$name == nameslist[x]),"row"] = geno[which(geno$plot == plotname),"row"]
    table[which(table$name == nameslist[x]),"block"] = geno[which(geno$plot == plotname),"block"]
}
  # assign class and measurement properties
  class(table) <- c("GXcurve", class(table))
  attr(table, "measurement_time") <- measurement.time
  attr(table, "machine") <- machine
  attr(table, "lightsource") <- lightsource
  attr(table, "AD.avgtime") <- AD.avgtime
  attr(table, "flow") <- flow
  attr(table, "par") <- par
  attr(table, "CO2_mixer") <- CO2
  attr(table, "Tblock") <- Tblock
  return(table)
}


