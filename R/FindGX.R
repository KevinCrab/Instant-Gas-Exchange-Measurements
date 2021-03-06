#FindGX
FindGX = function(table, id = NULL, DuplicateName = FALSE, MissRecording = FALSE, TooSmall = FALSE){
  id_all = vector()
  duplicatelist = vector()
  toosmall_names = vector()
  missrecording = vector()
  id_all = c(id_all,id)
  # read in id names that need to be found
  if(DuplicateName == TRUE){
    duplicatelist = attr(table, "duplicatelist")
    id_all = c(id_all, duplicatelist)}
  if(MissRecording == TRUE){
    missrecording = attr(table, "missrecording")
    id_all = c(id_all, missrecording)}
  if(TooSmall == TRUE){
    toosmall_names = attr(table, "toosmall_names")
    id_all = c(id_all, toosmall_names)}
    names(id_all) = c(rep("input",length(id)),
                      rep("Duplicate name",length(duplicatelist)),
                      rep("Missrecording",length( missrecording)), 
                      rep("too few observations",length(toosmall_names)))
  #check valid id 
  if(length(id_all)==0){
    stop("Please put in an ID.")
  }
  for(i in 1:length(id_all)){
    if(!id_all[i] %in% unique(table$name)){
      stop("id not in dataset")
    }
  }
  #find start line, end line for each id in the list
  for (j in 1:length(id_all)){
    cat("\n")
    cat(">>>",j,"th sample name:",id_all[j],"    issue: ",names(id_all[j]),
        " \n",sep="")
    index = which(table$name == id_all[j])
    start_index = vector()
    end_index = vector()
    start_index = c(start_index,index[1])
    # loop through dataset to record locations(might appear multiple times)
    for(x in 1:(length(index)-1)){
      if(index[x+1] != (index[x]+1)){
        start_index = c(start_index, index[x+1])
        end_index = c(end_index, index[x])
      }
    }  
    end_index = c(end_index,index[length(index)])
    # number of consecutive observation in each measurement
    consecutive_observation = end_index - start_index + 1
    set = table[end_index, "tag"]
    print(paste(start_index,"th to ",end_index,"th observation (",consecutive_observation," counts) in ",set,sep=""))
  }
}

