### plot out graphs for each individual leaf, categoried by genotype
plotGX = function(trait, table = NULL , genotype = NULL, id = NULL , dataset = NULL ){
  # check table argument provided or not
  if (length(table) > 0) {
    t = TRUE
    genolist = unique(table$genotype)
    dataset = table
  }else{ t = FALSE}
  # check genotype+dataset combination argument provided or not
  if (length(genotype) > 0 & nrow(dataset) > 0 ) {
    if ( all(genotype %in% dataset$genotype) ){
      g = TRUE
      genolist = genotype
    }else{
      stop("genotype provided is not in the dataset.")
    }
  }else{ g = FALSE }
  # check id+dataset combination argument provided or not
  if (length(id) > 0 & nrow(dataset) > 0) {
    if( all(id %in% dataset$name )){
      d = TRUE
      genolist = unique(dataset[which(dataset$name==id),"genotype"])
      dataset = dataset[which(dataset$name==id),]
    }else{
      stop("id is not in the dataset.")
    }
  }else{ d = FALSE}
  # only one of these three types of inputs can be allowed in one run
  tgd = c(t,g,d)
  if(sum(tgd == TRUE) != 1){
    stop("Please have only one type of input.")
  }else{
  # plot out selected trait of selected entry
  for (i in 1:length(genolist)) {
    sub = subset(dataset, genotype == genolist[i])
    sub = sub[order(as.numeric(sub$plot)),]
    p=ggplot(sub, aes_string("FTime", trait)) +geom_point(aes(colour=name), size =5) + labs(title=paste(genolist[i],trait))+ 
      theme_bw()+ 
      theme(plot.title = element_text(size=30, face="bold"),
            legend.title=element_blank(),
            legend.text = element_text(size = 30),
            legend.key.size = unit(2,"line"),
            legend.position="bottom")
    ggsave(filename=paste(trait," ",genolist[i],".png",sep=""))
    cat(">>>>>>>>>>>>>>",genolist[i],trait,"curve saved\n")
    }
  }
}


