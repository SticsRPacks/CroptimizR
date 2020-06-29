#' @keywords internal
#'
#'
create_synth_obs<-function(wrapper, model_options,t_obs,p_true,sigma=c(LAI=0.1,biom=1))
{
  observed<-list()
  res<-list()
  path <- model_options$path

  begin_end<-(model_options$begin_end)
  temper <- read.csv2(path)
  NUM_POSTE = as.numeric(as.vector(temper[,"NUM_POSTE"]))
  AN        = as.numeric(as.vector(temper[,"AN"]))
  MOIS      = as.numeric(as.vector(temper[,"MOIS"]))
  JOUR      = as.numeric(as.vector(temper[,"JOUR"]))
  TM        = as.numeric(as.vector(temper[,"TM"]))
  PAR       = as.numeric(as.vector(temper[,"PAR"]))
  table<-matrix(c(NUM_POSTE,AN,MOIS,JOUR,TM,PAR),ncol=6)
  colnames(table)<-c("NUM_POSTE","AN","MOIS","JOUR","TM","PAR")
  table<-as.data.frame(table)


#  date<-as.data.frame(sim_list)[,1]
#  LAI <-as.data.frame(sim_list)[,2]
#  biom<-as.data.frame(sim_list)[,3]



  Poste_An<-(unique(data.frame(NUM_POSTE,AN)))
  situation_names=paste(Poste_An$NUM_POSTE,Poste_An$AN,sep="_")
  situation_names=paste(situation_names,begin_end,sep="_")
  situation_names<-intersect(situation_names,dimnames(p_true)[[3]])


  nb_paramValues <- dim(p_true)[1]
  param_names <- dimnames(p_true)[[2]]

  res<-wrapper(model_options,p_true)
  sim_list<-res$sim_list


  for (i in 1:nb_paramValues){
    for (situation in situation_names){
      data_list_res<-as.data.frame(sim_list[[i]][situation])
      names(data_list_res)[1]<-paste("Date")
      names(data_list_res)[2]<-paste("LAI")
      names(data_list_res)[3]<-paste("biom")
      date_obs=as.POSIXct(as.character(as.Date(t_obs,origin=paste0(as.numeric(substr(situation,10,13)),"-01-01"))),
                          format="%Y-%m-%d",tz="UTC")

      n<-length(t_obs)

      data_list_res_syn<-data_list_res[which(data_list_res$Date %in% date_obs),]
      data_list_res_obs=data_list_res_syn


      noise1<-rnorm(n,0,0.1)
      noise2<-rnorm(n,0,1)

      data_list_res_obs$LAI<-data_list_res_syn$LAI+noise1
      data_list_res_obs$biom<-data_list_res_syn$biom+noise2

      names(data_list_res_obs)[2]<-paste("LAI_obs")
      names(data_list_res_obs)[3]<-paste("biom_obs")

      list_obs<-cbind(data_list_res_syn,data_list_res_obs$LAI_obs,data_list_res_obs$biom_obs)
      names(list_obs)[4]<-paste("LAI_obs")
      names(list_obs)[5]<-paste("biom_obs")
      list_obs
      observed$sim_list[[i]][[situation]]<-list_obs
      observed$error<-FALSE
  }
  }

  return(observed)
}

