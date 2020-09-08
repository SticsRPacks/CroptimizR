############################

context("test_create_synt")

temp_data_path <- system.file(file.path("extdata", "Bonsai_bio", "Tmoy_France_INRA_STATION_33550003 - R_mJ_m2.csv"),
                              package="CroptimizR")


model_options2<-list()
model_options2$path<-temp_data_path
model_options2$begin_end<-c("130260","100300","100360","050260","140270")

p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p1<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0,LAImax=10.197236,C=0)
p2<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=-500,LAImax=10.197236,C=500)
p3<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=0,C=0.035339)
x <- rbind(p,p1,p2,p3)

p_true <- array( c(p,p1,p2,p3),
                        dim=c(1,9,4),
                        dimnames=list(NULL,c("K", "Eimax","Eb","tzero","Ti",
                                             "deltaTs","B","LAImax","C")
                                      ,c("21106002_2013_130260","68028003_2013_100300",
                                         "33550003_2013_100360","84007004_2016_050260")))


t_obs<-c(140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230)
wrapper<-CroptimizR:::bonsai_bio_wrapper

obs_list<-CroptimizR:::create_synth_obs(wrapper, model_options2,t_obs,p_true)
obs_list

