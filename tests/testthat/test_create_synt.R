############################

context("test_create_synt")

temp_data_path <- system.file(file.path("extdata", "Bonsai_bio", "Tmoy_France_INRA_STATION_33550003 - R_mJ_m2.csv"),
                              package="CroptimizR")
p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p_true <- array( c(p,p,p,p),
                 dim=c(1,9,4),
                 dimnames=list(NULL,c("K", "Eimax","Eb","tzero","Ti",
                                      "deltaTs","B","LAImax","C")
                               ,c("21106002_2013_130260","68028003_2013_100300",
                                  "33550003_2013_100360","84007004_2016_050260")))

param_info1<-list()

param_info1<-list(lb=c(K=0.6,Eimax=0.9,Eb=0.9,
                       tzero=-1000,Ti=500,deltaTs=400,B=0.0011,LAImax=3,C=0.01),
                  ub=c(K=0.8,Eimax=0.99,Eb=2.8,tzero=1000,
                       Ti=2500,deltaTs=1200,B=0.003,LAImax=12,C=0.1))


# Set the model options (see '? stics_wrapper_options' for details)
#model_options=stics_wrapper_options(stics_path,stics_inputs_path,parallel=FALSE)
model_options_optim<-list()
model_options_optim$path<-temp_data_path
model_options_optim$begin_end<-c("130260","100300","100360","050260","140270")
model_options_optim$param_values_default<-p


t_obs<-c(140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230)
wrapper<-CroptimizR:::bonsai_bio_wrapper


obs_list<-CroptimizR:::create_synth_obs(wrapper, model_options_optim,t_obs,p_true,sigma=c(LAI=0.1,biom=1.5))
obs_list
