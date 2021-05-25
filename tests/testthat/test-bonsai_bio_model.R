library(testthat)
context("Test the bonsai-bio model")

weather_data_path <- system.file(file.path("extdata", "Bonsai_bio", "temperature_PAR.csv"),
                       package="CroptimizR")

p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p1<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,Ti=1560.870155,deltaTs=678.088316,B=0,LAImax=10.197236,C=0)
p2<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,Ti=1560.870155,deltaTs=678.088316,B=-4^200,LAImax=10.197236,C=3^200)
p3<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=0,C=0.035339)


temp <- read.csv2(weather_data_path,header = FALSE)
T=as.numeric(as.vector(temp[,1]))
PR=as.numeric(as.vector(temp[,2]))

t1=1
tfin=232
biom_t0=0
tzero<-204
result <-CroptimizR:::bonsai_bio(t1,tfin,tzero,p["Ti"],p["deltaTs"],p["B"],p["LAImax"],p["C"],p["Eb"],p["Eimax"],p["K"],T,PR,biom_t0)
LAI=as.numeric(as.vector(result[,"LAI"]))
biom_=as.numeric(as.vector(result[,"biomas"]))

#bonsai <- function(tzero,Ti,deltaTs,B,LAImax,C,vT)
test_that("bonsai", {
  expect_equal(CroptimizR:::bonsai(tzero,p["Ti"],p["deltaTs"],p["B"],p["LAImax"],p["C"],T), LAI,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(tzero,p1["Ti"],p1["deltaTs"],p1["B"],p1["LAImax"],p1["C"],T), c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(tzero,p2["Ti"],p2["deltaTs"],p2["B"],p2["LAImax"],p2["C"],T), c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(tzero,p3["Ti"],p3["deltaTs"],p3["B"],p3["LAImax"],p3["C"],T), c(1:232)*0,tolerance=1e-4)
})

#bonsai_bio <- function(t1,tfin,tzero,Ti,deltaTs,B,LAImax,C,Eb,Eimax,K,T,PAR,biom_t0)


test_that("bonsai_bio", { #t1 et tfin vont changer , biom est mesuré en 2 ans par exp (1,2,3 ca depend des cultures),
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p["Ti"],p["deltaTs"],p["B"],p["LAImax"],p["C"],p["Eb"],p["Eimax"],p["K"],T,PR,biom_t0)[,"biomas"], biom_,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p["Ti"],p["deltaTs"],p["B"],p["LAImax"],p["C"],p["Eb"],p["Eimax"],p["K"],T,PR,biom_t0)[,"LAI"], LAI,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p1["Ti"],p1["deltaTs"],p1["B"],p1["LAImax"],p1["C"],p1["Eb"],p1["Eimax"],p1["K"],T,PR,biom_t0)[,"biomas"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p1["Ti"],p1["deltaTs"],p1["B"],p1["LAImax"],p1["C"],p1["Eb"],p1["Eimax"],p1["K"],T,PR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p2["Ti"],p2["deltaTs"],p2["B"],p2["LAImax"],p2["C"],p2["Eb"],p2["Eimax"],p2["K"],T,PR,biom_t0)[,"biomas"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p2["Ti"],p2["deltaTs"],p2["B"],p2["LAImax"],p2["C"],p2["Eb"],p2["Eimax"],p2["K"],T,PR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p3["Ti"],p3["deltaTs"],p3["B"],p3["LAImax"],p3["C"],p3["Eb"],p3["Eimax"],p3["K"],T,PR,biom_t0)[,"biomas"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,tzero,p3["Ti"],p3["deltaTs"],p3["B"],p3["LAImax"],p3["C"],p3["Eb"],p3["Eimax"],p3["K"],T,PR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
})

#interface

