rm(list=ls());
DELAY=0;
INICIA=format(Sys.time(), "%a_%b_%d_%H_%M_%S");
library(foreach, lib.loc="/home/ucfilho/Documents/R_Lang")
library(iterators, lib.loc="/home/ucfilho/Documents/R_Lang")
library(GA, lib.loc="/home/ucfilho/Documents/R_Lang")

P="/home/ucfilho/Documents/Doutorados_Mestrados/Doutorado_Andre/GA_melhor_out_02_2017"
setwd(P)
# source('BEST_GA_out_02_2017.R');

GA_popSize = 100;GA_maxiter = 10;

# TIME=read.table('Melhor_ Ajustado_Sep_25_2017.txt',header=T);
# TIME=read.table('Melhor_ Ajustado_Sep_28_2017.txt',header=T);
TIME=read.table('Melhor_ Ajustado_ANDRE_Sep_29_2017.txt',header=T);

TRAIN_01=read.csv(file="fila_1_target.csv", header=TRUE, sep=",",na.strings=""); #loading train dataset
TRAIN_01[is.na(TRAIN_01)]= 0;

TRAIN_02=read.csv(file="fila_1_tch.csv", header=TRUE, sep=",",na.strings=""); #loading train dataset
TRAIN_02[is.na(TRAIN_02)]= 0;

TRAIN_03=read.csv(file="fila_1_fuel.csv", header=TRUE, sep=",",na.strings=""); #loading train dataset
TRAIN_03[is.na(TRAIN_03)]= 0;

TRAIN_04=read.csv(file="fila_1_vol.csv", header=TRUE, sep=",",na.strings=""); #loading train dataset
TRAIN_04[is.na(TRAIN_04)]= 0;

Tch=TRAIN_02$Tch;
Caminhoes=nrow(TIME);
#========================================
#========================================
# Caminhoes=10;
#========================================
#========================================

# DEMORA=Tch*0;#INICIALIZA VARIAVEL
INICIO=Tch;
AJUSTE=Tch*0;
DEMORA=TIME*0; SAI=TIME*0; FIM=rep(0,Caminhoes);
CALCULA=function(X)
 {
  X[X == 0] =1;
  for(II in 2:Caminhoes) { #for(II in 1:Caminhoes){ 
    DEMORA[II,X[(II-1)]]=TIME[(II-1),X[(II-1)]];
   } # for(II in 1:Caminhoes) { 
  SAI[1,X[1]]=Tch[1]+TIME[1,X[1]];FIM[1]=SAI[1,X[1]];
  for(i in 2:Caminhoes) { # for(i in 1:Caminhoes)
    if(Tch[i]> SAI[(i-1),X[i]]) { SAI[i,X[i]]=Tch[i]+TIME[i,X[i]];} else { SAI[i,X[i]]=SAI[(i-1),X[i]]+TIME[i,X[i]];}
    FIM[i]=SAI[i,X[i]];
  } #for(i in 2:Caminhoes) { # for(i in 1:Caminhoes)
  return((max(SAI)));
} # CALCULA=function(X)

  ANDRE=Tch*0;

  for(II in 1:Caminhoes) { #for(II in 1:Caminhoes){ 
    	ANDRE[II]=which(TRAIN_01[II,]==1)
  } # for(II in 1:Caminhoes)




fun = function(X)  # funcao do GA
{
  X=as.vector(X)
  X=round(X);
  X[X == 0] =1;
  AJUSTE=CALCULA(X);
  y.calc=AJUSTE;
  return(y.calc);
} # fim ----> fun <- function(x) { # FIM FUNCAO DO GA

MIN.GA=rep(0,Caminhoes);
MAX.GA=rep(4,Caminhoes);
x=rep(1,Caminhoes)
GA = ga(type = "real-valued", fitness =function(x) -fun(x),
        popSize = GA_popSize,
        pcrossover = 0.9,
        pmutation = 0.1,
	monitor = TRUE,
        min = MIN.GA, max = MAX.GA,
        maxiter =  GA_maxiter)
FIM=format(Sys.time(), "%a_%b_%d_%H_%M_%S");

summary(GA)
dev.new();plot(GA)
Mat=0;
Mat=GA@solution;
OTIMA=0;OTIMA=as.vector(round(Mat[1,]));
OTIMA[OTIMA == 0] =1;
AJUSTE=CALCULA(OTIMA);


cat("DELAY=",DELAY,"\n");
cat("INICIA=",INICIA,"\n");
cat("FIM=",FIM,"\n");
cat("AJUSTE=",AJUSTE,"\n");

AJUSTE=CALCULA(ANDRE);
cat("DELAY=",DELAY,"\n");
cat("INICIA=",INICIA,"\n");
cat("FIM=",FIM,"\n");
cat("AJUSTE=",AJUSTE,"\n");

print(table(ANDRE,OTIMA))

