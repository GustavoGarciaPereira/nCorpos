library(reshape)
library(ggplot2)
library(scales)
library(mgcv)

pontos = read.table("nCorpos600sequencial.tsv",sep = ",",header = T) 
pontosa= read.table("nCorpos600paralelo.tsv",sep = ",",header = T)

teste = pontos[1]/pontosa[1]

pontosa[1] = teste[1]
m<-qplot(Cores,Tempo, data= pontosa,ylim=c(0,6),xlab="Nucleos",ylab="Aceleração", colour=Complexidade , geom = c("point", "smooth"))+theme(axis.text=element_text(size=26,colour="black"),axis.title=element_text(size=20,face="bold"))+ theme(axis.text.x=element_text(colour="black",size=20),axis.text.y=element_text(colour="black",size=20),legend.text  = element_text(colour="black", size=20),legend.title  = element_text(colour="black", size=18))

