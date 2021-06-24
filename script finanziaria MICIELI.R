#################################################################
###                                                           ###
###   Studente: MICIELI OTTAVIO                               ###
###   Matricola: 214209                                       ###
###                                                           ###
###   Script tesina analisi serie storica di tipo finanziario ###
###   Esame MODELLI E TECNICHE DI PREVISIONE                  ###
###   Professore PIERFRANCESCO PERRI                          ###
###                                                           ###
#################################################################

####                  PACCHETTI UTILIZZATI                   ####
library(doParallel)
library(fBasics)
library(fitdistrplus)
library(fGarch)
library(forecast)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lmtest)
library(lubridate)
library(Metrics)
library(metRology)
library(moments)
library(MTS)
library(qqplotr)
library(rugarch)
library(seastests)
library(tidyverse)
library(timeSeries)
library(TSA)
library(tseries)
library(xts)

# Da usare per esecuzione parallela delle funzioni
numCores<-detectCores(); numCores
cl<-makeCluster(numCores)

####                  LETTURA DATI DA FILE                   ####

setwd("C:/Users/ottav/Desktop/Progetto Perri/Serie finanziaria")
temp<-read.csv("Nintendo LTD.csv")
price<-xts(temp[,5],order.by=as.Date(temp[,1]))

####         ANALISI DESCRITTIVA DELLA SERIE STORICA         ####

simple_rend<-returns(price,method="simple")
log_rend<-returns(price,method="continuous")

data<-data.frame(tempo=as.Date(temp[,1]),prezzi=price,Rendt=simple_rend,rt=log_rend)

ggplot(data,aes(x=tempo,y=prezzi))+
   geom_line(colour="#00C3E3",cex=1)+
   labs(title="Andamento dei prezzi azioni Nintendo LTD\n",x="",y="Prezzo di chiusura (in yen)\n")+
   scale_x_date(date_breaks="1 year",date_minor_breaks="3 months")+
   theme_bw()+
   theme(
      plot.title=element_text(hjust=0.5,size=20),
      panel.grid=element_line(linetype=4,color="grey80"),
      axis.title=element_text(size=12)
   )

grid.arrange(
ggplot(data,aes(x=tempo,y=x))+
   geom_line(colour="#00C3E3")+
   labs(title="Andamento dei rendimenti semplici Nintendo LTD\n",x="",y=bquote(R~""[t]~"\n"))+
   scale_x_date(date_breaks="1 year",date_minor_breaks="3 months")+
   theme_bw()+
   theme(
      plot.title=element_text(hjust=0.5,size=20),
      panel.grid=element_line(linetype=4,color="grey80"),
      axis.title=element_text(size=12)
   ),
ggplot(data,aes(x=tempo,y=x.1))+
   geom_line(colour="#e60012")+
   labs(title="Andamento dei rendimenti logaritmici Nintendo LTD\n",x="",y=bquote(r~""[t]~"\n"))+
   scale_x_date(date_breaks="1 year",date_minor_breaks="3 months")+
   theme_bw()+
   theme(
      plot.title=element_text(hjust=0.5,size=20),
      panel.grid=element_line(linetype=4,color="grey80"),
      axis.title=element_text(size=12)
   ))

log_rend<-xts(log_rend[-1],order.by=as.Date(temp[-1,1]))

basicStats(log_rend)

ksnormTest(log_rend)
shapiroTest(log_rend)
jarqueberaTest(log_rend)
dagoTest(log_rend)

agostino.test(log_rend)
anscombe.test(log_rend)

data<-data.frame(value=log_rend)

sstd_Fit<-sstdFit(log_rend)
ggplot(data=data,aes(x=value))+
   geom_histogram(aes(y=..density..),bins=80,size=0.1,color="white",fill="gray80")+
   geom_vline(aes(xintercept=mean(value)),cex=0.7)+
   stat_function(aes(colour="Distribuzione Gaussiana"),fun=dnorm,args=list(mean=mean(log_rend),sd=sd(log_rend)),cex=1)+
   stat_function(aes(colour="Distribuzione T-Student asimmetrica"),fun=dsstd,args=list(mean=sstd_Fit$estimate[1],sd=sstd_Fit$estimate[2],xi=sstd_Fit$estimate[4],nu=sstd_Fit$estimate[3]),cex=1)+
   labs(title="Istogramma dei rendimenti\n",x=bquote(r[t]))+
   annotate(geom="text",x=mean(data$value)+0.01,y=3.7,label=bquote("Media: "~.(round(mean(data$value),4))),angle=270,size=3.5)+
   scale_color_manual("",breaks=c("Distribuzione Gaussiana","Distribuzione T-Student asimmetrica"),
                      values=c("Distribuzione Gaussiana"="#00c3e3","Distribuzione T-Student asimmetrica"="#ff4554"))+
   theme_bw()+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70"),
      legend.position=c(.95,.95),
      legend.justification=c("right","top"),
      legend.box.just="right",
      legend.margin=margin(6,6,6,6)
   )

grid.arrange(
ggplot(data,aes(sample=value))+
   stat_qq_band(distribution="norm",
                dparams=list(mean=mean(data$value),sd=sd(data$value)))+
   stat_qq_point(distribution="norm",
                 dparams=list(mean=mean(data$value),sd=sd(data$value)),colour="#00c3e3",size=2)+
   stat_qq_line(distribution="norm",
                dparams=list(mean=mean(data$value),sd=sd(data$value)))+
   theme_bw()+
   labs(title="QQ plot Gaussiana\n",x="Teoriche",y="Osservate")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ),
ggplot(data,aes(sample=value))+
   stat_qq_band(distribution="sstd",
                dparams=list(mean=sstd_Fit$estimate[1],sd=sstd_Fit$estimate[2],xi=sstd_Fit$estimate[4],
                             nu=sstd_Fit$estimate[3]))+
   stat_qq_point(distribution="sstd",
                 dparams=list(mean=sstd_Fit$estimate[1],sd=sstd_Fit$estimate[2],xi=sstd_Fit$estimate[4],
                              nu=sstd_Fit$estimate[3]),colour="#ff4554",size=2)+
   stat_qq_line(distribution="sstd",
                dparams=list(mean=sstd_Fit$estimate[1],sd=sstd_Fit$estimate[2],xi=sstd_Fit$estimate[4],
                             nu=sstd_Fit$estimate[3]))+
   theme_bw()+
   labs(title="QQ plot T-Student asimmetrica\n",x="Teoriche",y="Osservate")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ),nrow=1)

corr_plot<-function(serie,lag.max,subtitle){
   correl<-acf(serie,plot=F,lag.max=lag.max)
   data<-data.frame(value=correl$acf,lag=correl$lag)
   ggplot(data,aes(x=lag,y=value))+
      geom_col(fill="#00C3E3",width=0.7)+
      geom_hline(yintercept=qnorm((1-0.95)/2)/sqrt(length(serie)),color="grey50",linetype="dashed",cex=1)+
      geom_hline(yintercept=-qnorm((1-0.95)/2)/sqrt(length(serie)),color="grey50",linetype="dashed",cex=1)+
      geom_hline(yintercept=0,color="black",cex=0.8)+
      theme_bw()+
      labs(title="Valori ACF",subtitle=subtitle,x="Lag k",y=bquote(hat(rho[k])))+
      theme(
         plot.title=element_text(size=12,hjust=0.5),
         plot.subtitle=element_text(size=12,hjust=0.5,color="grey50"),
         axis.title=element_text(size=10),
         axis.text=element_text(size=7),
         panel.grid=element_line(linetype="dashed",color="grey90")
      )}

grid.arrange(corr_plot(log_rend,lag.max=30,"rendimenti"),
            corr_plot(log_rend^2,lag.max=30,"rendimenti al quadrato"),
            corr_plot(abs(log_rend),lag.max=30,"rendimenti in valore assoluto"),nrow=3)

Box.test(log_rend,lag=30,type="Ljung-Box")
Box.test(log_rend^2,lag=30,type="Ljung-Box")
Box.test(abs(log_rend),lag=30,type="Ljung-Box")

# Analisi presenza effetto leva
cross_c<-data.frame(corr=ccf(log_rend^2,log_rend,plot=F)$acf,lag=ccf(log_rend^2,log_rend,plot=F)$lag)
ggplot(cross_c,aes(x=lag,y=corr))+
   geom_col(fill="#00C3E3",width=0.7)+
   geom_hline(yintercept=qnorm((1-0.95)/2)/sqrt(length(log_rend)),color="grey50",linetype="dashed",cex=1)+
   geom_hline(yintercept=-qnorm((1-0.95)/2)/sqrt(length(log_rend)),color="grey50",linetype="dashed",cex=1)+
   geom_hline(yintercept=0,color="black",cex=0.8)+
   theme_bw()+
   labs(title="Cross correlation",x="Lag k",y=bquote("corr("~r[t]^2~","~r[t-k]~")"))+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=10),
      axis.text=element_text(size=7),
      panel.grid=element_line(linetype="dashed",color="grey90")
   )

# Analisi persistenza
mc.test<-data.frame(mc=McLeod.Li.test(y=log_rend,plot=F,gof.lag=50),lag=1:50) #Presenza di persistenza nei dati
ggplot(mc.test,aes(x=lag,y=p.values))+
   geom_point(size=3,color="#00C3E3")+
   geom_hline(yintercept=0.05,color="grey50",linetype="dashed",cex=1)+
   scale_y_continuous(limits=c(0,1))+
   theme_bw()+
   labs(title="Mc.Leod-Li test\n",x="Lag k",y="p-values\n")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=12),
      axis.text=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey90")
   )

archTest(log_rend,lag=100)

####              ANALISI DELLA COMPONENTE TREND             ####

adf.test(log_rend,alternative="stationary")
adf.test(log_rend,alternative="explosive")
kpss.test(log_rend)
pp.test(log_rend,alternative="stationary")
pp.test(log_rend,alternative="explosive")

# Analisi della stagionalità
lag.plot(log_rend,lag=60)
lag.plot(apply.monthly(ts(log_rend),mean),lag=50)
isSeasonal(apply.monthly(log_rend,mean),freq=12)
isSeasonal(apply.monthly(log_rend,mean),freq=4)
isSeasonal(apply.monthly(log_rend,mean),freq=3)

# Analisi modello per la media condizionata

auto.arima(log_rend)
coeftest(auto.arima(log_rend)) #ar1 non significativo

p<-q<-0:2
par<-expand.grid(p,q)
arma_search<-lapply(1:nrow(par),function(i){
   md<-NULL
   md<-Arima(log_rend,order=c(par$Var1[i],0,par$Var2[i]))
   results<-data.frame(p=par$Var1[i],q=par$Var2[i],AIC=AIC(md),BIC=BIC(md))
}) %>% bind_rows()
arma_search %>% arrange(AIC) %>% head()
arma_search %>% arrange(BIC) %>% head()

coeftest(Arima(log_rend,order=c(1,0,2))) # tutti i coefficienti sono significativi
coeftest(Arima(log_rend,order=c(0,0,2))) # non tutti i coefficienti significativi

mean.model<-Arima(log_rend,order=c(1,0,2))
data<-data.frame(tempo=as.Date(temp[-1,1]),observed=log_rend,fitted=fitted(mean.model))
ggplot(data,aes(x=tempo))+
   geom_line(aes(y=observed,colour="Osservati"))+
   geom_line(aes(y=fitted,colour="Teorici"))+
   labs(title="Andamento dei log-rendimenti Nintendo LTD",subtitle=bquote("Confronto tra "~r[t]~" e "~hat(r[t])~" Modello ARMA(1,2)"),x="",y="")+
   scale_x_date(date_breaks="1 year",date_minor_breaks="3 months")+
   scale_colour_manual("",breaks=c("Osservati","Teorici"),values=c("Osservati"="#00C3E3","Teorici"="#ff4554"))+
   theme_bw()+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=15,hjust=0.5,color="grey40"),
      axis.text=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey90"),
      legend.position=c(.95,.95),
      legend.justification=c("right","top"),
      legend.box.just="right",
      legend.margin=margin(6,6,6,6)
   )

mc.test<-data.frame(mc=McLeod.Li.test(mean.model,plot=F,gof.lag=50),lag=1:50) #Presenza di persistenza nei dati
ggplot(mc.test,aes(x=lag,y=p.values))+
   geom_point(size=3,color="#ff4554")+
   geom_hline(yintercept=0.05,color="grey50",linetype="dashed",cex=1)+
   scale_y_continuous(limits=c(0,1))+
   theme_bw()+
   labs(title="Mc.Leod-Li test\n",x="Lag k",y="p-values\n")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=12),
      axis.text=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey90")
   )

Box.test(residuals(mean.model),lag=10,type="Ljung-Box")
Box.test(residuals(mean.model)^2,lag=10,type="Ljung-Box") # i residui al quadrato (proxy della volatilità) sono correlati
Box.test(abs(residuals(mean.model)),lag=10,type="Ljung-Box") # stessa cosa i residui in valore assoluto


####          MODELLO PER LA VARIANZA CONDIZIONATA           ####

# Analisi considerando il modello per la media già individuato

a<-1:3
b<-0:3
prob<-c("norm","snorm","std","sstd","nig","ged","sged","jsu")
par<-expand.grid(a,b,prob); par[,3]<-as.character(par[,3])
clusterExport(cl,varlist=c("par","log_rend"))
garch_search<-parLapply(cl,1:nrow(par),function(i){
   library(rugarch)
   md<-NULL
   spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(par$Var1[i],par$Var2[i])),
                    mean.model=list(armaOrder=c(1,2),include.mean=T),
                    distribution.model=par$Var3[i])
   md<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
   results<-data.frame(alpha=par$Var1[i],beta=par$Var2[i],Prob_dist=par$Var3[i],
                       AIC=infocriteria(md)[1],BIC=infocriteria(md)[2],Shibata=infocriteria(md)[3],H_Q=infocriteria(md)[4])
}) %>% bind_rows()
garch_search %>% arrange(AIC) %>% head()
garch_search %>% arrange(BIC) %>% head()
garch_search %>% arrange(Shibata) %>% head()
garch_search %>% arrange(H_Q) %>% head()

# Ignoriamo la media condizionata e individuiamo il miglior modello GARCH

a<-1:3
b<-0:3
prob<-c("norm","snorm","std","sstd","nig","ged","sged","jsu")
par<-expand.grid(a,b,prob); par[,3]<-as.character(par[,3])
clusterExport(cl,varlist=c("par","log_rend"))
garch_search2<-parLapply(cl,1:nrow(par),function(i){ #utilizziamo l'esecuzione parallela
   library(rugarch)
   md<-NULL
   spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(par$Var1[i],par$Var2[i])),
                    mean.model=list(armaOrder=c(0,0),include.mean=F),
                    distribution.model=par$Var3[i])
   md<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
   results<-data.frame(alpha=par$Var1[i],beta=par$Var2[i],Prob_dist=par$Var3[i],
                       AIC=infocriteria(md)[1],BIC=infocriteria(md)[2],Shibata=infocriteria(md)[3],H_Q=infocriteria(md)[4])
}) %>% bind_rows()
garch_search2 %>% arrange(AIC) %>% head()
garch_search2 %>% arrange(BIC) %>% head()
garch_search2 %>% arrange(Shibata) %>% head()
garch_search2 %>% arrange(H_Q) %>% head()

# Non forziamo nessun ordine per il modello per la media

a<-1:3
b<-0:3
p<-q<-0:2
prob<-c("norm","snorm","std","sstd","nig","ged","sged","jsu")
par<-expand.grid(a,b,p,q,prob); par[,5]<-as.character(par[,5])
clusterExport(cl,varlist=c("par","log_rend"))
garch_search3<-parLapply(cl,1:nrow(par),function(i){ #Ci impiega una decina di minuti con l'esecuzione parallela
   library(rugarch)
   md<-NULL
   spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(par$Var1[i],par$Var2[i])),
                    mean.model=list(armaOrder=c(par$Var3[i],par$Var4[i]),include.mean=T),
                    distribution.model=par$Var5[i])
   md<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
   results<-data.frame(alpha=par$Var1[i],beta=par$Var2[i],p=par$Var3[i],q=par$Var4[i],Prob_dist=par$Var5[i],
                       AIC=infocriteria(md)[1],BIC=infocriteria(md)[2],Shibata=infocriteria(md)[3],H_Q=infocriteria(md)[4])
}) %>% bind_rows()
garch_search3 %>% arrange(AIC) %>% head()
garch_search3 %>% arrange(BIC) %>% head()
garch_search3 %>% arrange(Shibata) %>% head()
garch_search3 %>% arrange(H_Q) %>% head()

####         ANALISI DEI MODELLI RISULTATI MIGLIORI          ####

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,2)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="sged")
A_1.2xG_2.2<-ugarchfit(spec=spec, data=log_rend, solver="hybrid") 
A_1.2xG_2.2

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="ged")
A_1.2xG_1.1<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
A_1.2xG_1.1

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="ged")
A_1.2xG_2.1<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
A_1.2xG_2.1

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="sged")
A_0.0xG_2.1<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
A_0.0xG_2.1

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
A_0.0xG_1.1<-ugarchfit(spec=spec, data=log_rend, solver="hybrid") 
A_0.0xG_1.1

criteria<-cbind(infocriteria(A_1.2xG_2.2),infocriteria(A_1.2xG_1.1),infocriteria(A_1.2xG_2.1),
                infocriteria(A_0.0xG_2.1),infocriteria(A_0.0xG_1.1))
colnames(criteria)<-c("A_1.2xG_2.2","A_1.2xG_1.1","A_1.2xG_2.1","A_0.0xG_2.1","A_0.0xG_1.1")
criteria<-data.frame(t(criteria))
criteria %>% arrange(Akaike)
criteria %>% arrange(Bayes)
criteria %>% arrange(Shibata)
criteria %>% arrange(Hannan.Quinn)

data<-data.frame(value=log_rend)
ged_Fit<-gedFit(log_rend)
ggplot(data=data,aes(x=value))+
   geom_histogram(aes(y=..density..),bins=80,size=0.1,color="white",fill="gray80")+
   geom_vline(aes(xintercept=mean(value)),cex=0.7)+
   geom_function(aes(colour="Distribuzione Gaussiana"),fun=dnorm,args=list(mean=mean(data$value),sd=sd(data$value)),cex=1)+
   stat_function(aes(colour="Distribuzione T-Student asimmetrica"),fun=dsstd,args=list(mean=sstd_Fit$estimate[1],sd=sstd_Fit$estimate[2],xi=sstd_Fit$estimate[4],nu=sstd_Fit$estimate[3]),cex=1)+
   stat_function(aes(colour="Generalized Error Distribution"),fun=dged,args=list(mean=ged_Fit$par[1],sd=ged_Fit$par[2],nu=ged_Fit$par[3]),cex=1)+
   labs(title="Istogramma dei rendimenti\n",x=bquote(r[t]))+
   annotate(geom="text",x=mean(data$value)+0.01,y=3.7,label=bquote("Media: "~.(round(mean(data$value),4))),angle=270,size=3.5)+
   scale_color_manual("",breaks=c("Distribuzione Gaussiana","Distribuzione T-Student asimmetrica","Generalized Error Distribution"),
                      values=c("Distribuzione Gaussiana"="#00c3e3","Distribuzione T-Student asimmetrica"="#ff4554","Generalized Error Distribution"="#492157"))+
   theme_bw()+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70"),
      legend.position=c(.95,.95),
      legend.justification=c("right","top"),
      legend.box.just="right",
      legend.margin=margin(6,6,6,6)
   )

ggplot(data,aes(sample=value))+
   stat_qq_band(distribution="ged",
               dparams=list(mean=ged_Fit$par[1],sd=ged_Fit$par[2],nu=ged_Fit$par[3]))+
   stat_qq_point(distribution="ged",
               dparams=list(mean=ged_Fit$par[1],sd=ged_Fit$par[2],nu=ged_Fit$par[3]),colour="#00c3e3",size=2)+
   stat_qq_line(distribution="ged",
               dparams=list(mean=ged_Fit$par[1],sd=ged_Fit$par[2],nu=ged_Fit$par[3]))+
   theme_bw()+
   labs(title="QQ plot Generalized Error Distribution\n",x="Teoriche",y="Osservate")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   )

####   ANALISI ACCURATEZZA PREVISIONI MODELLI INDIVIDUATI    ####

# ANALISI MODELLO A_1.2xG_1.1

data<-data.frame(time=A_1.2xG_1.1@model$modeldata$index,xseries=A_1.2xG_1.1@model$modeldata$data,sigma=A_1.2xG_1.1@fit$sigma)
g1<-ggplot(data,aes(x=time))+ # 2 Conditional SD superimposed plot
   geom_line(aes(y=xseries),colour="#00c3e3")+
   geom_line(aes(y=sigma*2),colour="#ff4554")+
   geom_line(aes(y=sigma*-2),colour="#ff4554")+
   theme_bw()+
   labs(title="Series with 2 Conditional SD Superimposed",x="Time",y="Returns",subtitle="Modello ARFIMA(1,0,2) e modello sGARCH(1,1)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 

g3<-ggplot(data,aes(x=time))+ #Conditional SD vs. |returns|
   geom_line(aes(y=abs(xseries)),colour="grey70")+
   geom_line(aes(y=sigma),colour="#ff4554",cex=1)+
   theme_bw()+
   labs(title=bquote("Conditional SD vs."~abs(r[t])),x="Time",y="Volatility",subtitle="Modello ARFIMA(1,0,2) e modello sGARCH(1,1)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 

# ANALISI MODELLO A_0.0xG_1.1

data<-data.frame(time=A_0.0xG_1.1@model$modeldata$index,xseries=A_0.0xG_1.1@model$modeldata$data,sigma=A_0.0xG_1.1@fit$sigma)
g2<-ggplot(data,aes(x=time))+ # 2 Conditional SD superimposed plot
   geom_line(aes(y=xseries),colour="#00c3e3")+
   geom_line(aes(y=sigma*2),colour="#ff4554")+
   geom_line(aes(y=sigma*-2),colour="#ff4554")+
   theme_bw()+
   labs(title="Series with 2 Conditional SD Superimposed",x="Time",y="Returns",subtitle="Modello ARFIMA(0,0,0) e modello sGARCH(1,1)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 

g4<-ggplot(data,aes(x=time))+ #Conditional SD vs. |returns|
   geom_line(aes(y=abs(xseries)),colour="grey70")+
   geom_line(aes(y=sigma),colour="#ff4554",cex=1)+
   theme_bw()+
   labs(title=bquote("Conditional SD vs."~abs(r[t])),x="Time",y="Volatility",subtitle="Modello ARFIMA(0,0,0) e modello sGARCH(1,1)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 
grid.arrange(g1,g2,nrow=2)
grid.arrange(g3,g4,nrow=2)

# Mis-specification Test
GMMTest(residuals(A_1.2xG_1.1,T),
        skew=dskewness("ged",shape=coef(A_1.2xG_1.1)["shape"]),
        kurt=3+dkurtosis("ged",shape=coef(A_1.2xG_1.1)["shape"]))

GMMTest(residuals(A_0.0xG_1.1,T),
        skew=dskewness("ged",shape=coef(A_0.0xG_1.1)["shape"]),
        kurt=3+dkurtosis("ged",shape=coef(A_0.0xG_1.1)["shape"]))

infocriteria(A_1.2xG_1.1); infocriteria(A_0.0xG_1.1)

####                 ANALISI MODELLO EGARCH                  ####

a<-b<-1:3
p<-q<-0:2
prob<-c("std","sstd","ged","sged")
par<-expand.grid(a,b,p,q,prob); par[,5]<-as.character(par[,5])
clusterExport(cl,varlist=c("par","log_rend"))
egarch_search<-parLapply(cl,1:nrow(par),function(i){ 
   library(rugarch)
   md<-NULL
   spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(par[i,1],par[i,2])),
                    mean.model=list(armaOrder=c(par[i,3],par[i,4]),include.mean=T),
                    distribution.model=par[i,5])
   md<-ugarchfit(spec=spec, data=log_rend, solver="hybrid")
   results<-data.frame(alpha=par[i,1],beta=par[i,2],p=par[i,3],q=par[i,4],Prob_dist=par[i,5],
                       AIC=infocriteria(md)[1],BIC=infocriteria(md)[2],Shibata=infocriteria(md)[3],H_Q=infocriteria(md)[4])
}) %>% bind_rows()
egarch_search %>% arrange(AIC) %>% head()
egarch_search %>% arrange(BIC) %>% head()
egarch_search %>% arrange(Shibata) %>% head()
egarch_search %>% arrange(H_Q) %>% head()

spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(3,3)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
A_0.0xeG_3.3<-ugarchfit(spec,log_rend,solver="hybrid")
A_0.0xeG_3.3

spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
A_0.0xeG_1.1<-ugarchfit(spec,log_rend,solver="hybrid") #parametro alpha1 non significativo
A_0.0xeG_1.1

criteria<-cbind(infocriteria(A_0.0xG_1.1),infocriteria(A_0.0xeG_3.3),infocriteria(A_0.0xeG_1.1))
colnames(criteria)<-c("A_0.0xG_1.1","A_0.0xeG_3.3","A_0.0xeG_1.1")
criteria<-data.frame(t(criteria))
criteria %>% arrange(Akaike)
criteria %>% arrange(Bayes)
criteria %>% arrange(Shibata)
criteria %>% arrange(Hannan.Quinn)

GMMTest(residuals(A_0.0xeG_3.3,T),
        skew=dskewness("ged",shape=coef(A_0.0xeG_3.3)["shape"]),
        kurt=3+dkurtosis("ged",shape=coef(A_0.0xeG_3.3)["shape"]))
GMMTest(residuals(A_0.0xeG_1.1,T),
        skew=dskewness("ged",shape=coef(A_0.0xeG_3.3)["shape"]),
        kurt=3+dkurtosis("ged",shape=coef(A_0.0xeG_3.3)["shape"]))

data<-data.frame(time=A_0.0xeG_3.3@model$modeldata$index,xseries=A_0.0xeG_3.3@model$modeldata$data,sigma=A_0.0xeG_3.3@fit$sigma)
g1<-ggplot(data,aes(x=time))+ # 2 Conditional SD superimposed plot
   geom_line(aes(y=xseries),colour="#00c3e3")+
   geom_line(aes(y=sigma*2),colour="#ff4554")+
   geom_line(aes(y=sigma*-2),colour="#ff4554")+
   theme_bw()+
   labs(title="Series with 2 Conditional SD Superimposed",x="Time",y="Returns",subtitle="Modello ARFIMA(0,0,0) e modello eGARCH(3,3)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 

g3<-ggplot(data,aes(x=time))+ #Conditional SD vs. |returns|
   geom_line(aes(y=abs(xseries)),colour="grey70")+
   geom_line(aes(y=sigma),colour="#ff4554",cex=1)+
   theme_bw()+
   labs(title=bquote("Conditional SD vs."~abs(r[t])),x="Time",y="Volatility",subtitle="Modello ARFIMA(0,0,0) e modello eGARCH(3,3)")+
   theme()+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   )

data<-data.frame(time=A_0.0xeG_1.1@model$modeldata$index,xseries=A_0.0xeG_1.1@model$modeldata$data,sigma=A_0.0xeG_1.1@fit$sigma)
g2<-ggplot(data,aes(x=time))+ # 2 Conditional SD superimposed plot
   geom_line(aes(y=xseries),colour="#00c3e3")+
   geom_line(aes(y=sigma*2),colour="#ff4554")+
   geom_line(aes(y=sigma*-2),colour="#ff4554")+
   theme_bw()+
   labs(title="Series with 2 Conditional SD Superimposed",x="Time",y="Returns",subtitle="Modello ARFIMA(0,0,0) e modello eGARCH(1,1)")+
   theme()+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 

g4<-ggplot(data,aes(x=time))+ #Conditional SD vs. |returns|
   geom_line(aes(y=abs(xseries)),colour="grey70")+
   geom_line(aes(y=sigma),colour="#ff4554",cex=1)+
   theme_bw()+
   labs(title=bquote("Conditional SD vs."~abs(r[t])),x="Time",y="Volatility",subtitle="Modello ARFIMA(0,0,0) e modello eGARCH(1,1)")+
   theme(
      plot.title=element_text(size=15,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,color="grey40"),
      axis.title=element_text(size=10),
      panel.grid=element_line(linetype="dashed",color="grey80")
   ) 
grid.arrange(g1,g2,nrow=2)
grid.arrange(g3,g4,nrow=2)

####              PREVISIONI SU MODELLO FINALE               ####

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="ged")
final_model<-ugarchfit(spec,log_rend,out.sample=220,solver="hybrid")
prev1<-ugarchforecast(final_model,n.roll=220,n.ahead=220)
fpm_A_1.2xG_1.1<-fpm(prev1)

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
final_model<-ugarchfit(spec,log_rend,out.sample=220,solver="hybrid")
prev2<-ugarchforecast(final_model,n.roll=220,n.ahead=220)
fpm_A_0.0xG_1.1<-fpm(prev2)

spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(3,3)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
final_model<-ugarchfit(spec,log_rend,out.sample=220,solver="hybrid")
prev3<-ugarchforecast(final_model,n.roll=220,n.ahead=220)
fpm_A_0.0xeG_3.3<-fpm(prev3)

spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0),include.mean=T),
                 distribution.model="ged")
final_model<-ugarchfit(spec,log_rend,out.sample=220,solver="hybrid")
prev4<-ugarchforecast(final_model,n.roll=220,n.ahead=220)
fpm_A_0.0xeG_1.1<-fpm(prev4)

results<-rbind(data.frame(RMSE=sqrt(mean(fpm_A_1.2xG_1.1[1,-c(117:221)])),MAE=mean(fpm_A_1.2xG_1.1[2,-c(117:221)])),
               data.frame(RMSE=sqrt(mean(fpm_A_0.0xG_1.1[1,-c(117:221)])),MAE=mean(fpm_A_0.0xG_1.1[2,-c(117:221)])),
               data.frame(RMSE=sqrt(mean(fpm_A_0.0xeG_3.3[1,-c(117:221)])),MAE=mean(fpm_A_0.0xeG_3.3[2,-c(117:221)])),
               data.frame(RMSE=sqrt(mean(fpm_A_0.0xeG_1.1[1,-c(117:221)])),MAE=mean(fpm_A_0.0xeG_1.1[2,-c(117:221)])))
rownames(results)<-c("A_1.2xG_1.1","A_0.0xG_1.1","A_0.0xeG_3.3","0.0xeG_1.1")
results %>% arrange(RMSE)
results %>% arrange(MAE)


spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="ged")
final_model_roll<-ugarchroll(spec,log_rend,n.ahead=1,forecast.length=242,refit.every=1,refit.window="moving",
                             parallel=T,parallel.control=list(pkg="multicores",cores=8),solver="hybrid",calculate.VaR = T,
                             VaR.alpha = c(0.010, 0.05))
fm_roll<-as.data.frame(final_model_roll,which="VaR")
resume(final_model_roll)
report(final_model_roll,type="fpm")
report(final_model_roll,type="VaR",VaR.alpha=0.01,conf.level=0.99)

data<-data.frame(time=as.Date(temp[1001:1242,1]),ret=last(log_rend,"year"),VaR=fm_roll$`alpha(1%)`)
ggplot(data,aes(x=time))+
   geom_point(aes(y=ret,colour="rendimenti"))+
   geom_point(aes(y=ret,colour="rendimento<VaR"),data=subset(data,ret<VaR),cex=2)+
   geom_line(aes(y=VaR,colour="VaR"),cex=1)+
   scale_color_manual("",breaks=c("rendimenti","rendimento<VaR","VaR"),
                      values=c("rendimenti"="#00c3e3","rendimento<VaR"="#ff4554","VaR"="grey40"))+
   labs(title="Rendimenti vs previsioni VaR",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\ntime",y=bquote(r[t]~" &  VaR"))+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70"),
      legend.position="bottom",
      legend.spacing.x=unit(0.5,"cm"),
      legend.box.just=.5,
      legend.margin=margin(3,3,8,3)
   )

data<-data.frame(time=as.Date(temp[1001:1242,1]),ret=last(log_rend,"year"),sigma=final_model_roll@forecast$density$Sigma)
ggplot(data,aes(x=time))+
   geom_line(aes(y=abs(ret),colour="rendimenti"),cex=1)+
   geom_line(aes(y=sigma,colour="sigma"),cex=1)+
   scale_color_manual("",breaks=c("rendimenti","sigma"),
                      values=c("rendimenti"="#00c3e3","sigma"="#ff4554"))+
   labs(title="|Rendimenti| vs SD condizionata",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\ntime",y=bquote(r[t]~" & "~hat(sigma[t])))+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70"),
      legend.position="bottom",
      legend.spacing.x=unit(0.5,"cm"),
      legend.box.just=.5,
      legend.margin=margin(3,3,8,3)
   )

spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(1,2),include.mean=T),
                 distribution.model="ged")
m1<-ugarchfit(spec,log_rend,solver="hybrid")
plot(m1,which=2)

param<-m1@fit$ipars[,1]
up1<-fitted(m1)+sigma(m1)*qdist(distribution="ged",0.005,0,1,lambda=0,skew=0,shape=0.9824217)
down1<-fitted(m1)+sigma(m1)*qdist(distribution="ged",0.995,0,1,lambda=0,skew=0,shape=0.9824217)
data<-data.frame(time=as.Date(temp[-1,1]),ret=log_rend,up=up1,down=down1)
ggplot(data,aes(x=time))+
   geom_line(aes(y=ret),colour="grey70")+
   geom_line(aes(y=up),colour="#00c3e3",cex=0.7)+
   geom_line(aes(y=down),colour="#ff4554",cex=0.7)+
   labs(title="Rendimenti con VaR all'99% di confidenza",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\ntime",y="Rendimenti")+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70")
   )

up1<-fitted(m1)+sigma(m1)*qdist(distribution="ged",0.025,0,1,lambda=0,skew=0,shape=0.9824217)
down1<-fitted(m1)+sigma(m1)*qdist(distribution="ged",0.975,0,1,lambda=0,skew=0,shape=0.9824217)
data<-data.frame(time=as.Date(temp[-1,1]),ret=log_rend,up=up1,down=down1)
ggplot(data,aes(x=time))+
   geom_line(aes(y=ret),colour="grey70")+
   geom_line(aes(y=up),colour="#00c3e3",cex=0.7)+
   geom_line(aes(y=down),colour="#ff4554",cex=0.7)+
   labs(title="Rendimenti con VaR all'95% di confidenza",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\ntime",y="Rendimenti")+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey70")
   )

prev<-ugarchforecast(m1,n.ahead=20)

time<-c(as.Date(temp[1202:1242,1]),seq(from=as.Date("2021/01/01"),to=as.Date("2021/01/20"),by="day"))
sigma<-c(m1@fit$sigma[1201:1241],prev@forecast$sigmaFor)
data<-data.frame(time=as.Date(time),sigma=sigma)
ggplot(data,aes(x=time))+
   geom_line(aes(y=sigma),data=subset(data[1:41,]),colour="#00c3e3",cex=1)+
   geom_line(aes(y=sigma),data=subset(data[41:61,]),colour="#ff4554",cex=1)+
   labs(title="Previsioni della volatilità",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\nTime/Horizon",y=bquote(sigma))+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey80")
   )

time<-c(as.Date(temp[1163:1242,1]),seq(from=as.Date("2021/01/01"),to=as.Date("2021/01/20"),by="day"))
series<-c(as.vector(log_rend[1162:1241]),prev@forecast$seriesFor[,1])
sigma<-c(rep(0,times=80),prev@forecast$sigmaFor[,1])
data<-data.frame(time=time,series=series,sigma=sigma)
ggplot(data,aes(x=time))+
   geom_ribbon(aes(ymin=series-sigma,ymax=series+sigma),data=subset(data[81:100,]),colour="#989898",alpha=0.4)+
   geom_line(aes(y=series,colour="Osservati"),data=subset(data[1:80,]),cex=1)+
   geom_line(aes(y=series,colour="Previsti"),data=subset(data[80:100,]),cex=1)+
   scale_colour_manual("",breaks=c("Osservati","Previsti"),values=c("Osservati"="#00c3e3","Previsti"="#ff4554"))+
   labs(title="Previsioni dei rendimenti",subtitle="Modello ARFIMA(1,2) e GARCH(1,1)",
        x="\nTime/Horizon",y="Serie storica")+
   theme_bw()+
   theme(
      plot.title=element_text(size=20,hjust=0.5),
      plot.subtitle=element_text(size=12,hjust=0.5,colour="grey50"),
      axis.title=element_text(size=12),
      panel.grid=element_line(linetype="dashed",color="grey80"),
      legend.position=c(.1,.85),
      legend.spacing.x=unit(0.5,"cm"),
      legend.box.just=.5,
      legend.margin=margin(3,3,8,3)
   )
