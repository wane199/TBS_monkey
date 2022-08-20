wdtmp =dt  ##data
i='Hb'     ##numvariable
fml <- "Surv(time,status)~Hb+gender+age"  ##model formula

get_cutoff_cox<-function(i,wdtmp,fml,tppmin=NA,tppmax=NA,dec=3) {
  
  xTMP <- wdtmp[,i]
  tmp.ss<-seq(0.05,0.95,0.05)
  tp<-quantile(xTMP,probs=tmp.ss,na.rm=TRUE) 
  tmp.llk<-rep(NA,length(tmp.ss))
  fml<-paste(fml,"+tmp.X")
  if (!is.na(tppmin) & !is.na(tppmax)) {
    tp2.min = tppmin; tp2.max = tppmax;
  } else {
    for (k in (1:length(tmp.ss))) {
      tmp.X<-(xTMP > tp[k])*(xTMP-tp[k]); wdtmp1<-cbind(wdtmp,tmp.X)
      tmp.mdl<- coxph(formula(fml),weights=wdtmp$weights,data=wdtmp,na.action=na.omit,method="breslow")
      tmp.llk[k]<-tmp.mdl$loglik[2];
      rm(wdtmp1, tmp.X)
    }
    tp1<-tmp.ss[which.max(tmp.llk)]
    tp2.min = tp1 - 0.04
    tp2.max = tp1 + 0.04
    if (tp2.min<0.05) {tp2.min=0.05}
    if (tp2.max>0.95) {tp2.max=0.95}
  }	
  tp.pctlrange<-quantile(xTMP,probs=c(tp2.min,tp2.max),na.rm=TRUE)
  tp.range<-unique(xTMP[xTMP>tp.pctlrange[1] & xTMP<tp.pctlrange[2]])
  while (length(tp.range)>5) {
    tmp.pct3<-quantile(tp.range,probs=c(0,0.25,0.5,0.75,1),type=3)
    tmp.llk3<-rep(NA,3)
    for (k in (2:4)) {
      tmp.X<-(xTMP>tmp.pct3[k])*(xTMP-tmp.pct3[k]); wdtmp1<-cbind(wdtmp,tmp.X)
      tmp.mdl<- coxph(formula(fml),data=wdtmp,na.action=na.omit)
      
      tmp.llk3[k-1]<-tmp.mdl$loglik[2];
      rm(wdtmp1, tmp.X)
    }
    tmp.min3<-which.max(tmp.llk3)
    tp.range<-tp.range[tp.range>=tmp.pct3[tmp.min3] & tp.range<=tmp.pct3[tmp.min3+2]]
  }
  if (length(tp.range)>0) {
    if (length(tp.range)==1) {tp.val=tp.range[1];} else {
      tmp.llk<-rep(NA,length(tp.range))
      for (k in (1:length(tp.range))) {
        tmp.X<-(xTMP>tp.range[k])*(xTMP-tp.range[k]); wdtmp1<-cbind(wdtmp,tmp.X)
        tmp.mdl<- coxph(formula(fml),data=wdtmp,na.action=na.omit)
        
        tmp.llk[k]<-tmp.mdl$loglik[2];
        rm(wdtmp1, tmp.X)
      }
      tp.val<-tp.range[which.max(tmp.llk)]
    }
  } else { tp.val<-tp.pctlrange[1];}
  return(round(tp.val,dec));
}
