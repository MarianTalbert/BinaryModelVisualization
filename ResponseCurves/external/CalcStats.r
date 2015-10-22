
calcStat<-function(pred,resp,Split,thresh){
    #For now only binomial response
    family="binomial"
#Written by Marian Talbert 2011
    auc.data<-data.frame(ID=1:length(pred),pres.abs=resp,pred=pred)
        #have to use roc here because auc in the PresenceAbsence package incorretly assumes that the value must be greater than .5
        #this isn't necessarily true for an independent evaluation set
        auc.fit<-roc(resp,pred)
        
            cmx <- cmx(auc.data,threshold=thresh)
            PCC <- pcc(cmx,st.dev=F)*100
            SENS <- sensitivity(cmx,st.dev=F)
            SPEC <- specificity(cmx,st.dev=F)
            KAPPA <- Kappa(cmx,st.dev=F)
            TSS <- SENS+SPEC-1

            devResid <- sign(resp-pred)*sqrt(2*abs((resp * log(pred)) + ((1 - resp) * 
            log(1 - pred))))
            
            devResid[is.nan(devResid)] <- NA
        
            return(list(auc.fit=auc.fit,Cmx=cmx,Pcc=PCC,Sens=SENS,
                Specf=SPEC,Kappa=KAPPA,Tss=TSS,devResid=devResid))
        
    }