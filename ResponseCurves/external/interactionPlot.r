interactionPlot<-function(fitLst,model,vals=NULL,theta=30,phi=25,x,y){
    
        #How to check that models and data match
        dat<-fitLst$dat$ma$train$dat[,-1]
        resp<-fitLst$dat$ma$train$dat[,1]
        VarNames<-names(dat)
        
        #Don't plot the interaction if neither predictor is in the model
        Col=Colors
        if(sum(c(x,y)%in%fitLst$mods$vnames)==0) Col=rep("grey92",times=length(Colors))
        
        myPredict <- function (x, y, ...) { 
          out <- predict(x, y, type='response', args=c("outputformat=logistic"), ...);
          return (out)
        }
        #response(m_me,  at=mean, expand=0, fun=myPredict)
        #response(m_glm, at=mean, expand=0, fun=myPredict)
        
         mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         vals<-as.vector(vals)
         if(is.null(vals)) vals<-means
         n <- 100

                          test <- do.call("rbind", replicate(n^2, vals, simplify=FALSE))
                          yCol= match(y,names(dat))
                          xCol=match(x,names(dat))
                          test[, yCol] <- rep(seq(mins[yCol], maxs[yCol], length.out=n),each=n)
                          test[, xCol] <- rep(seq(mins[xCol], maxs[xCol], length.out=n),times=n)
                          test<-as.data.frame(test)
                          colnames(test)<-names(means)
                           Response<-pred.fct(fitLst$mods$final.mod, test,model)
                           z<-matrix(Response,ncol=n)
                           nrz <- nrow(z)
                           ncz <- ncol(z)
                            zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
                            nbcol<-length(Colors)
                            # Recode facet z-values into color indices
                            facetcol <- cut(zfacet, nbcol)
                            Xlab<-paste(substr(x,start=1,stop=12),c("\n","")[1+(nchar(x)<=12)],
                                   substr(x,start=13,stop=nchar(x)),sep="")
                            Ylab<-paste(substr(y,start=1,stop=12),c("\n","")[1+(nchar(y)<=12)],
                                   substr(y,start=13,stop=nchar(y)),sep="")       
                           persp(x=seq(mins[xCol], maxs[xCol], length.out=n),y=seq(mins[yCol], maxs[yCol], length.out=n),
                               z=z,theta=theta,phi=phi,col=Col[facetcol],shade=.4,xlab=Xlab,ylab=Ylab,zlab="Prediction",
                               main=model,zlim=c(0,1),border=NA,cex.lab=1.3)
                           }

                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
      