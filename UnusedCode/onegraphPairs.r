ggpairs<-function(dat,alph,pointSize,DevScore,showResp,brushLoc){

   d<-data.frame(x=c(0,1),y=c(0,1))

 colOffset<-ifelse(showResp,1,0) 
  grid.newpage()
  pushViewport(viewport(layout=grid.layout((ncol(dat)-1),(ncol(dat)-1+colOffset))))
  vplayout<- function(x,y)
    viewport(layout.pos.row=x, layout.pos.col=y)
  

   dat[,ncol(dat)]<-as.numeric(as.character(dat[,ncol(dat)])) 
  if(showResp){
    #response column
  for(j in 1:(ncol(dat)-1)){
      respPlt<-ggplot(dat, aes_q(x = as.name(names(dat)[j]), 

      if(j==1) respPlt<-respPlt+ggtitle("Response")
      
      print(respPlt, 
        vp=vplayout(j,1))

     }
  }

   dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)]) 
  for(i in 1:(ncol(dat)-1)){
 #histogram above the diagonal
    hst<-ggplot(dat,
                aes_q(x=as.name(names(dat[i]))))+
                geom_histogram(fill="blue")+theme(plot.margin=unit(c(0,1,1,0),"mm"))+
     scale_y_discrete(breaks=NULL)+scale_x_continuous(breaks=NULL)+xlab("")
    if(i!=1) hst<-hst+ylab("")
    if(i==1) hst<-hst+ylab(paste(names(dat[i]),"\n",sep=""))+ggtitle(names(dat[i]))+ 
                  theme(axis.title=element_text(size=rel(1.3)))
    if(i==(ncol(dat)-1)) hst<-hst+xlab("\n")
    
     #title(names(dat[i]))              
     
  print(hst,
        vp=vplayout(i,i+colOffset))

   #pairs plot below the diagonal
    if(i>1){
    for(j in 1:(i-1)){ 
    
    g<-ggplot(dat, 
           aes_q(x = as.name(names(dat)[j]), 
                 y = as.name(names(dat)[i]),
                 colour=as.name(names(dat)[ncol(dat)])))+ geom_point(size=pointSize, alpha = alph)+
                 scale_color_manual(values=c("blue","red"))+ theme(legend.position = 'none')+
                 theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
                 plot.margin=unit(c(0,0,0,0),"mm"))+ 
                 theme(axis.text.y = element_text(angle = 90))+
                 theme(axis.text = element_text(size = rel(1.3)),axis.title=element_text(size=rel(1.3)))  
    if(j!=1) g<-g+ylab("")+scale_y_discrete(breaks=NULL)
    if(i!=1) g<-g+xlab("")
    if(i!=(ncol(dat)-1)) g<-g+scale_x_discrete(breaks=NULL)
    print(g,vp=vplayout(i,j+colOffset))             
     
     }  
  }

     Cols<-c("white",brewer.pal(9,"Reds"))
    
    if(i<(ncol(dat)-1)){
      for(j in (i+1):(ncol(dat)-1)){
        Cor<-cor(dat[,i],dat[,j])

         ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.6,to=1,length=length(Cols))))
         colPlot<-ggplot(d,  aes(x = x, y = y))+ geom_blank()+
           theme(panel.background = element_rect(fill = Cols[ColIndx]))+
 ggpairs<-function(dat,alph,pointSize,DevScore,showResp,brushLoc){
           scale_y_continuous(breaks=NULL)+theme(axis.title=element_text(size=rel(1.3)))+
           annotate("text", label= round(Cor,digits=2), x=.5, y=.5,
                    size=15*abs(Cor))
        if(i==1) colPlot<-colPlot+ggtitle(names(dat)[j])
        
        print(colPlot,vp=vplayout(i,j+colOffset))
      }  
    }
  }
 
 }
 
