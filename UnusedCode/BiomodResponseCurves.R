library(biomod2)

#BiomodCode<-"C:\\Users\\mtalbert\\Downloads\\biomod2\\R"
#sourceList<-list.files(BiomodCode,full.names=TRUE)
#unlist(lapply(as.list(sourceList),source))
DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
                                    package="biomod2"), row.names = 1)
head(DataSpecies)

# the name of studied species
myRespName <- 'GuloGulo'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]


# Environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
myExpl = stack( system.file( "external/bioclim/current/bio3.grd", 
                             package="biomod2"),
                system.file( "external/bioclim/current/bio4.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio7.grd", 
                             package="biomod2"),  
                system.file( "external/bioclim/current/bio11.grd", 
                             package="biomod2"), 
                system.file( "external/bioclim/current/bio12.grd", 
                             package="biomod2"))

# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)

SpDataFrame<-cbind(myBiomodData@coord,myBiomodData@data.species,myBiomodData@data.env.var)

#Pairs explore

correlationViewer(data=SpDataFrame)
# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Doing Modelisation

myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, 
                                     models = c('GLM','GBM','GAM','CTA','ANN',
                                                'FDA','MARS','RF'), 
                                     models.options = myBiomodOption, 
                                     NbRunEval=1, 
                                     DataSplit=70, 
                                     models.eval.meth = c('TSS','ROC'),
                                     SaveObj = FALSE,
                                     VarImport=5,
                                     do.full.models = FALSE)

myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl,
                                        proj.name = 'current',
                                        selected.models = 'all',
                                        binary.meth = 'TSS',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE,keep.in.memory=TRUE,on_0_1000=FALSE)

myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                       chosen.models = 'all',
                                       em.by = 'all',
                                       eval.metric = c('TSS'),
                                       eval.metric.quality.threshold = c(0.7),
                                       models.eval.meth = c('ROC'),
                                       prob.mean = TRUE,
                                       prob.cv = FALSE,
                                       prob.ci = FALSE,
                                       prob.ci.alpha = 0.05,
                                       prob.median = FALSE,
                                       committee.averaging = FALSE,
                                       prob.mean.weight = TRUE,
                                       prob.mean.weight.decay = 'proportional' )
BM <- BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                            EM.output = myBiomodEM)
#======================================================
#======================================================
#   Magic happens here for Wednesday I'm into the server secion pulling out the maps
#   Write a method for the initial data prep steps instead of if else everywhere
exploreCurves(myBiomodModelOut,inputLayers=myExpl,data=myBiomodData,threshold=2,boundary=wrld_simpl)
#====================================================
#====================================================

vals<-apply(SpDataFrame[,c(4:ncol(SpDataFrame))],2,FUN=sample,size=4)
varI<-as.data.frame(matrix(runif(5*8,0,1),nrow=5,ncol=8)) #five predictors 8 models
varIncl<-as.data.frame(varI>.05)

fitLst<-myBiomodModelOut
modelLst<-fitLst@models.computed
#in exploreCUrves don't subset fitLst until inside these functions think about ensemble biomod has many options... 
responseCurves(f=myBiomodModelOut,model=modelLst,vals=vals,
                varImp=as.list(varI),varIncluded=as.list(varIncl),addImp=F,pIdx=1,
               dat=SpDataFrame[,c(4:ncol(SpDataFrame))],resp=SpDataFrame[,3],Cols=Cols,Ensemble=FALSE)
modelLst<-myBiomodModelOut@models.computed
par(mfrow=c(3,3))
for(i in 1:length(modelLst)){
  interactionPlot(fitLst,modelLst[[i]],vals=NULL,theta=30,phi=25,
                  x="bio3",y="bio7",
                  dat=SpDataFrame[,c(4:ncol(SpDataFrame))],resp=SpDataFrame[,3],modelIndx=i)
}

interactionPlot(myBiomodModelOut,model=modelLst[3],vals=NULL,theta=30,phi=25,x=names(SpDataFrame)[4],y=names(SpDataFrame)[5],
                dat=SpDataFrame[,c(4:ncol(SpDataFrame))],resp=SpDataFrame[,3])

responseCurves(fitLst,model,vals=NULL,varImp,varIncluded,addImp,pIdx,dat,resp,Cols,Ensemble,mapType="none")
interactionPlot(fitLst[[1]],modelLst[[1]],vals=NULL,theta=30,phi=25,x="bio1",y="bio5",dat=responseInput$dat,resp=responseInput$resp)
densityPlot(fitLst[[3]])

plot(myBiomodProjection@proj@val)
#===============================
#project to points extracted from the raster
newData<-data.frame(bio3=c(10,20,30),bio4=c(100,200,400),bio7=c(59,69,79),bio11=c(0,100,200),bio12=c(100,200,300))
pro<-BIOMOD_Projection(myBiomodModelOut,newData,proj.name="new",selected.models=modelLst[1],keep.in.memory=TRUE,on_0_1000=FALSE)
pro@Proj


# 4. Loading some models built should be a strsplit if I need this
#ModelNames<-substr(myBiomodModelOut@models.computed,nchar(myBiomodModelOut@models.computed)-2,
#                   nchar(myBiomodModelOut@models.computed))
getMethod('predict',"ANN_biomod2_model")
availableModels<-bm.out@models.computed

#Biomod would need a seperate interface where you select the models computed to load
#figure out the load models here can I assume the working directory hasn't changed?
#Can require models to be saved in memory predict methods are in biomod2_models-class.R
#looks like they don't use a special function for raster predict 
fitLst<-list()
ModelNames<-for (mtl in 1:length(availableModels)) {
  fitLst[[mtl]]<-rdx.file.contents(file.path(bm.out@sp.name, "models", bm.out@modeling.id, 
                                       availableModels[mlt]))
}
#I need to use get_formal_model and figure their predict methods
#start with BIOMOD_Projection and see what I can get from there
"C:\Users\mtalbert\Documents\GuloGulo\models\1448114986\GuloGulo_AllData_RUN1_ANN"

myLoadedModels <-loadBiomodModels(myBiomodModelOut, models=ModelNames)

myLoadedModels <- BIOMOD_LoadModels(myBiomodModelOut, models="GBM")
get_formal_model(GuloGulo_AllData_RUN1_GBM)
ModelNames<-myBiomodModelOut@models.computed
FitLst<-lapply(ModelNames,get_formal_model)
eval("GuloGulo_AllData_RUN1_GBM")
get_formal_model(GuloGulo_AllData_RUN1_GBM)
