
setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
ShinyCode<-file.path("ResponseCurves\\External")
sourceList<-list.files(ShinyCode,full.names=TRUE)
unlist(lapply(as.list(sourceList),source))

ChkLibs(list("rgeos","maptools","randomForest","mgcv","dismo","shiny","earth","PresenceAbsence",
             "wesanderson","ggplot2","raster","grid","gridExtra","splines","RColorBrewer",
             "viridis","caret","MASS","mlbench","rpart"))
#=====================================================
# Put together some species distribution data using the bradypus data
# and snippets of code from the dismo package

files <- list.files(path=paste(system.file(package="dismo"),
                               '/ex', sep=''), pattern='grd', full.names=TRUE)
files<-files[c(1,3,4,5,2,7,8)]

file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file, header=TRUE, sep=",")
# we do not need the first column
bradypus <- bradypus[,-1]
presvals <- extract(layerStk, bradypus)
# setting random seed to always create the same
# random set of points for this example
set.seed(0)
#setting up an extent around the presence points
PresExt <- extent(-104.7,-36,-26,16)
backgr <- randomPoints(layerStk,ext=PresExt, 500)
colnames(backgr)<-c("lon","lat")
absvals <- extract(layerStk, backgr)
resp <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(rbind(bradypus,backgr),cbind(resp, rbind(presvals, absvals)))

#using shiny for interactive correlation filtering and selecting the 
#covariates
correlationViewer(sdmdata,layerStk)
#=====================================
#now from the caret package we'll split into test and train
TrainData <- sdmdata[,4:10]
TrainClasses <- factor(sdmdata[,3])
tmp <- createDataPartition(sdmdata$resp,
                           p = .8,
                           list = FALSE)
training <- sdmdata[ tmp,]
testing <- sdmdata[-tmp,]

#next we fit models with some "reasonable" predict methods
#or we can use or hopefully just about any from the caret package
knnFit1 <- train(TrainData[tmp,], TrainClasses[tmp],
                                  method = "knn",
                                  preProcess = c("center", "scale"),
                                  tuneLength = 10)
nnetFit <- train(TrainData[tmp,], TrainClasses[tmp],
                 method = "nnet",
                 preProcess = "range",
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

glmFit <- train(TrainData[tmp,], TrainClasses[tmp],
               method = "glm")

earthFit <- train(TrainData[tmp,], TrainClasses[tmp],
                  method = "bagEarthGCV")
                  
RandForest_Model <- train(TrainData[tmp,], TrainClasses[tmp],
                  method = "rf",nodesize=30)

rda_Model<-train(TrainData[tmp,],TrainClasses[tmp],method="rda")
GLM_Model = glm(resp ~ bio1 + bio5 + bio12+ bio7, data=sdmdata[tmp,],family=binomial)
MARS_Model = earth(resp~ bio1 + bio5 + bio12 + bio7, data=sdmdata[tmp,],glm=list(family=binomial))

#=====================================================================
#This is where the magic happens we put our model fit objects in a list
fitLst<-list(GLM=GLM_Model,MARS=MARS_Model,RandForest=RandForest_Model,nnet=nnetFit)

#then using the model fit list, the spatial layer stack, information on the test/train split
#and possibly a shape file we can interactively explore our models
exploreCurves(fitLst,inputLayers=layerStk,trainData=sdmdata[tmp,],threshold=2,
              boundary=wrld_simpl,testData=sdmdata[-tmp,])




