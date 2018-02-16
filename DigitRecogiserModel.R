  train<-read.csv("mnist_train.csv")
  
  test<-read.csv("mnist_test.csv")
  
  train<-train[sample(nrow(train)),]    # to shuffle the data\
  
  test<-train[sample(nrow(test)),]
  
 X<-train[,-1]   #selecting only the first column which is the output for supervised learining
 Y<-train[,1]
 
 Xnormalized<-X/255  #normalising as per the max value fo pixels
 
 library("nnet", lib.loc="~/R/win-library/3.4")
   Xcov=cov(Xnormalized)  #generating the covariance matrix

 Xpca<-prcomp(Xcov)    # application of principal compomnet analysis
   
  #getting the table from pca using rotations and selectimng only the 45 features
 Xfinal<-as.matrix(Xnormalized) %*% Xpca$rotation[,1:45]
 View(Xpca$rotation0)
 View(Xpca$rotation)
 View(Xpca)
 Yfinal<-class.ind(Y) #converting they data frame to a matrix
 model<-nnet(Xfinal,Yfinal,size=200,softmax = TRUE,maxit = 150,MaxNWts = 800000)# training the model
Xtest<-test[,-1]
XtestNormalised<-Xtest/255
predicted<-predict(model,XtestNormalised,type="class")
 predicted<-as.data.frame(predicted)
actual<-as.double(unlist(XtestNormalised[1])) #gettimng the actual outputs
accuracy<-round(mean(actual==predicted)*100,2) #accuracy of the model
Image=readImageImage=readImage("img_106.jpg") # its 3 
#View(255*imageData(Image))
vc=as.vector(255*imageData(Image))
testValue=as.matrix(t(vc))
testValue<-as.matrix(testValue) %*% Xpca$rotation[,1:45]
predictedValue=predict(model,testValue,type="class")
predictedValue

