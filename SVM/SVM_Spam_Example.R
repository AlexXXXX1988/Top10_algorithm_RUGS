###############################################################################
# load library
# if kernlab package is not installed
#install.packages("kernlab")
library(kernlab)

# load the inbuil dataset
data(spam)

#see spam in the table
View(spam)


################################################################################
# divide the spam data into the training set and testing set
# shuffle the row index
index <- sample(1:nrow(spam))
head(index,20L)

# taking the first half as training set
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
View(spamtrain)

# testing set
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
View(spamtest)

###########################################################
# training SVM

#function instruction 
?ksvm

# system.time(filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.05),C=5,cross=3))
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.05),C=5,cross=3,prob.model=T)
filter

## result interpretation

#number of SV
filter@nSV

# row numbers of boundary point (support vector)
head(unlist(filter@alphaindex),10L)
length(unlist(filter@alphaindex))

# see support vectors
View(spam[index[unlist(filter@alphaindex)], ])

#importance of the boundary points
unlist(filter@alpha)


#########################################################################
## predict mail type on the test set
mailtype <- predict(filter,spamtest[,-58])
head(mailtype)

# prediction result
View(cbind(truth = spamtest[,58],prediction = mailtype))
## Check results
table(mailtype,spamtest[,58])



##########################################################################
## mine tuning the model


SVMTune<-function(kernel,kernel.parameter,C){
  filter <- ksvm(type~.,data=spamtrain,kernel=kernel,kpar=kernel.parameter,C=C,cross=3,prob.model=T)
  mailtype <- predict(filter,spamtest[,-58])
  ConfussionMatrix<-table(mailtype,spamtest[,58])
  false<-ConfussionMatrix[2:3]
  false<-c(false,sum(false))
  false.rate<-false/c(sum(ConfussionMatrix[1:2]),sum(ConfussionMatrix[3:4]),sum(ConfussionMatrix))
  false.rate<-round(false.rate*100,2)
  false.rate<-paste(false.rate,"%",sep="")
  names(false.rate)<-c("nonspam->spam","spam->nonspam","False rate")
  return(false.rate)
}

## Gaussian Kernel
SVMTune("rbfdot",list(sigma=0.0001),5)
SVMTune("rbfdot",list(sigma=0.001),5)
SVMTune("rbfdot",list(sigma=0.01),5)
SVMTune("rbfdot",list(sigma=0.03),5)
SVMTune("rbfdot",list(sigma=0.05),5)
SVMTune("rbfdot",list(sigma=0.07),5)

## Polynomial Kernel
SVMTune("polydot",list(degree=1),5)
SVMTune("polydot",list(degree=2),5)
SVMTune("polydot",list(degree=3),5)
SVMTune("polydot",list(degree=4),5)
SVMTune("polydot",list(degree=5),5)



