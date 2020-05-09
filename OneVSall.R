



FilePath = "C:/Users/Nwolf/Documents/FALL 2019/Machine Learning/P1LogisticReg/MasterData/MasterScaled.csv"
data = read.csv(file=FilePath,header = TRUE,  sep = ",")
head(data)




typeof(data$Type)














if(FALSE){
# install.packages('Boruta')
    library(Boruta)
    #boruta_output <- Boruta(Type ~ ., data=na.omit(data), doTrace=0)  
    names(boruta_output)
    boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
    print(boruta_signif)  
    roughFixMod <- TentativeRoughFix(boruta_output)
    boruta_signif <- getSelectedAttributes(roughFixMod)
    print(boruta_signif)
    
    
    
    imps <- attStats(roughFixMod)
    imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
    head(imps2[order(-imps2$meanImp), ])  # descending sort
    
    
    
    
    
    # Plot variable importance
    plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
}








## Change to numeric for Type
data$Type = as.numeric(data$Type)

data = subset(data, select = -c(1))
dataCycling = data









#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}





#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}






## CYCLING
dataCycling = data

for(i in 1:nrow(dataCycling)){
  
  if(dataCycling$Type[i] != 1 ) {
    dataCycling$Type[i] = 0
  }
  else
  {
    dataCycling$Type[i] = 1
  }
  
}






dataCyclingX =  subset(dataCycling, select = -c(length(dataCycling)))


X <- as.matrix(dataCyclingX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataCycling$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probCycling = rep(0,nrow(X))
  
for(i in 1:nrow(X)){
  probCycling[i] =sigmoid(t(X[i,])%*%theta)
}









df <- cbind.data.frame( dataCycling)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Cycling",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })


















## DRIVING
dataDriving= data

for(i in 1:nrow(dataDriving)){
  
  if(dataDriving$Type[i] != 2 ) {
    dataDriving$Type[i] = 0
  }
  else
  {
    dataDriving$Type[i] = 1
    
  }
  
}



dataDrivingX =  subset(dataDriving, select = -c(length(dataDriving)))


X <- as.matrix(dataDrivingX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataDriving$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probDriving = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probDriving[i] =sigmoid(t(X[i,])%*%theta)
}







df <- cbind.data.frame( dataDriving)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Driving",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })









##Run
dataRun= data

for(i in 1:nrow(dataRun)){
  
  if(dataRun$Type[i] != 3 ) {
    dataRun$Type[i] = 0
  }
  else
  {
    dataRun$Type[i] = 1
    
  }
  
}



dataRunX =  subset(dataRun, select = -c(length(dataRun)))


X <- as.matrix(dataRunX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataRun$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probRun = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probRun[i] =sigmoid(t(X[i,])%*%theta)
}



df <- cbind.data.frame( dataRun)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Run",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })












## Sit
dataSit= data

for(i in 1:nrow(dataSit)){
  
  if(dataSit$Type[i] != 4 ) {
    dataSit$Type[i] = 0
  }
  else
  {
    dataSit$Type[i] = 1
    
  }
  
}



dataSitX =  subset(dataSit, select = -c(length(dataSit)))


X <- as.matrix(dataSitX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataSit$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probSit = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probSit[i] =sigmoid(t(X[i,])%*%theta)
}






df <- cbind.data.frame( dataSit)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Sit",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })











##STAIRUP

dataStairUp= data

for(i in 1:nrow(dataStairUp)){
  
  if(dataStairUp$Type[i] != 5 ) {
    dataStairUp$Type[i] = 0
  }
  else
  {
    dataStairUp$Type[i] = 1
    
  }
  
}



dataStairUpX =  subset(dataStairUp, select = -c(length(dataStairUp)))


X <- as.matrix(dataStairUpX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataStairUp$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probStairUp = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probStairUp[i] =sigmoid(t(X[i,])%*%theta)
}


df <- cbind.data.frame( dataStairUp)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="StairUp",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })














##STAIR DOWN
dataStairDown= data

for(i in 1:nrow(dataStairDown)){
  
  if(dataStairDown$Type[i] != 6 ) {
    dataStairDown$Type[i] = 0
  }
  else
  {
    dataStairDown$Type[i] = 1
    
  }
  
}



dataStairDownX =  subset(dataStairDown, select = -c(length(dataStairDown)))


X <- as.matrix(dataStairDownX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataStairDown$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probStairDown = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probStairDown[i] =sigmoid(t(X[i,])%*%theta)
}






df <- cbind.data.frame( dataStairDown)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="StairDown",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })








##STANDING
dataStanding= data

for(i in 1:nrow(dataStanding)){
  
  if(dataStanding$Type[i] != 7 ) {
    dataStanding$Type[i] = 0
  }
  else
  {
    dataStanding$Type[i] = 1
    
  }
  
}



dataStandingX =  subset(dataStanding, select = -c(length(dataStanding)))


X <- as.matrix(dataStandingX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataStanding$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probStanding = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probStanding[i] =sigmoid(t(X[i,])%*%theta)
}




df <- cbind.data.frame( dataStanding)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Standing",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })






##Walk
dataWalk= data

for(i in 1:nrow(dataWalk)){
  
  if(dataWalk$Type[i] != 8 ) {
    dataWalk$Type[i] = 0
  }
  else
  {
    dataWalk$Type[i] = 1
    
  }
  
}



dataWalkX =  subset(dataWalk, select = -c(length(dataWalk)))


X <- as.matrix(dataWalkX)
#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(dataWalk$Type)






#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
probWalk = rep(0,nrow(X))

for(i in 1:nrow(X)){
  probWalk[i] =sigmoid(t(X[i,])%*%theta)
}









df <- cbind.data.frame( dataWalk)

mdl <- glm( Type ~ AX.Avg + AX.Max , data = df , family=binomial)

slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 

library(lattice)
xyplot( main="Walk",AX.Avg ~ AX.Max , data = df, groups = Type,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })















head(probCycling)

head(probDriving)

head(probRun)

head(probSit)

head(probStairDown)

head(probStairUp)

head(probStanding)

head(probWalk)





FinalPreds = rep(0,length(probCycling))

for(i in 1:length(probCycling)){
  
  max = max(c(probCycling[i],probDriving[i],probRun[i],probSit[i],probStairDown[i],
          probStairUp[i],probStanding[i],probWalk[i]))
  
  if(max ==probCycling[i] )
  {
    FinalPreds[i] = 1
  }
  else if (max ==probDriving[i] )
  {
    FinalPreds[i] = 2
    
  }
  else if (max ==probRun[i] )
  {
    FinalPreds[i] = 3
    
  }
  else if (max ==probSit[i] )
  {
    FinalPreds[i] = 4
    
  }
  else if (max ==probStairDown[i] )
  {
    FinalPreds[i] = 5
    
  }
  else if (max ==probStairUp[i] )
  {
    FinalPreds[i] = 6
    
  }
  else if (max ==probStanding[i] )
  {
    FinalPreds[i] = 7
    
  }
  else if (max ==probWalk[i] )
  {
    FinalPreds[i] = 8
  }

    
  
}





calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}


calc_class_err(actual = data$Type, predicted = FinalPreds)







table(predicted = FinalPreds, actual = data$Type)

















calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}


for(i in 1:length(model_glm_pred)){
  
  if(model_glm_pred[i] == "Yes" ) {
    model_glm_pred[i] = 1
  }
  else
  {
    model_glm_pred[i] = 0
  }
}

calc_class_err(actual = dataCycling$Type, predicted = model_glm_pred)



plot(Type ~ LP2.Avg, data = dataCycling,
     col = "gold", pch = "|", ylim = c(-0.2, 1),
     main = "Using Logistic Regression for Classification")



abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
curve(predict(model_glm, data.frame(balance = x), type = "response"),
      add = TRUE, lwd = 3, col = "darkblue")
















