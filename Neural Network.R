FilePath = "C:/Users/Nwolf/Documents/FALL 2019/Machine Learning/P1LogisticReg/MasterData/MasterScaled.csv"
data.main.scaled = read.csv(file=FilePath,header = TRUE,  sep = ",")








idx <- sample(1:nrow(data.main.scaled), .8 * nrow(data.main.scaled))
data.train <- data.main.scaled[idx,  -1]
data.test  <- data.main.scaled[-idx, -1]




library(nnet)
net <- nnet(
  Type ~ .,
  data = data.train,
  size = 10
)
net	


preds <- predict(net, newdata = data.test, type = 'class')
table(actual = data.test$Type, prediction = preds)
mean(data.test$Type != preds)














library(devtools)

library(downloader)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


plot.nnet(net)



## MOST IMPORTANT VARS






library(nnet)
net <- nnet(
  Type ~ AX.Avg + AX.Max + AZ.Max + AY.Max,
  data = data.train,
  size = 10
)
net	


preds <- predict(net, newdata = data.test, type = 'class')
table(actual = data.test$Type, prediction = preds)
mean(data.test$Type != preds)



plot.nnet(net)



























