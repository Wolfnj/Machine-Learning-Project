startingDir 	<- "F:/Fall/CSCI 4957/Project 1"
roundingValue 	<- 5
outputCSV 		<- FALSE
outputGraphs	<- FALSE
options(scipen=999)

###########################################################
#Constant Variables
	#Columns to Access
		columnsLR <- c(1,3,4,5,6,7,8,9,10)
		columnsA <- c(1,5,7,9)
		columnsG <- c(1,11,13,15)

		columnsLR2 <- c(2,3,4,5,6,7,8,9,10)
	#File/Variables
		variableNames <- c(
			"Cycling1", "Cycling2", 
			"Driving1", "Driving2", "Driving3", "Driving4",
			"Run1", "Run2", "Run3",
			"Sit1", "Sit2", "Sit3",
			"StairDown1", "StairDown2", "StairDown3",
			"StairUp1", "StairUp2", "StairUp3",
			"Standing1", "Standing2", "Standing3",
			"Walk1", "Walk2"
		)

		weirdVariables <- c(
			"Cycling1",  
			"Driving1", 
			"Run1", "Run2", "Run3",
			"Sit1",
			"StairDown1",
			"StairUp1",
			"Standing1",
			"Walk1"
		)
###########################################################


###########################################################
#Set the directory
	setwd(startingDir)
	startingPath <- sprintf("%s/Data/", startingDir)
###########################################################


###########################################################
#Reading Data	
	for(i in variableNames)
	{
		#Set the file Path
		filePath <- sprintf("%s%s/", startingPath, i)

		#Creating the names
		leftName  <- sprintf("data.%s.left", i)
		rightName <- sprintf("data.%s.right", i)
		accelName <- sprintf("data.%s.accel", i)
		gyroName  <- sprintf("data.%s.gyro", i)

		#Creating the paths
		leftPath  <- sprintf("%sleft.csv", filePath)
		rightPath <- sprintf("%sright.csv", filePath)
		accelPath <- sprintf("%sacc.csv", filePath)
		gyroPath  <- sprintf("%sgyro.csv", filePath)

		#Read and set the data
		assign(leftName,  read.csv(file = leftPath,  header = T))
		assign(rightName, read.csv(file = rightPath, header = T))
		assign(accelName, read.csv(file = accelPath, header = F))
		assign(gyroName,  read.csv(file = gyroPath,  header = F))

		#head(get(leftName))
		#head(get(rightName))
		#head(get(accelName))
		#head(get(gyroName))

		#Filter only wanted data
		if(i %in% weirdVariables)
		{
			assign(leftName,  get(leftName) [,columnsLR])
			assign(rightName, get(rightName)[,columnsLR])
		}
		else
		{
			assign(leftName,  get(leftName) [,columnsLR2])
			assign(rightName, get(rightName)[,columnsLR2])
		}
		assign(accelName, get(accelName)[,columnsA])
		assign(gyroName,  get(gyroName) [,columnsG])

		#head(get(leftName))
		#head(get(rightName))
		#head(get(accelName))
		#head(get(gyroName))

		#Set the names
		assign(leftName,  setNames(get(leftName),  c("Timestamp", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")))
		assign(rightName, setNames(get(rightName), c("Timestamp", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")))
		assign(accelName, setNames(get(accelName), c("Timestamp", "X", "Y", "Z")))
		assign(gyroName,  setNames(get(gyroName),  c("Timestamp", "X", "Y", "Z")))

		#head(get(leftName))
		#head(get(rightName))
		#head(get(accelName))
		#head(get(gyroName))
	}
###########################################################


###########################################################
#Processing the Data
	for(i in variableNames)
	{
		#Creating the names
		leftName  <- sprintf("data.%s.left",  i)
		rightName <- sprintf("data.%s.right", i)
		accelName <- sprintf("data.%s.accel", i)
		gyroName  <- sprintf("data.%s.gyro",  i)

		leftData  <- get(leftName)
		rightData <- get(rightName)
		accelData <- get(accelName)
		gyroData  <- get(gyroName)		

		#Rounding the Timestamp
		leftData$Timestamp  <- round(leftData$Timestamp,  roundingValue)
		rightData$Timestamp <- round(rightData$Timestamp, roundingValue)
		accelData$Timestamp <- round(accelData$Timestamp, roundingValue)
		gyroData$Timestamp  <- round(gyroData$Timestamp,  roundingValue)

		#Timestamp, Avg, Min, Max, Std
		#Making the data frames
			left.frame <- data.frame(
				unique(leftData$Timestamp),
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN
			)
			right.frame <- data.frame(
				unique(rightData$Timestamp),
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 
				NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN
			)
			accel.frame <- data.frame(
				unique(accelData$Timestamp),
				NaN, NaN, NaN,
				NaN, NaN, NaN, 
				NaN, NaN, NaN, 
				NaN, NaN, NaN
 			)
			gyro.frame <- data.frame(
				unique(gyroData$Timestamp),
				NaN, NaN, NaN,
				NaN, NaN, NaN, 
				NaN, NaN, NaN, 
				NaN, NaN, NaN
 			)

		#Renaming the data frames
			names(left.frame) <- c(
				"Timestamp",
				"LP1.Avg", "LP2.Avg", "LP3.Avg", "LP4.Avg", "LP5.Avg", "LP6.Avg", "LP7.Avg", "LP8.Avg", 
				"LP1.Min", "LP2.Min", "LP3.Min", "LP4.Min", "LP5.Min", "LP6.Min", "LP7.Min", "LP8.Min", 
				"LP1.Max", "LP2.Max", "LP3.Max", "LP4.Max", "LP5.Max", "LP6.Max", "LP7.Max", "LP8.Max", 
				"LP1.Std", "LP2.Std", "LP3.Std", "LP4.Std", "LP5.Std", "LP6.Std", "LP7.Std", "LP8.Std"
			)
			names(right.frame) <- c(
				"Timestamp",
				"RP1.Avg", "RP2.Avg", "RP3.Avg", "RP4.Avg", "RP5.Avg", "RP6.Avg", "RP7.Avg", "RP8.Avg", 
				"RP1.Min", "RP2.Min", "RP3.Min", "RP4.Min", "RP5.Min", "RP6.Min", "RP7.Min", "RP8.Min", 
				"RP1.Max", "RP2.Max", "RP3.Max", "RP4.Max", "RP5.Max", "RP6.Max", "RP7.Max", "RP8.Max", 
				"RP1.Std", "RP2.Std", "RP3.Std", "RP4.Std", "RP5.Std", "RP6.Std", "RP7.Std", "RP8.Std"
			)

			names(accel.frame) <- c(
				"Timestamp",
				"AX.Avg", "AY.Avg", "AZ.Avg",  
				"AX.Min", "AY.Min", "AZ.Min",  
				"AX.Max", "AY.Max", "AZ.Max",  
				"AX.Std", "AY.Std", "AZ.Std"
			)
			names(gyro.frame) <- c(
				"Timestamp",
				"GX.Avg", "GY.Avg", "AZ.Gvg",  
				"GX.Min", "GY.Min", "GZ.Min",  
				"GX.Max", "GY.Max", "GZ.Max",  
				"GX.Std", "GY.Std", "GZ.Std"
			)


		
		#Setting the data
			#Left Data
			for(k in left.frame$Timestamp)
			{
				scrubIndexes 	<- which(leftData$Timestamp == k)
				scrubMeanIndex 	<- which(left.frame$Timestamp == k)

				for(l in 2:9) {
					left.frame[scrubMeanIndex, l] <- mean(na.omit( leftData[scrubIndexes, l     ])) }
				for(l in 10:17) {
					left.frame[scrubMeanIndex, l] <- min (na.omit( leftData[scrubIndexes, l - 8 ])) }
				for(l in 18:25) {
					left.frame[scrubMeanIndex, l] <- max (na.omit( leftData[scrubIndexes, l - 16])) }
				for(l in 26:33) {
					left.frame[scrubMeanIndex, l] <- sd  (na.omit( leftData[scrubIndexes, l - 24])) }
			}

			#Right Data
			for(k in right.frame$Timestamp)
			{
				scrubIndexes 	<- which(rightData$Timestamp == k)
				scrubMeanIndex 	<- which(right.frame$Timestamp == k)

				for(l in 2:9) {
					right.frame[scrubMeanIndex, l] <- mean(na.omit( rightData[scrubIndexes, l     ])) }
				for(l in 10:17) {
					right.frame[scrubMeanIndex, l] <- min (na.omit( rightData[scrubIndexes, l - 8 ])) }
				for(l in 18:25) {
					right.frame[scrubMeanIndex, l] <- max (na.omit( rightData[scrubIndexes, l - 16])) }
				for(l in 26:33) {
					right.frame[scrubMeanIndex, l] <- sd  (na.omit( rightData[scrubIndexes, l - 24])) }
			}

			#Accel Data
			for(k in accel.frame$Timestamp)
			{
				scrubIndexes 	<- which(accelData$Timestamp == k)
				scrubMeanIndex 	<- which(accel.frame$Timestamp == k)

				for(l in 2:4) {
					accel.frame[scrubMeanIndex, l] <- mean(na.omit( accelData[scrubIndexes, l     ])) }
				for(l in 5:7) {
					accel.frame[scrubMeanIndex, l] <- min (na.omit( accelData[scrubIndexes, l - 3 ])) }
				for(l in 8:10) {
					accel.frame[scrubMeanIndex, l] <- max (na.omit( accelData[scrubIndexes, l - 6])) }
				for(l in 11:13) {
					accel.frame[scrubMeanIndex, l] <- sd  (na.omit( accelData[scrubIndexes, l - 9])) }
			}

			#Gyro Data
			for(k in gyro.frame$Timestamp)
			{
				scrubIndexes 	<- which(gyroData$Timestamp == k)
				scrubMeanIndex 	<- which(gyro.frame$Timestamp == k)

				for(l in 2:4) {
					gyro.frame[scrubMeanIndex, l] <- mean(na.omit( gyroData[scrubIndexes, l     ])) }
				for(l in 5:7) {
					gyro.frame[scrubMeanIndex, l] <- min (na.omit( gyroData[scrubIndexes, l - 3 ])) }
				for(l in 8:10) {
					gyro.frame[scrubMeanIndex, l] <- max (na.omit( gyroData[scrubIndexes, l - 6])) }
				for(l in 11:13) {
					gyro.frame[scrubMeanIndex, l] <- sd  (na.omit( gyroData[scrubIndexes, l - 9])) }
			}

		#Get the common Timestamps
		commonTimestamps <- Reduce (intersect, list(
			left.frame$Timestamp,
			right.frame$Timestamp,
			accel.frame$Timestamp,
			gyro.frame$Timestamp)
		)

		#Make the data frame
		data.scrub <- data.frame(
			commonTimestamps,
			left.frame [which(left.frame$Timestamp  %in% commonTimestamps), -1],
			right.frame[which(right.frame$Timestamp %in% commonTimestamps), -1],
			accel.frame[which(accel.frame$Timestamp %in% commonTimestamps), -1],
			gyro.frame [which(gyro.frame$Timestamp  %in% commonTimestamps), -1]
		)
		names(data.scrub)[1] <- "Timestamp"

		#Assign the data
		assign(sprintf("data.%s", i), data.scrub)
	}
###########################################################


###########################################################
#OutputCSV
	if(outputCSV) {
	for(i in variableNames) {
		#Make the file path
			filePath = sprintf("%s/ScrubData/", getwd(), i)

		#Create the directory if it doesn't exist
			#if(file.exists(filePath) == FALSE) {
				#dir.create(filePath) }

		#Write the csv
			fileName = sprintf("%s/%s.csv", filePath, i)
			write.csv(get(sprintf("data.%s", i)), file = fileName, row.names = FALSE)
	}
	}
###########################################################

###########################################################
#Making graphs
	if(outputGraphs) {		
	for(i in variableNames)
	{
		d <- get(sprintf("data.%s", i))

		if(nrow(d) > 0) {
		#Left Data
			fileName = sprintf("%s/%sLeft.jpeg", filePath, i)
			jpeg(fileName, width = 1600, height = 1200)
			par(mfrow = c(4,8))

			
			for(j in 2:33)
			{
				plot(d$Timestamp, d[,j])
			}
			dev.off()

		#Right Data
			fileName = sprintf("%s/%sRight.jpeg", filePath, i)
			jpeg(fileName, width = 1600, height = 1200)
			par(mfrow = c(4,8))

			for(j in 34:65)
			{
				plot(d$Timestamp, d[,j])
			}
			dev.off()

		#Accel Data
			fileName = sprintf("%s/%sAccel.jpeg", filePath, i)
			jpeg(fileName, width = 1600, height = 1200)
			par(mfrow = c(4,3))

			for(j in 66:77)
			{
				plot(d$Timestamp, d[,j])
			}
			dev.off()

		#Gyro Data
			fileName = sprintf("%s/%sGyro.jpeg", filePath, i)
			jpeg(fileName, width = 1600, height = 1200)
			par(mfrow = c(4,3))

			for(j in 66:77)
			{
				plot(d$Timestamp, d[,j])
			}		
			dev.off()
		}
	}
	}
###########################################################


###########################################################
#Create one big File
	data.main <- data.frame(matrix(ncol = 90, nrow = 0))
	names(data.main) <- c(
		"Timestamp",
		"LP1.Avg", "LP2.Avg", "LP3.Avg", "LP4.Avg", "LP5.Avg", "LP6.Avg", "LP7.Avg", "LP8.Avg", 
		"LP1.Min", "LP2.Min", "LP3.Min", "LP4.Min", "LP5.Min", "LP6.Min", "LP7.Min", "LP8.Min", 
		"LP1.Max", "LP2.Max", "LP3.Max", "LP4.Max", "LP5.Max", "LP6.Max", "LP7.Max", "LP8.Max", 
		"LP1.Std", "LP2.Std", "LP3.Std", "LP4.Std", "LP5.Std", "LP6.Std", "LP7.Std", "LP8.Std",
		"RP1.Avg", "RP2.Avg", "RP3.Avg", "RP4.Avg", "RP5.Avg", "RP6.Avg", "RP7.Avg", "RP8.Avg", 
		"RP1.Min", "RP2.Min", "RP3.Min", "RP4.Min", "RP5.Min", "RP6.Min", "RP7.Min", "RP8.Min", 
		"RP1.Max", "RP2.Max", "RP3.Max", "RP4.Max", "RP5.Max", "RP6.Max", "RP7.Max", "RP8.Max", 
		"RP1.Std", "RP2.Std", "RP3.Std", "RP4.Std", "RP5.Std", "RP6.Std", "RP7.Std", "RP8.Std",
		"AX.Avg", "AY.Avg", "AZ.Avg",  
		"AX.Min", "AY.Min", "AZ.Min",  
		"AX.Max", "AY.Max", "AZ.Max",  
		"AX.Std", "AY.Std", "AZ.Std",
		"GX.Avg", "GY.Avg", "AZ.Gvg",  
		"GX.Min", "GY.Min", "GZ.Min",  
		"GX.Max", "GY.Max", "GZ.Max",  
		"GX.Std", "GY.Std", "GZ.Std",
		"Type"
	)

	for(i in variableNames)
	{
		d <- get(sprintf("data.%s", i))

		if(nrow(d) == 0)
		{
			print(sprintf("Zero Rows in %s", i))
			next
		}

		d$Type <- (substring(i, 1, nchar(i) - 1))

		print(i)

		data.main <- rbind(data.main, d)
	}

	data.main$Type <- factor(data.main$Type)
	data.main[,c(1,89)] <- as.numeric(data.main[,c(1,89)]

	#Scaling Main Data
		data.main.scaled <- data.main
		data.main.scaled[,c(2:89)] <- scale(data.main.scaled[,c(2:89)])

###########################################################

###########################################################
#OutputCSV
	if(outputCSV) {
		#Make the file path
			filePath = sprintf("%s/ScrubData/", getwd())

		#Write the csv
			#Master
			fileName = sprintf("%s/Master.csv", filePath)
			write.csv(data.main, file = fileName, row.names = FALSE)

			#MasterScaled
			fileName = sprintf("%s/MasterScaled.csv", filePath)
			write.csv(data.main.scaled, file = fileName, row.names = FALSE)
	}

###########################################################

###########################################################
#Classification Stuffs
	library(nnet)

	splitIndexes 	<- sample(nrow(data.main), .8 * nrow(data.main))
	trainData 		<- data.main[splitIndexes,]
	testData 		<- data.main[-splitIndexes,]

	mod.multi = multinom(Type ~ . - Timestamp, data = trainData)

	sums <- summary(mod.multi)
	z <- sums$coefficients/sums$standard.errors
	p <- round((1 - pnorm(abs(z), 0, 1)) * 2, 3)
	p

	#Prediction
	preds.train <- predict(mod.multi, newdata = trainData, type = "class")
	preds.test  <- predict(mod.multi, newdata = testData,  type = "class")
	
	error.train <- mean(preds.train != trainData$Type)
	error.test  <- mean(preds.test  != testData$Type)

	error.train #.06187 Train Error
	error.test  #.07865 Test Error
###########################################################

###########################################################
#Scaled Classification Stuffs
	library(nnet)

	splitIndexes.scaled	<- sample(nrow(data.main.scaled), .8 * nrow(data.main.scaled))
	trainData.scaled   	<- data.main.scaled[splitIndexes,]
	testData.scaled		<- data.main.scaled[-splitIndexes,]

	mod.multi.scaled = multinom(Type ~ . - Timestamp, data = trainData.scaled)

	sums.scaled <- summary(mod.multi.scaled)
	z.scaled <- sums.scaled$coefficients/sums.scaled$standard.errors
	p.scaled <- round((1 - pnorm(abs(z.scaled), 0, 1)) * 2, 3)
	p.scaled

	#Prediction
	preds.train.scaled <- predict(mod.multi.scaled, newdata = trainData.scaled, type = "class")
	preds.test.scaled  <- predict(mod.multi.scaled, newdata = testData.scaled,  type = "class")
	
	error.train.scaled <- mean(preds.train != trainData.scaled$Type)
	error.test.scaled  <- mean(preds.test  != testData.scaled$Type)

	error.train.scaled #.00000 Train Error
	error.test.scaled  #.02247 Test Error
###########################################################

