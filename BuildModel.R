########################################################################################################################
# 
# NAME:
#	BuildModel
#
# PURPOSE:
#	Apply different supervised Machine Learning algorithms to get optimized model based on the specified dataset. 
#	
# INPUTS: 	
#	modelPath: save and load model to/from that path
#	dataPath: save and load data to/from that path
#	campaign: campaign name
#
#	loadFromDB: used to fill dataset from database or binaries file
#	datasetPath: dataset file name
#	isSaveDataset: save dataset loaded from DB or not
#	dsnName: data source name
#	dbUser: database username
#	dbPwd: database password
#	dbTableName: database table name
#	dbRows: number of rows to extract
#	dbDateVariable: database date variable 
#	toDate: database start date parameter
#	fromDate: database end date parameter
#
#	extractVariables: array of variables (in the form of field names) to be eliminated from the dataset
#	targetVariableName: field name of target variable 
#	invPropOfValidation:  the value set in this variable will be inverted and then multiplied
#	mlTechnique: machine learning technique
#	interactions: interaction terms based on campaign analysis 
#	glmFamily: GLM family name
#	nnHiddenNodes: number of hidden nodes for neural network 
#	svmKernel: SVM kernel function
#	svmC: SVM cost value 
#	svmCross: SVM cross validation value
#	isSaveModel: save model or not
#	modelFileName: model file name
#
# OPTIONAL INPUTS:
# 	None
#
# OUTPUTS:
#	Trained model 
#
# RESTRICTIONS:
#	RODBC, library of respective statistical algorithm, and source of DimReduction is to be loaded before calling it
#
# LIBRARIES ASSUMED TO BE LOADED (Dependencies):
#	The respective library is assumed to be loaded before calling Predict routine
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:	
#	Name: A.R. Ali and H. Ali  
#	Date: November 21, 2009
#
########################################################################################################################

BuildModel = function(campaign = '', loadFromDB = FALSE, datasetPath = '', isSaveDataset = TRUE,
				dsnName = '', dbUser = '', dbPwd = '', dbTableName = '', dbRows = 0, 
				dbDateVariable = '', toDate = '', fromDate = '', extractVariables = '', 
				targetVariableName = '', invPropOfValidation = 3, mlTechnique = '', interactions = '.', 
				glmFamily = 'binomial', nnHiddenNodes = 12, svmKernel = '', svmC = 20, svmCross = 3, 
				isSaveModel = TRUE, modelPath = '') 
{
	#############################################
	# loading dataset from database/binary file #
	#############################################
	
	source('DimReduction.R');
	
	if (loadFromDB == TRUE) {
		if (dsnName != '' && dbUser != '' && dbPwd != '' && dbTableName != '') {		
			library('RODBC')																					# loading RODBC library for database access
			if (dbRows == 0) {
				sqlQuery = paste("SELECT * FROM ", dbTableName, sep = '');
			} else {
				sqlQuery = paste("SELECT TOP ", dbRows, "* FROM ", dbTableName, sep = '');
			}
			if (dbDateVariable != '' && fromDate != '' && toDate != '') {
				sqlQuery = paste(sqlQuery, " WHERE ", dbDateVariable, " >= '", toDate, "' AND ", 
							dbDateVariable, " <= '", fromDate, "'", sep = '');									# execute non-query on top N rows of dbTableName table
			}

			channel = odbcConnect(dsnName, dbUser, dbPwd);														# connecting to the database with the database
			dataset = sqlQuery(channel, sqlQuery, as.is = TRUE);												# execute non-query on dbTableName table
			odbcClose(channel);																					# close the database connection
			rm(channel);																						# remove database channel from the memory
			detach("package:RODBC");

			if(isSaveDataset == TRUE && datasetPath != '') {
				save(dataset, file = paste(datasetPath, sep = ''));												# save database table in binary file
			}
		} else {
			return (0);
		}
	} else if (datasetPath != '') {
		dataset = get(load(file = paste(datasetPath, sep = '')));												# load binary file on the memory 
	} else {
		return (0);
	} 

	results = DimReduction(dataset, campaign, extractVariables);
	dataset = as.data.frame(results$dataset);
	dsAttributes = as.data.frame(results$dsAttributes);
	means = results$means
	
	targetVariableIndx = match(tolower(targetVariableName), names(dataset));									# get index of target variable index
																						
	if(invPropOfValidation != 0)
	{
		index = 1:nrow(dataset);																				# getting a vector of same size 
		sample = sample(index, length(index) / invPropOfValidation);											# samples of the above vector
		
		trainingset = dataset[-sample, ];																		# get training dataset
		validationset = dataset[sample, -targetVariableIndx];													# get validation dataset
		validationTargets = dataset[sample, targetVariableIndx];												# get validation labels

		rm(sample);
	} else {
		trainingset = dataset;																					# get training dataset
	}
	rm(dataset);																								# remove dataset from memory
    
	if (!is.na(match('GLM', mlTechnique))) {																	# generlized linear model
		eval(parse(text = paste("model = glm(", targetVariableName, " ~ ", interactions, ", family = ", 
						glmFamily, "(), data = trainingset, y = FALSE)", sep = '')));			
		
		if(isSaveModel == TRUE) {
			save(model, file = paste(modelPath, 'GLMModel.mod', sep = ''));										# save model in binary file
		}
	} 
	
	if (!is.na(match('NN', mlTechnique))) {																		# neural network	
		trainingset = trainingset[, -targetVariableIndx];														# get training dataset
		trainingTargets = trainingset[, targetVariableIndx];													# get training labels 
										
		n = length(validationTargets);																			# NN cross validation
		testSeq = (1:n) / n;
		testset = testSeq <= 0.75;
		validset = testSeq > 0.75;
		
		model = rprop7(trainingset, trainingTargets, validationset[testset, ], validationTargets[testset], 
					validationset[validset, ], nnHiddenNodes, train_count = 0, min_train = 0, diag = F, 
					graph = F, rms_error = F, debug = F, spearman = F, fname = '', scale_y = T, 
					randomise_weights = T);

		rm(trainingTargets);
		
		if(isSaveModel == TRUE) {
			save(model, file = paste(modelPath, 'NNModel.mod', sep = ''));										# save model in binary file
		}
	} 
	
	if (!is.na(match('RF', mlTechnique))) {																		# random forest
		library(randomForest);
		eval(parse(text = paste("model = randomForest(as.factor(", targetVariableName, ") ~.", 
						", data = trainingset, importance = TRUE)", sep = '')));
		detach('package:randomForest');
		
		if(isSaveModel == TRUE) {
			save(model, file = paste(modelPath, 'RFModel.mod', sep = ''));										# save model in binary file
		}
	} 
	
	if(!is.na(match('NB', mlTechnique))) {																		# naive bayes 
		library(e1071);
		eval(parse(text = paste("model = naiveBayes(as.factor(", targetVariableName, ") ~ .", 
						", data = trainingset, laplace = 3)", sep = '')));							
		detach('package:e1071');
		
		if(isSaveModel == TRUE) {
			save(model, file = paste(modelPath, 'NBModel.mod', sep = ''));										# save model in binary file
		}
	} 
	
	if (!is.na(match('SVM', mlTechnique))) {																	# support vector machines
		library(class);
		library(e1071);	
		for(i in 1:length(svmKernel)) {																			
			eval(parse(text = paste("model = svm(as.factor(", targetVariableName, ") ~ .", 
							", data = trainingset, scaled = FALSE, kernel = '", svmKernel, "', epsilon = 0.1, 
							cost = ", svmC, ", probability = TRUE, cross = ", svmCross, ", cachesize = 2047)", 
							sep = '')));
		}									
		
		detach('package:e1071');
		
		if(isSaveModel == TRUE) {
			save(model, file = paste(modelPath, 'SVMModel.mod', sep = ''));										# save model in binary file
			#write.svm(model, svm.file = paste(modelPath, 'SVMModel.mod', sep = ''));
		}
	}

	rm(trainingset);
	if(invPropOfValidation != 0) {
		rm(validationset);
		rm(validationTargets);
	}
	
	gc();																										# garbage collector
	return (list(model = model, dsAttributes = dsAttributes, means = means));
}