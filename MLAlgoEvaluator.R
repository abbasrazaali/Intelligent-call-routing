########################################################################################################################
# 
# NAME:
#	MLAlgoEvaluator.R
#
# PURPOSE:
#	Call different supervised Machine Learning algorithms to get optimized model based on the dataset. 
#	
# INPUTS: 	
#	BuildModel.R
#	DimReduction.R
#	StoreModel.R
#	Prediction.R
#	ModelMetric.R
#
# OPTIONAL INPUTS:
# 	None
#
# OUTPUTS:
#	Trained model, store model in database, predict a model, and/or generate Area Under the Curve
#
# RESTRICTIONS:
#	source of scripts mentioned in INPUT is to be loaded before calling it
#
# LIBRARIES ASSUMED TO BE LOADED (Dependencies):
#	The respective library is assumed to be loaded before calling Predict routine
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:	
#	Name: H. Ali and A.R. Ali 
#	Date: November 21, 2009
#
# CALLING SEQUENCE
#	source('Source Code/R/EvaluatorR v2.2/MLAlgoEvaluator.R');
#
########################################################################################################################

gc();																											# garbage collector
rm(list = ls());																								# remove all variables reside in the memory
prevwd = getwd();
basePath = 'F:/SourceControlAIDev/AI/Source Code/R/EvaluatorR v2.2';
setwd(basePath);																			# set working directory

########################################################################################################################
# Extract variables
# AT&T: c("campaign", "pbx", "customerphonenumber", "time", "pbxid", "code", "agentid", "record_id", "phone")
# Charter: c("campaign", "switchid", "agentid", "home_telephone", "agent_mbit_score")
# VMC: c("switchid", "pbxid", "contact_phone", "report_date")
# DGS: c("agentid", "number")
# 
# ML Techniques 
# c('GLM', 'NN', 'RF', 'NB', 'SVM')
#
# SVM Kernel
# c('linear', 'polynomial', 'radial', 'sigmoid'); 
source('BuildModel.R');
campaign = 'VMC';
#mlTechnique = c('GLM', 'NB', 'SVM');
mlTechnique = c('GLM');
if(campaign == 'VMC') {
	interactions = "(clientgender_female + clientyoung + clientold + clientother + clientwhite + residence + business + mobile) %in% neducation +
					(clientgender_female + clientyoung + clientold + clientother + clientwhite + residence + business + mobile) %in% nrisk +
					(clientgender_female + clientyoung + clientold + clientother + clientwhite + residence + business + mobile) %in% nmathability +
					(clientgender_female + clientyoung + clientold + clientother + clientwhite + residence + business + mobile) %in% nlangability +
					(clientgender_female + clientyoung + clientold + clientother + clientwhite + residence + business + mobile) %in% agentrace_white";
}
results = BuildModel(campaign = campaign, loadFromDB = TRUE, 
			datasetPath = paste(basePath, '/Resources/Datasets/VMCDataset.dat', sep = ''), isSaveDataset = TRUE,
			dsnName = 'demoDSN', dbUser = 'sa', dbPwd = 'trg1234', dbTableName = 'VMC_Training3m', dbRows = 0, 
			dbDateVariable = '', toDate = '', fromDate = '', extractVariables = c('switchid', 'pbxid', 'contact_phone', 
			'report_date'), targetVariableName = 'disposition', invPropOfValidation = 0, mlTechnique = mlTechnique, 
			interactions = interactions, glmFamily = 'binomial', nnHiddenNodes = 12, svmKernel = c('linear'), svmC = 20, 
			svmCross = 3, isSaveModel = TRUE, modelPath = paste(basePath, '/Resources/Models/', campaign, sep = ''));

########################################################################################################################
source('StoreModel.R');
mlTechnique = c('GLM');
means = results$means;
if (!is.na(match('GLM', mlTechnique))) {																		# generlized linear model
	isModelStored = StoreModel(model = results$model, campaign = 'VMC', optimization = 'Revenue', mlTechnique = 'GLM', 
						dsAttributes = as.data.frame(results$dsAttributes), dsnName = 'evaluatorDBDSN', dbUser = 'sa', 
						dbPwd = 'trg1234', dbTableName = 'MLModelNew', replaceTable = TRUE, means = means);
}

########################################################################################################################

setwd(prevwd);
#dataset = get(load(file = paste(targetPath, "/", "VMC_Validationset.dat", sep = "")));
#print(dataset);					
#prediction = Predict(dataset, model, mlTechnique = "GLM", isSatMapON = TRUE);
#prediction;