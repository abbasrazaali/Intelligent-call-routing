########################################################################################################################
# 
# NAME:
#	Predict
#
# PURPOSE:
#	Predict a dataset based on the given model 
#	
# INPUTS: 	
#	agentCallsCross: agent and calls cross
#	model: trained model
#	mlTechnique: machine learning technique
#	isSatMapON: if SatMap on the best prediction will be returned otherwise worst 
#
# OPTIONAL INPUTS:
# 	None
#
# OUTPUTS:
#	Agents - Calls optimized ranking vector
#
# RESTRICTIONS:
#	None
#
# LIBRARIES ASSUMED TO BE LOADED (Dependencies):
#	The respective library is assumed to be loaded before calling Predict routine
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:	
#	Name: A.R. Ali  
#	Date: November 21, 2009
#
# DATABASE CONNECTION PARAMETERS
#	None 
#
# CALLING SEQUENCE
#	source("D:/SATMAP/AI/Source Code/R/ML Algorithms/Predict.R");
#	Predict(agentCallsCross, model, mlTechnique, isSatMapON = TRUE); 
#
########################################################################################################################

Predict = function(agentCallsCross, model, mlTechnique, isSatMapON = TRUE) 
{ 	
	if (mlTechnique == "GLM") {														# generlized linear model
		agentCallsCross = predict(model, agentCallsCross, type = "response");	
	} else if (mlTechnique == "NN") {													# neural networks
		agentCallsCross = nn_eval(model, agentCallsCross);
	} else if (mlTechnique == "RF") {													# random forest
		agentCallsCross = predict(model, agentCallsCross, type = "prob");
	} else if(mlTechnique == "NB") {													# naive bayes networks
		agentCallsCross = predict(model, agentCallsCross, type = "raw");					
	} else if (mlTechnique == "SVM") {													# support vector machines
		agentCallsCross = predict(model, agentCallsCross, type = "probabilities");
	}

	if(isSatMapON == TRUE) {
		return (agentCallsCross[, 1]);
	} else {
		return (agentCallsCross[, 2]);
	}
}