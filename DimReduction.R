########################################################################################################################
# 
# NAME:
#	DimReduction
#
# PURPOSE:
#	Generic and campaign based dimentionality reduction 
#	
# INPUTS: 	
#	dataset: dataset
#	campaign: campaign name
#	campaign: campaign name
#	extractVariables: array of variables (in the form of field names) to be eliminated from the dataset
#
# OPTIONAL INPUTS:
# 	None
#
# OUTPUTS:
#	Preprocessed and reduced dimentionality dataset
#
# RESTRICTIONS:
#	None
#
# LIBRARIES ASSUMED TO BE LOADED (Dependencies):
#	None
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:	
#	Name: A.R. Ali and H. Ali 
#	Date: November 21, 2009 
#
########################################################################################################################

DimReduction = function(dataset, campaign, extractVariables = "") 
{
	names(dataset) = tolower(names(dataset));																	# make all names lower case										
    # TODO: Replace is.na with mens rather than Zeros
    #       simple means = mean(dataset, na.rm=TRUE) wont wok because 
	dataset[is.na(dataset)] = 0;																				# transform NA's to zero

	if(length(extractVariables) > 0)																			# eliminate unused variables
		dataset = subset(dataset, select = -match(tolower(extractVariables), names(dataset)));				

	if(campaign == "att") {																						# AT&T 

	} else if(tolower(campaign) == "vmc") {																		# VMC
		dataset = transform(dataset, clientgender_male = ifelse(clientgender == 1, 1, 0),						# Add MBTI agent variables
				clientgender_female = ifelse(clientgender == 0, 1, 0));
		dsAttributes = c("clientgender_male", "clientgender", 1, 1);
		dsAttributes = c(dsAttributes, "clientgender_female", "clientgender", 0, 1);
		
		dataset = transform(dataset, clientyoung = clientage1525 + clientage2535);								# data transformation of 3 way age split
		dataset = transform(dataset, clientold = clientage3545 + clientage4555 + clientage5565 + clientage6599);
		dsAttributes = c(dsAttributes, "clientyoung", "clientage1525", 1, 1);
		dsAttributes = c(dsAttributes, "clientyoung", "clientage2535", 1, 1);
		dsAttributes = c(dsAttributes, "clientold", "clientage3545", 1, 1);
		dsAttributes = c(dsAttributes, "clientold", "clientage4555", 1, 1);
		dsAttributes = c(dsAttributes, "clientold", "clientage5565", 1, 1);
		dsAttributes = c(dsAttributes, "clientold", "clientage6599", 1, 1);

		dataset = transform(dataset, clientother = clientraceafricanamerican + clientracehispanic + 			# client ethnicity
					clientraceasian + clientracenativeamerican + clientraceother);
		dataset = transform(dataset, clientwhite = clientracejewish + clientracewhite);	
		dsAttributes = c(dsAttributes, "clientother", "clientraceafricanamerican", 1, 1);
		dsAttributes = c(dsAttributes, "clientother", "clientracehispanic", 1, 1);
		dsAttributes = c(dsAttributes, "clientother", "clientraceasian", 1, 1);
		dsAttributes = c(dsAttributes, "clientother", "clientracenativeamerican", 1, 1);
		dsAttributes = c(dsAttributes, "clientother", "clientraceother", 1, 1);
		dsAttributes = c(dsAttributes, "clientwhite", "clientracejewish", 1, 1);
		dsAttributes = c(dsAttributes, "clientwhite", "clientracewhite", 1, 1);
	
		# tranformation
		dataset = subset(dataset, select = -disposition);
		dataset = transform(dataset, disposition = as.numeric(dataset$fios_data | dataset$fios_tv));
		dataset = transform(dataset, disposition = ifelse(is.na(disposition), 0, disposition)); 

		neducation = rep(0, nrow(dataset));																		# education
		neducation[dataset$agenteducation_notfinishedhighschool == 1] = 1;
		neducation[dataset$agenteducation_highschool == 1] = 2;
		neducation[dataset$agenteducation_associates == 1] = 3;
		neducation[dataset$agenteducation_bachelors == 1] = 4;
		neducation[dataset$agenteducation_masters == 1] = 5;
		neducation[dataset$agenteducation_doctorate == 1] = 6;
		dataset = cbind(dataset, neducation);
		dataset = transform(dataset, neducation = ifelse(is.na(neducation), 0, neducation));
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_notfinishedhighschool", 1, 1);
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_highschool", 1, 2);
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_associates", 1, 3);
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_bachelors", 1, 4);
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_masters", 1, 5);
		dsAttributes = c(dsAttributes, "neducation", "agenteducation_doctorate", 1, 6);
	
		nrisk = rep(0, nrow(dataset));																			# risk
		nrisk[dataset$agentrisk_riskaverse == 1] = 1;
		nrisk[dataset$agentrisk_belowaveragerisk == 1] = 2;
		nrisk[dataset$agentrisk_averagerisk == 1] = 3;
		nrisk[dataset$agentrisk_aboveaveragerisk == 1] = 4;
		nrisk[dataset$agentrisk_largeamountofrisk == 1] = 5;
		dataset = cbind(dataset, nrisk);
		dataset = transform(dataset, nrisk = ifelse(is.na(nrisk), 0, nrisk));
		dsAttributes = c(dsAttributes, "nrisk", "agentrisk_riskaverse", 1, 1);
		dsAttributes = c(dsAttributes, "nrisk", "agentrisk_belowaveragerisk", 1, 2);
		dsAttributes = c(dsAttributes, "nrisk", "agentrisk_averagerisk", 1, 3);
		dsAttributes = c(dsAttributes, "nrisk", "agentrisk_aboveaveragerisk", 1, 4);
		dsAttributes = c(dsAttributes, "nrisk", "agentrisk_largeamountofrisk", 1, 5);
	
		nmathability = rep(0, nrow(dataset));																	# mathability
		nmathability[dataset$agentmathability_veryweak == 1] = 1;
		nmathability[dataset$agentmathability_weak == 1] = 2;
		nmathability[dataset$agentmathability_average == 1] = 3;
		nmathability[dataset$agentmathability_strong == 1] = 4;
		nmathability[dataset$agentmathability_verystrong == 1] = 5;
		dataset = cbind(dataset, nmathability);
		dsAttributes = c(dsAttributes, "nmathability", "agentmathability_veryweak", 1, 1);
		dsAttributes = c(dsAttributes, "nmathability", "agentmathability_weak", 1, 2);
		dsAttributes = c(dsAttributes, "nmathability", "agentmathability_average", 1, 3);
		dsAttributes = c(dsAttributes, "nmathability", "agentmathability_strong", 1, 4);
		dsAttributes = c(dsAttributes, "nmathability", "agentmathability_verystrong", 1, 5);

		nlangability = rep(0, nrow(dataset))																	# langability 
		nlangability[dataset$agentlanguageability_veryweak == 1] = 1;
		nlangability[dataset$agentlanguageability_weak == 1] = 2;
		nlangability[dataset$agentlanguageability_average == 1] = 3;
		nlangability[dataset$agentlanguageability_strong == 1] = 4;
		nlangability[dataset$agentlanguageability_verystrong == 1] = 5;
		dataset = cbind(dataset, nlangability);
		dsAttributes = c(dsAttributes, "nlangability", "agentlanguageability_veryweak", 1, 1);
		dsAttributes = c(dsAttributes, "nlangability", "agentlanguageability_weak", 1, 2);
		dsAttributes = c(dsAttributes, "nlangability", "agentlanguageability_average", 1, 3);
		dsAttributes = c(dsAttributes, "nlangability", "agentlanguageability_strong", 1, 4);
		dsAttributes = c(dsAttributes, "nlangability", "agentlanguageability_verystrong", 1, 5);
		
		dsAttributes = c(dsAttributes, "business", "phone", 1, 1);												# transoframtion only
		dsAttributes = c(dsAttributes, "residence", "phone", 1, 1);
		dsAttributes = c(dsAttributes, "mobile", "Wireless_Fraction", 1, 1);
		
		dataset = subset(dataset, select = -c(clientgender, clientage1525, clientage2535, clientage3545,  		# eliminate unused variables
					clientage4555, clientage5565, clientage6599, clientraceafricanamerican,  
					clientracehispanic, clientraceasian, clientracenativeamerican, clientraceother,  
					clientracejewish, clientracewhite, fios_data, fios_tv, 
					agenteducation_notfinishedhighschool, agenteducation_highschool, 
					agenteducation_associates, agenteducation_bachelors, agenteducation_masters, 
					agenteducation_doctorate, agentrisk_riskaverse, agentrisk_belowaveragerisk, 
					agentrisk_averagerisk, agentrisk_aboveaveragerisk, agentrisk_largeamountofrisk, 
					agentmathability_veryweak, agentmathability_weak, agentmathability_average,
					agentmathability_strong, agentmathability_verystrong, agentlanguageability_veryweak, 
					agentlanguageability_weak, agentlanguageability_average,
					agentlanguageability_strong, agentlanguageability_verystrong));	
	} else if(campaign == "cc") {																				# Charter Communications
	
	} else if(campaign == "dgs") {																				# DGS
	
	}

	dim(dsAttributes) = c(4, length(dsAttributes) / 4);
    # means are dependent on dimentionality so cannot move this part to Build Model
    mn = mean(dataset); 

	return (list(dataset = dataset, dsAttributes = dsAttributes, means = mn));
}