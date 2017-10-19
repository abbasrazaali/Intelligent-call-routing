########################################################################################################################
#
# NAME:
#	StoreXMLModel
#
# PURPOSE:
#	Function takes in a trained model and store it in database
#
# INPUTS:
#	model: trained model
#	campaign: campaign name 
#	optimization: 'Revenue', 'Cost', 'Satisfaction'
#	mlTechnique: machine learning technique used to train the model
#	dsAttributes: dataset attribute names
#	dsnName: data source name
#	dbUser: database username
#	dbPwd: database password
#	dbTableName: database table name
#	replaceTable: delete existing rows
#
# OPTIONAL INPUTS:
#	None
#
# OUTPUTS:
#	Model stored or not
#
# RESTRICTIONS:
#	None
#
# LIBRARIES ASSUMED TO BE LOADED:
#	RODBC
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:	
#	Name: A.R. Ali, H. Ali, H. Janjua  
#	Date: December 06, 2009
#
########################################################################################################################

StoreXMLModel = function(model, campaign, optimization, mlTechnique, dsAttributes, dsnName, dbUser, dbPwd, dbTableName, 
				replaceTable = TRUE, means) 
{
	library("RODBC");
	library("XML");
	XMLModel = xmlOutputDOM();
	
	if (dsnName == '' || dbUser == '' || dbPwd  == '' || dbTableName  == '') {		
		return (0);
	}
	channel = odbcConnect(dsnName, dbUser, dbPwd);																# connecting to the database with the database
	if (replaceTable == TRUE) {
		delTable = paste("Delete From ", dbTableName, sep = "");												# delete data from table 
		sqlQuery(channel, delTable);																			# execute non-query on dbTableName table
	}
	
	if (mlTechnique == 'GLM') {
		coeffs = model$coefficients;
		coeffs[is.na(model$coefficients)] = 0;
		
		XMLModel$addTag("model", close = FALSE);
		
		for (i in 1:length(coeffs))																				# store interactions in XML
		{
			XMLModel$addTag("interaction", close = FALSE);
			XMLModel$addTag("feature", names(coeffs[i]));
			XMLModel$addTag("value", coeffs[i]); 		
			XMLModel$closeTag();
		}
		
		XMLModel$addTag("transformations", close = FALSE);
		for (i in 1:length(dsAttributes))																		# store transformations in XML
		{
			XMLModel$addTag("transformation", close = FALSE);
			XMLModel$addTag("feature", dsAttributes[i][1, 1]);
			XMLModel$addTag("description", dsAttributes[i][2, 1]); 		
			XMLModel$addTag("case", dsAttributes[i][3, 1]);
			XMLModel$addTag("value", dsAttributes[i][4, 1]); 		
			XMLModel$closeTag();
		}
		XMLModel$closeTag();
		XMLModel$closeTag();
	 } else if (mlTechnique == 'NN') {	
		XMLModel$addTag("model", close = FALSE);		
		
		XMLModel$addTag("hidden_units", model$hid_nodes);																
	
		XMLModel$addTag("WIH", attrs = c(count = paste(dim(model$wih)[1])), close = FALSE);						# store weights of input - hidden unit 
		for(i in 1:(dim(model$wih)[1] - 1))
		{
			inputUnits = '';
		    for(j in 1:model$hid_nodes) 
		        inputUnits = paste(inputUnits, model$wih[i, j]);
			XMLModel$addTag(model$var_names[i], inputUnits);
		}
		biasUnit = '';
		for(j in 1:model$hid_nodes) 
			biasUnit = paste(biasUnit, model$wih[dim(model$wih)[1], j]);
		XMLModel$addTag("bias", biasUnit);
		XMLModel$closeTag();
		
		XMLModel$addTag("WHO", attrs = c(count = 1), close = FALSE);											# store weights of hidden - output unit 
		for(i in 1:model$hid_nodes) 
			XMLModel$addTag("input", model$who[i]);	
		XMLModel$addTag("bias", model$who[length(model$who)]);
		XMLModel$closeTag();	
		
		XMLModel$addTag("Scaling", attrs = c(count = 2), close = FALSE);										 
		varMin = '';																							# store var_min
		for(i in 1:length(model$var_min)) 
			varMin = paste(varMin, model$var_min[i]);
		XMLModel$addTag("VAR_MIN", varMin);
												
		varRange = '';																							# store var_range 
		for(i in 1:length(model$var_range)) 
			varRange = paste(varRange, model$var_range[i]);
		XMLModel$addTag("VAR_RANGE", varRange);
		XMLModel$closeTag();
		
		XMLModel$addTag("transformations", close = FALSE);
		for (i in 1:length(dsAttributes))																		# store transformations in XML
		{
			XMLModel$addTag("transformation", close = FALSE);
			XMLModel$addTag("feature", dsAttributes[i][1, 1]);
			XMLModel$addTag("description", dsAttributes[i][2, 1]); 		
			XMLModel$addTag("case", dsAttributes[i][3, 1]);
			XMLModel$addTag("value", dsAttributes[i][4, 1]); 		
			XMLModel$closeTag();
		}
		XMLModel$closeTag();
		XMLModel$closeTag();
	} else if (mlTechnique == 'SVM') {
		XMLModel$addTag("model", close = FALSE);
		
		XMLModel$addTag("svm_type", model$type);																# store SVM model in XML
		XMLModel$addTag("kernel_type", model$kernel);	
	    XMLModel$addTag("degree", model$degree);
	    XMLModel$addTag("gamma", model$gamma);
	    XMLModel$addTag("coef0", model$coef0);	

		XMLModel$addTag("nr_class", model$nclasses);
		XMLModel$addTag("total_sv", model$tot.nSV);
		XMLModel$addTag("rho", model$rho);

		XMLModel$addTag("labels", close = FALSE);
	    for (i in 1:length(model$labels))																		
			XMLModel$addTag(paste("label", i, sep = ''), model$labels[i]);
		XMLModel$closeTag();
	
		XMLModel$addTag("probA", model$probA);
		XMLModel$addTag("probB", model$probB);

		XMLModel$addTag("nr_sv", close = FALSE);
	    XMLModel$addTag("col1", model$nSV[1]);
	    XMLModel$addTag("col2", model$nSV[2]);
		XMLModel$closeTag();
		
		coefs = '';
		for(i in 1:length(model$coefs)) 
			coefs = paste(coefs, model$coefs[i], sep=" ");
		XMLModel$addTag("sv_coefs", coefs);
    
	    features = attr(model$terms, "term.labels");

		XMLModel$addTag("SV", attrs = c(count = paste(length(features))), close = FALSE);    
		for(i in 1:length(features))
		{
			SVs = '';
		    for(j in 1:model$tot.nSV) 
		        SVs = paste(SVs, model$SV[j, i]);
			XMLModel$addTag(features[i], SVs);
		}
		XMLModel$closeTag();
		
		XMLModel$addTag("transformations", close = FALSE);														# store transformations in XML
		for (i in 1:length(dsAttributes))																		
		{
			XMLModel$addTag("transformation", close = FALSE);
			XMLModel$addTag("feature", dsAttributes[i][1, 1]);
			XMLModel$addTag("description", dsAttributes[i][2, 1]); 		
			XMLModel$addTag("case", dsAttributes[i][3, 1]);
			XMLModel$addTag("value", dsAttributes[i][4, 1]); 		
			XMLModel$closeTag();
		}
		XMLModel$closeTag();
		XMLModel$closeTag();
	}

	insertTransformations = paste("Insert into ", dbTableName, "(EngineID, Skill, Optimization, L2Technique, 
								L2Model) values (1, '68001', '", optimization, "', '", mlTechnique, "', '", 
								toString(XMLModel$value()), "')", sep = '');
	sqlQuery(channel, insertTransformations);
	
	for (i in 1:length(means))																					# store transformations
	{
		insertTransformations = 
		    paste(
		    "Insert into ", 
		    dbTableName, "(Campaign, Optimization, MLTechnique, Features, Value, Type) values ('", 
		    campaign, "', '", 
		    optimization, "', '", 
			mlTechnique, "', '", 
			names(means[i]), "', '", 
			means[i], 
			"', 'means')", 
			sep = '');
		sqlQuery(channel, insertTransformations);
	}
	odbcClose(channel);																							# close the database connection
	rm(channel);																								# remove database channel from the memory
	rm(model);
	
	detach('package:RODBC');
	detach('package:XML');
	
	return (1);
}
