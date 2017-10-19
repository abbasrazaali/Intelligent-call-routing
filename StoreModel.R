########################################################################################################################
#
# NAME:
#	StoreModel
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
#	Name: A.R. Ali and H. Ali  
#	Date: December 06, 2009
#
########################################################################################################################

StoreModel = function(model, campaign, optimization, mlTechnique, dsAttributes, dsnName, dbUser, dbPwd, dbTableName, 
				replaceTable = TRUE, means) 
{
	library("RODBC");
	
	if (dsnName == '' || dbUser == '' || dbPwd  == '' || dbTableName  == '') {		
		return (0);
	}
	
	channel = odbcConnect(dsnName, dbUser, dbPwd);																# connecting to the database with the database
	if (replaceTable == TRUE) {
		delTable = paste("Delete From ", dbTableName, " Where Campaign = '", campaign, "' and Optimization = '", 
				optimization, "' and MLTechnique = '", mlTechnique, "'", sep = "");								# delete data from table 
		sqlQuery(channel, delTable);																			# execute non-query on dbTableName table
	}
	
	if (mlTechnique == 'GLM') {
		coeffs = model$coefficients;
		coeffs[is.na(model$coefficients)] = 0;
																												# store intercept
		insertInteractions = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,			
			Features, Value, Type) values ('", campaign, "', '", optimization, "', '", 
			mlTechnique, "', '", names(coeffs[1]), "', ", coeffs[1], ", 'intercept')", sep = '');
		sqlQuery(channel, insertInteractions);
			
		for (i in 2:length(coeffs))																				# store interactions
		{	
			insertInteractions = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,		
				Features, Value, Type) values ('", campaign, "', '", optimization, "', '", 
				mlTechnique, "', '", names(coeffs[i]), "', ", coeffs[i], ", 'interactions')", sep = '');
			sqlQuery(channel, insertInteractions);
		}
		
		for (i in 1:length(dsAttributes))																		# store transformations
		{
			insertTransformations = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,	
				Features, Description, Options, Value, Type) values ('", campaign, "', '", optimization, "', '", 
				mlTechnique, "', '", dsAttributes[i][1, 1], "', '", dsAttributes[i][2, 1], "', ", 
				dsAttributes[i][3, 1], ", ", dsAttributes[i][4, 1], ", 'transformations')", sep = '');
			sqlQuery(channel, insertTransformations);
		}
	} else if (mlTechnique == 'NN') {	
		for (i in 1:dim(model$wih)[1]) {																		# store WIH
			for (j in 1:dim(model$wih)[2]) {
				WIH = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,					
					NNLayers, NNLayer1Units, NNLayer2Units, Value, Type) values ('", campaign, "', 
					'", optimization, "', '", mlTechnique, "', 'WIH', ", i, ", ", j, ", ", model$wih[i, j], ")", 
					sep = '');
				sqlQuery(channel, WIH);
			}
		}
		
		for (i in 1:length(model$who)) {																		# store WHO
			WHO = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,						
				NNLayers, NNLayer1Units, NNLayer2Units, Value) values ('", campaign, "', 
				'", optimization, "', '", mlTechnique, "', 'WHO', ", i, ", 0, ", model$who[i], ")", 
				sep = '');
			sqlQuery(channel, WHO);
		} 
		
		for (i in 1:length(model$var_min)) {																	# store Var_Min
			varMin = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,					
				NNLayers, NNLayer1Units, NNLayer2Units, Value) values ('", campaign, "', 
				'", optimization, "', '", mlTechnique, "', 'VAR_MIN', ", i, ", 0, ", model$var_min[i], ")", 
				sep = '');
			sqlQuery(channel, varMin);
																												# store Var_Range 
			varRange = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,				
				NNLayers, NNLayer1Units, NNLayer2Units, Value) values ('", campaign, "', 
				'", optimization, "', '", mlTechnique, "', 'VAR_RANGE', ", i, ", 0, ", model$model$var_range[i], 
				")", sep = '');
			sqlQuery(channel, varRange);
		}
		
		for (i in 1:length(model$var_names)) {																	# store attributes 
			attributes = paste("Insert into ", dbTableName, "(Campaign, Optimization, MLTechnique,				
				NNLayers, NNLayer1Units, NNLayer2Units, Value, Type) values ('", campaign, "', 
				'", optimization, "', '", mlTechnique, "', 'COLUMN', ", i, ", 0, 0, '", model$var_names[i], 
				"')", 
				sep = '');
			sqlQuery(channel, attributes)
		}
	}
	for (i in 1:length(means))																		# store transformations
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
	
	return (1);
}
