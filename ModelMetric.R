###################################################################################################################
# 
# NAMD:
#	mlMetric
#
# PURPOSD:
#	Plots the famous double graph - histogram of outputs by histogram bin. Also computes also weighted least squares of latter graph and 
#	of latter graph and returns regression parameters.
#	
# INPUTS: 	
# 	predictedValues: predicted values  
#	actualValues: actual values
#	n_breaks: number of breaks in histogram
#	output: data relates to target variable
#	brk: if non-zero is sequence of bin intervals to use - overrides arguments breaks
#	yfix: yfix
#	notes: main notes
#	campaign: campaign name
#	mlTechnique: machine learning technique used
#	y2lab: y2lab
#	graph: plot graph or not
#
# OPTIONAL INPUTS:
# 	None
#
# OUTPUTS:
#	list(language model, histogram, area under the curve)
#
# RESTRICTIONS:
#	None
#
# LIBRARIES ASSUMED TO BE LOADED (Dependencies):
#	pspline: package for smothing spline 
#
# MODIFICATION HISTORY:
#	None
#
# AUTHORSHIP:
#	Name: S.J.P Spottiswoode  
#	Date: September 25, 2009
#
# DATABASE CONNECTION PARAMETERS
#	None
#
# CALLING SEQUENCE
#	mlMetric(predictedValues, actualValues, n_breaks = 10, output = "", brk = 0, yfix = 0, notes = "", 
#				campaign = "", mlTechnique = "", y2lab = "", graph = TRUE)
#
###################################################################################################################

mlMetric = function(predictedValues, actualValues, n_breaks = 10, output = "", brk = 0, yfix = 0, notes = "", 
				campaign = "", mlTechnique = "", y2lab = "", graph = TRUE) 
{
	library(pspline)																	# smoothing splines with penalties on order m derivatives
	
	nmc = 10000																		# number of cross matches to calculate
	nsteps = 200																	# number of steps in ROC calculation
	cexlab = 1.1
	cexaxis = 1.1
	cexmain = 1.2
	cexnotes = 0.7
	notelines = 3.5
	spline_df = 10																	# degrees of freedom of smoothing spline in ROC calculation

	disp = length(table(actualValues)) == 2
	nsteps = 200
	
	########################
	# Histogram of Results #
	########################

	if (graph == TRUE) {															
		par(mfrow = c(2, 1))
		par(mar = c(4.5, 3, 2, 3))
		par(mgp = c(2.0, 1, 0))

		if (length(brk) == 1) {
			r = hist(predictedValues, xlab = "Model Output", breaks = n_breaks,
			cex.lab = cexlab, cex.axis = cexaxis, main = "", col = "lightcyan") 
		} else {
			q = predictedValues >= min(brk) & predictedValues <= max(brk)
			r = hist(predictedValues[q], xlab = "Model Output", breaks = brk,
					cex.lab = cexlab, cex.axis = cexaxis, main = "", col = "lightcyan")
		}
		box()
	} else {
		if (length(brk) == 1) {
			r = hist(predictedValues, breaks = n_breaks, plot = F) 
		} else {
			q = predictedValues >= min(brk) & predictedValues <= max(brk)
			r = hist(predictedValues[q], breaks = brk, plot = F)
		}
	}	
	nb = length(r$breaks) - 1															# find hit rate in each break and compute new metric
	hr1 = rep(0, nb)
	cnt = rep(0, nb)
	mean_nn = rep(0, nb)
	sd_nn  = rep(0, nb)
	sey = rep(0, nb)
	for(i in 1:nb) {
		q = predictedValues > r$breaks[i] & predictedValues <= r$breaks[i + 1]
		if (disp) {
			hr1[i] = sum(q & (actualValues == 1)) / sum(q) * 100 
		} else {
			hr1[i] = mean(actualValues[q])
		}
		cnt[i] = sum(q)
	
		mean_nn[i] = mean(predictedValues[q])
		if (sum(q, na.rm = T) > 5) { 
			sd_nn[i] = sd(predictedValues[q])
		}

		if (cnt[i] > 5 & cnt[i] * hr1[i] > 0) {
			if (disp) {
				r1 = diff_prop(cnt[i] * hr1[i] / 100, cnt[i], 5, 10, verbose = F)
				sey[i] = r1$sex * 100 
			} else {
				sey[i] = sd(predictedValues[q]) / sqrt(cnt[i]) * 100
			}
		}	
	}

	if (disp) { 
		avg_hr = sum(actualValues) / length(actualValues) * 100
	} else {
		avg_hr = mean(actualValues)
	}
	hr2 = hr1 / avg_hr
	z = ((mean_nn - mean(predictedValues)) / sd(predictedValues))									# new metric is: 
	score = z * hr2																	# compute Z-score of histogram of each bin ((NN O/P - mean(NN O/P)) / sd(NN O/P)), say Z sub i
	weighted_mean_score = sum(cnt * score, na.rm = T) / sum(cnt, na.rm = T) * 100							# then find Z sub i * (1 - hit rate) where hit rate is 1 for mean hit rate
																				# then find weighted mean of above weighted by number in bin/total number
	m = lm(hr1 ~ r$mids, weights = cnt)														# Calculate WLS fir
	anovam = anova(m)
	fm = anovam[[4]][1]
	pm = anovam[[5]][1]

	if (graph) {
		print(anova(m))
	}

	maintext = paste("Analysis of ", campaign, " using ", toupper(mlTechnique), sep = "") 
	
	q = cnt > 5

	if (graph) {
		par(new = T)

		if (length(yfix) == 1) {
			plotCI(r$mids[q], hr1[q], sey[q], type = "l", col = "red", xlim = c(min(r$breaks), max(r$breaks)), 
				axes = F, xlab = "", ylab = "", main = maintext, cex.main = cexmain)
		} else {
			plotCI(r$mids[q], hr1[q], sey[q], type = "l", col = "red", xlim = c(min(r$breaks), max(r$breaks)), 
				axes = F, xlab = "", ylab = "", main = maintext, cex.main = cexmain, ylim = yfix)
		}

		axis(4, cex.lab = cexlab, cex.axis = cexaxis, col = "red")
		if (disp) {
			mtext("Validation Set Hit Rate (%)", 4, 2, cex = cexlab, col = "red") 
		} else {
			mtext(y2lab, 4, 2, cex = cexlab, col = "red")
		}

		lines(c(min(r$breaks), max(r$breaks)), c(avg_hr, avg_hr), col = "blue")
		grid(col = "darkgreen")

		if (disp) {
			mtext(paste("Histogram shows binned model outputs with bin counts on left Y axis, red points = validation ", 
				"set Sales Rate for calls in each bin with scale on, \n right Y axis, solid blue line is mean Sales Rate ",
				"for whole validation set & dashed black line is the weighted least squares fit through the red points.", 
				sep = ""), 1, notelines, cex = cexnotes, col = 'magenta') 
		} else {
			mtext(paste("Histogram shows binned model outputs with bin counts on left Y axis, red points = validation ",
				"set Handle Time for calls in each bin with scale on, \n right Y axis, solid blue line is mean Handle Time ", 
				"for whole validation set & dashed black line is the weighted least squares fit through the red points.", 
				sep = ""), 1, notelines, cex = cexnotes, col = 'magenta') 
		}
		box()

		abline(m, lty = 2)																# add WLS fit
		box(which = "figure", lwd = 2)
		if (notes != '') {																# add notes
			y_notes = max(c(hr1[q] + sey[q])) * 0.9
			text(r$mids[q][1] - 0.06 * (max(r$mids[q][2]) - min(r$mids[q][1])), y_notes, notes, cex = 0.8, 
				adj = 0.0)
			print(paste(r$mids[q[1]], y_notes))
		}
	}			
	
	##########################################################################
	# Produce Receiver Operating Characteristic (ROC) plot and calculate AUC #
	##########################################################################

	if (disp) {																
		sensitivity = rep(0, nsteps)
		specificity = rep(0, nsteps)
		miny = min(predictedValues)
		maxy = max(predictedValues)
		rany = maxy - miny
	
		for (i in 1:nsteps) {																# scan through range of model output in nsteps steps and 
			thresh = miny + i * rany / nsteps													# calculate the sensitivity and specificity at each threshold value

			tp = sum(predictedValues >=  thresh & actualValues == 1, na.rm = T)
			fp = sum(predictedValues >=  thresh & actualValues == 0, na.rm = T)
			tn = sum(predictedValues <  thresh & actualValues == 0, na.rm = T)
			fn = sum(predictedValues <  thresh & actualValues == 1, na.rm = T)
			sensitivity[i] = fp / (tn + fp) * 100
			specificity[i] = tp / (tp + fn) * 100
		}
					
		if (length(unique(sensitivity)) < nsteps) {												# if there are non-unique values in sensitivity it break spline
			sensitivity = sensitivity + seq(0, 0.1, length.out = nsteps)
		}
		
		sp = sm.spline(sensitivity, specificity, df = min(c(spline_df, length(unique(specificity)) - 1)))
		sp.predictedValues = predict(sp, seq(0, 100, length.out = nsteps))
		
		AUC = sum(sp.predictedValues, na.rm = T) / nsteps											# calculate area under curve (AUC)
		
		if (graph == TRUE) {
			par(mar = c(3, 3, 2, 1))
			plot(sensitivity, specificity, xlab = "Percent False Positives", ylab = "Percent True Positives", 
				cex.lab = cexlab, cex.axis = cexaxis, xlim = c(0, 100), ylim = c(0, 100), 
				main = "Receiver Operating Characteristic", cex.main = cexmain)
			par(new = T)
				
			plot(seq(0, 100, length.out = nsteps), sp.predictedValues, xlim = c(0, 100), ylim = c(0, 100), 
				xlab = "", ylab = "", axes = F, col = "red", type = "l")
			par(new = T)
			
			plot(0:1, 0:1, xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", axes = F, col = "darkblue", 
				type = "l", lty = 2)
			text(0.5, 0.47, "Chance Expectation AUC = 50.00", cex = cexnotes, col = "darkblue", srt = 45 / 3.0)
			
			lines(c(0.0, 0.0), c(0.0, 1.0), col = "darkgreen", lwd = 1)									# put in lines for perfect response
			lines(c(0.0, 1.0), c(1, 1), col = "darkgreen", lwd = 1)
			text(0.5, 0.97, "Perfect Response AUC = 100.00", cex = cexnotes, col = "darkgreen")
			
			legend("bottomright", paste("AUC:", formatC(AUC, digits = 2, format = "f")), 
				inset = 0.01, bg = "lightyellow", cex = 1.0)
			grid(col = "darkgreen")
			box(which = "figure", lwd = 2)
		}	
	}
	
	par(mfrow = c(1, 1)) 																	# reset graphics
	
	return(list(m = m, r = r, AUC = AUC))
}