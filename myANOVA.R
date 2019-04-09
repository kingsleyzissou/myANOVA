myANOVA <- function(a, b) {


	# extra precaution just incase the variables were switched
	serialized = serializeVariables(a, b)
	scale = unlist(serialized[1])
	nominal = unlist(serialized[2])
	
	
	checkVariables(scale, nominal)
	
	#drawBoxPlot(scale, nominal)
	
	levels = levels(nominal)
	outliers = boxplot(scale~nominal, horizontal = T, names = levels)$out
	
	if(length(outliers) != 0) {
		print("The data set has some outliers present, which may have an affect on the spread")
	}
	
	if(!anovaTest(scale, nominal)) {
		string = "There is a difference between the means of each group"
		string = paste(string, "The following pairs are different:", sep = "\n")
		pairs <- tukey(scale, nominal)
		for(pair in pairs) string = paste(string, pair, sep = "\n")
		return(cat(string))
	}
	
	if(isNormal(scale, nominal) == F) {
		return("The distributions of the groups are not normal")
	}
	
	#if(kruskalTest(scale, nominal) == F) {
		#return("K-U-Z-C-O")
	#}


}

serializeVariables <- function(a, b) {
	a_len = length(table(a))
	b_len = length(table(b))
	# arrange the variables (scale first, nominal second)
	# wrap the nominal variable in 'as.factor()' just
	# in case the factors are numerical
	ls <- if(a_len > b_len) list(a, as.factor(b)) else list(b, as.factor(a))
}

checkVariables <- function(scale, nominal) {
	scale_length = length(table(scale))
	nominal_length = length(table(nominal))
	if(scale_length < 5)
		stop("Both paramaters appear to be nominal variables")
	if(nominal_length < 2)
		stop("The nominal variable supplied does not have enough levels")
	if(nominal_length > 10) 
		stop("The paramater supplied as a nominal variable appears to be a scale variable")
}

#drawBoxPlot <- function(scale, nominal) {
#	levels = levels(nominal)
#	boxplot(scale~nominal, horizontal = T, names = levels)
#}

anovaTest <- function(scale, nominal) {
	a = summary(aov(scale~nominal))
	p = a[[1]][["Pr(>F)"]][[1]]
	return(p > 0.05)
}

tukey <- function(scale, nominal) {
	# reference
	# https://gist.github.com/timcdlucas/cdb52b68c8c01968e438f9dc1871771d
	nominal.aov <- aov(scale ~ nominal, nominal)
	tukey <- TukeyHSD(nominal.aov)	
	pairs <- tukey$nominal[tukey$nominal[, 'p adj'] < 0.05, ]
	pairsnames <- row.names(pairs)
	return(pairsnames)
}

isHomoscedastic <- function(scale, nominal) {
	results = levene.test(scale, nominal)
	return (results[2]$p.value > 0.05)
}

isNormal <- function(scale, nominal) {
	levels = levels(as.factor(nominal))
	for(level in levels) {
		result = shapiro.test(scale[nominal == level])
		p = result[2]$p.value
		if(p < 0.05) {
			return(F)
		}
	}	
	return(T)
}

kruskalTest <- function(scale, nominal) {
	result = kruskal.test(scale~nominal)
	p = result[3]$p.value
	return(p > 0.05)
}
