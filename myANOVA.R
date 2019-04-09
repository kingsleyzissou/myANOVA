myANOVA <- function(a, b) {
	# extra precaution just incase the variables were switched
	serialized = serializeVariables(a, b)
	scale = unlist(serialized[1])
	nominal = unlist(serialized[2])
	
	# check that the variables are of the correct type
	# and suitable for the test
	checkVariables(scale, nominal)
	
	# draw the box plot and notify if there are outliers
	drawBoxPlot(scale, nominal)
	
	# Check that the assumptions are met, if not, 
	# run the Kruskal-Wallis non-parametric test
	assumptions = checkAssumptions(scale, nominal)
	if(assumptions[1] == F) {
		# prints the message from the assumptions
		# and the Kruskal-Wallis test (if it was run)
		return(cat(assumptions[2]))
	}
	
	
	
	# Run the anova test
	# If the result is not statistically significant,
	# run Tukey Honest Significant difference post-hoc test
	# and report the pairs that are significantly different	
	if(!anovaTest(scale, nominal)) {
		string = "There is a difference between the means of each group"
		string = paste(string, "The following pairs are different:", sep = "\n")
		pairs <- tukeyTest(scale, nominal)
		for(pair in pairs) string = paste(string, pair, sep = "\n")
		return(cat(string))
	}

	# Since we've come this far, we can now accept the null hypothesis
	# and report our findings
	return("The means of the groups are the same")

}

serializeVariables <- function(a, b) {
	a_len = length(table(a)) # get the levels of the first variable
	b_len = length(table(b)) # get the levels of the second variable
	# arrange the variables (scale first, nominal second)
	# wrap the nominal variable in 'as.factor()' just
	# in case the factors are numerical
	ls <- if(a_len > b_len) list(a, as.factor(b)) else list(b, as.factor(a))
}

checkVariables <- function(scale, nominal) {
	scale_length = length(table(scale)) # get the levels of the scale variable
	nominal_length = length(table(nominal)) # get the levels of the nominal variable
	# if the scale variable only has 5 levels, it is clearly not a scale variable
	if(scale_length < 5)
		stop("Both paramaters appear to be nominal variables")
	# if the nominal variable only has 1 level, we can't proceed
	if(nominal_length < 2)
		stop("The nominal variable supplied does not have enough levels")
	# if the nominal variable has too many levels, it is probably a scale variable
	if(nominal_length > 10) 
		stop("The paramater supplied as a nominal variable appears to be a scale variable")
}

drawBoxPlot <- function(scale, nominal) {
	levels = levels(nominal)
	outliers = boxplot(scale~nominal, horizontal = T, names = levels)$out # extract the outliers
	if(length(outliers) != 0) {
		# notify the user that there could be some outliers that could affect the test
		cat("The data set has some outliers present, which may have an affect on the spread\n")
	}
}

anovaTest <- function(scale, nominal) {
	# wrapper for the anova (aov) test
	a = summary(aov(scale~nominal))
	p = a[[1]][["Pr(>F)"]][[1]]
	return(p > 0.05)
}

tukeyTest <- function(scale, nominal) {
	# reference
	# https://gist.github.com/timcdlucas/cdb52b68c8c01968e438f9dc1871771d
	# wrapper for the Tukey Honest Significant difference test
	nominal.aov <- aov(scale ~ nominal, nominal)
	tukey <- TukeyHSD(nominal.aov)
	tk = tukey$nominal
	# if there are only two levels, return the names 
	# of both of the levels
	if(length(levels(nominal)) == 2) 
		return(row.names(tk))
	# return the names of the levels that are significantly
	# different
	pairs <- tk[, 'p adj'] < 0.05
	return(names(pairs[pairs == T]))
}

isHomoscedastic <- function(scale, nominal) {
	# check if the variance of the groups is the same
	
	# due to a bug in lawstat::levene.test
	# it was necessary to treat the na values
	# before running the test
	# https://stackoverflow.com/a/26905025
	if(length(levels(nominal)) == 2) {
		frame = na.omit(data.frame(scale, nominal))
		scale = frame[,1]
		nominal = frame[,2]
	}
	
	results = levene.test(scale, nominal)
	return (results[2]$p.value > 0.05)
}

isNormal <- function(scale, nominal) {
	levels = levels(nominal)
	# loop through each of the groups to check if they are normal
	for(level in levels) {
		if(length(scale[nominal == level]) > 3) {
			result = shapiro.test(scale[nominal == level])
			p = result[2]$p.value
			if(p < 0.05) {
				# if there is a non-normal group, exit as we only need one
				# of the distributions to be non-normal for the assumption to fail
				return(F)
			}
		}
	}	
	# all the groups are normal
	return(T)
}

checkAssumptions <- function(scale, nominal) {
	# method to print the message if the assumptions fail
	normal = isNormal(scale, nominal)
	homoscedastic = isHomoscedastic(scale, nominal)
	if(!normal || !homoscedastic) {
		string = "The following assumptions for our tests have not been met: "
		if(!normal) string = paste(string, "- The distributions of the groups are not normal", sep = "\n")
		if(! homoscedastic) string = paste(string, "- The variances of the groups are different", sep = "\n")
		string = paste(string, "We will now try a slightly weaker test:", sep = "\n")
		# run the Kruskal-Wallis test, if the assumptions fail
		# if this passes, we can report our findings
		if(!kruskalTest(scale, nominal)) {
			# since the test did not pass, we reject the null hypothesis
			string = paste(string, "The means of the groups are not the same", sep = "\n")
			return(c(F, string))
		}
		# since the test passed, we accept the null hypothesis	
		string = paste(string, "The means of the groups are the same" ,sep = "\n")
		return(c(F, string))	
	}
	return(c(T, ""))
}

kruskalTest <- function(scale, nominal) {
	result = kruskal.test(scale~nominal)
	p = result[3]$p.value
	return(p > 0.05)
}
