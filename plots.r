comment("
plots.r:
Generate plots. Save to disk.
")

#In case quantreg was compiled under a later version of R, temporarily prevent conversion of warnings to errors.
warn_current = getOption('warn')
options(warn = 1)
library(quantreg)
options(warn = warn_current)

#Generate a single bootstrap sample of dfr.
boot1 = function(dfr) {
	idx = sample(1:nrow(dfr), replace=TRUE) #Sample with replacement.
	dfr[idx, ]
}
#Bootstrap the slope for the indicated x axis.
brep = function(dfr, xax, reps=100) {
	formula = paste('log(gdppc) ~', xax)
	collect = numeric(reps)
	#Prevent warnings about non-unique solutions.
	warn_old = getOption('warn')
	on.exit(options(warn = warn_old))
	options(warn = 0)
	for (ii in 1:reps) {
		b1 = boot1(dfr)
		options()
		fit = rq(formula=formula, data=b1)
		collect[ii] = summary(fit)$coefficients[xax, 'coefficients']
	}
	sort(collect)
}

plots = function() {
	#Load the data.
	dfr = read.csv('clean.csv')

	#Generate plots, and goodness of fit, for the indicated x axis.
	plots_xax = function(xax) {
		#xax: x axis 'score' or 'rank'.

		#Width and height of graphs in pixels.
		width = 600; height=480
		cex = 1.5
		lty = 2
		lwd = 4
		pch = 19
		xlab = paste('Economic freedom', xax)

		#xax x gdppc
		filename = paste(xax, 'x gdppc.png')
		png(filename=filename, width=width, height=height)
		plot(dfr[[xax]], dfr$gdppc, xlab=xlab, ylab='GDP per capita', type='n', bg='red')
		points(dfr[[xax]], dfr$gdppc, pch=pch, cex=cex, col='black')
		dev.off()

		#xax x log(gdppc)
		filename = paste(xax, 'x log(gdppc).png')
		png(filename=filename, width=width, height=height)
		plot(dfr[[xax]], log(dfr$gdppc), xlab=xlab, ylab='log(GDP per capita)', type='n', bg='red')
		points(dfr[[xax]], log(dfr$gdppc), pch=pch, cex=cex, col=rgb(0, 0.1, 0, 0.9))
		dev.off()

		#Compute median regression.
		formula = paste('log(gdppc) ~', xax)
		fit = rq(formula=formula, data=dfr)

		#Add median regression to plot.
		filename = paste(xax, 'median_regression.png')
		png(filename=filename, width=width, height=height)
		plot(dfr[[xax]], log(dfr$gdppc), xlab=xlab, ylab='log(GDP per capita)', type='n', bg='red')
		points(dfr[[xax]], log(dfr$gdppc), pch=pch, cex=cex, col=rgb(0, 0.1, 0, 0.9))
		abline(fit, lty=lty, lwd=lwd, col='blue')
		dev.off()	

		#Compute loess.
		lfit = loess(formula=formula, data=dfr)

		#Plot median regression + loess.
		filename = paste(xax, 'loess.png')
		png(filename=filename, width=width, height=height)
		plot(dfr[[xax]], log(dfr$gdppc), xlab=xlab, ylab='log(GDP per capita)', type='n', bg='red')
		points(dfr[[xax]], log(dfr$gdppc), pch=pch, cex=cex, col=rgb(0, 0.1, 0, 0.9))
		abline(fit, lty=lty, lwd=lwd, col='blue')
		#The x axis must be sorted for the curve to display properly.
		lines(sort(lfit$x), predict(lfit)[order(lfit$x)], col=rgb(0.66, .05, 0), lty=lty, lwd=lwd)
		dev.off()

		#Median regression goodness of fit (GOF).
		fit0 = rq(log(gdppc) ~ 1, data=dfr) #This yields the median as the best fit to the data.
		gof = 1 - sum(abs(fit$resid))/sum(abs(fit0$resid))
		message('\nx axis: ', xax)
		message('Goodness of fit')
		message('Median Regression: ', round(gof, 2))
		#Loess GOF.
		lgof = 1 - sum(abs(lfit$resid))/sum(abs(fit0$resid))
		message('Loess: ', round(lgof, 2))

		#Bootstrap.
		reps = 1000
		bt = brep(dfr, xax, reps)
		message('Summary of ', reps, ' bootstrap replications:')
		print(round(summary(bt), 3))
	}
	plots_xax('score')
	plots_xax('rank')
}
