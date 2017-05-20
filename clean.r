comment("
clean.r:
For the Robust-GOF project, load the data and clean the relevant data. Save to 'clean.csv'.

Data source
The data are downloaded from

	http://www.heritage.org/index/download

The download is the spreadsheet

	index2017_data.xls

To allow for easy import into R I manually export from the above to

	index2017_data.csv

I drop countries without complete data.
")

clean = function() {
	dfr = read.csv('index2017_data.csv')

	#Change some names to make them easier to work with.
	idx = grep('Country.Name', names(dfr))
	names(dfr)[idx] = 'name'
	idx = grep('X2017.Score', names(dfr))
	names(dfr)[idx] = 'score'
	idx = grep('World.Rank', names(dfr))
	names(dfr)[idx] = 'rank'
	idx = grep('GDP..Billions..PPP.', names(dfr))
	names(dfr)[idx] = 'gdp' #In billions of $.
	idx = grep('GDP.per.Capita..PPP.', names(dfr))
	names(dfr)[idx] = 'gdppc' #Abbreviates GDP per capita. In dollars.

	#Keep the columns shown below.
	dfr = dfr[c('name', 'score', 'rank', 'gdp', 'gdppc')]

	#Change data types of score and rank.
	#Changing data types generates warnings that we need to ignore.
	warn_old = getOption('warn')
	on.exit(options(warn = warn_old), add=TRUE)
	options(warn = 1)
	dfr$score = as.numeric(dfr$score)
	dfr$rank = as.integer(dfr$rank)

	#A few cells of gdp of gdppc include remarks enclosed in parentheses. Remove these remarks.
	dfr$gdp = gsub('[(].*[)]', '', dfr$gdp)
	dfr$gdppc = gsub('[(].*[)]', '', dfr$gdppc)

	#Remove dollar signs, commas, and spaces.
	dfr$gdp = gsub('[$, ]', '', dfr$gdp)
	dfr$gdppc = gsub('[$, ]', '', dfr$gdppc)

	#Convert gdp, gdppc to numeric and integer respectively.
	dfr$gdp = as.numeric(dfr$gdp)
	dfr$gdppc = as.integer(dfr$gdppc)

	#To enhance readability, order by country name.
	dfr = dfr[order(dfr$name), ]

	#Separate complete and incomplete cases.
	message('Total rows including incomplete cases: ', nrow(dfr))
	idx = which(!complete.cases(dfr))
	xxx = dfr[idx, ]
	dfr = na.omit(dfr)
	message('Total row excluding incomplete cases: ', nrow(dfr))
	#Reset row names.
	row.names(xxx) = NULL
	row.names(dfr) = NULL
	#Display xxx. Important to notice which countries are excluded.
	message('Incomplete cases:')
	print(xxx)

	#Save.
	write.csv(dfr, file='clean.csv', row.names=FALSE)
}
