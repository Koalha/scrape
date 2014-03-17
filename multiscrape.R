# A function to query multiple species at once from the BiolFlora database
# Argument 'urllist' is a list of character strings containing the urls of species to be queried
# Remember to first load speciesquery() from pagescrape.R

multibf = function(speciesandurls){ # remember, stringsAsFactors must be FALSE!!!

df = NULL	# create a data frame for our results

   for(i in seq(along = speciesandurls$url)){
   row = cbind(species = speciesandurls$species[i],speciesquery(speciesandurls$url[i]))
   df = rbind(df,row)
   }

return(df)
}