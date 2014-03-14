# A function to query multiple species at once from the BiolFlora database
# Argument 'urllist' is a list of character strings containing the urls of species to be queried
# Remember to first load bfquery() from pagescrape.R

multibf = function(urllist = list()){
result = lapply(urllist, bfquery)
do.call(rbind,result)
}