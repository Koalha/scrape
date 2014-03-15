# Constructing a function to find URLs to BiolFlor's species pages based on a list of species provided by the user


# An experimental list of species

specs = list("Picea abies","Pinus sylvestris","Betula pubescens","Eristalis arbustorum","Salix myrsinifolia")

##
#BEGIN
##

genusweb = function(specieslist = specs){

options(strinsAsFactors = FALSE)
require(XML)

# split the species list from " ", select only the genus and display only unique gena.

genus = unique(sapply(sapply(specieslist,strsplit," "),'[',1))

# Load the genus list of BiolFlor

html = htmlTreeParse("http://www2.ufz.de/biolflor/overview/gattung.jsp",useInternalNodes=TRUE)

# create a loop or apply to go through all specified gena and return the link URLs
# if a genus is not listed, return NA

genusurls=data.frame(genus = rep(NA,length(genus)),url = rep(NA,length(genus)))

for(i in seq(along = genus)){
temppath = getNodeSet(html, paste0("//td/a[contains(.,'",genus[i],"') and not(.//*)]"))

if(length(temppath) == 0){temppath = NA} else
temppath = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de/biolflor/overview/",strsplit(toString.XMLNode(temppath[[1]]),'"')[[1]][2])))

genusurls$genus[i] = genus[i]
genusurls$url[i] = temppath
}

return(genusurls)
}

###
# Now the function has searched for all our species and returned a vector of URLS (or NA:s) for gena
# Next we will search all these for species on our species list
# (remember to write a function to check, if these species names can be found from the DB)
###

# remove: ULRS = genusweb(specs)

#! In the next function you have an absolute reference to species list 'specs'.
#! When you combine these functions, remember to change it to point at the species
#! list provided to genusweb() or what ever function it will be

speciesurls = function(genaindaweb=ULRS){

speciesandurls = NULL	# Create data frame for results based on our species list

for(i in seq(along=genaindaweb$url)){	# Loop through the page of each genus

html = htmlTreeParse(genaindaweb$url[i], useInternalNodes = TRUE)
specsofgenus = specs[sapply(sapply(specs, strsplit, " "),"[",1)==genaindaweb$genus[i]] # get all the species in our list that belong to the genus being processed

   for(i in seq(along=specsofgenus)){	# Go through each species
   specurl = getNodeSet(html, paste0("//a[contains(.,'",specsofgenus[i],"') and not(.//*)]"))
   if(length(specurl)==0){specurl = NA}
   else specurl = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de",strsplit(toString.XMLNode(specurl[[1]]),'"')[[1]][2])))
   speciesandurls = rbind(speciesandurls,c(specsofgenus[i],specurl))
   }


}
return(speciesandurls)
}

###
