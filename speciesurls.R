# Constructing a function to find URLs to BiolFlor's species pages based on a list of species provided by the user


# An experimental list of species

specs = list("Picea abies","Pinus sylvestris","Betula pubescens","Eristalis arbustorum","Salix myrsinifolia")

##
#BEGIN
##

genusweb = function(specieslist){

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

speciesurls = function(genaindaweb=ULRS){

for(i in seq(along=genaindaweb$url)){
html = htmlTreeParse(genaindaweb$url[i], useInternalNodes = TRUE)
specsofgenus = specs[sapply(sapply(specs, strsplit, " "),"[",1)==genaindaweb$genus[i]]
}

}

###
