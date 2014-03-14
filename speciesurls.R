# Constructing a function to find URLs to BiolFlor's species pages based on a list of species provided by the user


# An experimental list of species

specs = list("Picea abies","Pinus sylvestris","Betula pubescens","Betula pendula")

##
#BEGIN
##


# split the species list from " ", select only the genus and display only unique gena.

genus = unique(sapply(sapply(specs,strsplit," "),'[',1))

# Load the genus list of BiolFlor

html = htmlTreeParse("http://www2.ufz.de/biolflor/overview/gattung.jsp",useInternalNodes=TRUE)

# create a loop or apply to go through all specified gena and return the link URLs
# that means = replace genus[[1]] with genus[[i]]

temppath = getNodeSet(html, paste0("//td/a[contains(.,'",genus[1],"') and not(.//*)]"))
temppath = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de/biolflor/overview/",strsplit(toString.XMLNode(temppath[[1]]),'"')[[1]][2])))
