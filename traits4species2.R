## First, loadf bfquery()

# This function takes a species list, searches BiolFlor for the species
# and returns the urls of the species pages for those species.
# A missing page is printed as NA

bfquery = function(specieslist){

options(stringsAsFactors = FALSE)
require(XML)

genusweb = function(specieslist){

# split the species list from " ", select only the genus and display only unique gena.

genus = unique(sapply(sapply(specieslist,strsplit," "),'[',1))

# Load the genus list of BiolFlor

html = htmlTreeParse("http://www2.ufz.de/biolflor/overview/gattung.jsp",useInternalNodes=TRUE)

# create a loop or apply to go through all specified gena and return the link URLs
# if a genus is not listed, return NA

genusurls=data.frame(genus = rep(NA,length(genus)),url = rep(NA,length(genus)))

for(i in seq(along = genus)){
temppath = getNodeSet(html, paste0("//td/a[contains(.,'",genus[i],"') and not(.//*)]"))

if(length(temppath) == 0){temppath = NA} else{
temppath = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de/biolflor/overview/",strsplit(toString.XMLNode(temppath[[1]]),'"')[[1]][2])))}

genusurls$genus[i] = genus[i]
genusurls$url[i] = temppath
}

return(genusurls)
} #endgenusweb

################################

speciesurls = function(genaindaweb=ULRS){

speciesandurls = NULL	# Create data frame for results based on our species list

for(i in seq(along=genaindaweb$url)){	# Loop through the page of each genus

if(is.na(genaindaweb$url[i])){	# if the genus has no listed url, url = NA for all species of the genus
specsofgenus = specieslist[sapply(sapply(specieslist, strsplit, " "),"[",1)==genaindaweb$genus[i]]

   for(i in seq(along=specsofgenus)){
   specurl = NA
   speciesandurls = rbind(speciesandurls,c(specsofgenus[i],specurl))
   }

}


else{
html = htmlTreeParse(genaindaweb$url[i], useInternalNodes = TRUE)
specsofgenus = specieslist[sapply(sapply(specieslist, strsplit, " "),"[",1)==genaindaweb$genus[i]] # get all the species in our list that belong to the genus being processed

   for(i in seq(along=specsofgenus)){	# Go through each species
   specurl = getNodeSet(html, paste0("//a[contains(.,'",specsofgenus[i],"') and not(.//*)]"))
   if(length(specurl)==0){specurl = NA}
   else specurl = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de",strsplit(toString.XMLNode(specurl[[1]]),'"')[[1]][2])))
   speciesandurls = rbind(speciesandurls,c(specsofgenus[i],specurl))
   }
}

}
speciesandurls = data.frame(species = unlist(speciesandurls[,1]),url = unlist(speciesandurls[,2])) # SET stringsAsFactors AS FALSE!!!!
return(speciesandurls)
}	# end speciesurls()


ULRS = genusweb(specieslist)
out = speciesurls()
return(out)

}	# end bfquery()

#######
## Then load function to get traits from urls
##
## REMEBER: THIS SHOULD BE THE ONLY FUNCTION YOU NEED TO MANIPULATE
## TO ADD TRAITS YOU WANT TO SCRAPE
##
###

# A function to extract grimes strategy for any plant in the BiolFlor database

speciesquery = function(url){

options(stringsAsFactors = FALSE)
require(XML)

# Parse URL to a tree to use xPath queries

if(is.na(url)){
speciesname = NA
family = NA
grime = NA
germinuletype = NA
germinuleweight = NA
}

else{
html = htmlTreeParse(url,useInternalNodes = TRUE)

# Find the species name that this page is about

speciesname = getNodeSet(html, "//td[contains(.,'species information') and not(.//*)]/../../tr[3]/td[1]")
speciesname = xmlValue(speciesname[[1]])

# Find the family name for our species

family = getNodeSet(html, "//td[contains(.,'species information') and not(.//*)]/../../tr[3]/td[2]")
family = xmlValue(family[[1]])
family = gsub(" »", "", family)

# Find Grimes strategy for our plant

grime = getNodeSet(html, "//td[contains(.,'Strategy type') and not(.//*)]/../td[2]/a")
if(length(grime) == 0){grime = NA} else {grime = xmlValue(grime[[1]])
grime = strsplit(grime, " ")[[1]][1]
} # endelse near

# Find germinule info

germinuletype = getNodeSet(html, "//td[contains(.,'Measured object') and not(./*)]/parent::tr[contains(.,'Germinule')]/following-sibling::tr[1]/td[2]/a")
if(length(germinuletype) == 0){germinuletype = NA} else {germinuletype = xmlValue(germinuletype[[1]])}

germinuleweight = getNodeSet(html, "//td/a[contains(.,'Weights') and not(./*)]/../../following-sibling::tr[contains(.,'Measured object') and contains(.,'Germinule')]/following-sibling::tr[contains(.,'Weight (mean)')]/td[2]")
if(length(germinuleweight) == 0){germinuleweight = NA} else {germinuleweight = xmlValue(germinuleweight[[1]])}

} # endelse far away

# Bind results to a data frame

df = data.frame(speciesname = speciesname[[1]], family = family[[1]], grime = grime[[1]], germinuletype = germinuletype[[1]], meangerminuleweight_mg = germinuleweight[[1]])

# Wait for 2 seconds to not DSA our DB

Sys.sleep(2)

return(df)
}

###
### Then load function to speciesquery the result of bfquery()
###


multibf = function(speciesandurls){ # remember, stringsAsFactors must be FALSE!!!

df = NULL	# create a data frame for our results

   for(i in seq(along = speciesandurls$url)){
   df = rbind(df,cbind(species = speciesandurls$species[i],speciesquery(speciesandurls$url[i])))
   }

return(df)
}

###
###	Then form a new function to use on any list of species
###

traits4species = function(alistofspecies){

one = bfquery(alistofspecies)
two = multibf(one)

return(two)
}
