# This function takes a species list, searches BiolFlor for the species
# and returns the urls of the species pages for those species.
# A missing page is printed as NA

bfquery = function(specieslist){

options(strinsAsFactors = FALSE)
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

if(length(temppath) == 0){temppath = NA} else
temppath = gsub("amp;","",gsub("/./","/",paste0("http://www2.ufz.de/biolflor/overview/",strsplit(toString.XMLNode(temppath[[1]]),'"')[[1]][2])))

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

################################

ULRS = genusweb(specieslist)
out = speciesurls()
return(out)

}	# end bfquery()