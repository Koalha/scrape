
speciesurls = function(genaindaweb=ULRS){

speciesandurls = NULL	# Create data frame for results based on our species list

for(i in seq(along=genaindaweb$url)){	# Loop through the page of each genus

if(is.na(genaindaweb$url[i])){	# if the genus has no listed url, url = NA for all species of the genus
specsofgenus = specs[sapply(sapply(specs, strsplit, " "),"[",1)==genaindaweb$genus[i]]

   for(i in seq(along=specsofgenus)){
   specurl = NA
   speciesandurls = rbind(speciesandurls,c(specsofgenus[i],specurl))
   }

}


else{
html = htmlTreeParse(genaindaweb$url[i], useInternalNodes = TRUE)
specsofgenus = specs[sapply(sapply(specs, strsplit, " "),"[",1)==genaindaweb$genus[i]] # get all the species in our list that belong to the genus being processed

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
}
