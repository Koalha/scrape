# A function to extract grimes strategy for any plant in the BiolFlor database

speciesquery = function(url){

require(XML)

# Parse URL to a tree to use xPath queries

if(is.na(url)){
speciesname = NA
family = NA
grime = NA
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
} # endelse far away


# Bind results to a data frame

df = data.frame(speciesname = speciesname[[1]], family = family[[1]], grime = grime[[1]])

# Wait for 2 seconds to not DSA our DB

Sys.sleep(2)

return(df)
}
