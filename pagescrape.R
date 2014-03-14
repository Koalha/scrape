# A function to extract grimes strategy for any plant in the BiolFlor database

bfquery = function(url="http://www2.ufz.de/biolflor/taxonomie/taxonomie.jsp?action=filter&ID_Familie=-1&ID_Gattung=257&ID_Taxonomie=982"){

require(XML)

# Parse URL to a tree to use xPath queries

html = htmlTreeParse(url,useInternalNodes = TRUE)

# Find the species name that this page is about

speciesname = getNodeSet(html, "//td[contains(.,'species information') and not(.//*)]/../../tr[3]/td[1]");speciesname
speciesname = xmlValue(speciesname[[1]])

# Find the family name for our species

family = getNodeSet(html, "//td[contains(.,'species information') and not(.//*)]/../../tr[3]/td[2]");family
family = xmlValue(family[[1]]);family

# Find Grimes strategy for our plant

grime = getNodeSet(html, "//td[contains(.,'Strategy type') and not(.//*)]/../td[2]/a");grime
if(length(grime) == 0){grime = NA} else {grime = xmlValue(grime[[1]])}

# Bind results to a data frame

df = cbind(speciesname,family,grime)

# Wait for 2 seconds to not DSA our DB

Sys.sleep(2)

return(df)
}
