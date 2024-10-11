# Package ID: knb-lter-jrn.200047001.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Map of ecological sites and ecological states for the USDA Jornada Experimental Range.
# Data set creator:  Laura M Burkett - USDA-ARS Jornada Experimental Range 
# Data set creator:  Brandon T Bestelmeyer - USDA-ARS Jornada Experimental Range 
# Metadata Provider:    - Jornada Basin LTER 
# Contact:    - Information Manager Jornada Basin LTER  - jornada.data@nmsu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 


# STEPS WITHIN THIS R SCRIPT
# 1. READING IN THE DATA
# 2. CLEANING THE DATA
# 3. EXAMINING THE DATA
# 4. ANALYZING THE DATA


# 1. READING IN THE DATA
# The dataset is located at the URL in inUrl1.
# A temporary file (infile1) is created to store the downloaded file.
# download.file() attempts to download the file using the curl method.
# If the download fails (indicated by the temporary file having no size), it retries using the auto method.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/200047001/3/0f0e1d2bf059f4e18faa943540a4007b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


# 2. CLEANING THE DATA
# read.csv() reads the downloaded CSV file into dt1. The file has no headers (header=F), so the specified column names are used.
# It skips the first row (skip=1), uses commas as separators (sep=","), and handles quotes (quot='"').
# The temporary file is deleted after reading the data with unlink().
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "OBJECTID",     
                 "soil_mu",     
                 "SSURGO_dat",     
                 "MLRA",     
                 "LRU",     
                 "esite",     
                 "esite_alt",     
                 "esite_alt2",     
                 "state_code",     
                 "dom1",     
                 "dom2",     
                 "dom3",     
                 "dom4",     
                 "grass_dur",     
                 "ration",     
                 "Shape_Length",     
                 "Shape_Area"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$OBJECTID)=="factor") dt1$OBJECTID <-as.numeric(levels(dt1$OBJECTID))[as.integer(dt1$OBJECTID) ]               
if (class(dt1$OBJECTID)=="character") dt1$OBJECTID <-as.numeric(dt1$OBJECTID)
if (class(dt1$soil_mu)!="factor") dt1$soil_mu<- as.factor(dt1$soil_mu)
if (class(dt1$SSURGO_dat)!="factor") dt1$SSURGO_dat<- as.factor(dt1$SSURGO_dat)
if (class(dt1$MLRA)!="factor") dt1$MLRA<- as.factor(dt1$MLRA)
if (class(dt1$LRU)!="factor") dt1$LRU<- as.factor(dt1$LRU)
if (class(dt1$esite)!="factor") dt1$esite<- as.factor(dt1$esite)
if (class(dt1$esite_alt)!="factor") dt1$esite_alt<- as.factor(dt1$esite_alt)
if (class(dt1$esite_alt2)!="factor") dt1$esite_alt2<- as.factor(dt1$esite_alt2)
if (class(dt1$state_code)!="factor") dt1$state_code<- as.factor(dt1$state_code)
if (class(dt1$dom1)!="factor") dt1$dom1<- as.factor(dt1$dom1)
if (class(dt1$dom2)!="factor") dt1$dom2<- as.factor(dt1$dom2)
if (class(dt1$dom3)!="factor") dt1$dom3<- as.factor(dt1$dom3)
if (class(dt1$dom4)!="factor") dt1$dom4<- as.factor(dt1$dom4)
if (class(dt1$grass_dur)!="factor") dt1$grass_dur<- as.factor(dt1$grass_dur)
if (class(dt1$ration)!="factor") dt1$ration<- as.factor(dt1$ration)
if (class(dt1$Shape_Length)=="factor") dt1$Shape_Length <-as.numeric(levels(dt1$Shape_Length))[as.integer(dt1$Shape_Length) ]               
if (class(dt1$Shape_Length)=="character") dt1$Shape_Length <-as.numeric(dt1$Shape_Length)
if (class(dt1$Shape_Area)=="factor") dt1$Shape_Area <-as.numeric(levels(dt1$Shape_Area))[as.integer(dt1$Shape_Area) ]               
if (class(dt1$Shape_Area)=="character") dt1$Shape_Area <-as.numeric(dt1$Shape_Area)

# Convert Missing Values to NA for non-dates


# 3. EXAMINING THE DATA
# str(dt1) displays the structure of the data (data types and a preview of each column).
# attach(dt1) makes the columns directly accessible by name (without the need to reference dt1$colname).
# summary() provides basic statistical summaries for each column (e.g., min, max, mean for numeric data, and frequencies for categorical data).
# detach(dt1) undoes the attach() to prevent variable name conflicts later.
# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            


# 4. ANALYZING THE DATA
# These lines check how many unique categories each categorical column contains and their distribution.
summary(OBJECTID)
summary(soil_mu)
summary(SSURGO_dat)
summary(MLRA)
summary(LRU)
summary(esite)
summary(esite_alt)
summary(esite_alt2)
summary(state_code)
summary(dom1)
summary(dom2)
summary(dom3)
summary(dom4)
summary(grass_dur)
summary(ration)
summary(Shape_Length)
summary(Shape_Area) 
# Get more details on character variables

summary(as.factor(dt1$soil_mu)) 
summary(as.factor(dt1$SSURGO_dat)) 
summary(as.factor(dt1$MLRA)) 
summary(as.factor(dt1$LRU)) 
summary(as.factor(dt1$esite)) 
summary(as.factor(dt1$esite_alt)) 
summary(as.factor(dt1$esite_alt2)) 
summary(as.factor(dt1$state_code)) 
summary(as.factor(dt1$dom1)) 
summary(as.factor(dt1$dom2)) 
summary(as.factor(dt1$dom3)) 
summary(as.factor(dt1$dom4)) 
summary(as.factor(dt1$grass_dur)) 
summary(as.factor(dt1$ration))
detach(dt1)               
