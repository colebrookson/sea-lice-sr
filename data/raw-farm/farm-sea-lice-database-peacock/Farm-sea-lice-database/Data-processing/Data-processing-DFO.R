setwd("~/Google Drive/Lice database")
################################################################################################
# This code takes the raw DFO data, as downloaded from the Sea Lice Reports A:
# http://www.pac.dfo-mpo.gc.ca/aquaculture/reporting-rapports/lice-pou-eng.html
# The only processing steps done to the data before this code is run are:
# 1) HTML files (2011 to 2014Q1) are copied and pasted into Excel
# 2) PDF files (2014Q2-2015Q3) are converted to Excel using Adobe Acrobat Pro
# 3) For PDF files, the header on each page is deleted, and the merged cells for Month 
#    and Fish Health Zone (if Adobe merges them) are unmerged.
#
# Email questions to stephanie.j.peacock at gmail.com
################################################################################################

setwd("Farm-sea-lice-database") #Set working directory

library(xlsx) #package that allows Excel files to be read, to avoid further processing data to .csv

rm(list=ls())

#------------------------------------------------------------------------------------------------
# Reading in the raw data from each quarter and compiling into a single data.frame
#------------------------------------------------------------------------------------------------

# Loop through each quarter ad extract data:

for(i in 2011:2015){ #For each year
	for(j in 1:4){# For each quarter
		
		if(i<2014|(i==2014&j==1)){# If the data are available as an HTML file, the sheet is called Sheet1
			
			dat<-read.xlsx(paste("Raw-data/DFO/DFO_", i, "_Q", j, ".xlsx", sep=""), sheetName="Sheet1", colIndex=c(1:16))
			
		}else{ #If the data are available as a pdf file, the sheet is called Table 1
			
			dat<-read.xlsx(paste("Raw-data/DFO/DFO_", i, "_Q", j, ".xlsx", sep=""), sheetName="Table 1", colIndex=c(1:16))
					
		}
		
		# No audit data for 2011-Q1, need to add columns
		
		if(i==2011&j==1){
			dat<-cbind(dat[,1:7], rep(NA, dim(dat)[1]), dat[,8], rep(NA, dim(dat)[1]), dat[,9], rep(NA, dim(dat)[1]), dat[,10], rep(NA, dim(dat)[1]), dat[,11:12])
			}
		
		
		# Cut the extra info in the header
		
		dat1<-dat[which(dat[,5]==c("January", "April", "July", "October")[j]):dim(dat)[1],]
		
		
		# Add legible column names
		
		names(dat1)<-c("Facility_reference_number", "License_holder", "Site_common_name", "Fish_health_zone", "Month", "Number_of_counts_performed", "L.salmonis_motile", "L.salmonis_motile_audit", "L.salmonis_female", "L.salmonis_female_aduit", "Chalimus", "Chalimus_audit", "Caligus", "Caligus_audit", "Comments", "Year_class")
		
		
		# Cut <NA> rows at the bottom
		
		if(sum(is.element(unique(dat1$License_holder), c(NA, "Cermaq", "Mainstream Canada", "Grieg Seafood", "Grieg", "Marine Harvest")))<length(unique(dat1$License_holder))) stop("License holder not matching") else maxLines<-max(which(is.element(dat1$License_holder, c("Mainstream Canada", "Grieg Seafood", "Marine Harvest", "Cermaq"))))
		dat1<-dat1[1:maxLines,]	
			
		
		# Infill months
		
		monthInd<-which(is.na(dat1$Month)==FALSE)
		
		if(length(monthInd)>3) stop("Problem with monthInd: more than three months") else{
			dat1$Month[1:(monthInd[2]-1)]<-c("January", "April", "July", "October")[j]
			dat1$Month[monthInd[2]:(monthInd[3]-1)]<-c("February", "May", "August", "November")[j]
			dat1$Month[monthInd[3]:maxLines]<-c("March", "June", "September", "December")[j]
			}
		
		dat1<-dat1[-monthInd,] # Remove the month lines that are blank
			
		
		# Infill zones
		
		zoneInd<-which(is.na(dat1$Fish_health_zone)==FALSE)
		
		for(m in 1:length(zoneInd)){
			if(m==length(zoneInd)) dat1$Fish_health_zone[zoneInd[m]:(dim(dat1)[1])]<-dat1$Fish_health_zone[zoneInd[m]] else dat1$Fish_health_zone[zoneInd[m]:(zoneInd[m+1]-1)]<-dat1$Fish_health_zone[zoneInd[m]]
			}
		
		dat1<-dat1[-zoneInd,] #Remove the lines with just the zones
		
		# There was a problem with the HTML files that some comments ended up in multiple cells
		# Find these comments and make them one cell
		
		dat1$Comments<-as.character(dat1$Comments)
		
		#Find gaps indicating comments spanning multipe rows
		indNA<-which(is.na(dat1$Site_common_name)==TRUE)
		# Select those indices that are the FIRST if there is a series of NAs
		indNA2<-indNA[c(1,which(diff(indNA)>1)+1)]
		# If the comment is NA, then it's just a dummy row and can be removed
		if(sum(is.na(indNA2))>0) indNA2<-indNA2[is.na(indNA2)==FALSE]
		
		if(length(indNA2)>0){
			for(m in 1:length(indNA2)){
							
				#---------
				#If it's just one NA
				if(is.na(dat1$Site_common_name[indNA2[m]+1])==FALSE){ 
					#If it's a number, needs to be converted back to date format
					if(is.na(as.numeric(dat1$Comments[indNA2[m]-1]))==FALSE){ 
						
						dat1$Comments[indNA2[m]-1]<-paste(as.character(as.Date(as.numeric(dat1$Comments[indNA2[m]-1]), origin="1899-12-30")), dat1$Comments[indNA2[m]], sep=" ")
						
					}else{# if it's not a date, just compile them
						
						dat1$Comments[indNA2[m]-1]<-paste(dat1$Comments[indNA2[m]-1], dat1$Comments[indNA2[m]], sep=" ")
						
					}
					
					dat1$Comments[indNA2[m]]<-NA
				
				#-------
				# if there are four NAs
				}else if(sum(is.na(dat1$Site_common_name[indNA2[m]+c(1,2,3)]))==3){ 
					#If it's a number, needs to be converted back to date format
					if(is.na(as.numeric(dat1$Comments[indNA2[m]-1]))==FALSE){ 
						
						dat1$Comments[indNA2[m]-1]<-paste(as.character(as.Date(as.numeric(dat1$Comments[indNA2[m]-1]), origin="1899-12-30")), dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], dat1$Comments[indNA2[m]+2], dat1$Comments[indNA2[m]+3], sep=" ")
						
					}else{# if it's not a date, just compile them
						
						dat1$Comments[indNA2[m]-1]<-paste(dat1$Comments[indNA2[m]-1], dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], dat1$Comments[indNA2[m]+2], dat1$Comments[indNA2[m]+3], sep=" ")
						
					}
					
					dat1$Comments[indNA2[m]+c(0,1,2,3)]<-NA
				
				#-------
				# if there are 3 NAs
				} else if(sum(is.na(dat1$Site_common_name[indNA2[m]+c(1,2)]))==2){ 
					#If it's a number, needs to be converted back to date format
					if(is.na(as.numeric(dat1$Comments[indNA2[m]-1]))==FALSE){ 
						
						dat1$Comments[indNA2[m]-1]<-paste(as.character(as.Date(as.numeric(dat1$Comments[indNA2[m]-1]), origin="1899-12-30")), dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], dat1$Comments[indNA2[m]+2], sep=" ")
						
					}else{# if it's not a date, just compile them
						
						dat1$Comments[indNA2[m]-1]<-paste(dat1$Comments[indNA2[m]-1], dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], dat1$Comments[indNA2[m]+2], sep=" ")
						
					}
					
					dat1$Comments[indNA2[m]+c(0,1,2)]<-NA
				
				#-------
				# if there are 2 NAs
				} else if(is.na(dat1$Site_common_name[indNA2[m]+1])==TRUE){ # If there are 2 NAs
					#If it's a number, needs to be converted back to date format
					if(is.na(as.numeric(dat1$Comments[indNA2[m]-1]))==FALSE){ 
						
						dat1$Comments[indNA2[m]-1]<-paste(as.character(as.Date(as.numeric(dat1$Comments[indNA2[m]-1]), origin="1899-12-30")), dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], sep=" ")
						
					}else{# if it's not a date, just compile them
						
						dat1$Comments[indNA2[m]-1]<-paste(dat1$Comments[indNA2[m]-1], dat1$Comments[indNA2[m]], dat1$Comments[indNA2[m]+1], sep=" ")
						
					}
					
					dat1$Comments[indNA2[m]+c(0,1)]<-NA
					}
					
				}# end m
			}#end if length
		
		# Remove NA lines within data
		
		naInd<-which(is.na(dat1$Site_common_name))
		if(length(naInd)>0) dat1<-dat1[-naInd,]
		
		
		# Compile dataframe
		if(i==2011&j==1){ #If it's the first file in the dataset, start dataframe called allData,
			allData<-dat1
		}else{ # otherwise, add it to allData
			allData<-rbind(allData, dat1)
		}
		rm(dat, dat1)
	} # end j
} # end i

# write.csv(allData, "Data-processing/DFO_data_compiled.csv")


#------------------------------------------------------------------------------------------------
# Checking data
#------------------------------------------------------------------------------------------------

		