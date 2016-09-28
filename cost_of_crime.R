################
# housekeeping #
################

# load required packages
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)

# function for calculating Purchasing Power of dollars in different years
# based on Purchasing Power section from http://www.bls.gov/cpi/cpimathfs.pdf
cpi.adjust <- function(amount, amount.year, desired_year) {
        
        u <- "https://research.stlouisfed.org/fred2/data/CPIAUCSL.csv"
        
        if(!file.exists(basename(u))) {
        
                download.file(u, file.path("./inputs/", basename(u)))
        }
        
        monthly_cpi <- read.csv("./inputs/CPIAUCSL.csv", header = TRUE) 
        
        monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
        
        # averages annual CPI from the monthly CPIs 
        yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))
        
        # which year's dollars do you want? this is the amount.base_year
        yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == desired_year]
        
        # find index for the year you have your dollars in
        year.index <- yearly_cpi$cpi_year[length(yearly_cpi$adj_factor)] - amount.year
        
        # Find the adjustment factor for the year you have your dollars in
        adjustment.factor <- yearly_cpi$adj_factor[length(yearly_cpi$adj_factor)-year.index]
        
        cpi_adjustment <- amount/adjustment.factor
        
        return(cpi_adjustment)
}

# inputs 

# directory for crime csv [ADD A TEMPORAL ELEMENT]
csv_location = path.expand("./inputs/police_inct.csv")

# directories for input shapefiles
neighborhoods_shp_directory = path.expand("./inputs/Neighborhoods_Philadelphia")
#blocks_shp_directory = path.expand("./inputs/Census_Blocks_2010")
block_groups_shp_directory = path.expand("./inputs/acs-block-groups-2013/")

# names of input shapefile layers
neighborhoods_shp_name = "Neighborhoods_Philadelphia"
#blocks_shp_name = "Census_Blocks_2010"
block_groups_shp_name = "acs-block-groups-2013"

# outputs

# directory for output shapefiles
output_directory = path.expand("./outputs") 

# names of output shapefile layers
neighborhoods_output_layer = "neighborhood"
#blocks_output_layer = "blocks"
block_groups_output_layer = "block_groups"

# read in crime data
crimes_raw <- read.table(file = csv_location,
                         sep=",",
                         header = T,
                         comment="",
                         #fill = T,
                         quote = '"',
                         stringsAsFactors = F,
                         strip.white=T,
                         na.strings = "")

# read in shapefile of neighborhoods
neighborhoods_shp <- readOGR(neighborhoods_shp_directory, neighborhoods_shp_name)

# read in shapefile of census blocks
#blocks_shp <- readOGR(blocks_shp_directory, blocks_shp_name)

# read in shapefile of census block groups
# MAKE SURE TO DROP OFF ALL AREAS THAT DO NOT FALL WITHIN THE COUNTY
# THEY WILL REFERENCE GEOID_STRP
block_groups_shp <- readOGR(block_groups_shp_directory, block_groups_shp_name)
# filter out all areas that are not in Philadelphia County (only numbers starting with "42101" are in the county)
block_groups_shp <- block_groups_shp[grep("^42101.",block_groups_shp$GEOID_STRP),]

# sets minimum population for each block group to the bottom 1% of population

# total pop + margin
block_groups_shp$ADJPOP <- as.numeric(block_groups_shp$TOTALPOP + as.numeric(as.character(block_groups_shp$mTOTALPOP)))
#block_groups_shp$ADJPOP <- pmax(block_groups_shp$TOTALPOP, quantile(block_groups_shp$TOTALPOP[block_groups_shp$TOTALPOP>0], .01))

block_groups_shp$ADJPOP <- ifelse(block_groups_shp$TOTALPOP > quantile(block_groups_shp$TOTALPOP[block_groups_shp$TOTALPOP>0], .01),
                                  block_groups_shp$TOTALPOP + as.numeric(as.character(block_groups_shp$mTOTALPOP)),
                                  NA_integer_)

# save crime data as a data frame table
crimes <- tbl_df(crimes_raw)
crimes <- filter(crimes, DISPATCH_DATE < "2016-01-01")

# reformat crimes to contain datetime, id, xcoord, ycoord, classification
crimes$id <- crimes$OBJECTID
crimes$report_time <- crimes$DISPATCH_DATE_TIME
crimes$class <- crimes$TEXT_GENERAL_CODE
crimes$pointx <- crimes$POINT_X
crimes$pointy <- crimes$POINT_Y
crimes$datasource <- "PhillyOpen"
# reformat to only include the columns we want
crimes <- crimes[,(ncol(crimes)-5):ncol(crimes)]

######################
# prepare crime data #
######################

# combine classes 
# using http://www.rand.org/jie/centers/quality-policing/cost-of-crime.html

# murder
crimes$class <- gsub(".*Homicide.*", "Murder", crimes$class)

# rape is already classified properly

# robbery
crimes$class <- gsub(".*Robbery.*", "Robbery", crimes$class)

# aggravated_assault
crimes$class <- gsub(".*Aggravated Assault.*", "Aggravated Assault", crimes$class)

# residential burglary
crimes$class <- gsub("^Burglary Residential$", "Residential Burglary", crimes$class)

# non-residential burglary
crimes$class <- gsub("^Burglary Non-Residential$", "Commercial Burglary", crimes$class)

# larceny 
crimes$class <- gsub(".*Thefts.*", "Larceny", crimes$class)

# theft from vehicle
# this class doesn't exist on the RAND study, 
# but Philly has a high enough volume that modeling it separately makes sense
# it is already classified appropriately

# motor_vehicle_theft is already classified appropriately

# select only the crime classes we want now
crime_type <- c("Murder", "Rape", "Aggravated Assault", "Robbery", "Residential Burglary", 
                "Motor Vehicle Theft", "Commercial Burglary", "Larceny", "Theft from Vehicle")

crimes <- crimes %>% filter(class %in% crime_type)
# remove rows with no coordinates

# show how many are NA by crime type
# crimes.na <- crimes %>% filter(is.na(pointx))

crimes <- crimes %>% filter(!is.na(pointx))

################
# set weights #
###############

# create a vector with cost per crime
# using http://www.rand.org/jie/centers/quality-policing/cost-of-crime.html
# Theft from Vehicle is set to same value as "Larceny"
# Non-res burglary is set using methodology below

# commercial burglary
# http://www.popcenter.org/library/crisp/commercial-burglary.pdf
# 5209 in 2008 dollars - uses "Accounting-Based Methods" from: 
# http://www.rand.org/content/dam/rand/pubs/occasional_papers/2010/RAND_OP279.pdf (pages 2-3)
# "...property-crime costs derive primarily from actual property losses" (RAND, p. 3)

# adjust from 2008 dollars to 2007 dollars
burglary.commercial <- cpi.adjust(5209, 2008, 2007)

cost.2007 <- c(8649216,217866,87238,67277,13096,9079,
               burglary.commercial,2139,2139)

# create a table with crime types and cost per crime
cost_of_crime <- data.frame(crime_type, cost.2007)
cost_of_crime <- tbl_df(cost_of_crime)
# order/sort by severity
cost_of_crime <- cost_of_crime[with(cost_of_crime, order(-cost.2007)), ]

# convert cost_of_crime to 2015 dollars
cost_of_crime$cost.2015 <- cpi.adjust(cost_of_crime$cost.2007, 2007, 2015)

#
# MORE DATA PREP
#

# convert 'crimes' data to a spatial points data frame
crimes <- as.data.frame(crimes)
coordinates(crimes)<-~pointx+pointy

# set projection for crime data - make sure this is correct! Assumes crime data is in WGS 84
proj4string(crimes) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# reproject to WGS 84

# 'crimes' is already in WGS 84. Uncomment below if this is not true
# crimes <- spTransform(crimes, CRSobj = CRS(proj4string(neighborhoods_shp)))
neighborhoods_shp <- spTransform(neighborhoods_shp, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
#blocks_shp <- spTransform(blocks_shp, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
block_groups_shp <- spTransform(block_groups_shp, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# separate out the different crime types
crimes.murder <- subset(crimes, class == "Murder")
crimes.rape <- subset(crimes, class == "Rape")
crimes.aggravated_assault <- subset(crimes, class == "Aggravated Assault")
crimes.robbery <- subset(crimes, class == "Robbery")
crimes.residential_burglary <- subset(crimes, class == "Residential Burglary")
crimes.motor_vehicle_theft <- subset(crimes, class == "Motor Vehicle Theft")
crimes.commercial_burglary <- subset(crimes, class == "Commercial Burglary")
crimes.larceny <- subset(crimes, class == "Larceny")
crimes.theft_from_vehicle <- subset(crimes, class == "Theft from Vehicle")

# sets field names for Total Crime Cost Calculation
crime_cost_field_names <- c("murd_cost",
                            "rape_cost",
                            "agg_cost",
                            "rob_cost",
                            "rbur_cost",
                            "mvt_cost",
                            "cbur_cost",
                            "larc_cost",
                            "tfv_cost")

# sets field names for Percentage of Cost of Crime
perc_field_names <- c("murd_perc",
                      "rape_perc",
                      "agg_perc",
                      "rob_perc",
                      "rbur_perc",
                      "mvt_perc",
                      "cbur_perc",
                      "larc_perc",
                      "tfv_perc")

# sets crime count field names
crime_count_field_names <- c("murd_num", 
                             "rape_num",
                             "agg_num",
                             "rob_num", 
                             "rbur_num", 
                             "mvt_num",
                             "cbur_num",
                             "larc_num", 
                             "tfv_num")

# crime type labels
crime_labels <- c("Murder", 
                  "Rape", 
                  "Aggravated Assault", 
                  "Robbery", 
                  "Residential Burglary", 
                  "Motor Vehicle Theft",
                  "Commercial Burglary",
                  "Larceny", 
                  "Theft from Vehicle", 
                  "No Events")

#
#
#
# NEIGHBORHOODS
#
#
#

#################################
# tabulate counts per geography #
#################################

# output shapefile with counts per geographic area

# counts of each crime type per neighborhood

counts.murder <- over(crimes.murder, neighborhoods_shp)
counts.murder.na <- crimes.murder[is.na(counts.murder$NAME),]
counts.murder.table <- as.data.frame(table(counts.murder$NAME))
colnames(counts.murder.table) <- c("NAME", "murd_num")

counts.rape <- over(crimes.rape, neighborhoods_shp)
counts.rape.na <- crimes.rape[is.na(counts.rape$NAME),]
counts.rape.table <- as.data.frame(table(counts.rape$NAME))
colnames(counts.rape.table) <- c("NAME", "rape_num")

counts.aggravated_assault <- over(crimes.aggravated_assault, neighborhoods_shp)
counts.aggravated_assault.na <- crimes.aggravated_assault[is.na(counts.aggravated_assault$NAME),]
counts.aggravated_assault.table <- as.data.frame(table(counts.aggravated_assault$NAME))
colnames(counts.aggravated_assault.table) <- c("NAME", "agg_num")

counts.robbery <- over(crimes.robbery, neighborhoods_shp)
counts.robbery.na <- crimes.robbery[is.na(counts.robbery$NAME),]
counts.robbery.table <- as.data.frame(table(counts.robbery$NAME))
colnames(counts.robbery.table) <- c("NAME", "rob_num")

counts.residential_burglary <- over(crimes.residential_burglary, neighborhoods_shp)
counts.residential_burglary.na <- crimes.residential_burglary[is.na(counts.residential_burglary$NAME),]
counts.residential_burglary.table <- as.data.frame(table(counts.residential_burglary$NAME))
colnames(counts.residential_burglary.table) <- c("NAME", "rbur_num")

counts.motor_vehicle_theft <- over(crimes.motor_vehicle_theft, neighborhoods_shp)
counts.motor_vehicle_theft.na <- crimes.motor_vehicle_theft[is.na(counts.motor_vehicle_theft$NAME),]
counts.motor_vehicle_theft.table <- as.data.frame(table(counts.motor_vehicle_theft$NAME))
colnames(counts.motor_vehicle_theft.table) <- c("NAME", "mvt_num")

counts.commercial_burglary <- over(crimes.commercial_burglary, neighborhoods_shp)
counts.commercial_burglary.na <- crimes.commercial_burglary[is.na(counts.commercial_burglary$NAME),]
counts.commercial_burglary.table <- as.data.frame(table(counts.commercial_burglary$NAME))
colnames(counts.commercial_burglary.table) <- c("NAME", "cbur_num")

counts.larceny <- over(crimes.larceny, neighborhoods_shp)
counts.larceny.na <- crimes.larceny[is.na(counts.larceny$NAME),]
counts.larceny.table <- as.data.frame(table(counts.larceny$NAME))
colnames(counts.larceny.table) <- c("NAME", "larc_num")

counts.theft_from_vehicle <- over(crimes.theft_from_vehicle, neighborhoods_shp)
counts.theft_from_vehicle.na <- crimes.theft_from_vehicle[is.na(counts.theft_from_vehicle$NAME),]
counts.theft_from_vehicle.table <- as.data.frame(table(counts.theft_from_vehicle$NAME))
colnames(counts.theft_from_vehicle.table) <- c("NAME", "tfv_num")

###### INSERTS POINTS TO NEAREST POLYGON 
###### RETURNS TABLE WITH TOTAL COUNTS INCLUDING THESE NEW POINTS

pts.to.nearest.polys.tbl <- function(pts, polys, output.table, crime_type) {
        
        if(nrow(pts) == 0) {
                
                return(output.table)
        } 
        
        else {

        Fdist <- list()
        for(i in 1:dim(pts)[1]) {
                pDist <- vector()
                for(j in 1:dim(polys)[1]) { 
                        pDist <- append(pDist, gDistance(pts[i,],polys[j,])) 
                }
                Fdist[[i]] <- pDist
        } 
        
        # RETURN POLYGON (NUMBER) WITH THE SMALLEST DISTANCE FOR EACH POINT  
        min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1]))  
        
        # RETURN DISTANCE TO NEAREST POLYGON
        PolyDist <- unlist(lapply(Fdist, FUN=function(x) min(x)[1]))  
        
        # CREATE POLYGON-ID AND MINIMUM DISTANCE COLUMNS IN POINT FEATURE CLASS
        pts@data <- data.frame(pts@data, PolyID=min.dist, PDist=PolyDist)
        
        # get the name IDs of the closest polygons
        name_ids <- as.vector(pts$PolyID)
        
        # IMPORTANT needs the right column name
        polygon_names <- as.data.frame(polys$NAME)
        
        # get NAME and append to shapefile. Do a merge. Sum two columns and delete old column
        name_ids <- as.vector(pts$PolyID)
        
        # IMPORTANT needs the right column name
        name_df <- as.data.frame(polys$NAME)
        
        # saved counts
        pts.table <- as.data.frame(table(pts$PolyID))
        
        # now change var to be the names
        # IMPORTANT needs the right column name
        pts.table$NAME <- name_df[as.vector(pts.table$Var1),]
        pts.table$near_count <- pts.table$Freq
        
        # now extract just NAME and near_count
        pts.table <- pts.table[,3:4]
        
        # merge with existing counts
        combined_counts <- merge(output.table, pts.table, all.x=TRUE)
        # set NAs to 0
        combined_counts$near_count[is.na(combined_counts$near_count)] <- 0
        
        output <- list()
        output$NAME <- output.table$NAME
        
        # need this to be the right tag
        if(crime_type == "murder") {
                output$murd_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "rape") {
                output$rape_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "aggravated_assault") {
                output$agg_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "robbery") {
                output$rob_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "residential_burglary") {
                output$crime_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "motor_vehicle_theft") {
                output$mvt_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "commercial_burglary") {
                output$crime_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "larceny") {
                output$larc_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "theft_from_vehicle") {
                output$tfv_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else {
                break
        }
        }
}

# assign points that don't fall inside polygons to the closest polygon
counts.murder.table <- pts.to.nearest.polys.tbl(counts.murder.na,
                                                neighborhoods_shp,
                                                counts.murder.table, 
                                                "murder")

counts.rape.table <- pts.to.nearest.polys.tbl(counts.rape.na,
                                              neighborhoods_shp,
                                              counts.rape.table, 
                                              "rape")

counts.aggravated_assault.table <- pts.to.nearest.polys.tbl(counts.aggravated_assault.na,
                                                            neighborhoods_shp,
                                                            counts.aggravated_assault.table, 
                                                            "aggravated_assault")

counts.robbery.table <- pts.to.nearest.polys.tbl(counts.robbery.na,
                                                 neighborhoods_shp,
                                                 counts.robbery.table, 
                                                 "robbery")

counts.residential_burglary.table <- pts.to.nearest.polys.tbl(counts.residential_burglary.na,
                                                              neighborhoods_shp,
                                                              counts.residential_burglary.table,
                                                              "residential_burglary")

counts.motor_vehicle_theft.table <- pts.to.nearest.polys.tbl(counts.motor_vehicle_theft.na,
                                                             neighborhoods_shp,
                                                             counts.motor_vehicle_theft.table, 
                                                             "motor_vehicle_theft")

counts.commercial_burglary.table <- pts.to.nearest.polys.tbl(counts.commercial_burglary.na,
                                                             neighborhoods_shp,
                                                             counts.commercial_burglary.table,
                                                             "commercial_burglary")

counts.larceny.table <- pts.to.nearest.polys.tbl(counts.larceny.na, 
                                                 neighborhoods_shp, 
                                                 counts.larceny.table, 
                                                 "larceny")

counts.theft_from_vehicle.table <- pts.to.nearest.polys.tbl(counts.theft_from_vehicle.na,
                                                            neighborhoods_shp,
                                                            counts.theft_from_vehicle.table, 
                                                            "theft_from_vehicle")


##### END INSERTS POINTS TO NEAREST POLYGON

# add counts of each crime type as a new row in neighborhoods_shp

neighborhoods_shp <- merge(neighborhoods_shp, counts.murder.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.rape.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.aggravated_assault.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.robbery.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.residential_burglary.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.motor_vehicle_theft.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.commercial_burglary.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.larceny.table, all.x=TRUE)
neighborhoods_shp <- merge(neighborhoods_shp, counts.theft_from_vehicle.table, all.x=TRUE)

# remove NAs (sets them to 0)
neighborhoods_shp$murd_num[is.na(neighborhoods_shp$murd_num)] <- 0
neighborhoods_shp$rape_num[is.na(neighborhoods_shp$rape_num)] <- 0
neighborhoods_shp$agg_num[is.na(neighborhoods_shp$agg_num)] <- 0
neighborhoods_shp$rob_num[is.na(neighborhoods_shp$rob_num)] <- 0
neighborhoods_shp$rbur_num[is.na(neighborhoods_shp$rbur_num)] <- 0
neighborhoods_shp$mvt_num[is.na(neighborhoods_shp$mvt_num)] <- 0
neighborhoods_shp$cbur_num[is.na(neighborhoods_shp$cbur_num)] <- 0
neighborhoods_shp$larc_num[is.na(neighborhoods_shp$larc_num)] <- 0
neighborhoods_shp$tfv_num[is.na(neighborhoods_shp$tfv_num)] <- 0

###############################
# tabulate cost per geography #
###############################

# add total cost of crime for each crime type
neighborhoods_shp$murd_cost <- as.numeric(cost_of_crime[1,3]) * as.numeric(neighborhoods_shp$murd_num)
neighborhoods_shp$rape_cost <- as.numeric(cost_of_crime[2,3]) * as.numeric(neighborhoods_shp$rape_num)
neighborhoods_shp$agg_cost <- as.numeric(cost_of_crime[3,3]) * as.numeric(neighborhoods_shp$agg_num)
neighborhoods_shp$rob_cost <- as.numeric(cost_of_crime[4,3]) * as.numeric(neighborhoods_shp$rob_num)
neighborhoods_shp$rbur_cost <- as.numeric(cost_of_crime[5,3]) * as.numeric(neighborhoods_shp$rbur_num)
neighborhoods_shp$mvt_cost <- as.numeric(cost_of_crime[6,3]) * as.numeric(neighborhoods_shp$mvt_num)
neighborhoods_shp$cbur_cost <- as.numeric(cost_of_crime[7,3]) * as.numeric(neighborhoods_shp$cbur_num)
neighborhoods_shp$larc_cost <- as.numeric(cost_of_crime[8,3]) * as.numeric(neighborhoods_shp$larc_num)
neighborhoods_shp$tfv_cost <- as.numeric(cost_of_crime[9,3]) * as.numeric(neighborhoods_shp$tfv_num)

# Total Crime Cost Calculation

# tablulate total cost of crime across all crime types for each row
crime_cost <- rowSums(as.data.frame(neighborhoods_shp[,crime_cost_field_names]))
# add field with value for total cost across all crime types
neighborhoods_shp$crime_cost <- crime_cost

# adds percentage breakdown
neighborhoods_shp$murd_perc <- as.numeric(neighborhoods_shp$murd_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$rape_perc <- as.numeric(neighborhoods_shp$rape_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$agg_perc <- as.numeric(neighborhoods_shp$agg_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$rob_perc <- as.numeric(neighborhoods_shp$rob_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$rbur_perc <- as.numeric(neighborhoods_shp$rbur_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$mvt_perc <- as.numeric(neighborhoods_shp$mvt_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$cbur_perc <- as.numeric(neighborhoods_shp$cbur_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$larc_perc <- as.numeric(neighborhoods_shp$larc_cost) / as.numeric(neighborhoods_shp$crime_cost)
neighborhoods_shp$tfv_perc <- as.numeric(neighborhoods_shp$tfv_cost) / as.numeric(neighborhoods_shp$crime_cost)

# adds primary crime type
# works for one row at a time, saves the row and a value for what the row is
crime_perc_max <- apply(as.data.frame(neighborhoods_shp[,perc_field_names]), 1, which.max)

# append to neighborhoods_shp
neighborhoods_shp$prim_crime <- crime_perc_max

# if murd (1) is selected by 'which.max', check to see if any crimes happened in that area
# if no, then assign the value to "9" - if yes, don't change anything
neighborhoods_shp$prim_crime <- ifelse(neighborhoods_shp$prim_crime == 1,
                                      ifelse(rowSums(as.data.frame(neighborhoods_shp[,crime_count_field_names])) == 0, 
                                             neighborhoods_shp$prim_crime + 9, 
                                             neighborhoods_shp$prim_crime), 
                                      neighborhoods_shp$prim_crime)

# assign labels to numeric values from above
neighborhoods_shp$prim_crime <- crime_labels[neighborhoods_shp$prim_crime]

#
#
#
# BLOCK GROUPS
#
#
#

# counts of each crime type per census block group
counts.murder <- over(crimes.murder, block_groups_shp)
counts.murder.na <- crimes.murder[is.na(counts.murder$GEOID_STRP),]
counts.murder.table <- as.data.frame(table(counts.murder$GEOID_STRP))
colnames(counts.murder.table) <- c("GEOID_STRP", "murd_num")

counts.rape <- over(crimes.rape, block_groups_shp)
counts.rape.na <- crimes.rape[is.na(counts.rape$GEOID_STRP),]
counts.rape.table <- as.data.frame(table(counts.rape$GEOID_STRP))
colnames(counts.rape.table) <- c("GEOID_STRP", "rape_num")

counts.aggravated_assault <- over(crimes.aggravated_assault, block_groups_shp)
counts.aggravated_assault.na <- crimes.aggravated_assault[is.na(counts.aggravated_assault$GEOID_STRP),]
counts.aggravated_assault.table <- as.data.frame(table(counts.aggravated_assault$GEOID_STRP))
colnames(counts.aggravated_assault.table) <- c("GEOID_STRP", "agg_num")

counts.robbery <- over(crimes.robbery, block_groups_shp)
counts.robbery.na <- crimes.robbery[is.na(counts.robbery$GEOID_STRP),]
counts.robbery.table <- as.data.frame(table(counts.robbery$GEOID_STRP))
colnames(counts.robbery.table) <- c("GEOID_STRP", "rob_num")

counts.residential_burglary <- over(crimes.residential_burglary, block_groups_shp)
counts.residential_burglary.na <- crimes.residential_burglary[is.na(counts.residential_burglary$GEOID_STRP),]
counts.residential_burglary.table <- as.data.frame(table(counts.residential_burglary$GEOID_STRP))
colnames(counts.residential_burglary.table) <- c("GEOID_STRP", "rbur_num")

counts.motor_vehicle_theft <- over(crimes.motor_vehicle_theft, block_groups_shp)
counts.motor_vehicle_theft.na <- crimes.motor_vehicle_theft[is.na(counts.motor_vehicle_theft$GEOID_STRP),]
counts.motor_vehicle_theft.table <- as.data.frame(table(counts.motor_vehicle_theft$GEOID_STRP))
colnames(counts.motor_vehicle_theft.table) <- c("GEOID_STRP", "mvt_num")

counts.commercial_burglary <- over(crimes.commercial_burglary, block_groups_shp)
counts.commercial_burglary.na <- crimes.commercial_burglary[is.na(counts.commercial_burglary$GEOID_STRP),]
counts.commercial_burglary.table <- as.data.frame(table(counts.commercial_burglary$GEOID_STRP))
colnames(counts.commercial_burglary.table) <- c("GEOID_STRP", "cbur_num")

counts.larceny <- over(crimes.larceny, block_groups_shp)
counts.larceny.na <- crimes.larceny[is.na(counts.larceny$GEOID_STRP),]
counts.larceny.table <- as.data.frame(table(counts.larceny$GEOID_STRP))
colnames(counts.larceny.table) <- c("GEOID_STRP", "larc_num")

counts.theft_from_vehicle <- over(crimes.theft_from_vehicle, block_groups_shp)
counts.theft_from_vehicle.na <- crimes.theft_from_vehicle[is.na(counts.theft_from_vehicle$GEOID_STRP),]
counts.theft_from_vehicle.table <- as.data.frame(table(counts.theft_from_vehicle$GEOID_STRP))
colnames(counts.theft_from_vehicle.table) <- c("GEOID_STRP", "tfv_num")



###### INSERTS POINTS TO NEAREST POLYGON 
###### RETURNS TABLE WITH TOTAL COUNTS INCLUDING THESE NEW POINTS

pts.to.nearest.polys.tbl <- function(pts, polys, output.table, crime_type) {
        
        if(nrow(pts) == 0) {
                
                return(output.table)
        } 
        
        else {
                
                Fdist <- list()
                for(i in 1:dim(pts)[1]) {
                        pDist <- vector()
                        for(j in 1:dim(polys)[1]) { 
                                pDist <- append(pDist, gDistance(pts[i,],polys[j,])) 
                        }
                        Fdist[[i]] <- pDist
                } 
                
                # RETURN POLYGON (NUMBER) WITH THE SMALLEST DISTANCE FOR EACH POINT  
                min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1]))  
                
                # RETURN DISTANCE TO NEAREST POLYGON
                PolyDist <- unlist(lapply(Fdist, FUN=function(x) min(x)[1]))  
                
                # CREATE POLYGON-ID AND MINIMUM DISTANCE COLUMNS IN POINT FEATURE CLASS
                pts@data <- data.frame(pts@data, PolyID=min.dist, PDist=PolyDist)
                
                # get the name IDs of the closest polygons
                name_ids <- as.vector(pts$PolyID)
                
                # IMPORTANT needs the right column name
                polygon_names <- as.data.frame(polys$GEOID_STRP)
                
                # get GEOID_STRP and append to shapefile. Do a merge. Sum two columns and delete old column
                name_ids <- as.vector(pts$PolyID)
                
                # IMPORTANT needs the right column name
                name_df <- as.data.frame(polys$GEOID_STRP)
                
                # saved counts
                pts.table <- as.data.frame(table(pts$PolyID))
                
                # now change var to be the names
                # IMPORTANT needs the right column name
                pts.table$GEOID_STRP <- name_df[as.vector(pts.table$Var1),]
                pts.table$near_count <- pts.table$Freq
                
                # now extract just GEOID_STRP and near_count
                pts.table <- pts.table[,3:4]
                
                # merge with existing counts
                combined_counts <- merge(output.table, pts.table, all.x=TRUE)
                # set NAs to 0
                combined_counts$near_count[is.na(combined_counts$near_count)] <- 0
                
                output <- list()
                output$GEOID_STRP <- output.table$GEOID_STRP
                
                # need this to be the right tag
                if(crime_type == "murder") {
                        output$murd_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "rape") {
                        output$rape_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "aggravated_assault") {
                        output$agg_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "robbery") {
                        output$rob_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "residential_burglary") {
                        output$rbur_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "motor_vehicle_theft") {
                        output$mvt_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "commercial_burglary") {
                        output$cbur_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "larceny") {
                        output$larc_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else if(crime_type == "theft_from_vehicle") {
                        output$tfv_num <- rowSums(combined_counts[,2:3])
                        return(output)
                }
                else {
                        break
                }
        }
}

# assign points that don't fall inside polygons to the closest polygon
counts.murder.table <- pts.to.nearest.polys.tbl(counts.murder.na,
                                                block_groups_shp,
                                                counts.murder.table, 
                                                "murder")

counts.rape.table <- pts.to.nearest.polys.tbl(counts.rape.na,
                                              block_groups_shp,
                                              counts.rape.table, 
                                              "rape")

counts.aggravated_assault.table <- pts.to.nearest.polys.tbl(counts.aggravated_assault.na,
                                                            block_groups_shp,
                                                            counts.aggravated_assault.table, 
                                                            "aggravated_assault")

counts.robbery.table <- pts.to.nearest.polys.tbl(counts.robbery.na,
                                                 block_groups_shp,
                                                 counts.robbery.table, 
                                                 "robbery")

counts.residential_burglary.table <- pts.to.nearest.polys.tbl(counts.residential_burglary.na,
                                                              block_groups_shp,
                                                              counts.residential_burglary.table,
                                                              "residential_burglary")

counts.motor_vehicle_theft.table <- pts.to.nearest.polys.tbl(counts.motor_vehicle_theft.na,
                                                             block_groups_shp,
                                                             counts.motor_vehicle_theft.table, 
                                                             "motor_vehicle_theft")

counts.commercial_burglary.table <- pts.to.nearest.polys.tbl(counts.commercial_burglary.na,
                                                             block_groups_shp,
                                                             counts.commercial_burglary.table,
                                                             "commercial_burglary")

counts.larceny.table <- pts.to.nearest.polys.tbl(counts.larceny.na, 
                                                 block_groups_shp, 
                                                 counts.larceny.table, 
                                                 "larceny")

counts.theft_from_vehicle.table <- pts.to.nearest.polys.tbl(counts.theft_from_vehicle.na,
                                                            block_groups_shp,
                                                            counts.theft_from_vehicle.table, 
                                                            "theft_from_vehicle")

##### END INSERTS POINTS TO NEAREST POLYGON




# add counts of each crime type as a new row in block_groups_shp
block_groups_shp <- merge(block_groups_shp, counts.murder.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.rape.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.aggravated_assault.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.robbery.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.residential_burglary.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.motor_vehicle_theft.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.commercial_burglary.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.larceny.table, all.x=TRUE)
block_groups_shp <- merge(block_groups_shp, counts.theft_from_vehicle.table, all.x=TRUE)

# remove NAs (sets them to 0)
block_groups_shp$murd_num[is.na(block_groups_shp$murd_num)] <- 0
block_groups_shp$rape_num[is.na(block_groups_shp$rape_num)] <- 0
block_groups_shp$agg_num[is.na(block_groups_shp$agg_num)] <- 0
block_groups_shp$rob_num[is.na(block_groups_shp$rob_num)] <- 0
block_groups_shp$rbur_num[is.na(block_groups_shp$rbur_num)] <- 0
block_groups_shp$mvt_num[is.na(block_groups_shp$mvt_num)] <- 0
block_groups_shp$cbur_num[is.na(block_groups_shp$cbur_num)] <- 0
block_groups_shp$larc_num[is.na(block_groups_shp$larc_num)] <- 0
block_groups_shp$tfv_num[is.na(block_groups_shp$tfv_num)] <- 0

###############################
# tabulate cost per geography #
###############################

# add total cost of crime for each crime type
block_groups_shp$murd_cost <- as.numeric(cost_of_crime[1,3]) * as.numeric(block_groups_shp$murd_num)
block_groups_shp$rape_cost <- as.numeric(cost_of_crime[2,3]) * as.numeric(block_groups_shp$rape_num)
block_groups_shp$agg_cost <- as.numeric(cost_of_crime[3,3]) * as.numeric(block_groups_shp$agg_num)
block_groups_shp$rob_cost <- as.numeric(cost_of_crime[4,3]) * as.numeric(block_groups_shp$rob_num)
block_groups_shp$rbur_cost <- as.numeric(cost_of_crime[5,3]) * as.numeric(block_groups_shp$rbur_num)
block_groups_shp$mvt_cost <- as.numeric(cost_of_crime[6,3]) * as.numeric(block_groups_shp$mvt_num)
block_groups_shp$cbur_cost <- as.numeric(cost_of_crime[7,3]) * as.numeric(block_groups_shp$cbur_num)
block_groups_shp$larc_cost <- as.numeric(cost_of_crime[8,3]) * as.numeric(block_groups_shp$larc_num)
block_groups_shp$tfv_cost <- as.numeric(cost_of_crime[9,3]) * as.numeric(block_groups_shp$tfv_num)

# Total Crime Cost Calculation

# tablulate total cost of crime across all crime types for each row
crime_cost <- rowSums(as.data.frame(block_groups_shp[,crime_cost_field_names]))
# add field with value for total cost across all crime types
block_groups_shp$crime_cost <- crime_cost

# adds percentage breakdown
block_groups_shp$murd_perc <- as.numeric(block_groups_shp$murd_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$rape_perc <- as.numeric(block_groups_shp$rape_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$agg_perc <- as.numeric(block_groups_shp$agg_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$rob_perc <- as.numeric(block_groups_shp$rob_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$rbur_perc <- as.numeric(block_groups_shp$rbur_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$mvt_perc <- as.numeric(block_groups_shp$mvt_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$cbur_perc <- as.numeric(block_groups_shp$cbur_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$larc_perc <- as.numeric(block_groups_shp$larc_cost) / as.numeric(block_groups_shp$crime_cost)
block_groups_shp$tfv_perc <- as.numeric(block_groups_shp$tfv_cost) / as.numeric(block_groups_shp$crime_cost)

# remove NAs (sets them to 0)
block_groups_shp$murd_perc[is.na(block_groups_shp$murd_perc)] <- 0
block_groups_shp$rape_perc[is.na(block_groups_shp$rape_perc)] <- 0
block_groups_shp$agg_perc[is.na(block_groups_shp$agg_perc)] <- 0
block_groups_shp$rob_perc[is.na(block_groups_shp$rob_perc)] <- 0
block_groups_shp$rbur_perc[is.na(block_groups_shp$rbur_perc)] <- 0
block_groups_shp$mvt_perc[is.na(block_groups_shp$mvt_perc)] <- 0
block_groups_shp$cbur_perc[is.na(block_groups_shp$cbur_perc)] <- 0
block_groups_shp$larc_perc[is.na(block_groups_shp$larc_perc)] <- 0
block_groups_shp$tfv_perc[is.na(block_groups_shp$tfv_perc)] <- 0

# adds primary crime type

# order crimes by precedence above (at the top)

# works for one row at a time, saves the row and a value for what the row is
crime_perc_max <- apply(as.data.frame(block_groups_shp[,perc_field_names]), 1, which.max)

# append to block_groups_shp
block_groups_shp$prim_crime <- crime_perc_max

# if murd (1) is selected by 'which.max', check to see if any crimes happened in that area
# if no, then assign the value to "9" - if yes, don't change anything
block_groups_shp$prim_crime <- ifelse(block_groups_shp$prim_crime == 1,
                                      ifelse(rowSums(as.data.frame(block_groups_shp[,crime_count_field_names])) == 0, 
                                             block_groups_shp$prim_crime + 9, 
                                             block_groups_shp$prim_crime), 
                                      block_groups_shp$prim_crime)
                                                             
# assign labels to numeric values from above
block_groups_shp$prim_crime <- crime_labels[block_groups_shp$prim_crime]

# normalizes for population

block_groups_shp$nmurd_cost <- as.numeric(block_groups_shp$murd_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nrape_cost <- as.numeric(block_groups_shp$rape_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nagg_cost <- as.numeric(block_groups_shp$agg_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nrob_cost <- as.numeric(block_groups_shp$rob_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nrbur_cost <- as.numeric(block_groups_shp$rbur_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nmvt_cost <- as.numeric(block_groups_shp$mvt_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$ncbur_cost <- as.numeric(block_groups_shp$cbur_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$nlarc_cost <- as.numeric(block_groups_shp$larc_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$ntfv_cost <- as.numeric(block_groups_shp$tfv_cost) / as.numeric(block_groups_shp$ADJPOP)
block_groups_shp$ncrm_cost <- as.numeric(block_groups_shp$crime_cost) / as.numeric(block_groups_shp$ADJPOP)

###############################
# save as separate shapefiles #
###############################

# write out as a new shapefile

# neighborhoods
writeOGR(neighborhoods_shp, output_directory, layer = neighborhoods_output_layer, driver = "ESRI Shapefile", overwrite_layer = T)

# block groups
writeOGR(block_groups_shp, output_directory, layer = block_groups_output_layer, driver = "ESRI Shapefile", overwrite_layer = T)

# # # # # # # # # # # #
# summary statistics  #
# # # # # # # # # # # #

# create tbl_df so dplyr can be used
neighborhoods <- tbl_df(as.data.frame(neighborhoods_shp))
#blocks <- tbl_df(as.data.frame(blocks_shp))
block_groups <- tbl_df(as.data.frame(block_groups_shp))

# pulls in the fields we care about
grp <- c("murd_num",
         "rape_num",
         "agg_num",
         "rob_num",
         "rbur_num",
         "mvt_num",
         "cbur_num",
         "larc_num",
         "tfv_num",
         "murd_cost",
         "rape_cost",
         "agg_cost",
         "rob_cost",
         "rbur_cost",
         "mvt_cost",
         "cbur_cost",
         "larc_cost",
         "tfv_cost",
         "crime_cost")

# sum counts and costs to compare
neighborhoods.totals <- colSums(neighborhoods[,grp])
#blocks.totals <- colSums(blocks[,grp])
block_groups.totals <- colSums(block_groups[,grp])

# compare counts across the different shapefiles
crimes.summary <- cbind(as.data.frame(neighborhoods.totals),
                        #as.data.frame(blocks.totals),
                        as.data.frame(block_groups.totals))

# pull in total counts from CSV
csv.totals <- rbind(nrow(crimes.murder),
                    nrow(crimes.rape),
                    nrow(crimes.aggravated_assault),
                    nrow(crimes.robbery),
                    nrow(crimes.residential_burglary),
                    nrow(crimes.motor_vehicle_theft),
                    nrow(crimes.commercial_burglary),
                    nrow(crimes.larceny),
                    nrow(crimes.theft_from_vehicle),
                    nrow(crimes.murder)* as.numeric(cost_of_crime[1,3]),
                    nrow(crimes.rape)* as.numeric(cost_of_crime[2,3]),
                    nrow(crimes.aggravated_assault)* as.numeric(cost_of_crime[3,3]),
                    nrow(crimes.robbery)* as.numeric(cost_of_crime[4,3]),
                    nrow(crimes.residential_burglary)* as.numeric(cost_of_crime[5,3]),
                    nrow(crimes.motor_vehicle_theft)* as.numeric(cost_of_crime[6,3]),
                    nrow(crimes.commercial_burglary)* as.numeric(cost_of_crime[7,3]),
                    nrow(crimes.larceny)* as.numeric(cost_of_crime[8,3]),
                    nrow(crimes.theft_from_vehicle)* as.numeric(cost_of_crime[9,3]))
                    

# calculate total cost of crime
csv.cost_of_crime <- sum(csv.totals[10:18])

# add total cost of crime ot csv.totals
csv.totals <- rbind(csv.totals,
                    as.numeric(csv.cost_of_crime))

# add csv.totals to the crimes.summary
crimes.summary$csv.totals <- csv.totals