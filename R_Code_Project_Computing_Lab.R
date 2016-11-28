library(sp)
library(rworldmap)
library(rworldxtra)
library(RMySQL)

# initialise mysql connection
con <- dbConnect(MySQL(),user = "almysql", password = "pass", host = "127.0.0.1", dbname = "project")

# read raw data
raw_data<-dbGetQuery(con,"SELECT * FROM 300k")
regions <- read.csv('regions2.csv')
raw_data <- cbind(raw_data,regions)

# Function to get countries given coordinates
coords2country = function(points){  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$ADMIN
}

# Function to get continents given coordinates
coords2continent = function(points){  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$REGION
}

data <- raw_data

# Create country variable
data$country <- coords2country(rev(data[,2:3]))
data$country <- as.character(data$country)

# Change country name so the map package can read the country properly
data$country[data[,"country"] == "United States of America"] <- "usa"
data$country[data[,"country"] == "United Kingdom"] <- "UK"

# Create continent variable
data$continent <- coords2continent(rev(data[,2:3]))
data$continent <- as.character(data$continent)

# Remove city variable - it is incorrect
data$city <- NULL

# Get pokemon info and merge
Pokemon<-dbGetQuery(con, "SELECT * FROM pkmn_info")
names(Pokemon)[3]<-'type1'
names(Pokemon)[4]<-'type2'
names(Pokemon)[9]<-'sp_atk'
names(Pokemon)[10]<-'sp_def'
data <- merge(data,Pokemon[,1:3],by.x='class',by.y='id',sort=F)

# Write db to mySQL
dbWriteTable(con,"poke_spawns",data)

par(mar=rep(4,4)) 
map(database="world",c("Canada","usa","Mexico"))
points(data$longitude,data$latitude,pch=18,cex=0.5,col="red")