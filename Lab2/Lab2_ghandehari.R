# Load the "foreign" package
library(foreign)

# Load data
MN = read.dbf("data/MNMapPLUTO.dbf")

# Map all of the buildings in Manhattan
plot(YCoord ~ XCoord, data=MN)

# Look at the range of building x any y coordinates
# Note that the coordinate range includes zero.

summary(MN[ ,c("YCoord", "XCoord")])

# Select locations with X, Y coordinates
# Overwrite MN to contain only buildings with valid coordinates
MN <- MN[MN$YCoord > 0 & MN$XCoord > 0,]

plot(YCoord ~ XCoord, data=MN)

# Find buildings owned by someone famous who lives in MN
# Grep() is a text searching function
# in the line below grep() will identify rows where the owner name includes "trump"
# You can replace "trump" with the last name of anyone who owns a building in Manhattan
trumpBldgs <- grep("trump", MN$OwnerName, ignore.case=TRUE)

# Print the address, and value of the building (note the output includes the row #)
MN[trumpBldgs, c("Address", "AssessTot", "OwnerName")]

# Identifying Historic Districts
summary(MN$HistDist)


# Note: this line will return an error when you try to run it.
# Correct the line by making it evaluate all rows in the MN table
MN$HD = ifelse(is.na(MN[,"HistDist"]), 0, 1)

summary(MN$HD)


# Convert MN$HD to a factor
MN$HD = as.factor(MN$HD) 
# Note how the summary changes after changing the "HD" column to a factor
summary(MN$HD)

# "col" changes the color of dots depending upon the value in the "HD" column
# "pch" sets the symbol to a solid dot
# "cex"  makes the dot .5 the normal size
# Note that setting asp=1 will set the aspect ratio to 1

plot(YCoord ~ XCoord, data=MN, col=HD, pch=16, cex=.5, asp=1, align='center' )
legend('topright', c("historic buildings","non-historic buildings"), cex=.8, col=c('red', 'black'), pch=c(16,16))


# Add a comment describing the next line
inHD = MN[MN$HD ==1, ] 

# Add a comment describing the next line
outHD = MN[MN$HD ==0, ]

# Our null hypothesis is that the designation of historic districts has no effect on property
#values, the buildings in a historic district have the same value as those outside of a 
#historic district, and difference between the two groups is due to random chance.

# Insert a comment describing the line below
# In the comment state the null hypothesis
t.test(x=inHD$AssessTot, y=outHD$AssessTot)  # Hypothesis Test 1

boxplot(inHD~outHD, data=outHD, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose")
# Add a comment describing this test
t.test(x=inHD$BldgArea, y=outHD$BldgArea)  # Hypothesis Test 2


# Select buildings on the same block as a historic district
# Get a list of all blocks that contain historic buildings
blocks = inHD$Block 

# Select all buildings (from MN) that are on the same block as historic buildings
# The line below selects all rows where the block column contains values
# in our list of blocks. It also saves the result as a new 'object'.
HDB = MN[MN$Block %in% blocks, ] 

# non-historic biulding inside of historic districts. 
# Hint: What does the object HDB_out contain?
HDB_out = HDB[HDB$HD == 0, ]

# All of the historic buildings
# Hint: What does the object HDB_in contain?
HDB_in = HDB[HDB$HD == 1, ]

t.test(x=HDB_in$AssessTot, y=HDB_out$AssessTot)  # Hypothesis Test 3


# This could mean the lot is vacant, it could be an error.
# either way it makes it hard to compute the price per square foot.
# We need to exlude these zero area buildings from out t-test

# Calcuate price per square foot for historic buildings
# _Only_ for buildings with an area greater than 0
HDB_in_sqft = 
  HDB_in[HDB_in$BldgArea > 0, "AssessTot"] / 
  HDB_in[HDB_in$BldgArea > 0, "BldgArea"] 

# Calcuate price per square foot for non-historic buildings
HDB_out_sqft = 
  HDB_out[HDB_out$BldgArea > 0, "AssessTot"] / 
  HDB_out[HDB_out$BldgArea > 0, "BldgArea"] 

t.test(x=HDB_in_sqft, y=HDB_out_sqft)  # Hypothesis Test 4

#----------------
d <- density(log(HDB_out_sqft)) # returns the density data 
plot(d) # plots the results

#--------------
t.test(x=inHD$AssessTot, y=outHD$AssessTot, "greater")  # Hypothesis Test 1
t.test(x=inHD$AssessTot, y=outHD$AssessTot, "less")  # Hypothesis Test 1
#-------------
t.test(x=inHD$BldgArea, y=outHD$BldgArea,"greater")  # Hypothesis Test 2
t.test(x=inHD$BldgArea, y=outHD$BldgArea,"less")  # Hypothesis Test 2
#------------
boxplot(inHD$BldgArea,outHD$BldgArea)
