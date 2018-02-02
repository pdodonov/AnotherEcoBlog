#########################################################
## Script for opening, merging and summarizing data    ##
## For a blog post at anotherecoblog.wordpress.com     ##
#########################################################
## by Pavel Dodonov - pdodonov@gmail.com               ##
#########################################################
## No rights reserved - feel free to modify, share etc ##
#########################################################

## Step one - set working directory ##
setwd("/home/pavel/Profissional/Extensao/Blogs/AnotherEcoBlog/dataOrganization")

# There's a metadata file, look at it :-)

## Step two - open the files ##
data_quad <- read.table("dataOrganization_Quadrats.csv", header=T, sep=",")
data_spec <- read.table("dataOrganization_Species.csv", header=T, sep=",")

## Step three - check data structure ##
str(data_quad)
# We have three columns (variables): Distance - integer, Species - factor, Hmax - integer
str(data_spec)
# We have four categorical columns (variables): Species, Family, Lifeform, Disp (dispersal)

# If you want, you can look at them using View:
View(data_quad)
View(data_spec)

## There are also some NA values because we have no family, dispersal or lifeforms for "indet" (indeterminate species)

# Our goal is to analyze the number of species and families, the number of species with each dispersal syndromes and lifeforms, and overall maximum height per plot
## Step four - combine the two objects

data_all <- merge(x=data_quad, y=data_spec, by="Species", all.x=T, all.y=F, sort=T)
str(data_all)
View(data_all)

# cool, we have it all in one place!

## Step five - sort it by distacnce to make it prettier
data_all <- data_all[order(data_all$Distance),]

# And put distance first also to make it prettier
data_all <- data_all[, c("Distance", "Species", "Family", "Lifeform", "Disp", "Hmax")]
str(data_all)

## Step six - summarize results per quadrat
# Using table to count instance
Nsp_temp <- table(data_all$Distance)
View(Nsp_temp)
Ndisp_temp <- table(data_all$Distance, data_all$Disp)
View(Ndisp_temp)
Nlife_temp <- table(data_all$Distance, data_all$Lifeform)
# Using aggregate to summarize the height values
Hmax_temp <- aggregate(Hmax ~ Distance, data=data_all, FUN=max)
# Calculate number of families may be a little harder...
Nfam_temp <- table(data_all$Distance, data_all$Family)
# This shows the number of species in each family
Nfam_temp[Nfam_temp >1] <- 1
# This converts it to a presence/absence matrix
Nfam_temp <- apply(Nfam_temp, 1, sum)
# This sums the presences, showing the number of families per plot
View(Nfam_temp)

# Now we have to check if the row names are the same, i.e. if the order is the same in all objects
all(rownames(Nsp_temp) == rownames(Ndisp_temp))
all(rownames(Nsp_temp) == rownames(Nlife_temp))
all(rownames(Nsp_temp) == Hmax_temp$Distance)
all(rownames(Nsp_temp) == rownames(Nfam_temp))

# Cool, the rownames are the same, so the objects can be joined.
# Let's transform Hmax_temp into a matrix
Hmax_temp <- as.matrix(Hmax_temp)
# And run cbind to combine the data
data_an <- cbind(Hmax_temp, Nsp_temp, Ndisp_temp, Nlife_temp, Nfam_temp)
# data_an is the object that will be used for future analyses
str(data_an)
# We can rename the third column:
colnames(data_an)[3] <- "Nsp"
View(data_an)

## Step seven - save the reorganized data if you feel like

write.table(data_an, file="data_analysis.txt", quote=F, sep=",", row.names=F)

## And so it is done :-)





