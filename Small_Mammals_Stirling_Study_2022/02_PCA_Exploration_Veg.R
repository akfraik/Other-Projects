### Let's set up our working directory and libraries
setwd("~/HERE")
library("lme4")
library("MuMIn")
library("devtools")
library("PCAmixdata")
library("factoextra")
library(ggbiplot)
library(PCAtest)

## Read in our small mammal vegetative variables
veg<-read.csv("SM_Summary1.csv")
X.quanti<-as.vector(names(veg[c(5:32)]))
veg<-veg[X.quanti]
pca_2<-prcomp(veg,center=TRUE,scale=TRUE)

## Std Deviation of PCs (square roots of the eigenvalues of the covariance matrix)
# Rotation- eigenvectors for PCs
std_dev<-pca_2$sdev
rotation<-pca_2$rotation
summary(pca_2)
biplot(pca_2)
plot(pca_2,type="l")

## Eigen values 
eig<-(pca_2$sdev)^2
variance<-eig*100/sum(eig)
pr_var<-std_dev^2
prop_varex<-pr_var/sum(pr_var)
plot(cumsum(prop_varex),type="b")

#First five PCs have cumulative eigenvalues <5
var<-get_pca_var(pca_2)
contrib<-var$contrib[,1:5]
var$cor[,1:5]
Results_DF<-data.frame(eig=eig,variance=variance,stdev=std_dev,proportion_var=prop_varex,contrib)
write.csv(Results_DF,"Vegetation_Results_DF.csv")

## Barplot of the variances (total porportion of variance)
barplot(variance,xlab="PCs",ylab="Proportion of Total Variance",names.arg=1:nrow(Results_DF),main="Variances",col="steelblue")

#Principal Component Analysis Results for variables
#===================================================
#  Name       Description                                    
#1 "$coord"   "Coordinates for the variables"                
#2 "$cor"     "Correlations between variables and dimensions"
#3 "$cos2"    "Cos2 for the variables"                       
#4 "$contrib" "contributions of the variables" 

#We used a PCA to describe variation in habitat structure amongst plots
pdf(file="Revised_PCA.pdf")
test<-ggbiplot(pca_2,choice=c(1,2),obs.scale=1,var.scale=1,ellipse=TRUE)
test<-test+scale_color_discrete(name='')
test<-test+theme(plot.background=element_blank(),panel.background=element_blank())
test<-test+theme(legend.background=element_rect(fill="white", colour=NA))
test<-test+theme(axis.text=element_text(size=rel(1), face="bold"))  
test<-test+theme(axis.ticks=element_line(size=0.2))
test<-test+theme(strip.text=element_text(face="bold",size=10),strip.background=element_rect(color="black"),panel.border=element_rect(color="black",fill="NA"))
test<-test+xlim(-8,8)+ylim(-8,8)
test
dev.off()

### Now let's go ahead and run a PCA test to look at the variance across PCs
PCA<-PCAtest(veg,nperm=500,nboot=500,alpha=0.05,plot=TRUE)

####################################################################################
## Now let's try to narrow this down based on our hypotheses
# 1. Food abundance: BA, Hardwoods, PerHard, Per_Con, SoftMastBearing, HardMastBearing, FoodProducing, Browse, Productivity
# 2. Cover/shelter: CWD20, Ground_Layer, Mean_DD, Mean_DS, Ground_Cover, DenRest, Shelter
# 3. Both: Canopy Cover, shrub index, QMD, Richness, Total Shrubs, Total Value, Average Value

## Now let's try rerunning PCAs just based on each of these groupings
veg<-read.csv("SM_Summary1.csv")
food<-as.vector(names(veg[c("BasalArea","Per_Hard","Per_Con","SoftMast","HardMast2","Productivity","Browse")]))
cover<-as.vector(names(veg[c("CWD20","GroundLayer","Mean_DD","Mean_DS","GroundCover","DenRest","Shelter")]))
both<-c(food,cover,"CanopyCover","Shrub_Index","TotalShrubs","TotalValue")
veg<-veg[both]
veg<-na.omit(veg)
pca_2<-prcomp(veg,center=TRUE,scale=TRUE)
std_dev<-pca_2$sdev
rotation<-pca_2$rotation
summary(pca_2)

#Eigen values 
eig<-(pca_2$sdev)^2
variance<-eig*100/sum(eig)
pr_var<-std_dev^2
prop_varex<-pr_var/sum(pr_var)
var<-get_pca_var(pca_2)
contrib<-var$contrib[,1:7]
var$cor[,1:7]
Results_DF<-data.frame(eig=eig,variance=variance,stdev=std_dev,proportion_var=prop_varex,contrib)
write.csv(Results_DF,"Vegetation_Results_DF_Both.csv")

#We used a PCA to describe variation in habitat structure amongst plots
pdf(file="Revised_PCA_Both.pdf")
test<-ggbiplot(pca_2,choice=c(1,2),obs.scale=1,var.scale=1,ellipse=TRUE)
test<-test+scale_color_discrete(name='')
test<-test+theme(plot.background=element_blank(),panel.background=element_blank())
test<-test+theme(legend.background=element_rect(fill="white", colour=NA))
test<-test+theme(axis.text=element_text(size=rel(1), face="bold"))  
test<-test+theme(axis.ticks=element_line(size=0.2))
test<-test+theme(strip.text=element_text(face="bold",size=10),strip.background=element_rect(color="black"),panel.border=element_rect(color="black",fill="NA"))
test<-test+xlim(-8,8)+ylim(-8,8)
test
dev.off()
