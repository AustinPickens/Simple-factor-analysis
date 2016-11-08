
##### Factor analysis that produces identical data as SPSS factor analysis #####

X= #matrix of numbers with non missing values
H= #The name for my matrix with human clinical data such as bmi and waist circumference

###  Scree Plot  ####
install.packages("FactoMineR")
library(FactoMineR)
tmp=PCA(X)
plot(tmp$eig$eigenvalue)
	## based on my Scree plot there are 4 factors
	#References for determining number of factors:
		##Ledesma, R.D., P. Valero-Mora, and G. Macbeth, The Scree Test and the Number of Factors: a 				#Dynamic Graphics Approach. Span J Psychol, 2015. 18: p. E11.
	    ##Kaiser, H.F., The Application of Electronic Computers to Factor Analysis. Educational and 				#Psychological Measurement, 1960. 20(1): p. 141-151.


### Factor Analysis ###
install.packages("psych")
library(psych)
tmp<- principal(X, nfactors = 4, rotate = "varimax")
	#varimax is an orthogonal transformation of factor-loading matrix
tmp$loadings # varimax matrix of lipid loadings in each component
tmp$scores # each persons loading score into each factor

### matrix to store outputs of regressions ###
fanal=matrix(NA, nrow=4, ncol = 4) #creating a matrix for factor analysis outputs
colnames(fanal)= c("bmi.beta", "bmi.pval", "wc.beta", "wc.pval")
rownames(fanal)= c('factor1', "factor2", "factor3", "factor4")

### BMI and WC regression with factor scores  ###
for (i in 1:4){ #for factor 1 to 4, this loop runs through each factor individually
  bmireg=summary(lm(H$BMI~ tmp$scores[,i])) #regressing BMI on factor i
  fanal[i,1]=bmireg$coefficients[2,1] #indexing the estimated effects from factor i regression
  fanal[i,2]=bmireg$coefficients[2,4] #indexing p-value from factor i regression
  wcreg=summary(lm(H$WC~ tmp$scores[,i])) #regressing waist circumferences on factor i
  fanal[i,3]=wcreg$coefficients[2,1] #indexing the estimated effects from factor i regression
  fanal[i,4]=wcreg$coefficients[2,4] #indexing p-value from factor i regression
}
