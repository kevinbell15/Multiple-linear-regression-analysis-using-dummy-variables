
############################ Load the house prices dataset#########################################

getwd()
HousePrices=read.csv(“HousePrices.csv”,header=TRUE)
fix(HousePrices)
attach(HousePrices)

#Summary stat for the variables
summary(HousePrices)


####### Create dummy variables and calculate the percentage of houses with Driveway, Gas-Heat, and AC ###############


##################################### Driveway Dummy ############################
numrows=nrow(HousePrices)

driveway_dummy<-matrix(nrow=numrows,ncol=1)
head(HousePrices)

#Create dummy variable
for (i in 1:numrows)
    {

	if (driveway[i]=='yes') {
   	driveway_dummy[i,]=1
	} else {
   	driveway_dummy[i,]=0
	}
}
head(driveway_dummy)

########################################## Gas Heat Dummy ################################

gasheat_dummy<-matrix(nrow=numrows,ncol=1)

#Create dummy variable
for (i in 1:numrows)
    {

	if (gasheat[i]=='yes') {
   	gasheat_dummy[i,]=1
	} else {
   	gasheat_dummy[i,]=0
	}
}
head(gasheat_dummy)

############################################ air conditioning dummy #############################

aircon_dummy<-matrix(nrow=numrows,ncol=1)

#Create dummy variable
for (i in 1:numrows)
    {

	if (aircon[i]=='yes') {
   	aircon_dummy[i,]=1
	} else {
   	aircon_dummy[i,]=0
	}
}

head(aircon_dummy)



# Calculate the mean for the variables
mean(driveway_dummy)
mean(gasheat_dummy)
mean(aircon_dummy)


#############################Linear Regression bedrooms ###########################

#Perform linear regression to determine if bedrooms has an effect on home price
lm.fit=lm(price~bedrooms)
summary(lm.fit)


############################ Linear regression using all variables ########################

lm.fit2=lm(price~lotsize+bedrooms+bathrooms+stories+driveway_dummy+recreation+fullbase+gasheat_dummy+aircon_dummy+garage+prefer)
summary(lm.fit2)




##################################### Load the credit dataset ###########################
credit=read.csv("Credit.csv",header=TRUE)
fix(credit)
attach(credit)

#Number of rows in the data and diminsions 
nrow(credit)
dim(credit)

#Summary stat for the credit variables
summary(credit)


#################### Create dummy variables for student and gender ##############################
#Create student dummy variable
Student_dummy<-matrix(nrow=numrows,ncol=1)

for (i in 1:numrows)
    {

	if (Student[i]=='Yes') {
   	Student_dummy[i,]=1
	} else {
   	Student_dummy[i,]=0
	}
}
head(Student_dummy)


#Create Gender dummy variable
Gender_dummy<-matrix(nrow=numrows,ncol=1)

for (i in 1:numrows)
    {

      if (Gender[i]=='Female') {
	Gender_dummy[i,]=1
      } else {
      Gender_dummy[i,]=0
	}
}
head(Gender_dummy)

#Calculate the percent of Students and Females in the dataset
mean(Student_dummy)
mean(Gender_dummy)


################################## Multiple linear regression analysis ####################################

#Perform multiple linear regression analysis to predict Balance
lm.fit=lm(Balance~Rating+Student_dummy+Rating*Student_dummy)
summary(lm.fit)


################################## Simple linear regression analysis ###################################
lm.fit2=lm(Balance~Age)
summary(lm.fit2)


#################### Show interaction between Age and Rating on balance########

lm.fit3=lm(Balance~Age+Rating+Age*Rating)
summary(lm.fit3)















