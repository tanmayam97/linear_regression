filename = "C:/Users/Tanmay Ambatkar/Documents/DataSets/real_estate.csv"
real_estate=read.csv(filename, header = T)
head(real_estate) # shows the top 6 rows of the data set
colnames(real_estate) # display the column names 
# View(real_estate) # show the data set

real_estate$No = NULL # removes the insignificant features

colnames(real_estate) # check that the above operation has done

str(real_estate) # showa the structure of the features in data set

# EDA

checknull=function(x)return(any(is.na(real_estate)))
null=colnames(real_estate)[apply(real_estate,2,checknull)] # nulls checks
if(length(null)==0)
  print("There are no NULLS") else
    print(paste("NULLS in column",null))

checkzero=function(x)return(any(x<=0)) #check zero
zero=colnames(real_estate)[apply(real_estate,2,checkzero)]
if(length(zero)==0)
  print("Rhere are no Zeros") else
    print(paste("Zeros in column:",zero))

library(corrplot) # check the multicollinearity
corr=cor(real_estate)
corrplot(corr,type = 'lower',method = 'number')

# Check for the outliers by histogram and boxplot 

for(c in colnames(real_estate))
{
  title=paste("histogram for",c)
  hist(unlist(real_estate[c]),main = title,col = "red")  #unlist is imp for histogram
} 

for(a in colnames(real_estate))
{
  title=paste("boxplot for",a)
  boxplot(real_estate[a],main = title,col = "red",horizontal = T) #we dont unlist data in boxplot
} 

#check for Y-distribution
table(real_estate$Y.house.price.of.unit.area)

#split the data into train and test
total=nrow(real_estate)
r=sample(seq(1,total),0.7*total)
train=real_estate[r,]
test=real_estate[-r,]
print(paste(dim(train), dim(test)))

#model 1

m1 = lm(Y.house.price.of.unit.area~., data = train) # model created with all the features
summary(m1) # Summary of the model it states that longitude is not significant so lets remove it and build again

m1 = lm(Y.house.price.of.unit.area~. - X6.longitude, data = train)
summary(m1) # it shows that now all the remaining features are imp

# prediction of the model 1

p1 = predict(m1,test)

# now compare the predicted and actual output we can use plot
plot(test$Y.house.price.of.unit.area,type = "l",lty = 1.8,col = "green")
lines(p1,type = "l",col = "blue")

# now we can see that the model have predicted quite currect






