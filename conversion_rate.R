# Conversion rate
#libraries needed 
library(dplyr)  
library(rpart) 
library(ggplot2) 
library(randomForest)
#Let's read the dataset into R.
data = read.csv('C:/DSLA/conversion_rate/conversion_data.csv')
#We should get something like this:
head(data)
str(data)
summary(data)
sort(unique(data$age), decreasing=TRUE)
#Those 123 and 111 values seem unrealistic. How many users are we talking about:
subset(data, age>79)
#It is just 2 users! In this case, we can remove them, nothing will change. 
#In general, depending on the problem: remove the entire row saying you don't trust the data treat those values as NAs if there is a pattern, 
#try to figure out what went wrong. In doubt, always go with removing the row. It is the safest choice.

#Anyway, here is probably just users who put wrong data. So let's remove them:
data = subset(data, age<80)

#To get sense of data Let's just pick a couple of vars as an example, 

data_country = data %>%                             
  group_by(country) %>%                                
  summarise(conversion_rate = mean(converted))   
  ggplot(data=data_country, aes(x=country, y=conversion_rate)) + geom_bar(stat = "identity", aes(fill = country))
# Here it clearly looks like Chinese convert at a much lower rate than other countries!

data_pages = data %>%  
  group_by(total_pages_visited) %>%    
  summarise(conversion_rate = mean(converted)) 
qplot(total_pages_visited, conversion_rate, data=data_pages, geom="line")
  
# Definitely spending more time on the site implies higher probability of conversion

#############Machine Learning ##########################################################

# Let's now build a model to predict conversion rate. Outcome is binary and you care about insightsto give product and marketing team some ideas. You should probably choose among thefollowing models:

#Logistic regression 
#Decision Trees 
#RuleFit (this is often your best choice)
#Random Forest in combination with partial dependence plots

# Firstly, "Converted" should really be a factor here as well as new_user. So let's change them:

data$converted = as.factor(data$converted)  # let's make the class a factor 
data$new_user = as.factor(data$new_user) #also this a factor
levels(data$country)[levels(data$country)=="Germany"]="DE" # Shorter name, easier to plot.
# Create test/training set with a standard 66% split (if the data were too small, I would cross-validate) and then build the forest with standard values for the 3 most important parameters (100 trees, trees as large as possible, 3 random variables selected at each split).

train_sample = sample(nrow(data), size = nrow(data)*0.66) 
train_data = data[train_sample,] 
test_data = data[-train_sample,] 
rf = randomForest(y=train_data$converted, x = train_data[, -ncol(train_data)],  ytest = test_data$converted, xtest = test_data[, -ncol(test_data)],ntree = 100, mtry = 3, keep.forest = TRUE) 
rf
#OOB error and test error are pretty similar: 1.5% and 1.4%. We are confident we are not overfitting. Error is pretty low. However, we started from a 97% accuracy (that's the case if we classified everything as "non converted"). So, 98.5% is good, but nothing shocking. Indeed, 30% of conversions are predicted as "non conversion". 

# Let's start checking variable importance:

varImpPlot(rf,type=2)
# Total pages visited is the most important variance
# Unfortunately, it is probably the least "actionable". People visit many pages cause they already want to buy. Also, in order to buy you have to click on multiple pages. 

#Let's rebuild the RF without that variable. Since classes are heavily unbalanced and we don't have that very powerful variable anymore, let's change the weight a bit, just to make sure we will get something classified as 1.

rf = randomForest(y=train_data$converted, x = train_data[, -c(5, ncol(train_data))], 
                  ytest = test_data$converted, 
                  xtest = test_data[, -c(5, ncol(train_data))],ntree = 100, mtry = 3,keep.forest = TRUE, classwt = c(0.7,0.3)) 
rf
#Accuracy went down, but that's fine. The model is still good enough to give us insights. 

#Let's recheck variable importance:
varImpPlot(rf,type=2)
#Interesting! New user is the most important one. Source doesn't seem to matter at all. 

#Let's check partial dependence plots for the 4 vars:
op <- par(mfrow=c(2, 2)) 
partialPlot(rf, train_data, country, 1) 
partialPlot(rf, train_data, age, 1) 
partialPlot(rf, train_data, new_user, 1) 
partialPlot(rf, train_data, source, 1)

#In partial dependence plots, we just care about the trend, not the actual y value. So this shows that: 
#Users with an old account are much better than new users 
#China is really bad, all other countries are similar with Germany being the best 
#The site works very well for young people and bad for less young people (>30 yrs old) 
#Source is irrelevant 

#Let's now build a simple decision tree and check the 2 or 3 most important segments:

tree = rpart(data$converted ~ ., data[, -c(5,ncol(data))],  control = rpart.control(maxdepth = 3),parms = list(prior = c(0.7, 0.3))) 
tree

# Some conclusions and suggestions: 
#1. The site is working very well for young users. Definitely let's tell marketing to advertise and use marketing channel which are more likely to reach young people. 
#2. The site is working very well for Germany in terms of conversion. But the summary showed that there are few Germans coming to the site: way less than UK, despite a larger population. Again, marketing should get more Germans. Big opportunity. 
#3. Users with old accounts do much better. Targeted emails with offers to bring them back to the site could be a good idea to try. 
#4. Something is wrong with the Chinese version of the site. It is either poorly translated, doesn't fit the local culture, some payment issue or maybe it is just in English! Given how many users are based in China, fixing this should be a top priority. Huge opportunity. 
#5. Maybe go through the UI and figure out why older users perform so poorly? From 30 y/o conversion clearly starts dropping. #6. If I know someone has visited many pages, but hasn't converted, she almost surely has high purchase intent. I could email her targeted offers or sending her reminders. Overall, these are probably the easiest users to make convert. 

# As we can see, conclusions usually end up being about: 
# 1. tell marketing to get more of the good performing user segments 
# 2. tell product to fix the experience for the bad performing ones


  





  



