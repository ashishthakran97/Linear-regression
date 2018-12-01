#Importing the file
file <- readxl::read_xlsx('E:/ANALYTIXLABS/DATA SCIENCE USING R/linear regression cs/Linear Regression Case/Linear Regression Case.xlsx')
summary(file) ##Descriptive statistics

#UDF to see the data distributon
mystat <- function(x){
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  mean <- mean(a)
  l <- length(a)
  sd <- sd(a)
  min <- min(a)
  p1 <- quantile(a,.01)
  p5 <- quantile(a,.05)
  p10 <- quantile(a,.10)
  p25 <- quantile(a,.25)
  p50 <- quantile(a,.50)
  p75 <- quantile(a,.75)         
  p90 <- quantile(a,.90)
  p95 <- quantile(a,.95)
  p99 <- quantile(a,.99)
  max <- max(a)
  uc <- mean+3*sd
  lc <- mean-3*sd
  outlier <- max>p95|min<p5
  return(c(nmiss=nmiss,mean=mean,lenth=l,sd=sd,min=min,p1=p1,p5=p5,p10=p10,p25=p25,p50=p50,p75=p75,p90=p90,p95=p95,p99=p99,
           max=max,uc=uc,lc=lc,outlier=outlier))
  
}

#Applying UDF using sapply         
daig_stats <- t((sapply(file[,c(2:6,8:132)],mystat))) 
write.csv(daig_stats,'daig_stats.csv')



#outlier treatment  
file$age[file$age>76] <- 76 
file$ed[file$ed>20] <- 20
file$employ[file$employ>31] <-31 
file$income[file$income>147] <-147 
file$lninc[file$lninc>4.990432587] <- 4.990432587
file$debtinc[file$debtinc>22.2] <-22.2 
file$creddebt[file$creddebt>6.3730104] <- 6.3730104


file$lncreddebt[file$lncreddebt>1.852297333] <- 1.852297333

file$othdebt[file$othdebt>11.8159808] <- 11.8159808

file$lnothdebt[file$lnothdebt>2.469586375] <- 2.469586375

file$spoused[file$spoused>18] <- 18
file$spousedcat[file$spousedcat>4] <-4 
file$reside[file$reside>5] <-5 
file$pets[file$pets>10] <- 10
file$pets_cats[file$pets_cats>2] <- 2
file$pets_dogs[file$pets_dogs>2] <- 2
file$pets_birds[file$pets_birds>1] <- 1
file$pets_reptiles[file$pets_reptiles>0] <- 0
file$pets_small[file$pets_small>1] <- 1
file$pets_saltfish[file$pets_saltfish>0] <- 0
file$pets_freshfish[file$pets_freshfish>8] <- 8
file$address[file$address>40] <- 40
file$cars[file$cars>4] <- 4
file$carvalue[file$carvalue>72] <- 72
file$commute[file$commute>8] <- 8
file$commutecat[file$commutecat>4] <- 4
file$commutetime[file$commutetime>35] <- 35
file$polview[file$polview>6] <- 6
file$card[file$card>4] <- 4
file$cardtenure[file$cardtenure>38] <- 38
file$card2tenure[file$card2tenure>29] <- 29
file$carditems[file$carditems>16] <- 16
file$cardspent[file$cardspent>782.3155] <- 782.3155

file$card2items[file$card2items>9] <- 9
file$card2spent[file$card2spent>419.447] <- 419.447

file$longmon[file$longmon>36.7575] <- 36.7575

file$lnlongmon[file$lnlongmon>3.604341892] <- 3.604341892

file$longten[file$longten>2567.65] <- 2567.65

file$lnlongten[file$lnlongten>7.850744761] <- 7.850744761

file$tollmon[file$tollmon>43.5] <- 43.5


file$lntollmon[file$lntollmon>3.926911618] <- 3.926911618


file$tollten[file$tollten>2620.2125] <- 2620.2125


file$lntollten[file$lntollten>8.106641625] <- 8.106641625


file$equipmon[file$equipmon>49.0525] <- 49.0525

file$lnequipmon[file$lnequipmon>4.065473393] <- 4.065473393


file$equipten[file$equipten>2600.99] <- 2600.99


file$lnequipten[file$lnequipten>8.117630841] <- 8.117630841

file$cardmon[file$cardmon>42] <- 42
file$lncardmon[file$lncardmon>3.839452313] <- 3.839452313
file$cardten[file$cardten>2455.75] <- 2455.75
file$lncardten[file$lncardten>7.923257452] <- 7.923257452

file$wiremon[file$wiremon>51.305] <- 51.305
file$lnwiremon[file$lnwiremon>4.267281665] <- 4.267281665
file$wireten[file$wireten>2687.9225] <- 2687.9225
file$lnwireten[file$lnwireten>8.310816686] <- 8.310816686
file$hourstv[file$hourstv>28] <- 28

file$age[file$age<20] <- 20
file$edcat[file$ed<9] <- 9
file$income[file$income<13] <-13 
file$lninc[file$lninc<2.564949357] <- 2.564949357
file$debtinc[file$debtinc<1.9] <- 1.9
file$creddebt[file$creddebt<0.101088] <- 0.101088
file$othdebt[file$othdebt<0.2876923] <- 0.2876923
file$commutetime[file$commutetime<16] <- 16
file$polview[file$polview<2] <- 2
file$cardspent[file$cardspent<91.3045] <- 91.3045
file$card2spent[file$card2spent<14.8195] <- 14.8195
file$tenure[file$tenure<4] <- 4
file$longmon[file$longmon<2.9] <- 2.9
file$longten[file$longten<12.62] <- 12.62
file$hourstv[file$hourstv<12] <- 12




#missing value treatment


#file=na.omit(file$name)     remove the rows with missing values.

require(Hmisc)

mydt3 <- data.frame(sapply(file[,c(2:6,8:132)],function(x) impute(x,mean)))    #imputing missing values
summary(mydt3)
apply(is.na(mydt3[]),2,sum) #to check the missing values
mydt3$totalcard_spent <- mydt3$cardspent+mydt3$card2spent
-which(colnames(mydt3)=='cardspent')
-which(colnames(mydt3)=='card2spent')
mydt3 <- mydt3[,-c(77,79)]

#rm(mydt3)

#checking normality
hist(log(mydt3$totalcard_spent))

mydt3$totalcard_spent <- log(mydt3$totalcard_spent)






# selecting the variables using anova
ANOVA_segment <- aov(mydt3$totalcard_spent~region+townsize+gender+ agecat+edcat+jobcat+ union+
                     
                     employ+  empcat+ retire+ inccat+default+ jobsat+ marital+ spousedcat+ homeown+hometype+
                       
                       address+     addresscat+cars+carown+ cartype+ carcatvalue+carbought+ carbuy+commute+
                       commutecat+commutecar+commutemotorcycle+commutecarpool+ commutebus+commuterail+
                       commutepublic+ commutebike+ commutewalk+commutenonmotor+telecommute+ reason+ polview+
                       
                       polparty+polcontrib+ vote+card+cardtype+cardbenefit+cardfee+cardtenure+ 
                       cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+ card2tenurecat+active+bfast+
                       churn+tollfree+equip+callcard+wireless+ multline+voice+ pager+internet+ callid+ 
                       callwait+forward+confer+ebill+owntv+ownvcr+owndvd+ owncd+ownpda+ownpc+ownipod+owngame+ ownfax+
                       news+response_01+ response_02+response_03,data=mydt3)  
                     
summary(ANOVA_segment)

#Building model using lm()
fit <- lm(mydt3$totalcard_spent~mydt3$region+mydt3$gender+mydt3$agecat+mydt3$edcat+
            mydt3$employ+mydt3$empcat+mydt3$retire+mydt3$inccat+mydt3$card+mydt3$card2+mydt3$internet+mydt3$ownvcr+mydt3$owndvd+mydt3$response_03+
            
         
          age+
          
          ed+
         
          
          
          lninc+
          
          debtinc+
         
          lncreddebt+
          
          lnothdebt+
         
         
          spoused+
          
          reside+
          pets+
          pets_cats+
          pets_dogs+
          pets_birds+
          pets_reptiles+
          pets_small+
          pets_saltfish+
          pets_freshfish+
          
          cars+
         
          carvalue+
         
          commutetime+
         
          
          
      
         
          tenure+
          
          
          lnlongmon+
          
          lnlongten+
          
        
          lntollmon+
         
          lntollten+
         
         
          lnequipmon+
         
          lnequipten+
         
         
          lncardmon+
          
          lncardten+
         
         
          lnwiremon+
          
          lnwireten+
          
         
         
          hourstv
          ,data=mydt3)

summary(fit)

require(car)
require(MASS)

vif(fit)  #to check multicollinearity

#converting into factors
mydt3$gender <- factor(mydt3$gender)
mydt3$card <- factor(mydt3$card)
mydt3$card2 <- factor(mydt3$card2)
mydt3$internet <- factor(mydt3$internet)

#splitting the data into training,validation and testing
set.seed(12345)
train_ind <- sample(1:nrow(mydt3),size = floor(.70*nrow(mydt3)))


train <- mydt3[train_ind,]
test <- mydt3[-train_ind,]

#step <- stepAIC(fit,direction = 'both')


fit1 <-lm(totalcard_spent ~  gender + card + 
            card2 +  
            age  + lninc  
            ,data= train)

summary(fit1)
#checking multicollinearity
vif(fit1)



#removing influential obsn.
train$cd <- cooks.distance(fit1)
train1 <- subset(train,cd<(4/3500))


fit2 <-lm(totalcard_spent ~  gender + card + 
            card2 +   
            age  + lninc  
          ,data= train1)
summary(fit2)  #adjusted R square is 37.67% ##########

train1$cd1<-cooks.distance(fit2)
train2<-subset(train1,cd1<(4/3377))

fit3 <-lm(totalcard_spent ~  gender + card + 
            card2 +   
            age  + lninc  
          ,data= train2)
summary(fit3) ##adjusted R square 40.39%

#predicting on training
t1 <- cbind(train,pred=exp(predict(fit2,train)))
names(t1)
t1$pred   

t1<- transform(t1, APE = abs(t1$pred -(t1$totalcard_spent)/(t1$totalcard_spent)))
mean(t1$APE)
#predicting on testing
t2<-cbind(test, pred=exp(predict(fit2,test)))
t2<- transform(t2, APE = abs(t2$pred -(t2$totalcard_spent)/(t2$totalcard_spent)))
mean(t2$APE)


#decile analysis report
deloc <- quantile(t1$pred,prob=seq(.1,.9,by=.1))
t1$decile <- findInterval(t1$pred,c(-Inf,deloc,Inf))

require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred) as avg_pre_spend,   
               avg(totalcard_spent) as avg_Actual_spend
               from t1
               group by decile
               order by decile desc")
write.csv(t1_DA,'t1_da.csv')

deloc <- quantile(t2$pred,prob=seq(.1,.9,by=.1))
t2$decile <- findInterval(t2$pred,c(-Inf,deloc,Inf))


t2_DA <- sqldf("select decile, count(decile) as count, avg(pred) as avg_pre_spend,   
               avg(totalcard_spent) as avg_Actual_spend
               from t2
               group by decile
               order by decile desc")
write.csv(t2_DA,'t2_da.csv')




coefficients(fit3) # model coefficients 



# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
 plot(fit2)
############END OF THE CASE STUDY############################

 
#####ROUGH WORK############ 
# train$cd<-cooks.distance(fit3)
# train1<- subset(train, cd<(4/3500(N)))
#Removing influential observation(Cook\'s D) & updating model
w <- abs(rstudent(fit1)) < 3 & abs(cooks.distance(fit1)) < 4/nrow(fit$model) 

fit2 <- update(fit1, weights=as.numeric(w))

fit7 <- lm(dep~(card==2)+(card2==3)



















