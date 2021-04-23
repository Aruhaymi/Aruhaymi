- 👋 Hi, I’m @Aruhaymi
- 👀 I’m interested in review my R code for missing data MAR missingness

- 🌱 I’m currently learning R for missing data
- 💞️ I’m looking to collaborate on MAR CODE IN r FOR MISSING DATA
- 📫 How to reach me 202 813 7169 OR EMAIL azmotairi@hotmail.com

<!---
Aruhaymi/Aruhaymi is a ✨ special ✨ repository because its `##########################
library("rpart")
library("randomForest")
library("e1071")
library("caret")
library("caTools")
KDDsubset <- read.csv("C:/Users/azmot/OneDrive/Desktop/full_unprocessed_data.csv")
KDDsubset <- subset(KDDsubset,select = -c(num_outbound_cmds,is_host_login))
#before<-KDDsubset
label<-KDDsubset$label
#before$label<-label
#before$label<-as.factor(before$label)
set.seed(123)
sample<- sample.split(KDDsubset$label,SplitRatio = .66)
train<-subset(KDDsubset,sample==TRUE)
test<-subset(KDDsubset,sample==FALSE)
train_KDDsubset<-train[1:326047,]
train_label<-train_KDDsubset$label
train_KDDsubset<- subset(train_KDDsubset, select = -c(label))
KDDsubset<-train_KDDsubset
nr<-nrow(KDDsubset)
nc<-ncol(KDDsubset)
k<-ncol(KDDsubset) 
pattern<-matrix(1,k,k)
for (i in 1:k){
  pattern[i,sample(1:39,15)]<-0
}   
frq<-rep(1/k,k)  
nc<-k 
nr<-nrow(KDDsubset) # number of units
weight<-pattern #WEIGHTED SUM OF Score,The WSS is the outcome of a linear regression equation 
 
KDDsubset<-KDDsubset[1:494013,] # exactly multiple of k (I remove the last 8 obs)# IF I DONT WANT TO REMOVE OBS, I HAVE TO CHANGE freq!
nr<-nrow(KDDsubset)
weightedSS<-function(KDDsubset, patterns, weights, freq){  #split data according to frequency p.2915
  dfSpliter<-split(1:nrow(KDDsubset), sample(rep(1:nrow(patterns), round(freq*nrow(KDDsubset),0)))) 
  dfList<-list()
  for(i in 1:nrow(patterns)){
    dfList[[i]]<-KDDsubset[which(rownames(KDDsubset)%in%dfSpliter[[i]]),]
    dfMatrix<-as.matrix(dfList[[i]])
    df<-dfList[[i]] # randomizing weights #  weight <- sample(1:nrow(pattern),1)
    df$score<-as.vector(dfMatrix%*%weights[i,])
    dfList[[i]]<-df
  } 
  return(dfList)
} 
 
dfFinalList<-list()
dfSubList<-weightedSS(KDDsubset, pattern, weight, frq)# then we do MAR Missingness
prop<-.1
ListCase=FALSE

for (i in 1:nrow(pattern)) {
  dfTemp <- dfSubList[[i]]
  meanScores <- mean(dfTemp$score) # this line enables me to use logistic probability which depend on mean and scale 
  if(ListCase==FALSE){
    shift<-rnorm(mean=.5-(nc*prop/sum(pattern[i,]==0)),sd=1,nc*nr)  #shift<-rnorm(mean=prop,sd=1,30000)
    shift<-mean(scale(shift)) 
    # pMarginal <- 1 -plogis(scale(meanScores - dfTemp$score)+log ((-nc*prop)/((nc*prop-1)))+(nc*prop-.5)+shift)
    pMarginal <- 1 -plogis(scale(meanScores - dfTemp$score)+log ((-nc*prop/sum(pattern[i,]==0))/((nc*prop/sum(pattern[i,]==0)-1)))+(nc*prop/sum(pattern[i,]==0)-.5)+shift)
    
  }else{
    
    shift<-rnorm(mean=.5-prop,sd=1,nc*nr)
    shift<-mean(scale(shift))
    pMarginal <-1 -plogis(scale(meanScores - dfTemp$score)+log((-prop)/((prop-1)))+(prop-.5)+shift)
  }
  
  dfTemp$score <- NULL # draw 1,0 from binomial distribution using marginal probability
 
  r.marright <- rbinom(nrow(dfTemp), 1, pMarginal)
  dfTemp[r.marright==0, pattern[i,]==0]<-NA
  dfFinalList[[i]] <- dfTemp
}
mergedDf <- do.call("rbind", dfFinalList)
#mergedDf<- replace (mergedDf,is.na(mergedDf),-9999)

sum(is.na(mergedDf))
 

