df=read.csv('train.csv')
sp=df['SalePrice']
View(df)
l=lm(SalePrice~LotArea+OverallQual,data=df)
summary(l)
LA=df['LotArea']
OQ=df['OverallQual']
pre=1.45*LA+44328.49*OQ-104702.66
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
for(i in 1:nrow(sp))
{o=sp[i,]
m=pre[i,]
r=c(RMSE(m,o),r)
}
trial=matrix(r, ncol=1)
colnames(trial)=c('rmse')
trial.table=as.table(trial)
trial=matrix(1:nrow(trial.table), ncol=1)
colnames(trial)=c('id')
trial.table1=as.table(trial)
cbind(trial.table1,trial.table)
plot(r,col='skyblue')
hist(r,col='red')
boxplot(r)
