> setwd("~/bio/Pattern_Recognition_and_Prediction/!expr/paper/data/hcc-survival")
> hcc.data <- read.csv('hcc-data.txt')
> for (r in 1:164) {
    for (c in 1:50) {
        if (hcc.data[r,c]=='?') {
            hcc.data[r,c] <- NA
        }
    }
}
> colnames(hcc.data) = paste("A", 1:50, sep = "")
> hcc.data.deleted <- hcc.data[-delete.case,]
> hcc.data.deleted <- hcc.data.deleted[,-delete.fac]

#> library(mice)
#> md.pattern(hcc.pred, rotate.names = TRUE)
> library(VIM)
> aggr(hcc.pred, prop=TRUE, numbers=TRUE)

############numeric
> hcc.complete.num <- apply(hcc.complete.num[,1:50],2,as.numeric)

> hcc.complete.num.part <- hcc.complete.num[,24:50]


########complete数据主成分分析
> fa.parallel(hcc.complete.num.part[,1:26], fa='pc', n.iter=10, show.legend=FALSE, main='Scree plot with parallel analysis of complete cases')
######根据碎石图factors=6
> principal(r = hcc.complete.num.part[,1:26], nfactors = 6, scores = TRUE)

#######缺失值的相关系数
> hcc.na <- as.data.frame(abs(is.na(hcc.pred.coln)))
> hcc.pred.cor <- cor(hcc.na[which(apply(hcc.na, 2, sum)>0)])
> write.table(hcc.pred.cor, 'hcc.pred.cor.txt')
> library(pheatmap)
> pheatmap(hcc.pred.cor, border=FALSE, show_rownames=T, cluster_rows = TRUE, cluster_cols = TRUE)
###########含缺失值变量与其他可观测变量间的关系
> hcc.pred.cor.r <- cor(hcc.na, hcc.na[which(apply(hcc.na, 2, sum)>0)], use='pairwise.complete.obs')
Warning message:
In cor(hcc.na, hcc.na[which(apply(hcc.na, 2, sum) > 0)],  :
  标准差为零
#####相关系数矩阵，行为可观测变量，列为表示缺失的指示变量
> write.table(hcc.pred.cor.r, 'hcc.pred.cor.r.txt')
######[,24:50]变量间的相关系数矩阵
> hcc.complete.cor.p <- cor(hcc.complete.num[,24:50])
> pheatmap(hcc.complete.cor.p, border=FALSE, show_rownames=T, cluster_rows = TRUE, cluster_cols = TRUE)

####pMiss
> pMiss <- function(x){sum(is.na(x))/length(x)*100}
> apply(hcc.pred, 2, pMiss)
> apply(hcc.pred, 1, pMiss)



> hcc.pred.coln <- hcc.data[,-c(50)]
#####添加列名
> colnames(hcc.pred.coln) = paste("A", 1:49, sep = "")

########查过文献,对于在10~20%的比例范围内均可使用,10%以下更不用说,一般使用多重插补是针对多属性或高维数据有用,但有类别数据除外
> delete.case <- which(apply(hcc.pred.coln, 1, pMiss)>15)
> hcc.predict.coln_new <- hcc.pred.coln[-delete.case,]
> delete.fac <- which(apply(hcc.pred.coln, 2, pMiss)>15)
> hcc.predict.coln_new <- hcc.predict.coln_new[,-delete.fac]

#############缺失值的相关系数
> hcc.predict.coln.na <- as.data.frame(abs(is.na(hcc.predict.coln)))
> hcc.predict.coln.na.cor <- cor(hcc.predict.coln.na[which(apply(hcc.predict.coln.na, 2, sum)>0)])
> pheatmap(hcc.predict.coln.na.cor, border=FALSE, show_rownames=T, cluster_rows = TRUE, cluster_cols = TRUE)


###乙型肝炎表面抗原(Hepatitis B Surface Antigen)、乙型肝炎核心抗体(Hepatitis B Core Antibody)、丙型肝炎病毒抗体(Hepatitis C Virus Antibody) 这3个特征列存在缺失值，但它们作为临床诊断非常重要的指标，不宜剔除，因此剔除涉及缺失这3个特征的个案
> delete.case.2 <- which(is.na(hcc.data.deleted$A4))
> hcc.data.deleted.2 <- hcc.data.deleted[-delete.case.2,]
> delete.case.2 <- which(is.na(hcc.data.deleted.2$A6))
> hcc.data.deleted.2 <- hcc.data.deleted.2[-delete.case.2,]
> delete.case.2 <- which(is.na(hcc.data.deleted.2$A7))
> hcc.data.deleted.2 <- hcc.data.deleted.2[-delete.case.2,]
#########剔除存在缺失值的定性特征列
> hcc.data.deleted.20 <- subset(hcc.data.deleted.2, ,select=-c(A2,A11,A12,A13,A16,A17,A19,A20,A21,A22))
#########保存剔除结果
> write.csv(hcc.data.deleted.2, 'hcc.data.deleted.2.csv')
> hcc.data.deleted.1 <- read.csv('hcc.data.deleted.2.csv', header = TRUE)


> hcc.predict.coln.numr <- apply(hcc.predict.coln[,1:39],2,as.numeric)
> nonimpute <- hcc.predict.coln.numr




> imp.rf <- mice(hcc.data.deleted.1, meth = "rf", ntree = 10)
> imp.cart <- mice(hcc.data.deleted.1, meth = "cart")

########分类回归树(CART, Classification And Regression Tree)也属于一种决策树
Imputation of y by classification and regression trees. The procedure is as follows:

Fit a classification or regression tree by recursive partitioning;

For each ymis, find the terminal node they end up according to the fitted tree;

Make a random draw among the member in the node, and take the observed value from that draw as the imputation.

#######插补结果可视化
> stripplot(imp.rf, col=c("grey",mdc(2)),pch=c(1,20))
> stripplot(imp.cart, col=c("grey",mdc(2)),pch=c(1,20))

########经评估，选3
> fit=with(imp.cart,lm(A24 ~ A30))
> summary(fit)
> pool(fit)
#####
> imputed <- complete(imp.cart, 3)
> write.csv(imputed, 'imputed_mice.cart_3.csv')
##> fa.parallel(imputed[,1:29], fa='pc', n.iter=10, show.legend=FALSE, main='Scree plot with parallel analysis of imputed')
##Parallel analysis suggests that the number of factors =  NA  and the number of components =  6 
## 10