#加载包
library(cmprsk)
rm(list = ls())
data("bmtcrr", package = "casebase")
str(bmtcrr)

####1.Fine-Grey检验（单因素分析）####
bmtcrr$Status <- factor(bmtcrr$Status)
f <- cuminc(bmtcrr$ftime, bmtcrr$Status, bmtcrr$D) #cumulative incidence
f#结果中1代表结局事件，2代表竞争风险事件，第一行P表示在控制了2后两组1的累计发生风险





####2.图形展示结果####
plot(f, xlab = "Month", ylab = "CIF", lwd = 2, Ity = 1,
     col = c("red", "blue", "black", 'forestgreen'))

####3.用ggplot2重新绘图####
library(ggplot2)
#提取数据
ALL1 <- data.frame(ALL1_t = f[[1]][[1]], ALL1_C = f[[1]][[2]])
AML1 <- data.frame(AML1_t = f[[2]][[1]], AML1_C = f[[2]][[2]])
ALL2 <- data.frame(ALL2_t = f[[3]][[1]], ALL2_C = f[[3]][[2]])
AML2 <- data.frame(AML2_t = f[[4]][[1]], AML2_C = f[[4]][[2]])
ggplot()+
  geom_line(data = ALL1, aes(ALL1_t,ALL1_C))+
  geom_line(data = ALL2, aes(ALL2_t,ALL2_C))+
  geom_line(data = AML1, aes(AML1_t,AML1_C))+
  geom_line(data = AML2, aes(AML2_t,AML2_C))+
  labs(x="month",y="cif")+
  theme_bw()
#美化
tmp <- data.frame(month = c(ALL1$ALL1_t,AML1$AML1_t,ALL2$ALL2_t,AML2$AML2_t),
                  cif = c(ALL1$ALL1_C,AML1$AML1_C,ALL2$ALL2_C,AML2$AML2_C),
                  type = rep(c("ALL1","AML1","ALL2","AML2"), c(58,58,58,88))#每种类型对应的观测数
)

ggplot(tmp, aes(month, cif))+
  geom_line(aes(color=type, group=type),linewidth=1.2)+#线条颜色和分组由 type 列控制
  theme_bw()+#应用了一个简洁的黑白主题
  theme(legend.position = "top")







####4.竞争风险模型（多因素分析）####
covs <- subset(bmtcrr, select = - c(ftime,Status))#提取列
covs[,c(1:3,5)] <- lapply(covs[,c(1:3,5)],as.integer)#修改数据格式为整数型
str(covs)
#构建竞争风险模型
f2 <- crr(bmtcrr$ftime, bmtcrr$Status, covs, failcode = 1, cencode = 0)
#failcode指定结局事件，cencode指定截尾事件，其他默认为竞争风险事件2
summary(f2)
