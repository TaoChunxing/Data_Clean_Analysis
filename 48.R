data.orig <- read_excel("orig.all.baselineLip.xlsx")


######1.3.2 剔除is.na(Dyslip), 剔除TC,TG基线都为0的异常值####
data.orig <- data.orig %>% 
  group_by(ID) %>%
  filter(!(is.na(Dyslip))) %>% 
  filter(!(TG == 0) | !(TC == 0))


data.ATRtime <- read_excel("orig.all.info.xlsx",
                           sheet = "Sheet2")


data.orig <- data.orig %>% left_join(data.ATRtime, by = "ID")



######6.1.3 新建TimefromATR#####
data.hlme.time <- data.orig %>%
  mutate(
    TimefromART = round(interval(start = ARTinitialDate, end = TestingDate) / months(1), 
                        digits = 2)
  ) %>% 
  filter(TimefromART > 0) %>% 
  filter(TimefromART < 96) %>% 
  group_by(ID) %>% 
  mutate(ID.count = n()) %>% 
  filter(ID.count > 3) %>% 
  select(-ID.count)




#删除cd4cd8比值异常的
data.hlme <- data.hlme.time %>%
  filter(!(cd4cd8Ratio < 0.1)) %>%
  filter(!(cd4cd8Ratio > 5))



######1.3.3 新建基线血脂“baselineLIP”####
data.hlme <- data.hlme %>%
  group_by(ID) %>% #第一次检测日期下血脂异常为“Y”的记为基线血脂异常
  mutate(baselineLIP = if_else(first(Dyslip) == "N", "N", "Y"))


######1.3.5 移除基线血脂异常的人####
data.hlme <- data.hlme %>%
  filter(baselineLIP != "Y")





#####重新定义血脂异常：发生两次Dyslip####
data.hlme <- data.hlme %>%
  group_by(ID) %>%
  mutate(Dyslip.count = sum(Dyslip == "Y")) %>%
  mutate(Dyslip.x = if_else(Dyslip.count >= 2, 1, 0))


#数据记录保留至第二次发生血脂异常
data.hlme<- data.hlme %>%
  arrange(ID, TestingDate) %>% 
  group_by(ID) %>%
  mutate(first_Y = cumsum(Dyslip == "Y")) %>%
  filter(first_Y == 0 | row_number() <= which(first_Y == 2)[1]) %>%
  select(-first_Y)


#剔除少于3次ID记录的数据
data.lessthan3 <- data.hlme %>%
  group_by(ID) %>%
  filter(n() < 3) %>%
  ungroup()
data.hlme <- anti_join(data.hlme, data.lessthan3, by = "ID")

data.Morethan10 <- data.hlme %>% 
  group_by(ID) %>% 
  filter(n() > 10)
data.hlme <- anti_join(data.hlme, data.Morethan10, by = "ID")


#剔除Time大于48的数据（4年）
data.hlme <- data.hlme %>% 
  filter(TimefromART< 48)



#简化数据框
data.hlme <- data.hlme %>% 
  select(2, 8, 11, 14) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  mutate(Subject = cur_group_id())

#gmm模型
library(lcmm)

model.1 <- hlme(fixed = cd4cd8Ratio ~ 1 + TimefromART + I(TimefromART^2),
                random = ~ 1 + TimefromART,
                ng = 1, 
                data = data.hlme, 
                subject = "Subject")
summary(model.1)

lin <- c(model.1$ng, model.1$BIC)
model <- list(model.1)
for (i in 2:3) {
  mi <- hlme(fixed = cd4cd8Ratio ~ 1 + TimefromART + I(TimefromART^2),
             mixture = ~ 1 + TimefromART + I(TimefromART^2),
             random = ~ 1 + TimefromART,
             ng = i,
             nwg = TRUE, 
             B = model.1,
             idiag = FALSE, 
             maxiter = 50,
             data =data.hlme, 
             subject = "Subject")
  lin <- rbind(lin, c(i, mi$BIC))
  model[[i]] <- mi
}

modelout <- knitr::kable(lin, col.names = c("k", "BIC"), row.names = FALSE, align = "c")
modelout
#summary
model[[2]]
summary(model[[2]])


#可视化
windows(width=10, height=8)

datnew  <- data.frame(TimefromART = seq(0,50, length = 1000))

plotpred <- predictY(model[[2]], datnew, var.time ="TimefromART")

plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="TimefromART (Months)", ylab="cd4cd8Ratio", 
     legend.loc = "topleft", cex=0.75)

##提取subject对应的class
class.data <- model[[2]]$pprob[,1:2]


#后验概率
postprob(model[[2]])






data.unique <- data.hlme %>% distinct(ID, .keep_all = TRUE)
data.unique.class <- data.unique %>% left_join(class.data, by = "Subject")
write.xlsx(data.unique.class, "data.unique.class.b.xlsx")

