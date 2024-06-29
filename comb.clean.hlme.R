all.indicator <- read_excel("orig.all.baselineLip.xlsx")
str(all.indicator)

write.xlsx(all.indicator, "all.indicator.xlsx")

######1.3.2 剔除is.na(Dyslip), 剔除TC,TG基线都为0的异常值####
all.indicator.DyslipNonNA <- all.indicator %>% 
  group_by(ID) %>%
  filter(!(is.na(Dyslip))) %>% 
  filter(!(TG == 0) | !(TC == 0))

write.xlsx(all.indicator.DyslipNonNA, "all.indicator.DyslipNonNA.xlsx")


######
data.hlme <- all.indicator.baselineNormLIP %>% left_join(all.info, by = "ID")

######6.1.3 新建TimefromATR#####
data.hlme<- data.hlme %>%
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

data.hlme <- data.hlme %>% select(-9,-10)

#删除cd4cd8比值异常的
data.hlme <- data.hlme %>%
  filter(!(cd4cd8Ratio < 0.1)) %>%
  filter(!(cd4cd8Ratio > 5))



######1.3.3 新建基线血脂“baselineLIP”####
data.hlme <- data.hlme %>%
  group_by(ID) %>% #第一次检测日期下血脂异常为“Y”的记为基线血脂异常
  mutate(baselineLIP = if_else(first(Dyslip) == "N", "N", "Y"))

write.xlsx(all.indicator.baselineLIP,"all.indicator.baselineLIP.xlsx")


######1.3.4 统计基线血脂正常与异常的人数####
count.Basedyslip <- data.hlme %>% 
  group_by(baselineLIP) %>% 
  summarise(UniqueIDCount = n_distinct(ID))
count.Basedyslip 


######1.3.5 移除基线血脂异常的人####
data.hlme <- data.hlme %>%
  filter(baselineLIP != "Y")


all.info <- read_excel("orig.all.info.xlsx",
                       sheet = "Sheet2")
str(all.info)



#发生血脂异常的d4cd8测量值保留至首次出现血脂异常###
data.hlme<- data.hlme %>%
  arrange(ID, TestingDate) %>% 
  group_by(ID) %>%
  mutate(first_Y = cumsum(Dyslip == "Y")) %>%
  filter(first_Y == 0 | row_number() <= which(first_Y == 1)[1]) %>%
  select(-first_Y) 








#剔除只有单次ID记录的数据
all.single.records<- data.hlme%>%
  group_by(ID) %>%
  filter(n() == 1) %>%
  ungroup()
all.indicator.SigRecordRemov <- anti_join(all.indicator.firstDyslip, all.single.records, by = "ID")
write.xlsx(all.indicator.SigRecordRemov, "all.indicator.SigRecordRemov.xlsx")



#合并indicator与info数据集
all.info <- read_excel("orig.all.info.xlsx")
str(all.info)

all.final <- all.indicator.SigRecordRemov %>% left_join(all.info, by = "ID")





######6.1.3 新建ARTage、TimefromATR#####
all.final<- all.final %>%
  mutate(ARTage = as.integer((ARTinitialDate - BirthDate) / dyears(1))) %>%#ARTage
  select(1:15, ARTage, everything()) %>% 
  filter(ARTage > 18) %>% 
  mutate(
    TimefromART = round(interval(start = ARTinitialDate, end = TestingDate) / months(1), 
                             digits = 2)
  ) %>% 
  filter(TimefromART > 0) %>% 
  group_by(ID) %>% 
  mutate(ID.count = n()) %>% 
  filter(ID.count > 1) %>% 
  select(-ID.count)
    
write.xlsx(all.final, "all.final.xlsx")

#统计ID数
count.all.final.id <- all.final %>% group_by(ID) %>% 
  summarise(UniqueIDcount = n_distinct(ID)) #10804人


#新建血脂异常与否分组（1异常、0不异常）
data.hlme <- data.hlme %>% 
  group_by(ID) %>% 
  mutate(
    LipGroup = if_else(any(Dyslip == "Y"), 1, 0)
  ) 

#简化数据框,生成subject
data.hlme.simply <- data.hlme %>% 
  select(2, 8, 12) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  mutate(Subject = cur_group_id())


all.forhlme <- all.forhlme %>% 
  filter(!(cd4cd8Ratio == 0)) #filter函数不需要指定数据框来引用列

#剔除只有单次ID记录的数据
all.single.records<- all.forhlme %>%
  group_by(ID) %>%
  filter(n() == 1) %>%
  ungroup()
all.forhlme <- anti_join(all.forhlme, all.single.records, by = "ID")


write.xlsx(all.forGMM, "all.forhlme.xlsx")

all.final <- read_excel("all.final.xlsx")


summary(all.final$cd4cd8Ratio)

count.01 <- all.final %>% 
  filter(cd4cd8Ratio < 0.1) %>% 
  nrow()

count.5 <- all.final %>% 
  filter(cd4cd8Ratio > 5) %>% 
  nrow()

all.final.b <- all.final %>% 
  filter(!(cd4cd8Ratio < 0.1)) %>% 
  filter(!(cd4cd8Ratio > 5))

singleRecord <- all.final.b %>% 
  group_by(ID) %>% 
  filter(n() == 1)

all.final.b <- all.final.b %>% left_join(singleRecord, by = "ID")
#简化数据框
all.forhlme.b <- all.final.b %>% 
  select(2, 8, 12, 26) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  mutate(Subject = cur_group_id())

  

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


#剔除Time大于96的数据（8年）
data.hlme <- all.forhlme.d %>% 
  filter(TimefromART.x < 96)
data.fewerThree <- all.forhlme.e %>%
  group_by(ID) %>%
  filter(n() < 3) %>%
  ungroup()
all.forhlme.e <- anti_join(all.forhlme.e, data.fewerThree, by = "ID")



#剔除时间大于48的数据（4年）
all.forhlme.f <- all.forhlme.e %>% 
  filter(TimefromART.x < 72) %>% 
  group_by(ID) %>% 
  filter(n() >= 3) %>% 
  mutate(Subject = cur_group_id())

write.xlsx(all.forhlme.f, "all.forhlme.f.xlsx")
options(digits = 6)




#gmm模型
library(lcmm)

model.1 <- hlme(fixed = cd4cd8Ratio ~ 1 + TimefromART + I(TimefromART^2),
                  random = ~ 1 + TimefromART,
                  ng = 1, 
                  data = data.hlme.simply, 
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
             data =data.hlme.simply, 
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

datnew  <- data.frame(TimefromART = seq(0,100, length = 1000))

plotpred <- predictY(model[[2]], datnew, var.time ="TimefromART")

plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="TimefromART (Months)", ylab="cd4cd8Ratio", 
     legend.loc = "topleft", cex=0.75)

##提取subject对应的class
class_data <- model[[2]]$pprob[,1:2]

#后验概率
postprob(model[[2]])


data.hlme.ss <- data.hlme%>%
  group_by(ID) %>% 
  mutate(
    LipGroup = if_else(any(Dyslip == "Y"), 1, 0)
  ) %>% 
  mutate(Subject = cur_group_id()) %>% 
  select(LipGroup, Subject) %>% 
  distinct(Subject, .keep_all = TRUE)

#h合并class_data与all.forhlme.g
class.dt <- data.hlme.ss %>% left_join(class_data, by = "Subject")
write.xlsx(class.dt, "class.dt.xlsx")

stats <- class.dt %>%
  group_by(class, LipGroup.x) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)
print(stats)
