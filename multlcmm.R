data.multlcmm <- data.hlme %>% 
  select(2, 3, 6, 7, 8, 9, 11, 13, 14) %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  mutate(Subject = cur_group_id())

#剔除少于3次ID记录的数据
data.lessthan3 <- data.multlcmm %>%
  group_by(ID) %>%
  filter(n() < 3) %>%
  ungroup()
data.multlcmm <- anti_join(data.multlcmm, data.lessthan3, by = "ID")



outliers.tg <- boxplot.stats(data.multlcmm$TG)$out
data.multlcmm <- data.multlcmm[!data.multlcmm$TG %in% outliers.tg, ]
summary(data.multlcmm$TG)
boxplot(data.multlcmm$TG)
hist(data.multlcmm$TG, breaks = 30)


outliers.tc <- boxplot.stats(data.multlcmm$TC)$out
data.multlcmm <- data.multlcmm[!data.multlcmm$TC %in% outliers.tc, ]
summary(data.multlcmm$TC)
boxplot(data.multlcmm$TC)
hist(data.multlcmm$TC, breaks = 30)

data.multlcmm.nonNA <- data.multlcmm %>% 
  filter(!is.na(TG) & !is.na(TC))

data.multlcmm.nonNA <- data.multlcmm.nonNA %>%
  group_by(ID) %>%
  filter(n() > 3)

lcmm.0 <- multlcmm(fixed = TG + TC ~ 1 + TimefromART + I(TimefromART^2),
                   random = ~ 1 + TimefromART,
                   ng = 1, 
                   data = data.multlcmm.nonNA, 
                   maxiter = 100,
                   subject = "Subject")
summary(lcmm.0)


lcmm.2 <- multlcmm(fixed = TG + TC ~ 1 + TimefromART + I(TimefromART^2),
                   mixture = ~ 1 + TimefromART + I(TimefromART^2),
                   random = ~ 1 + TimefromART,
                   ng = 2, 
                   B = lcmm.0,
                   data = data.multlcmm.nonNA, 
                   maxiter = 100,
                   subject = "Subject")
summary(lcmm.2)



lcmm.3 <- multlcmm(fixed = TG + TC ~ 1 + TimefromART + I(TimefromART^2),
                   mixture = ~ 1 + TimefromART + I(TimefromART^2),
                   random = ~ 1 + TimefromART,
                   ng = 3, 
                   B = lcmm.0,
                   data = data.multlcmm.nonNA, 
                   maxiter = 100,
                   subject = "Subject")
summary(lcmm.3)


lcmm.4 <- multlcmm(fixed = TG + TC ~ 1 + TimefromART + I(TimefromART^2),
                   mixture = ~ 1 + TimefromART + I(TimefromART^2),
                   random = ~ 1 + TimefromART,
                   ng = 4, 
                   B = lcmm.0,
                   data = data.multlcmm.nonNA, 
                   maxiter = 100,
                   subject = "Subject")
summary(lcmm.4)





#可视化
windows(width=10, height=8)

newdata <- data.frame(TimefromART = seq(0,50, length = 1000))

plotpred <- predictY(lcmm.4, newdata, var.time ="TimefromART")

plot(plotpred, lty = 1, lwd = 5,marg = FALSE, shades = T,
     xlab = "TimefromART (Months)", ylab = "LipidLevel", 
     nsim = 20, methInteg=0,
     legend.loc = "topleft", cex=0.75)

##提取subject对应的class
class.4 <- lcmm.4$pprob[,1:2]



data.multlcmm.nonNA.subject <- data.multlcmm.nonNA %>% 
  left_join(class.4, by = "Subject")




data.multlcmm.nonNA.subject <- data.multlcmm.nonNA.subject %>%
  arrange(ID, TestingDate) %>% # 按ID和测试日期排序
  mutate(
    NextTestingDate = lead(TestingDate, 1), # 获取下一次测试日期
    NextCd4Cd8Ratio = lead(`cd4cd8Ratio`, 1), # 获取下一个cd4/cd8值
    DaysBetween = as.numeric(difftime(NextTestingDate, TestingDate, units = "days")), # 计算日期差
    RecoverCondition = (`cd4cd8Ratio` >= 1 & NextCd4Cd8Ratio >= 1) & # 连续两次大于等于1
      (DaysBetween > 180 & DaysBetween < 365*2) # 时间间隔条件
  ) %>%
  mutate(
    RatioRecover = ifelse(RecoverCondition, "Y", "N") # 基于条件设置Recover
  ) %>%
  select(-NextTestingDate, -NextCd4Cd8Ratio, -DaysBetween, -RecoverCondition) %>% #移除辅助列
  group_by(ID) %>%
  mutate(RecoverGroup = ifelse(any(RatioRecover == "Y"), 1, 0)) #分Ratio恢复组1和未恢复组0





data.multlcmm.subject.unique <- data.multlcmm.nonNA.subject %>% 
  group_by(Subject) %>% 
  distinct(Subject, .keep_all = TRUE)
write.xlsx(data.multlcmm.subject.unique, "data.multlcmm.subject.unique.xlsx")
