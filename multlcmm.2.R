data.orig <- read_excel("orig.all.baselineLIP.xlsx")
data.orig <- na.omit(data.orig)
write.xlsx(data.orig, "data.orig.xlsx")
#用血脂做轨迹
#剔除错误的血脂数据
#剔除错误的cd4cd8比值数据

#cd4cd8比值正常不纳入队列，比值异常纳入队列
#查看Time的分布，决定观察终点

#剔除记录少于3次的数据

#剔除为0的数据
data.clean <- data.orig %>% 
  filter(! (TG == 0 | TC == 0 | cd4cd8Ratio == 0))


out.tg <- boxplot.stats(data.clean$TG)$out
data.clean <- data.clean[!data.clean$TG %in% out.tg, ]
summary(data.clean$TG)
boxplot(data.clean$TG)
hist(data.clean$TG, breaks = 30)


out.tc <- boxplot.stats(data.clean$TC)$out
data.clean <- data.clean[!data.clean$TC %in% out.tc, ]
summary(data.clean$TC)
boxplot(data.clean$TC)
hist(data.clean$TC, breaks = 30)

out.ratio <- boxplot.stats(data.clean$cd4cd8Ratio)$out
data.clean <- data.clean[!data.clean$cd4cd8Ratio %in% out.ratio,] #逗号表示基于行进行子集化，而保留所有列（row，col）
summary(data.clean$cd4cd8Ratio)
boxplot(data.clean$cd4cd8Ratio)
hist(data.clean$cd4cd8Ratio, breaks = 30)



#识别每个ID首次cd4cd8Ratio是否大于1

data.basratio.less1 <- data.clean %>%
  arrange(ID, TestingDate) %>%
  group_by(ID) %>%
  filter(cd4cd8Ratio[1] < 1)  # 保留首次cd4cd8Ratio小于1的该ID所有数据


data.info <- read_excel("orig.all.info.xlsx",
                        sheet = "Sheet2")

data.time <- data.basratio.less1 %>% left_join(data.info, by = "ID")


######4.2.2 新建"Time",并剔除该值小于等于0的数据####
data.time <- data.time %>% 
  mutate(
    Time = round(interval(start = ARTinitialDate, end = TestingDate) / months(1), 
                         digits = 2)
  ) %>% 
  filter(Time > 0)

summary(data.time$Time)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.03   11.63   34.93   45.12   70.22  233.52 


data.time <- data.time %>% filter(Time < 96)


#剔除少于3次ID记录的数据
data.multlcmm <- data.time %>%
  group_by(ID) %>%
  filter(n() > 3)

data.multlcmm <- data.multlcmm %>% 
  mutate(Subject = cur_group_id())





lcmm.1 <- multlcmm(fixed = TG + TC ~ 1 + Time + I(Time^2),
                   random = ~ 1 + Time,
                   ng = 1,    
                   data = data.multlcmm,
                   subject = "Subject")
summary(lcmm.1)
