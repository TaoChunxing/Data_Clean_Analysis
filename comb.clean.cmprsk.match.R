#1 整理indicator数据集####
#####1.1 导入并合并4个indicator数据集####
cz.indicator <- read_excel("CZ_select_rename.xlsx",
                          sheet = "indicator.orig",
                          col_types = c("text", "text", "date", 
                                        "numeric", "numeric","numeric", "numeric"))
str(cz.indicator)


fcg.indicator <- read_excel("FCG_select_rename.xlsx",
                           sheet = "indicator.orig",
                           col_types = c("text", "text", "date", 
                                         "numeric", "numeric","numeric", "numeric"))
str(fcg.indicator)


qz.indicator <- read_excel("QZ_select_rename.xlsx",
                          sheet = "indicator.orig",
                          col_types = c("text", "text", "date", 
                                        "numeric", "numeric","numeric", "numeric"))
str(qz.indicator)


nn.indicator <- read_excel("NN_select_rename.xlsx", 
                           sheet = "indicator.orig")
nn.indicator$TestingDate <- as.Date(nn.indicator$TestingDate)
nn.indicator$cd4Count <- as.numeric(nn.indicator$cd4Count)
nn.indicator$cd8Count <- as.numeric(nn.indicator$cd8Count)
nn.indicator$TG <- as.numeric(nn.indicator$TG)
nn.indicator$TC <- as.numeric(nn.indicator$TC)
str(nn.indicator)


all.indicator <- bind_rows(cz.indicator, fcg.indicator, qz.indicator, nn.indicator)



#####1.2 整理cd4cd8数据#####
######1.2.1 筛选留下有完整cd4cd8值或TCTG至少一个值的数据#####
all.indicator <- all.indicator %>% 
  filter(
    (!(is.na(cd4Count) & is.na(cd8Count)) ) | (!is.na(TC) & !is.na(TG))
  )


######1.2.2 剔除cd4或cd8为0的数据#####
all.indicator <- all.indicator %>%
  filter(!(cd4Count == 0 | cd8Count == 0))


######1.2.3箱式图剔除异常值：小于P25-1.5*IQR，或大于P75+1.5*IQR#####
outliers.cd4 <- boxplot.stats(all.indicator$cd4Count)$out
all.indicator <- all.indicator[!all.indicator$cd4Count %in% outliers.cd4, ]

outliers.cd8 <- boxplot.stats(all.indicator$cd8Count)$out
all.indicator <- all.indicator[!all.indicator$cd8Count %in% outliers.cd8, ]


######1.2.4 检查数据是否还存在异常值####
hist(all.indicator$cd4Count, breaks = 40)#柱状图查看
summary(all.indicator$cd4Count)

hist(all.indicator$cd8Count, breaks = 40)#柱状图查看
summary(all.indicator$cd8Count)


######1.2.5 剔除ID只记录1次的数据，后续无需再剔除ID只有一次的数据#####
all.single.records <- all.indicator %>%
  group_by(ID) %>%
  filter(n() == 1) %>%
  ungroup()
all.indicator <- anti_join(all.indicator, all.single.records, by = "ID")


######1.2.6 生成cd4cd8Ratio，并新建"RecoverGroup"#####
all.indicator$"cd4cd8Ratio" <- round(all.indicator$cd4Count / all.indicator$cd8Count, digits = 2)

all.indicator <- all.indicator %>%
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
  mutate(RecoverGroup = ifelse(any(RatioRecover == "Y"), 1, 0)) %>% #分Ratio恢复组1和未恢复组0
  ungroup()


######1.2.7 统计cd4cd8比值恢复与未恢复的人数#####
all.count.RecoverGroup <- all.indicator %>% 
  group_by(RecoverGroup) %>% 
  summarise(UniqueIDCount = n_distinct(ID)) %>% 
  ungroup()
all.count.RecoverGroup



#####1.3 整理血脂数值#####
######1.3.1 生成"Dyslip"#####
all.indicator$"Dyslip" <- ifelse(
  (!is.na(all.indicator$TG) & all.indicator$TG > 1.7) | 
    (!is.na(all.indicator$TC) & all.indicator$TC > 5.2), "Y", 
  ifelse(is.na(all.indicator$TG) & is.na(all.indicator$TC), NA, "N")
)
write.xlsx(all.indicator,"all.indicator.xlsx")


######1.3.2 剔除完全缺失Dyslip数据的人,即缺乏血脂数据####
all.indicator<- all.indicator %>% 
  group_by(ID) %>%
  filter(!all(is.na(Dyslip)))


######1.3.3 新建基线血脂“baselineLIP”####
all.baselineLIP <- all.indicator %>%
  group_by(ID) %>% #第一次检测日期下血脂异常为“Y”的记为基线血脂异常
  mutate(baselineLIP = if_else(is.na(first(Dyslip)) | first(Dyslip) == "N", "N", "Y")) %>%
  ungroup()

write.xlsx(all.baselineLIP,"all.baselineLIP.xlsx")


######1.3.4 统计基线血脂正常与异常的人数####
count.all.baselineLIP <- all.baselineLIP %>% 
  group_by(baselineLIP) %>% 
  summarise(UniqueIDCount = n_distinct(ID))


######1.3.5 移除基线血脂异常的人####
all.indicator <- all.baselineLIP %>%
  filter(baselineLIP != "Y")

write.xlsx(all.indicator,"all.indicator.xlsx")










#2 整理died数据集并合并入indicator####
#2.1 导入并合并4个died数据集
cz.died <- read.xlsx("CZ_select_rename.xlsx", sheet = "died.1371")
cz.died$DiedDate <- as.Date(cz.died$DiedDate)
str(cz.died)

fcg.died <- read_excel("FCG_select_rename.xlsx", sheet = "died.456")
fcg.died$DiedDate <- as.Date(fcg.died$DiedDate)
str(fcg.died)

qz.died <- read_excel("QZ_select_rename.xlsx", sheet = "died.2197")
qz.died$DiedDate <- as.Date(qz.died$DiedDate)
str(qz.died)

nn.died <- read_excel("NN_select_rename.xlsx", sheet = "died.4437")
nn.died$DiedDate <- as.Date(nn.died$DiedDate)
str(nn.died)


all.died <- bind_rows(cz.died, fcg.died, qz.died, nn.died)



#####2.2 整理died数据#####
######2.2.1 去除died中重复记录的ID，即保留每个ID的第一条记录######
all.died <- all.died %>% 
  distinct(ID, .keep_all = TRUE)

write.xlsx(all.died, "all.died.xlsx")



#####2.3 合并indicator与died数据集#####
all.indicator.died <- all.indicator %>% 
  left_join(all.died, by = "ID")

write.xlsx(all.indicator.died, "all.indicator.died.xlsx")



#####2.4 新建Outcome和EndpointDate#####
all.outcome.endpoint <- all.indicator.died %>%
  arrange(ID, TestingDate) %>%
  group_by(ID) %>%
  mutate(
    Outcome = case_when(
      "Y" %in% Dyslip ~ 1L,
      TRUE ~ if_else(is.na(DiedDate), 0L, 2L)
    ),
    EndpointDate = case_when(
      Outcome == 1 ~ min(TestingDate[Dyslip == "Y"], na.rm = TRUE),
      Outcome == 2 ~ first(DiedDate[!is.na(DiedDate)]),
      Outcome == 0 ~ TestingDate[last(which(Dyslip == "N"))],
      TRUE ~ as.Date(NA)
    )
  ) %>%
  ungroup()

write.xlsx(all.outcome.endpoint,"all.outcome.endpoint.xlsx")



#####2.5 简化数据框，并用distinct()把每个ID只保留一条结果#####
all.outcome.endpoint.unique <- all.outcome.endpoint %>% 
  select(1:2, 10:13, 15:16) %>% 
  distinct(ID, .keep_all = TRUE)

write.xlsx(all.outcome.endpoint.unique,"all.outcome.endpoint.unique.xlsx")








#3 整理info数据集####
#####3.1导入并合并4个info数据集
cz.info <- read_excel("CZ_select_rename.xlsx", sheet = "info.6165")
str(cz.info)

fcg.info <- read_excel("FCG_select_rename.xlsx", sheet = "info.2735")
str(fcg.info)

qz.info <- read_excel("QZ_select_rename.xlsx", sheet = "info.11182")
str(qz.info)

nn.info <- read_excel("NN_select_rename.xlsx", sheet = "info.22906")
nn.info$Gender <- as.numeric(nn.info$Gender)
nn.info$MaritalStatus <- as.numeric(nn.info$MaritalStatus)
nn.info$WHOStage <- as.numeric(nn.info$WHOStage)
nn.info$BaselineCD4 <- as.numeric(nn.info$BaselineCD4)
nn.info$InfectedRout <- as.numeric(nn.info$InfectedRout)
nn.info$FirstVL <- as.numeric(nn.info$FirstVL)
nn.info$BirthDate <- as.Date(nn.info$BirthDate)
nn.info$ARTinitialDate <- as.Date(nn.info$ARTinitialDate)
nn.info$DiagnosedDate <- as.Date(nn.info$DiagnosedDate)
str(nn.info)


all.info <- bind_rows(cz.info, fcg.info, qz.info, nn.info)



#####3.2 用distinct()去除重复记录的ID#####
all.info <- all.info %>%
  distinct(ID, .keep_all = TRUE)


#####3.3 对治疗方案"克力芝""双汰芝"进行重命名#####
all.info$Regimen <- gsub("克力芝", "LPV/r", all.info$Regimen)
all.info$Regimen <- gsub("双汰芝", "AZT+3TC",all.info$Regimen)  


#####3.4移除ARTinitialDate为NA的数据#####
all.info <- all.info %>% 
  filter(!is.na(ARTinitialDate))

write.xlsx(all.info,"all.info.xlsx")










#4 创建并整理final数据集####
#####4.1 合并outcome.endpoint与info，创建final数据集####
all.final <- left_join(all.outcome.endpoint.unique, all.info, by = "ID")


#####4.2 整理final数据集####
######4.2.1 筛选ARTinitialDate为NA的数据，并剔除####
all.final <- all.final %>% 
  filter(!is.na(ARTinitialDate))


######4.2.2 新建"TimefromART",并剔除该值小于等于0的数据####
all.final<- all.final %>% 
  mutate(
    TimefromART = round(interval(start = ARTinitialDate, end = EndpointDate) / months(1), 
                        digits = 2)
  ) %>% 
  filter(TimefromART > 0)

write.xlsx(all.final,"all.final.xlsx")










#5 Fine-Grey检验（单因素分析）####
all.final$RecoverGroup <- factor(all.final$RecoverGroup)
all.final$Outcome <- factor(all.final$Outcome)
str(all.final)

all.cmprskModel <- cuminc(all.final$TimefromART, all.final$Outcome, all.final$RecoverGroup)#生存时间，结局，检验变量
all.cmprskModel










#6 PSM匹配####
#####6.1 数据准备（PSM不允许匹配变量存在缺失值）####
######6.1.1 新建DiagnosedAge与DiagnosedAge.a并移除该值为NA的数据#####
all.final.psm <- all.final %>%
  mutate(DiagnosedAge = as.integer((DiagnosedDate - BirthDate) / dyears(1))) %>%
  select(1:11, DiagnosedAge, everything())#DiagnosedAge

all.final.psm <- all.final.psm %>%#DiagnosedAge.a
  mutate(
    DiagnosedAge.a = case_when(
      DiagnosedAge < 30 ~ 1,
      DiagnosedAge >= 30 & DiagnosedAge <= 50 ~ 2,
      DiagnosedAge > 50 ~ 3
    )
  ) %>% 
  select(1:12, DiagnosedAge.a, everything()
  ) %>% 
  filter(!is.na(DiagnosedAge.a)) %>% 
  filter(DiagnosedAge > 18)


######6.1.2 新建MaritalStatus.a,并移除该值为NA的数据#####
all.final.psm <- all.final.psm %>%
  mutate(
    MaritalStatus.a = case_when(
      MaritalStatus %in% c(1, 3, 4) ~ 1, #未婚离异丧偶
      MaritalStatus == 2 ~ 2 #已婚
    )
  ) %>% 
  select(1:14, MaritalStatus.a, everything())

all.final.psm <- all.final.psm %>% 
  filter(!is.na(all.final.psm$MaritalStatus.a))


######6.1.3 新建ARTage#####
all.final.psm <- all.final.psm %>%
  mutate(ARTage = as.integer((ARTinitialDate - BirthDate) / dyears(1))) %>%#ARTage
  select(1:13, ARTage, everything())

all.final.psm <- all.final.psm %>%
  mutate(
    ARTage.a = case_when(
      ARTage < 30 ~ 1,
      ARTage >= 30 & ARTage <= 50 ~ 2,
      ARTage > 50 ~ 3
    )
  ) %>% 
  select(1:14, ARTage.a, everything()#ARTage.a
  ) %>% 
  filter(ARTage > 18)


######6.1.4 新建InfectedRout.a，并剔除该值NA的数据#####
all.final.psm <- all.final.psm %>% 
  mutate(
    InfectedRout.a = case_when(
      InfectedRout %in% c(4, 5, 6, 7, 8, 9, 10) ~ 4,
      InfectedRout == 1 ~ 1,
      InfectedRout == 2 ~ 2,
      InfectedRout == 3 ~ 3
    )
  ) %>% 
  select(1:21, InfectedRout.a, everything())


######6.1.5 新建BaselineCD4.a，并剔除该值NA的数据#####

all.final.psm <- all.final.psm %>% 
  mutate(
    BaselineCD4.a = case_when(
      BaselineCD4 < 200 ~ 1,
      BaselineCD4 >= 200  ~ 2
    )
  ) %>% 
  select(1:23, BaselineCD4.a, everything()) %>% 
  filter(!is.na(BaselineCD4.a))

write.xlsx(all.final.psm,"all.final.psm.xlsx")


#6.2 匹配####
library(MatchIt)

#####6.2.1 Nearest Neighbor Matching#####
all.matchlist <- matchit(RecoverGroup ~ Gender + MaritalStatus.a + InfectedRout.a + 
                          ARTage.a + DiagnosedAge.a + BaselineCD4.a,
                        data = all.final.psm,
                        method = "nearest",
                        distance = "glm",
                        caliper = 0.02,
                        ratio = 1,
                        replace = F)
summary(all.matchlist)



#####6.2.2 提取匹配后的数据#####
all.matchdata <- match.data(all.matchlist,
                           group = "all",
                           distance = "distance",
                           weights = "weights",
                           subclass = "subclass",
                           data = NULL,
                           include.s.weights = TRUE,
                           drop.unmatched = TRUE)


#####6.2.3 匹配后进行模型分析####
library(cmprsk)
library(survival)

psm.all.cmprskModel <- cuminc(all.matchdata$TimefromART, all.matchdata$Outcome, all.matchdata$RecoverGroup)
psm.all.cmprskModel

all.cmprskModel <- cuminc(all.final.psm$TimefromART, all.final.psm$Outcome, all.final.psm$RecoverGroup)
all.cmprskModel








#7 结果可视化####
plot(psm.all.cmprskModel, 
     xlab = "Month",
     ylab = "CIF",
     lwd = 2,
     lty = 1,
     color = c('red', 'blue','red', 'blue'))

