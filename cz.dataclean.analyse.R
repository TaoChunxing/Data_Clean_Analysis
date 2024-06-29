library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate) # 用于日期运算


#1 整理indicator数据集####
#####1.1 导入数据
cz.indicator <- read_excel("CZ_select_rename.xlsx", 
                               sheet = "indicator.orig", 
                               col_types = c("text", "text", "date", "numeric", 
                                             "numeric", "numeric", "numeric"))



#####1.2 整理cd4cd8数据#####
######1.2.1 筛选留下有完整cd4cd8值或TCTG至少一个值的数据#####
cz.indicator <- cz.indicator %>% 
  filter(
    (!(is.na(cd4Count) & is.na(cd8Count)) ) | (!is.na(TC) & !is.na(TG))
  )


######1.2.2 剔除cd4或cd8为0的数据#####
cz.indicator <- cz.indicator %>%
  filter(!(cd4Count == 0 | cd8Count == 0))


######1.2.3箱式图剔除异常值：小于P25-1.5*IQR，或大于P75+1.5*IQR#####
outliers.cd4 <- boxplot.stats(cz.indicator$cd4Count)$out
cz.indicator <- cz.indicator[!cz.indicator$cd4Count %in% outliers.cd4, ]

outliers.cd8 <- boxplot.stats(cz.indicator$cd8Count)$out
cz.indicator <- cz.indicator[!cz.indicator$cd8Count %in% outliers.cd8, ]


######1.2.4 检查数据是否还存在异常值####
hist(cz.indicator$cd4Count, breaks = 40)#柱状图查看
summary(cz.indicator$cd4Count)

hist(cz.indicator$cd8Count, breaks = 40)#柱状图查看
summary(cz.indicator$cd8Count)


######1.2.5 剔除ID只记录1次的数据，后续无需再剔除ID只有一次的数据#####
cz.single.records <- cz.indicator %>%
  group_by(ID) %>%
  filter(n() == 1) %>%
  ungroup()
cz.indicator <- anti_join(cz.indicator, cz.single.records, by = "ID")


######1.2.5 生成cd4cd8Ratio，并新建"RecoverGroup"#####
cz.indicator$"cd4cd8Ratio" <- round(cz.indicator$cd4Count / cz.indicator$cd8Count, digits = 2)

cz.indicator <- cz.indicator %>%
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


######1.2.6 统计cd4cd8比值恢复与未恢复的人数#####
cz.count.RecoverGroup <- cz.indicator %>% 
  group_by(RecoverGroup) %>% summarise(UniqueIDCount = n_distinct(ID)) %>% 
  ungroup()
cz.count.RecoverGroup



#####1.3 整理血脂数值#####
######1.3.1 生成"Dyslip"#####
cz.indicator$"Dyslip" <- ifelse(
  (!is.na(cz.indicator$TG) & cz.indicator$TG > 1.7) | 
    (!is.na(cz.indicator$TC) & cz.indicator$TC > 5.2), "Y", 
  ifelse(is.na(cz.indicator$TG) & is.na(cz.indicator$TC), NA, "N")
)


######1.3.2 剔除完全缺失Dyslip数据的人,即缺乏血脂数据####
cz.indicator<- cz.indicator %>% 
  group_by(ID) %>%
  filter(!all(is.na(Dyslip)))


######1.3.3 新建基线血脂“baselineLIP”####
cz.baselineLIP <- cz.indicator %>%
  group_by(ID) %>% #第一次检测日期下血脂异常为“Y”的记为基线血脂异常
  mutate(baselineLIP = if_else(is.na(first(Dyslip)) | first(Dyslip) == "N", "N", "Y")) %>%
  ungroup()

write.xlsx(cz.baselineLIP,"cz.baselineLIP.xlsx")


######1.3.4 统计基线血脂正常与异常的人数####
count.cz.baselineLIP <- cz.baselineLIP %>% 
  group_by(baselineLIP) %>% 
  summarise(UniqueIDCount = n_distinct(ID))


######1.3.5 移除基线血脂异常的人####
cz.indicator <- cz.baselineLIP %>%
  filter(baselineLIP != "Y")

write.xlsx(cz.indicator,"cz.indicator.xlsx")










#2 整理died数据集并合并入indicator####
#####2.1 整理died数据#####
######2.1.1 导入died######
cz.died <- read.xlsx("CZ_select_rename.xlsx",
                     sheet = "died.1371")

cz.died$DiedDate <- as.Date(cz.died$DiedDate)#更改为日期型变量

str(cz.died)


######2.1.2 去除died中重复记录的ID，即保留每个ID的第一条记录######
cz.died <- cz.died %>% 
  distinct(ID, .keep_all = TRUE)

write.xlsx(cz.died, "cz.died.xlsx")



#####2.2 合并indicator与died数据集#####
cz.indicator.died <- cz.indicator %>% 
  left_join(cz.died, by = "ID")

write.xlsx(cz.indicator.died, "cz.indicator.died.xlsx")
  

#####2.3 新建Outcome和EndpointDate#####
cz.outcome.endpoint <- cz.indicator.died %>%
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

write.xlsx(cz.outcome.endpoint,"cz.outcome.endpoint.xlsx")


#####2.4 用distinct()把每个ID只保留一条结果#####
cz.outcome.endpoint.unique <- cz.outcome.endpoint %>% 
  select(1:2, 10:13, 15:16) %>% 
  distinct(ID, .keep_all = TRUE)

write.xlsx(cz.outcome.endpoint.unique,"cz.outcome.endpoint.unique.xlsx")






#3 整理info数据集####
#####3.1 导入数据集####
cz.info <- read_excel("CZ_select_rename.xlsx", 
                      sheet = "info.6165")


#####3.2 用distinct()去除重复记录的ID#####
cz.info <- cz.info %>%
  distinct(ID, .keep_all = TRUE)


#####3.3 对治疗方案"克力芝""双汰芝"进行重命名#####
cz.info$Regimen <- gsub("克力芝", "LPV/r", cz.info$Regimen)
cz.info$Regimen <- gsub("双汰芝", "AZT+3TC",cz.info$Regimen)  


#####3.4移除ARTinitialDate为NA的数据#####
cz.info <- cz.info %>% 
  filter(!is.na(ARTinitialDate))

write.xlsx(cz.info,"cz.info.xlsx")






#4 创建并整理final数据集####
#####4.1 合并outcome.endpoint与info，创建final数据集####
cz.final <- left_join(cz.outcome.endpoint.unique, cz.info, by = "ID")


######4.2 整理final数据集####
######4.2.1 筛选ARTinitialDate为NA的数据，并剔除####
cz.final <- cz.final %>% 
  filter(!is.na(ARTinitialDate))


######4.2.2 新建"TimefromART",并剔除该值小于等于0的数据####
cz.final<- cz.final %>% 
  mutate(
    TimefromART = round(interval(start = ARTinitialDate, end = EndpointDate) / months(1), 
                        digits = 2)
  ) %>% 
  filter(TimefromART > 0)

write.xlsx(cz.final,"cz.final.xlsx")






#5 Fine-Grey检验（单因素分析）####
cz.final$RecoverGroup <- factor(cz.final$RecoverGroup)
cz.final$Outcome <- factor(cz.final$Outcome)
str(cz.final)

cz.cmprskModel <- cuminc(cz.final$TimefromART, cz.final$Outcome, cz.final$RecoverGroup)
cz.cmprskModel









#6 PSM匹配####
#####6.1 数据准备（PSM不允许匹配变量存在缺失值）####

######6.1.1 新建DiagnosedAge与DiagnosedAge.a并移除该值为NA的数据#####
cz.final.psm <- cz.final %>%
  mutate(DiagnosedAge = as.integer((DiagnosedDate - BirthDate) / dyears(1))) %>%
  select(1:9, DiagnosedAge, everything())#DiagnosedAge

cz.final.psm <- cz.final.psm %>%#DiagnosedAge.a
  mutate(
    DiagnosedAge.a = case_when(
      DiagnosedAge < 30 ~ 1,
      DiagnosedAge >= 30 & DiagnosedAge <= 50 ~ 2,
      DiagnosedAge > 50 ~ 3
    )
  ) %>% 
  select(1:10, DiagnosedAge.a, everything()
  )


######6.1.2 新建MaritalStatus.a,并移除该值为NA的数据#####
cz.final.psm <- cz.final.psm %>%
  mutate(
    MaritalStatus.a = case_when(
      MaritalStatus %in% c(1, 3, 4) ~ 1, #未婚离异丧偶
      MaritalStatus == 2 ~ 2 #已婚
    )
  ) %>% 
  select(1:12, MaritalStatus.a, everything())

cz.final.psm <- cz.final.psm %>% 
  filter(!is.na(cz.final.psm$MaritalStatus.a))


######6.1.3 新建ARTage#####

cz.final.psm <- cz.final.psm %>%
  mutate(ARTage = as.integer((ARTinitialDate - BirthDate) / dyears(1))) %>%#ARTage
  select(1:11, ARTage, everything())

cz.final.psm <- cz.final.psm %>%
  mutate(
    ARTage.a = case_when(
      ARTage < 30 ~ 1,
      ARTage >= 30 & ARTage <= 50 ~ 2,
      ARTage > 50 ~ 3
    )
  ) %>% 
  select(1:12, ARTage.a, everything()#ARTage.a
  )


######6.1.4 新建InfectedRout.a，并剔除该值NA的数据#####
cz.final.psm <- cz.final.psm %>% 
  mutate(
    InfectedRout.a = case_when(
      InfectedRout %in% c(4, 5, 6, 7, 8, 9, 10) ~ 4,
      InfectedRout == 1 ~ 1,
      InfectedRout == 2 ~ 2,
      InfectedRout == 3 ~ 3
    )
  )


######6.1.5 新建BaselineCD4.a，并剔除该值NA的数据#####

cz.final.psm <- cz.final.psm %>% 
  mutate(
    BaselineCD4.a = case_when(
      BaselineCD4 < 200 ~ 1,
      BaselineCD4 >= 200  ~ 2
    )
  )

cz.final.psm <- cz.final.psm %>%
  filter(!is.na(BaselineCD4.a))

write.xlsx(cz.final.psm,"cz.final.psm.xlsx")


#6.2 匹配####
library(MatchIt)

#####6.2.1 Nearest Neighbor Matching#####
cz.matchlist <- matchit(RecoverGroup ~ Gender + MaritalStatus.a + InfectedRout.a + 
                          ARTage.a + DiagnosedAge.a + BaselineCD4.a,
                        data = cz.final.psm,
                        method = "nearest",
                        distance = "glm",
                        caliper = 0.05,
                        ratio = 2,
                        replace = F)
summary(cz.matchlist)


#####6.2.2 提取匹配后的数据#####
cz.matchdata <- match.data(cz.matchlist,
                           group = "all",
                           distance = "distance",
                           weights = "weights",
                           subclass = "subclass",
                           data = NULL,
                           include.s.weights = TRUE,
                           drop.unmatched = TRUE)


#####6.2.3 
cz.matchdata$RecoverGroup <- factor(cz.matchdata$RecoverGroup)
cz.matchdata$Outcome <- factor(cz.matchdata$Outcome)
str(cz.matchdata)

psm.cz.cmprskModel <- cuminc(cz.matchdata$TimefromART, cz.matchdata$Outcome, cz.matchdata$RecoverGroup)
psm.cz.cmprskModel






#####7.统计每个变量的频数和构成比#####
#性别
# count.Gender <- table(cz.final$Gender)
# prop.Gender <- prop.table(count.Gender)*100
# df.Gender <- data.frame(Frequency = count.Gender, Percentage = round(prop.Gender, 2))
# print(df.Gender)
# #诊断时年龄
# count.DiagnosedAge.a <- table(cz.final$DiagnosedAge.a)
# prop.DiagnosedAge.a <- prop.table(count.DiagnosedAge.a)*100
# df.Age <- data.frame(Frequency = count.DiagnosedAge.a, Percentage = round(prop.DiagnosedAge.a, 2))
# print(df.Age)
# #婚姻状况
# count.MaritalStatus.a <- table(cz.final$MaritalStatus.a)
# prop.MaritalStatus.a <- prop.table(count.MaritalStatus.a)*100
# df.MaritalStatus <- data.frame(Frequency = count.MaritalStatus.a,
#                               Percentage = round(prop.MaritalStatus.a, 2))
# print(df.MaritalStatus)
# #开启ART年龄
# count.ARTage.a <- table(cz.final$ARTage.a)
# prop.ARTage.a <- prop.table(count.ARTage.a)*100
# df.ARTage.a <- data.frame(Frequency = count.ARTage.a,
#                                Percentage = round(prop.ARTage.a, 2))
# print(df.ARTage.a)
# 
# #感染途径
# count.InfectedRout.a<- table(cz.final$InfectedRout.a)



