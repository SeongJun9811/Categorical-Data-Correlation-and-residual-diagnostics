install.packages("readxl")
library(readxl)

data <- read.csv('C:/Users/seongjun/Desktop/SPSS_data_all_set.csv', header = T)
data <- data[-1,]

###################################################################################

# 3. 지역(city)에 따른 pcr 양성
df3<-data[data$RT.PCR.Hanta.virus..Lung. %in% c("neg","pos"),]
df3<-df3[,c('City..Department','RT.PCR.Hanta.virus..Lung.')]
# "City..Department" 열의 공백 제거
df3$City..Department <- gsub(" ", "", df3$City..Department)


df3$City..Department <- tolower(df3$City..Department)

table3<-table(df3$City..Department,df3$RT.PCR.Hanta.virus..Lung.)
ch3 <- chisq.test(table3)

# 0값인 셀에 0.5 더하기
table3[table3 == 0] <- 0.5

# 피셔의 정확 검정 수행
fisher_result3 <- fisher.test(table3, simulate.p.value = TRUE, B = 10000)
print(fisher_result3)

# 잔차진단
table3<-table(df3$City..Department,df3$RT.PCR.Hanta.virus..Lung.)
ch3 <- chisq.test(table3)
residuals3 <- as.data.frame(ch3$residuals)
# 잔차의 절대값이 2 이상인 셀 추출
residuals3[abs(residuals3$Freq) >= 2,]

###################################################################################

# 4.지역(province)에 따른 pcr 양성
df4<-data[data$RT.PCR.Hanta.virus..Lung.%in% c("neg","pos"),]
df4<-df4[,c('Province','RT.PCR.Hanta.virus..Lung.')]
table4<-table(df4$Province,df4$RT.PCR.Hanta.virus..Lung.)
ch4 <- chisq.test(table4)

# 잔차검정
residuals4 <- as.data.frame(ch4$residuals)
residuals4

residuals4[abs(residuals4$Freq) >= 3,]
###################################################################################

# 5. 시기에 따른 pcr 양성
df5 <- data[data$RT.PCR.Hanta.virus..Lung. %in% c("pos", "neg"), ]
df5 <- df5[!(df5$month %in% c("-", "N.D")), , drop = FALSE]
# "month" 열을 날짜로 변환
df5$month <- as.Date(df5$month, format = "%Y-%m-%d")

# 잘못된 형식을 갖는 행 제거
df5 <- df5[complete.cases(df5), , drop = FALSE]
# 월만 추출
df5$month <- format(df5$month, "%m")
# month 열을 정수로 변환
df5$month <- as.integer(df5$month)

# 계절 정보를 범주화하는 함수 정의
season_function <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# apply 함수를 사용하여 모든 월에 대해 계절을 할당
df5$season <- sapply(df5$month, season_function)
df5 <- df5[,c('season', 'RT.PCR.Hanta.virus..Lung.')]

# 상관관계 파악
table(df5$RT.PCR.Hanta.virus..Lung., df5$season)
result5 <- chisq.test(df5$season, df5$RT.PCR.Hanta.virus..Lung., correct = TRUE)
result5

residuals5 <- as.data.frame(result5$residuals)
residuals5

# 잔차의 절대값이 3 이상인 셀 추출
residuals5[abs(residuals5$Freq) >= 3,]
####################################################################################

# 6,7. 성별에 따른 IFA(HTNV+SEO) 양성

# "-" 값을 NA로 바꾸기
data$IFA.SEO[data$IFA.SEO == "-"] <- NA

# "w+", "1+", "2+", "3+", "4+" 값을 "pos"로 바꾸기
data$IFA.SEO[data$IFA.SEO %in% c("w+", "1+", "2+", "3+", "4+")] <- "pos"
data$IFA.HTN[data$IFA.HTN %in% c("w+", "1+", "2+", "3+", "4+")] <- "pos"
data$IFA_plus <- ifelse(data$IFA.HTN == "pos" | data$IFA.SEO == "pos", "pos",
                        ifelse(data$IFA.HTN == "neg" & data$IFA.SEO == "neg", "neg", NA))


df7 <- data[data$IFA_plus %in% c("pos", "neg"), ]
df7 <- df7[,c('Sex', 'IFA_plus')]
row_index <- which(row.names(df7) == "12901")
df7 <- df7[-row_index, ]
table7 <- table(df7)
table7
# 상관관계 파악
result7 <- chisq.test(table7)
result7

residuals7 <- as.data.frame(result7$residuals)
residuals7

# 잔차의 절대값이 3 이상인 셀 추출
residuals7[abs(residuals7$Freq) >= 3,]

####################################################################################

# 8. 무게에 따른 IFA(HTNV+SEO) 양성
df8 <- data[data$IFA_plus %in% c("pos", "neg"), ]
df8 <- df8[,c('Weight..grams.', 'IFA_plus')]

# 무게 구간 나누기
df8$Weight..grams. <- as.numeric(df8$Weight..grams.)
df8$Weight_Group <- cut(df8$Weight..grams., breaks = c(0, 10, 20, 30, 40, Inf), 
                        labels = c("0-10", "11-20", "21-30", "31-40", "40+"), include.lowest = TRUE)

# 상관관계 파악
table8 <- table(df8$IFA_plus, df8$Weight_Group)
result8 <- chisq.test(df8$Weight_Group, df8$IFA_plus, correct = TRUE)
result8

# 0값인 셀에 0.5 더하기
table8[table8 == 0] <- 0.5

# 피셔의 정확 검정 수행
fisher_result8 <- fisher.test(table8, simulate.p.value = TRUE, B = 10000)
print(fisher_result8)

residuals8 <- as.data.frame(result8$residuals)
residuals8

# 잔차의 절대값이 3 이상인 셀 추출
residuals8[abs(residuals8$Freq) >= 2,]
####################################################################################

# 9. 지역(city)에 따른 IFA(HTNV+SEO) 양성
df9 <- data[data$IFA_plus %in% c("pos", "neg"), ]
df9 <- df9[,c('City..Department', 'IFA_plus')]

df9$City..Department <- tolower(df9$City..Department)


# 상관관계 파악
table9 <- table(df9$IFA_plus, df9$City..Department)
result9 <- chisq.test(df9$City..Department, df9$IFA_plus, correct = TRUE)
result9

# 0값인 셀에 0.5 더하기
table9[table9 == 0] <- 0.5

# 피셔의 정확 검정 수행
fisher_result9 <- fisher.test(table9, simulate.p.value = TRUE, B = 10000)
print(fisher_result9)

residuals9 <- as.data.frame(result9$residuals)
residuals9

# 잔차의 절대값이 2 이상인 셀 추출
residuals9[abs(residuals9$Freq) >= 2,]
####################################################################################

# 10. 지역(province)에 따른 IFA(HTNV+SEO)양성
df10 <- data[data$IFA_plus %in% c("pos", "neg"), ]
df10 <- df10[,c('Province', 'IFA_plus')]

table10 <- table(df10$IFA_plus, df10$Province)
result10 <- chisq.test(df10$Province, df9$IFA_plus, correct = TRUE)
result10

# 0값인 셀에 0.5 더하기
table10[table10 == 0] <- 0.5

# 피셔의 정확 검정 수행
fisher_result10 <- fisher.test(table10, simulate.p.value = TRUE, B = 10000)
print(fisher_result10)

residuals10 <- as.data.frame(result10$residuals)
residuals10

# 잔차의 절대값이 2 이상인 셀 추출
residuals10[abs(residuals10$Freq) >= 2,]
####################################################################################

# 11. 시기에 따른 IFA 양성
df11 <- data[data$IFA_plus %in% c("pos", "neg"), ]
df11 <- df11[!(df11$month %in% c("-", "N.D")), , drop = FALSE]
# "month" 열을 날짜로 변환
df11$month <- as.Date(df11$month, format = "%Y-%m-%d")

# 잘못된 형식을 갖는 행 제거
df11 <- df11[complete.cases(df11), , drop = FALSE]
# 월만 추출
df11$month <- format(df11$month, "%m")
# month 열을 정수로 변환
df11$month <- as.integer(df11$month)

# 계절 정보를 범주화하는 함수 정의
season_function <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# apply 함수를 사용하여 모든 월에 대해 계절을 할당
df11$season <- sapply(df11$month, season_function)
df11 <- df11[,c('season', 'IFA_plus')]

# 상관관계 파악
table(df11$IFA_plus, df11$season)
result11 <- chisq.test(df11$season, df11$IFA_plus, correct = TRUE)
result11

residuals11 <- as.data.frame(result11$residuals)
residuals11

# 잔차의 절대값이 3 이상인 셀 추출
residuals11[abs(residuals11$Freq) >= 3,]


####################################################################################
# 1. 성별- 환자 발생 사이 연관성
# 주어진 데이터
male <- c(25859888, 25818686, 25870941, 25925697, 25948706, 25857689, 25736793, 25670949, 25585894, 25445077, 25285319, 25187380, 25069867, 24881114, 24774341, 24671648, 24491190, 24369561, 24243234, 24210740, 24120872, 23991377, 23854971)
female <- c(25852731, 25853883, 25898598, 25910542, 25816116, 25727369, 25625118, 25546854, 25429053, 25301582, 25143574, 25012473, 24866771, 24672998, 24533494, 24383060, 24192448, 24068731, 23941327, 23871779, 23771458, 23653359, 23515193)
years <- 2001:2023

# 데이터프레임 생성
population_df <- data.frame(Year = years, Male = rev(male), Female = rev(female))

# 결과 출력
print(population_df)

# 주어진 환자 데이터
male <- c(179, 200, 224, 243, 229, 254, 255, 218, 190, 260, 223, 214, 300, 240, 249, 341, 335, 269, 236, 146, 176, 176, 287)
female <- c(144, 136, 168, 184, 192, 168, 195, 157, 144, 213, 147, 150, 227, 104, 135, 234, 196, 164, 163, 124, 134, 126, 164)
years <- 2001:2023

# 새로운 데이터프레임 생성
patient_df <- data.frame(Year = years, Male_Patients = male, Female_Patients = female)

# 결과 출력
print(patient_df)

# 음성 환자 데이터 계산
negative_male <- population_df$Male - patient_df$Male_Patients
negative_female <- population_df$Female - patient_df$Female_Patients

# 새로운 데이터프레임 생성
negative_patient_df <- data.frame(Year = population_df$Year, Negative_Male_Patients = negative_male, Negative_Female_Patients = negative_female)

# 결과 출력
print(negative_patient_df)

# 행렬 생성
table_data <- matrix(c(sum(patient_df$Male_Patients), sum(negative_patient_df$Negative_Male_Patients),
                       sum(patient_df$Female_Patients), sum(negative_patient_df$Negative_Female_Patients)), 
                     nrow = 2, byrow = TRUE)

# 데이터프레임 생성
table_df <- as.data.frame(table_data)

# 행, 열 이름 설정
rownames(table_df) <- c("Male_Patient", "Female_Patient")
colnames(table_df) <- c("Patient", "Negative_Patient")

# 카이제곱 독립성 검정 수행
result <- chisq.test(table_df)
result

residuals1_1 <- as.data.frame(result$residuals)
residuals1_1


####################################################################################
# 2. 지역- 환자 발생 사이의 연관성 
# 주어진 데이터
population <- c(51712619, 51672569, 51769539, 51836239, 51764822, 51585058, 51361911, 51217803, 51014947, 50746659, 50428893, 50199853, 49936638, 49554112, 49307835, 49054708, 48683638, 48438292, 48184561, 48082519, 47892330, 47644736, 47370164)
years <- 2023:2001

# 데이터 뒤집기
population <- rev(population)
years <- rev(years)

# 데이터프레임 생성
population_df <- data.frame(Year = years, Population = population)

# 주어진 데이터
cities <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주", "세종")
populations <- c(462, 249, 99, 239, 215, 125, 78, 1744, 479, 484, 1269, 1134, 1174, 811, 598, 25, 28)

# 데이터프레임 생성
city_population_df <- data.frame(City = cities, Population = populations)

# 결과 출력
print(city_population_df)

# 2001년부터 2023년까지의 총 인구 수 계산
total_population <- sum(population_df$Population)

# 각 도시별 발생 수를 제외한 인구 수 계산
neg_population <- total_population - city_population_df$Population

# 결과 출력
result_df <- data.frame(City = city_population_df$City, neg_Population = neg_population)
print(result_df)

# city_population_df와 result_df를 도시 이름을 기준으로 병합
merged_df <- merge(city_population_df, result_df, by = "City")

# 교차표 생성
cross_table <- matrix(c(merged_df$Population, merged_df$neg_Population), nrow = 2, byrow = TRUE)

# 데이터프레임으로 변환
cross_df <- as.data.frame(cross_table)

# 행과 열 이름 설정
rownames(cross_df) <- c("Population", "neg_Population")
colnames(cross_df) <- cities

# 결과 출력
print(cross_df)


chisq_result <- chisq.test(cross_df)

# 결과 출력
print(chisq_result)

chisq_result$residuals

####################################################################################
# 3. 시기-환자 발생 사이의 연관성
# 데이터 입력
data <- matrix(c(
  20, 8, 8, 4, 5, 10, 4, 3, 21, 71, 127, 42,
  13, 8, 5, 6, 11, 13, 11, 6, 13, 84, 121, 45,
  17, 7, 3, 6, 16, 14, 12, 13, 18, 94, 122, 70,
  33, 15, 5, 8, 17, 10, 15, 10, 13, 96, 150, 55,
  24, 7, 10, 10, 11, 11, 7, 7, 21, 86, 155, 72,
  24, 10, 9, 9, 13, 15, 13, 11, 31, 95, 128, 64,
  21, 11, 7, 8, 14, 6, 14, 12, 9, 73, 211, 64,
  27, 8, 6, 5, 10, 13, 10, 11, 17, 80, 130, 58,
  15, 6, 9, 11, 10, 20, 12, 10, 19, 60, 110, 52,
  16, 11, 10, 15, 16, 22, 24, 20, 33, 81, 158, 67,
  16, 10, 13, 7, 11, 17, 16, 13, 35, 82, 96, 54,
  28, 14, 8, 6, 15, 18, 16, 16, 17, 101, 94, 31,
  20, 11, 15, 5, 17, 22, 23, 29, 29, 150, 154, 52,
  27, 8, 9, 3, 8, 15, 9, 13, 32, 60, 82, 78,
  13, 7, 14, 10, 25, 19, 20, 22, 19, 76, 99, 60,
  32, 17, 12, 18, 35, 39, 26, 37, 45, 87, 145, 82,
  39, 14, 19, 28, 28, 40, 34, 40, 41, 91, 105, 52,
  21, 13, 8, 18, 27, 44, 45, 36, 33, 73, 71, 44,
  16, 16, 14, 19, 18, 31, 31, 18, 34, 64, 86, 52,
  19, 10, 8, 8, 13, 20, 20, 20, 19, 41, 60, 32,
  20, 8, 14, 21, 20, 30, 19, 18, 15, 35, 79, 31,
  14, 8, 5, 5, 15, 24, 31, 12, 17, 35, 104, 32,
  20, 8, 14, 13, 30, 28, 45, 32, 36, 72, 124, 29),
  nrow = 23, byrow = TRUE)

# 데이터프레임으로 변환
df <- data.frame(data)

# 행과 열 이름 설정
rownames(df) <- 2001:2023
colnames(df) <- c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월")

# 결과 출력
print(df)

# 12-2월, 3-5월, 6-8월, 9-11월의 합을 계산하여 새로운 열로 추가
df$`겨울` <- rowSums(df[, c("12월", "1월", "2월")])
df$`봄` <- rowSums(df[, c("3월", "4월", "5월")])
df$`여름` <- rowSums(df[, c("6월", "7월", "8월")])
df$`가을` <- rowSums(df[, c("9월", "10월", "11월")])

# 결과 출력
print(df)

# 겨울: 12월 + 1월 + 2월
겨울 <- sum(df$`12월`) + sum(df$`1월`) + sum(df$`2월`)

# 봄: 3월 + 4월 + 5월
봄 <- sum(df$`3월`) + sum(df$`4월`) + sum(df$`5월`)

# 여름: 6월 + 7월 + 8월
여름 <- sum(df$`6월`) + sum(df$`7월`) + sum(df$`8월`)

# 가을: 9월 + 10월 + 11월
가을 <- sum(df$`9월`) + sum(df$`10월`) + sum(df$`11월`)

# 결과 출력
sum_df <- data.frame(계절 = c("겨울", "봄", "여름", "가을"), 합계 = c(겨울, 봄, 여름, 가을))
print(sum_df)

# 열의 합 계산
total_population <- sum(population_df$Population)

# 각 계절별 환자 수 계산
겨울 <- sum(df$`12월`) + sum(df$`1월`) + sum(df$`2월`)
봄 <- sum(df$`3월`) + sum(df$`4월`) + sum(df$`5월`)
여름 <- sum(df$`6월`) + sum(df$`7월`) + sum(df$`8월`)
가을 <- sum(df$`9월`) + sum(df$`10월`) + sum(df$`11월`)

# 열의 합에서 각 계절별 환자 수 빼기
result <- total_population - c(겨울, 봄, 여름, 가을)

# 결과 출력
result_df <- data.frame(계절 = c("겨울", "봄", "여름", "가을"), `neg` = result)
print(result_df)

cross_table <- matrix(c(sum_df$합계, result_df$neg), nrow = 2, byrow = TRUE)

# 데이터프레임으로 변환
cross_df <- as.data.frame(cross_table)

# 행과 열 이름 설정
rownames(cross_df) <- c("Population", "neg_Population")
colnames(cross_df) <- c("겨울", "봄", "여름", "가을")

cross_df

chisq_result <- chisq.test(cross_df)
chisq_result

chisq_result$residuals
