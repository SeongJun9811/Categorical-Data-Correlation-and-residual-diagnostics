sample <- read.csv('C:/Users/seongjun/Desktop/SPSS_data_all_set.csv')

# pos가 있는 행을 추출
df5 <- sample[sample$RT.PCR.Hanta.virus..Lung. %in% c("pos"), ]
df5$year <- as.Date(df5$year, format = "%Y-%m-%d")
df5$year<-year(df5$year)
df5<-df5[,c(4,14)]

# 각 연도별로 pos 수 구하기
df5_pos <- df5 %>%
  filter(RT.PCR.Hanta.virus..Lung. == "pos") %>%
  count(year)

# 연도를 기준으로 sample 발생 수, 환자 발생 수 데이터 병합
merged<- merge(data1, df5_pos, by="year")

# 환자와 sample 데이터의 연도별 발생 추이(꺾은선 그래프)
plot1<-ggplot(merged) +
  geom_line(aes(x = year, y = n.x, color = "Patients"), size = 1.5) +
  geom_line(aes(x = year, y = n.y, color = "Sample"), size = 1.5) +
  scale_color_manual(values = c("Patients" = "blue", "Sample" = "red"), labels = c("Patients", "Sample")) +
  labs(x = "Year", y = "Number of pos") +
  scale_x_continuous(breaks=seq(2001,2023,2)) +
  theme(legend.position="bottom")+
  theme_minimal()

# sample과 환자데이터에서 발생 비율
merged$tot<-merged$n.x+merged$n.y
merged$xratio<-merged$n.x/sum(merged$tot)
merged$yratio<-merged$n.y/sum(merged$tot)

# 막대그래프를 그리기 위한 데이터
data <- rbind(
  data.frame(year = merged$year, type = "Patient", rate = merged$xratio),
  data.frame(year = merged$year, type = "Sample", rate = merged$yratio)
)

# 적층형 막대그래프 그리기
plot2<-ggplot(data, aes(x = factor(year), y = rate, fill = type)) +
  geom_bar(stat = "identity",position='fill') +
  scale_fill_manual(values = c("blue", "firebrick"))
labs(x = "Year", y = "Rate", fill = "Type") +
  theme_minimal()

# 한 화면에 꺾은선그래프와 막대그래프 그리기
grid.arrange(plot1, plot2, ncol = 1)

########################################################################################
# 2번
########################################################################################

# 데이터 불러오기
data2 <- read.csv('C:/Users/seongjun/Desktop/환자_기간_연도_월.csv', header = T, fileEncoding = 'euc-kr')
data2<-data2[-c(24:26),-2]

# 열 이름 바꾸기
colnames(data2)=c("year","Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# data2의 2열부터 13열을 숫자형태로 변환
data2 <- mutate_at(data2, vars(-year), as.numeric)

seasonal_sums <- data2 %>%
  mutate(Spring = Mar + Apr + May,
         Summer = Jun + Jul + Aug,
         Fall = Sep + Oct + Nov,
         Winter = Dec + Jan + Feb) %>%
  select(year, Spring, Summer, Fall, Winter)

# 결과 출력
print(seasonal_sums)

data_long <- pivot_longer(seasonal_sums, cols = c(Spring, Summer, Fall, Winter), names_to = "Season", values_to = "Count")
data1 <- as.data.frame(data_long)
data1


# sample 데이터 불러오기
sample <- read.csv('C:/Users/seongjun/Desktop/SPSS_data_all_set.csv', header = T)
sample<- sample[-1,]

# 시기에 따른 pcr 양성
sample_s <- sample[sample$RT.PCR.Hanta.virus..Lung. %in% c("pos"), ]
sample_s <- sample_s[,c('year', 'month', 'RT.PCR.Hanta.virus..Lung.')]

# "month" 열을 날짜로 변환
sample_s$month <- as.Date(sample_s$month, format = "%Y-%m-%d")

sample_s$year <- substr(sample_s$year, 1, 4)
# 잘못된 형식을 갖는 행 제거
sample_s <- sample_s[complete.cases(sample_s), , drop = FALSE]
# 월만 추출
sample_s$month <- format(sample_s$month, "%m")
# month 열을 정수로 변환
sample_s$month <- as.integer(sample_s$month)

# 계절 정보를 범주화하는 함수 정의
season_function <- function(month) {
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  } else {
    return("Winter")
  }
}

# apply 함수를 사용하여 모든 월에 대해 계절을 할당
sample_s$season <- sapply(sample_s$month, season_function)
sample_s <- sample_s[,c('year','season', 'RT.PCR.Hanta.virus..Lung.')]


pos_count <- sample_s %>%
  filter(RT.PCR.Hanta.virus..Lung. == "pos") %>%  # pos인 행만 필터링합니다.
  group_by(year, season) %>%                        # year와 season을 기준으로 그룹화합니다.
  summarise(pos_sum = n())                          # pos의 개수를 세어 pos_sum 열에 저장합니다.

# 결과를 출력합니다.
print(pos_count)

# year 열을 숫자형으로 변환합니다.
pos_count$year <- as.numeric(pos_count$year)

# season 열을 팩터(factor) 형식으로 변환하고 계절의 순서를 지정합니다.
pos_count$season <- factor(pos_count$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# year와 season을 기준으로 데이터를 정렬합니다.
pos_count <- pos_count %>%
  arrange(year, season)

# 결과를 출력합니다.
print(pos_count)

data1_1 <- as.data.frame(pos_count)
data1
data1_1

# 연도와 계절의 모든 조합을 만듭니다.
all_combinations <- expand.grid(year = 2001:2023, season = c("Spring", "Summer", "Fall", "Winter"))

# 누락된 데이터를 찾습니다.
missing_data <- anti_join(all_combinations, data1_1, by = c("year", "season"))

# 누락된 데이터를 0으로 채워넣습니다.
missing_data$pos_sum <- 0

# 누락된 데이터를 기존 데이터에 추가합니다.
sample_data <- bind_rows(data1_1, missing_data) %>%
  arrange(year, match(season, c("Spring", "Summer", "Fall", "Winter")))

sample_data <- sample_data %>%
  mutate(pos_sum = ifelse(pos_sum == 0, lead(pos_sum, default = 0), pos_sum))

# 결과 출력
sample_data
data1

pdata2 <- data.frame(year = data1$year, season = data1$Season, patient = data1$Count, sample = sample_data$pos_sum)
# year와 season을 결합하여 year_season 열 생성
pdata2$year_season <- paste0(pdata2$year, substr(pdata2$season, 1, 2))
pdata2$year_season <- factor(pdata2$year_season, levels = unique(pdata2$year_season))
# 결과 확인
print(pdata2)



# 시계열 그래프 그리기
pp2 <- ggplot(data = pdata2, aes(x = season, y = sample, group = 1, color = "sample")) + 
  geom_line() + 
  geom_line(aes(y = patient, color = "patient")) +
  labs(x = "Year Season", y = "Count", color = " ") +
  theme_minimal() +
  theme_bw() + # 바탕을 흰색으로 설정
  theme(panel.background = element_rect(fill = "white"), # 패널 배경 흰색으로 설정
        panel.border = element_rect(color = "white"))+ # 패널 테두리 흰색으로 설정
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,size = 6.5))

ggsave("pp2.png", plot = pp2, dpi = 300)
########################################################################################
# 3번
########################################################################################

# 환자 월 별 데이터 불러오기
data3 <- read.csv('C:/Users/syj90/OneDrive/바탕 화면/환자_기간_연도_월.csv', header = T)
data3<-data3[-c(24:26),-c(1,2)]

# 행이름 바꾸기
colnames(data3)=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 월 별 발생 수(열별로 합계)
n <- colSums(data3)

# 월별 데이터프레임 생성
data3 <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  n = n
)

# sample 데이터 불러오기
sample <- read.csv('C:/Users/syj90/OneDrive/바탕 화면/SPSS_data_all_set1.csv', header = T)
sample<- sample[-1,]
sample <- sample[sample$RT.PCR.Hanta.virus..Lung. %in% c("pos"), ]
# "month" 열을 날짜로 변환
sample$month <- as.Date(sample$month, format = "%Y-%m-%d")
sample<-sample[,c(5,14)]
sample$month<-month.abb[month(sample$month)]

sample_m <- sample %>%
  filter(RT.PCR.Hanta.virus..Lung. == "pos") %>%
  count(month)%>% arrange(month)

# 월 별로 sample 발생 수, 환자 발생 수 데이터 병합
merged<- merge(data3, sample_m, by="month")
# month 열을 factor로 변환하여 월 순서대로 정렬
merged$month <- factor(merged$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# 정렬된 데이터 출력
merged<-merged[order(merged$month), ]

# 환자와 sample 데이터의 연도별 발생 추이(꺾은선 그래프)
ggplot(merged) +
  geom_line(aes(x = month, y = n.x, color = "Patients"), size = 1.5,group=1) +
  geom_line(aes(x = month, y = n.y, color = "Sample"), size = 1.5,group=1) +
  scale_color_manual(values = c("Patients" = "blue", "Sample" = "red"), labels = c("Patients", "Sample")) +
  labs(x = "Month", y = "Number of pos") +
  scale_y_continuous(breaks=seq(0,3000,200))+
  theme(legend.position="bottom")+
  theme_minimal()

# sample과 환자데이터에서 발생 비율
merged$tot<-merged$n.x+merged$n.y

# 합계 포함한 그래프
ggplot(merged) +
  geom_line(aes(x = month, y = n.x, color = "Patients"), size = 1.5,group=1) +
  geom_line(aes(x = month, y = n.y, color = "Sample"), size = 1.5,group=1) +
  geom_line(aes(x = month, y = tot, color = "total"), size = 1.5,group=1)+
  scale_color_manual(values = c("Patients" = "blue", "Sample" = "red","total"="green"), labels = c("Patients", "Sample","total")) +
  labs(x = "Month", y = "Number of pos") +
  scale_y_continuous(breaks=seq(0,3000,200))+
  theme(legend.position="bottom")+
  theme_minimal()

###################################################################################

# 데이터 정의
city <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주", "세종")
population <- c(462, 249, 99, 239, 215, 125, 78, 1744, 479, 484, 1269, 1134, 1174, 811, 598, 25, 28)

# 데이터 프레임 생성
city_population_df <- data.frame(City = city, Population = population)

# 결과 출력
print(city_population_df)

data <- sample[c("Province", "RT.PCR.Hanta.virus..Lung.")]

# "RT.PCR.Hanta.virus..Lung." 열 값이 "pos"인 행 추출
pos_samples <- data[data$RT.PCR.Hanta.virus..Lung. == "pos", ]

# 1열에서 특정 지명을 다른 이름으로 바꾸기
pos_samples$Province <- gsub("Gangwon", "강원", pos_samples$Province)
pos_samples$Province <- gsub("Gyeonggi", "경기", pos_samples$Province)
pos_samples$Province <- gsub("Jeju", "제주", pos_samples$Province)
pos_samples$Province <- gsub("Jeollanam", "전남", pos_samples$Province)
pos_samples$Province <- gsub("Seoul", "서울", pos_samples$Province)

# 1열 이름을 "city"로 변경
colnames(pos_samples)[1] <- "city"

# pos_samples 데이터프레임을 교차표 형식으로 변환
cross_table <- table(pos_samples$city, pos_samples$RT.PCR.Hanta.virus..Lung.)

# 교차표 데이터프레임으로 변환
cross_df <- as.data.frame(cross_table)
df <- data.frame(City  = cross_df$Var1, Population = cross_df$Freq)

# pos_samples와 city_population_df를 city를 기준으로 병합
merged_df <- merge(city_population_df, df, by = "City", all.x = TRUE)

# pos_samples에 없는 도시의 값을 0으로 설정
merged_df[is.na(merged_df)] <- 0

library(ggplot2)

# 막대 그래프를 그리기 위한 데이터프레임 생성
bar_df <- data.frame(
  City = merged_df$City,
  Population = merged_df$Population.x,
  Positive_Cases = merged_df$Population.y
)

# 막대 그래프 그리기
p4 <- ggplot(bar_df, aes(x = City)) +
  geom_bar(aes(y = Population, fill = "Patient"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Positive_Cases, fill = "Sample"), stat = "identity", position = "dodge") +
  labs(title = "Patient & Sample",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = ""))

ggsave("p4.png", plot = p4, dpi = 300)




