sample <- read.csv('C:/Users/seongjun/Desktop/SPSS_data_all_set.csv')

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




