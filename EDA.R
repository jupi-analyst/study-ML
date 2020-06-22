library(ggplot2)
library(tidyverse)
# 1. 변동성(변수 내에)
# 범주형 막대 그래프
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# 연속형 변수는 히스토그램으로 변동을 이해
diamonds %>%
  group_by(cut) %>% tally
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# 연속형 변수 범주화
diamonds %>%
  count(cut_width(carat, 0.5))
ggplot(diamonds, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1) #히스토그램 겹쳐사용 할 경우 geom_freqpoly()


# 2.이상점
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual

#결측값 제거
diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))

#이상값을 결측값으로 치환
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y >20, NA, y))

# 3. 변수간의 변수 (공변수)
# 연속형 x 범주형 -> 밀도그래프, 박스플롯
ggplot(diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# 범주형 x 범주형
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# 연속형 x 연속형
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 0.01) # 투명도 도입

ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price)) #직사각형 구간

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price)) # 육각형 구간

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20))) #연속형 변수를 범주화 하고 박스플롯

library(readr)
store.df <- read_csv("http://r-marketing.r-forge.r-project.org/data/rintro-chaper3.csv")
