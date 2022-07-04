# 라이브러리
library(dplyr)
library(lubridate)
library(reshape)
library(rvest)
library(RColorBrewer)
library(corrplot)
library(Metrics)
library(ggplot2)

# 저장할 경로 정하기
ggsave_path = "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/EDA_plot/"

## 데이터 불러오기
final_raw_data = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/final_raw_data3.csv", stringsAsFactors = F)
final_test_data = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/final_test_data3.csv", stringsAsFactors = F)

raw_data_prop = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/raw_data_prop6.csv", stringsAsFactors = F)
test_data_prop = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/test_data_prop6.csv", stringsAsFactors = F)

tmp = final_raw_data$period %>% table
tmp %>% sum
tmp[( 40 < ( names(tmp) %>% as.numeric)) &  (( names(tmp) %>% as.numeric) <= 60 )] %>% sum
12807-12504

303 / 12807 * 100

## 자료형 변환
if( (final_raw_data$broad_s %>% nchar %>% max) == 16){
  final_raw_data$broad_s = ymd_hm(final_raw_data$broad_s)
}else{
  final_raw_data$broad_s = ymd_hms(final_raw_data$broad_s)
}
final_test_data$broad_s = ymd_hms(final_test_data$broad_s)
final_raw_data$p_group = factor(final_raw_data$p_group); final_test_data$p_group = factor(final_test_data$p_group)
final_raw_data$month = factor(final_raw_data$month); final_test_data$month = factor(final_test_data$month)
final_raw_data$day = factor(final_raw_data$day); final_test_data$day = factor(final_test_data$day)
final_raw_data$hour = factor(final_raw_data$hour); final_test_data$hour = factor(final_test_data$hour)
final_raw_data$minute = factor(final_raw_data$minute); final_test_data$minute = factor(final_test_data$minute)
final_raw_data$weekdays = factor(final_raw_data$weekdays); final_test_data$weekdays = factor(final_test_data$weekdays)
final_raw_data$pay_method = factor(final_raw_data$pay_method); final_test_data$pay_method = factor(final_test_data$pay_method)

raw_data_prop$broad_time = ymd_hms(raw_data_prop$broad_time); test_data_prop$broad_time = ymd_hms(test_data_prop$broad_time)
raw_data_prop$pay_method = factor(raw_data_prop$pay_method);  test_data_prop$pay_method = factor(test_data_prop$pay_method)

## 변수 및 임시 데이터 만들기
# 데이터 전처리 및 사용할 파생변수 생성
tmp = final_raw_data
tmp0 = final_raw_data %>% filter(prod_sales!=0)
tmp$Y_t1.sales = tmp$Y_t1*tmp$price
tmp$Y_t2.sales = tmp$Y_t2*tmp$price
tmp$Y_t_mean.sales = tmp$Y_t_mean*tmp$price
tmp0$Y_t1.sales = tmp0$Y_t1*tmp0$price
tmp0$Y_t2.sales = tmp0$Y_t2*tmp0$price
tmp0$Y_t_mean.sales = tmp0$Y_t_mean*tmp0$price

# 연속형 변수 취급액, 로그-취급액과의 산점도 + spline + 회귀 직선 + 상관계수 구하는 함수(그림 저장)
numeric_EDA = function(var_name){
  tt1 = tmp[,var_name]
  tt2 = tmp0[,var_name]
  
  file_name1 = paste0(ggsave_path,var_name,"(spline).jpg")
  file_name2 = paste0(ggsave_path,var_name,"(log-spline).jpg")
  
  tt3 = cor(tt1, tmp$prod_sales, use = "complete") %>% round(4)
  tt4 = cor(tt2, log(tmp0$prod_sales), use = "complete" ) %>% round(4)
  
  plot1 = ggplot(tmp, aes(x = tt1, y = prod_sales) ) + geom_point() +
    geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = var_name, title = paste0(var_name," spline, cor : ",tt3))
  plot2 = ggplot(tmp0, aes(x = tt2, y = log(prod_sales)) ) + geom_point() +
    geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = var_name, title = paste0(var_name," log-spline, cor : ",tt4))
  
  ggsave(file_name1, plot = plot1)
  ggsave(file_name2, plot = plot2)
  
  paste0("[",var_name," - Correlation] Y : ", tt3,", log(Y) : ", tt4) %>% print
}

# 범주형 변수 취급액, 로그-취급액과의 박스-상자그림 + ANOVA p-value 구하는 함수(그림 저장)
categorical_EDA = function(var_name){
  tt1 = tmp[,var_name] %>% factor
  tt2 = tmp0[,var_name] %>% factor
  
  file_name1 = paste0(ggsave_path,var_name,"(boxplot).jpg")
  file_name2 = paste0(ggsave_path,var_name,"(log-boxplot).jpg")
  
  
  tt3 = (lm(formula(paste0("prod_sales ~ ",var_name)), data = tmp) %>% anova)$`Pr(>F)` %>% nth(1) %>% round
  tt4 = (lm(formula(paste0("log(prod_sales) ~ ",var_name)), data = tmp0) %>% anova)$`Pr(>F)` %>% nth(1) %>% round
  
  plot1 = ggplot(tmp, aes(x = tt1, y = prod_sales) ) + geom_boxplot() +
    labs(x = var_name, title = paste0(var_name," boxplot, Anova : ",tt3))
  plot2 = ggplot(tmp0, aes(x = tt2, y = log(prod_sales)) ) + geom_boxplot() +
    labs(x = var_name, title = paste0(var_name," boxplot, Anova : ",tt3))
  
  ggsave(file_name1, plot = plot1)
  ggsave(file_name2, plot = plot2)
  
  paste0("[",var_name," - Anova p.value] Y : ", tt3 %>% round(4),", log(Y) : ",tt4 %>% round(4) ) %>% print
}

# 종속변수 : 취급액, 로그-취급액
plot_hist1 = ggplot(data = tmp, aes(x=prod_sales)) + geom_histogram() + labs(x = "취급액", title = "취급액 히스토그램")
plot_hist2 = ggplot(data = tmp, aes(x=log(prod_sales)) ) + geom_histogram() + labs(x = "로그-취급액", title = "로그-취급액 히스토그램")
ggsave( paste0(ggsave_path,"prod_sales_histogram.png") , plot = plot_hist1)
ggsave( paste0(ggsave_path,"log_prod_sales_histogram.png") , plot = plot_hist2)
rm(plot_hist1, plot_hist2)

############### 상품 특성 관련 변수 ###############
##1.판매단가
numeric_EDA("price")

##2.상품군
categorical_EDA("p_group")

##3.지불방식
categorical_EDA("pay_method")


############### 방송 일시 관련 변수 ###############
##1.방송시작시각
categorical_EDA("hour")

##2.요일
categorical_EDA("weekdays")

##3.공휴일
##categorical_EDA("holiday") : X
tt1 = tmp[,"weekdays"] %>% as.character
tt1[tt1=="월요일"] = 1; tt1[tt1=="화요일"] = 2; tt1[tt1=="수요일"] = 3; tt1[tt1=="목요일"] = 4
tt1[tt1=="금요일"] = 5; tt1[tt1=="토요일"] = 6; tt1[tt1=="일요일"] = 7
tt1 = factor(tt1)
levels(tt1) = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")

tt2 = tmp0[,"weekdays"] %>% as.character
tt2[tt2=="월요일"] = 1; tt2[tt2=="화요일"] = 2; tt2[tt2=="수요일"] = 3; tt2[tt2=="목요일"] = 4
tt2[tt2=="금요일"] = 5; tt2[tt2=="토요일"] = 6; tt2[tt2=="일요일"] = 7
tt2 = factor(tt2)
levels(tt2) = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")

file_name1 = paste0(ggsave_path,"weekdays","(boxplot).jpg")
file_name2 = paste0(ggsave_path,"weekdays","(log-boxplot).jpg")

tt3 = (lm(prod_sales~weekdays, data = tmp) %>% anova)$`Pr(>F)` %>% nth(1) %>% round
tt4 = (lm(log(prod_sales)~weekdays, data = tmp0) %>% anova)$`Pr(>F)` %>% nth(1) %>% round

plot1 = ggplot(tmp, aes(x = tt1, y = prod_sales) ) + geom_boxplot() +
  labs(x = "weekdays", title = paste0("weekdays"," boxplot, Anova : ",tt3))
plot2 = ggplot(tmp0, aes(x = tt2, y = log(prod_sales)) ) + geom_boxplot() +
  labs(x = "weekdays", title = paste0("weekdays"," boxplot, Anova : ",tt3))

ggsave(file_name1, plot = plot1)
ggsave(file_name2, plot = plot2)
rm(tt1, tt2, file_name1, file_name2, tt3, tt4, plot1, plot2)


############### 방송 시간 관련 변수 ###############
##1.방송시간
numeric_EDA("period")

##2.방송 판매 상품수
numeric_EDA("with_p_num")
categorical_EDA("with_p_num")

##3.예상 노출시간
numeric_EDA("ex_period")


############### 인기 관련 변수 ###############
##1.별점
numeric_EDA("grade_star")

##2.투표수, New투표수
numeric_EDA("grade_n")
numeric_EDA("new_grade_n")

##3.n번째 판매
numeric_EDA("nth_sale")


############### 브랜드 관련 변수 ###############
##1.브랜드 제품 노출수
numeric_EDA("brand_power1")

##2.브랜드 방송 노출수
numeric_EDA("brand_power2")

##3.브랜드 판매액 평균
numeric_EDA("brand_sales")

##4.브랜드 판매액 누적합
numeric_EDA("brand_sales1")

##5.브랜드 판매수 평균
numeric_EDA("brand_amount")

##6.브랜드 판매수 누적합
numeric_EDA("brand_amount1")


############### 상품명 관련 변수 ###############
##1.18K_단어변수
categorical_EDA("word_18k")

##2.TV_단어변수
categorical_EDA("word_TV")


############### 외부 관련 변수 ###############
##1.날씨변수
numeric_EDA("PCA1")
numeric_EDA("PCA2")
numeric_EDA("PCA3")
numeric_EDA("PCA4")
numeric_EDA("PCA5")
numeric_EDA("PCA6")
numeric_EDA("PCA7")
numeric_EDA("PCA8")
numeric_EDA("PCA9")

##2.소비자 심리지수
numeric_EDA("csi")

##3.시청률
channel = c("KBS1","KBS2","MBC","SBS","tvN","YTN","연합뉴스TV","OCN","MBCevery1") %>% paste0("day_rating.",.)
for(i in 1:length(channel)){
  numeric_EDA(channel[i])
}


############### 과거 실적 관련 변수 ###############
##1.취급액_t1
numeric_EDA("Y_t1.sales")

##2.취급액_t평균
numeric_EDA("Y_t_mean.sales")

##3.취급액_t2
numeric_EDA("Y_t2.sales")


############### 분당 실적 관련 변수 ###############
##1.방송시간 비율
tt3.1 = cor(raw_data_prop$period_prop, raw_data_prop$sales, use = "complete") %>% round(4)
tt4.1 = cor(raw_data_prop[raw_data_prop$sales!=0,"period_prop"],
            log(raw_data_prop[raw_data_prop$sales!=0,"sales"]) , use = "complete") %>% round(4)

plot1.1 = ggplot(raw_data_prop, aes(x = period_prop, y = sales) ) + geom_point() +
  geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = "period_prop", title = paste0("period_prop spline, cor : ",tt3.1))
plot2.1 = ggplot(raw_data_prop %>% filter(sales!=0), aes(x = period_prop, y = log( sales )) ) + geom_point() +
  geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = "period_prop", title = paste0("period_prop spline, cor : ",tt4.1))

ggsave(paste0(ggsave_path,"period_prop(spline).jpg"), plot = plot1.1)
ggsave(paste0(ggsave_path,"period_prop(log-spline).jpg"), plot = plot2.1)

paste0("[period_prop - Correlation] Y : ", cor(raw_data_prop$period_prop, raw_data_prop$sales, use = "complete") %>% round(4),
       ", log(Y) : ", cor(raw_data_prop[raw_data_prop$sales!=0,"period_prop"],
                          log(raw_data_prop[raw_data_prop$sales!=0,"sales"]) , use = "complete") %>% round(4) ) %>% print

##2.방송시간 누적비율
tt3.1 = cor(raw_data_prop$period_cum_prop, raw_data_prop$sales, use = "complete") %>% round(4)
tt4.1 = cor(raw_data_prop[raw_data_prop$sales!=0,"period_cum_prop"],
          log(raw_data_prop[raw_data_prop$sales!=0,"sales"]) , use = "complete") %>% round(4)

plot1.1 = ggplot(raw_data_prop, aes(x = period_cum_prop, y = sales) ) + geom_point() +
  geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = "period_cum_prop", title = paste0("period_cum_prop spline, cor : ",tt3.1))
plot2.1 = ggplot(raw_data_prop %>% filter(sales!=0), aes(x = period_cum_prop, y = log( sales )) ) + geom_point() +
  geom_smooth(color = "red") + geom_smooth(method="lm") + labs(x = "period_cum_prop", title = paste0("period_cum_prop spline, cor : ",tt4.1))

ggsave(paste0(ggsave_path,"period_cum_prop(spline).jpg"), plot = plot1.1)
ggsave(paste0(ggsave_path,"period_cum_prop(log-spline).jpg"), plot = plot2.1)

paste0("[period_cum_prop - Correlation] Y : ", tt3.1,", log(Y) : ",  tt4.1) %>% print





