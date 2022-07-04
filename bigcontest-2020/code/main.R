# install.packages(c("dplyr","lubridate","reshape","rvest","RColorBrewer","corrplot"))
## 라이브러리
library(dplyr)
library(lubridate)
library(reshape)
library(rvest)
library(RColorBrewer)
library(corrplot)
library(Metrics)
library(xgboost)
library(caret)
library(mlr)

# 실적 데이터 경로 지정(_after.csv파일 포함)
after_path = "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/01_제공데이터"
# EDA plot 저장할 경로 지정
ggsave_path = "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/EDA_plot/"
# 외부 데이터 경로 지정
ex_path = "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/External Data/"
# 저장하고 읽을 최종 데이터 경로 지정
final_path = "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/"

## set working directory
setwd(after_path)

############################## loading data ##############################
##1.BigContest data : 분당 실적 데이터
#간단한 것들만 모은 전처리 함수
pre1 = function(rdata){
  #데이터 형태 정의 및 간단한 전처리
  rdata$broad_time = rdata$broad_time %>% ymd_hm()
  rdata$price = rdata$price %>% gsub(",","",.) %>% gsub("-","",.) %>% as.numeric
  rdata$sales = rdata$sales %>% gsub(",","",.) %>% as.numeric
  rdata[(rdata$sales==50000) & (!is.na(rdata$sales)),"sales"] = 0
  #판매수 변수 생성
  rdata$amount = rdata$sales/rdata$price

  #상품명 : 무이자 일시불 전처리
  rdata$p_name = rdata$p_name %>%
    gsub("[[:punct:]]무[[:punct:]] ","무이자 ",.) %>% gsub("[[:punct:]]무[[:punct:]]","무이자 ",.) %>%
    gsub("[[:punct:]]일[[:punct:]] ","일시불 ",.) %>% gsub("[[:punct:]]일[[:punct:]]","일시불 ",.) %>%
    gsub("무[[:punct:]] ","무이자 ",.) %>% gsub("무[[:punct:]]","무이자 ",.) %>%
    gsub("일[[:punct:]] ","일시불 ",.) %>% gsub("일[[:punct:]]","일시불 ",.) %>%
    trimws(x = ., which = "right") %>% trimws(x = ., which = "left")

  #연도, 월, 일, 시각, 분, 요일 변수 생성
  rdata$year = year(rdata$broad_time)
  rdata$month = month(rdata$broad_time)
  rdata$day = day(rdata$broad_time)
  rdata$hour = hour(rdata$broad_time)
  rdata$minute = minute(rdata$broad_time)
  rdata$weekdays = weekdays(rdata$broad_time)
  
  # 노출(분) 변수 결측치 채우기
  na_which = which(is.na(rdata$ex_minute)); length(na_which)
  sum(rdata[na_which-1,"broad_time"]!=rdata[na_which,"broad_time"])
  while(length(na_which)>0){
    na_which = which(is.na(rdata$ex_minute))
    rdata[na_which,"ex_minute"] = rdata[na_which-1,"ex_minute"]
  }

  return(rdata)
}

# 분당 실적 데이터 불러오기
raw_data_sale = read.csv("2020 빅콘테스트 데이터분석분야-챔피언리그_2019년 실적데이터_after.csv", stringsAsFactors=F,
                         col.names = c("broad_time","ex_minute","m_code","p_code","p_name","p_group","price","sales"), header=T)
test_data_sale = read.csv("2020 빅콘테스트 데이터분석분야-챔피언리그_2020년 6월 판매실적예측데이터(평가데이터)_after.csv",
                          stringsAsFactors=F, col.names = c("broad_time","ex_minute","m_code","p_code","p_name","p_group","price","sales"), header=T)

# 앞서 만든 간단한 전처리 함수 적용
raw_data_sale = pre1(raw_data_sale)
test_data_sale = pre1(test_data_sale)

#my_id 변수 생성
raw_data_sale$my_id = 1:nrow(raw_data_sale)
test_data_sale$my_id = 1:nrow(test_data_sale)

##2.상품별 실적 데이터로 만들기
#같은 방송, 같은 상품을 구분하기 위한 my_group 변수 생성
tmp = raw_data_sale %>% arrange(., p_code, m_code, broad_time)
tmp$diff_time = c(0,diff(tmp$broad_time) %>% as.numeric(., units="mins"))
tmp$change = 0
tmp[(tmp$diff_time<0)|(tmp$diff_time>90),"change"]=1
tmp$my_group = cumsum(tmp$change)
tmp$diff_time=NULL; tmp$change = NULL
#my_id 변수를 활용하여 원 데이터 순서로 바꾸고 my_id 변수 제거
raw_data_sale = tmp[order(tmp$my_id),]
raw_data_sale$my_id = NULL
tmp$my_id = NULL
#my_group 변수와 dplyr 라이브러리를 활용하여 상품별 실적 데이터 생성
#방송시간은 노출(분)을 합쳐서 생성, 취급액 및 판매수 또한 합쳐서 생성
tmp1 = tmp %>% arrange(broad_time) %>% group_by(my_group) %>%
  summarise(broad_s = min(broad_time), period = sum(ex_minute),
            m_code = unique(m_code), p_code = unique(p_code), p_name = unique(p_name),
            p_group = unique(p_group), price = unique(price),
            sales = sum(sales), amount = sum(amount)) %>% arrange(., broad_s) %>% as.data.frame
#상품군 무형 제외
raw_data = tmp1 %>% filter(p_group != "무형")
rm(tmp, tmp1)

#위의 작업과 동일하게 test set도 상품별 실적 데이터 생성
tmp = test_data_sale %>% arrange(., p_code, m_code, broad_time)
tmp$diff_time = c(0,diff(tmp$broad_time) %>% as.numeric(., units="mins"))
tmp$change = 0
tmp[(tmp$diff_time<0)|(tmp$diff_time>90),"change"]=1
tmp$my_group = cumsum(tmp$change)
tmp$diff_time=NULL; tmp$change = NULL

test_data_sale = tmp[order(tmp$my_id),]
tmp$my_id = NULL

tmp1 = tmp %>% arrange(broad_time) %>% group_by(my_group) %>%
  summarise(broad_s = min(broad_time), period = sum(ex_minute),
            m_code = unique(m_code), p_code = unique(p_code), p_name = unique(p_name),
            p_group = unique(p_group), price = unique(price),
            sales = sum(sales), amount = sum(amount)) %>% arrange(., broad_s) %>% as.data.frame
test_data = tmp1 %>% filter(p_group != "무형")
rm(tmp, tmp1)



##3.외부 데이터와 합치기
{
################ Ns shop 데이터 : nsshop_data_after.csv
# Ns shop 데이터 불러오기
nsshop = read.csv(paste0(ex_path,"nsshop_data_after.csv"), stringsAsFactors = F)

# 방송일시 전처리
tp = nsshop$broad_time %>% gsub(" ","",.) %>% strsplit("~")
tp1 = tp %>% sapply(.,"[",1)
tp2 = tp %>% sapply(.,"[",2)
tp1[grepl("오전",tp1)] = paste(tp1[grepl("오전",tp1)],"AM")
tp1[grepl("오후",tp1)] = paste(tp1[grepl("오후",tp1)],"PM")
tp2[grepl("오전",tp2)] = paste(tp2[grepl("오전",tp2)],"AM")
tp2[grepl("오후",tp2)] = paste(tp2[grepl("오후",tp2)],"PM")
nsshop$broad_s = tp1 %>% gsub("오전","",.) %>% gsub("오후","",.) %>% paste(nsshop$date,.) %>% ymd_hm()
nsshop$broad_e = tp2 %>% gsub("오전","",.) %>% gsub("오후","",.) %>% paste(nsshop$date,.) %>% ymd_hm()
rm(tp,tp1,tp2)

# 상품명 전처리
tmp = nsshop$prod_name %>% strsplit("]")
tmp2 = cbind(tmp %>% sapply(.,"[",1), tmp %>% sapply(.,"[",2))
tmp3 = tmp %>% sapply(.,"length")
tmp4 = NULL
tmp4[tmp3==1] = tmp2[tmp3==1,1]
tmp4[tmp3==2] = tmp2[tmp3==2,2]
tmp5 = tmp4 %>%
  gsub("[[:punct:]]무이자[[:punct:]]","무이자 ",.) %>% gsub("[[:punct:]]일시불[[:punct:]]","일시불 ",.) %>%
  gsub("[[:punct:]]무[[:punct:]] ","무이자 ",.) %>% gsub("[[:punct:]]무[[:punct:]]","무이자 ",.) %>% 
  gsub("[[:punct:]]일[[:punct:]] ","일시불 ",.) %>% gsub("[[:punct:]]일[[:punct:]]","일시불 ",.) %>%
  trimws(x = ., which = "right") %>% trimws(x = ., which = "left")
nsshop$prod_name = tmp5
rm(tmp, tmp2, tmp3, tmp4, tmp5)

# 시작 시각, 시작 분, 종료 시각, 종료 분 변수 생성
nsshop$hour_s = nsshop$broad_s %>% hour
nsshop$mins_s = nsshop$broad_s %>% minute
nsshop$hour_e = nsshop$broad_e %>% hour
nsshop$mins_e = nsshop$broad_e %>% minute

# 방송일시 전처리
nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==56), "broad_s" ] =
  nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==56), "broad_s" ] + minutes(4)
nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==56), "broad_e" ] =
  nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==56), "broad_e" ] + minutes(4)
nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==59), "broad_s" ] =
  nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==59), "broad_s" ] + minutes(1)
nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==59), "broad_e" ] =
  nsshop[(nsshop$hour_e == 6) & (nsshop$mins_e==59), "broad_e" ] + minutes(1)

# 방송일시 전처리 후 다시 시작 시각, 시작 분, 종료 시각, 종료 분 변수 재생성
nsshop$hour_s = nsshop$broad_s %>% hour
nsshop$mins_s = nsshop$broad_s %>% minute
nsshop$hour_e = nsshop$broad_e %>% hour
nsshop$mins_e = nsshop$broad_e %>% minute

# 새벽 방송 제거
tmp1 = nsshop[!((2<=nsshop$hour_s) & (nsshop$hour_s<6)), ]

# 크롤링 과정에서 00시 넘어갈 때 날짜가 넘어가지 않는 오류 전처리
tmp1[tmp1$hour_s %in% 0:2,"broad_s"] = tmp1[tmp1$hour_s %in% 0:2,"broad_s"] + days(1)
tmp1[tmp1$hour_e %in% 0:2,"broad_e"] = tmp1[tmp1$hour_e %in% 0:2,"broad_e"] + days(1)

# 방송일시 전처리 후 다시 시작 시각, 시작 분, 종료 시각, 종료 분 변수 재생성
tmp1$hour_s = tmp1$broad_s %>% hour
tmp1$mins_s = tmp1$broad_s %>% minute
tmp1$hour_e = tmp1$broad_e %>% hour
tmp1$mins_e = tmp1$broad_e %>% minute

nsshop = tmp1; rm(tmp1)

# 방송일시 전처리
nsshop[nsshop$mins_s==57, "broad_s"] = nsshop[nsshop$mins_s==57, "broad_s"] + minutes(3)
nsshop[nsshop$mins_s==57, "broad_e"] = nsshop[nsshop$mins_s==57, "broad_e"] + minutes(3)
nsshop[(nsshop$mins_s==58) | (nsshop$mins_s==18), "broad_s"] =
  nsshop[(nsshop$mins_s==58) | (nsshop$mins_s==18), "broad_s"] + minutes(2)
nsshop[(nsshop$mins_s==58) | (nsshop$mins_s==18), "broad_e"] =
  nsshop[(nsshop$mins_s==58) | (nsshop$mins_s==18), "broad_e"] + minutes(2)
nsshop[(nsshop$mins_s==59) | (nsshop$mins_s==19), "broad_s"] =
  nsshop[(nsshop$mins_s==59) | (nsshop$mins_s==19), "broad_s"] + minutes(1)
nsshop[(nsshop$mins_s==59) | (nsshop$mins_s==19), "broad_e"] =
  nsshop[(nsshop$mins_s==59) | (nsshop$mins_s==19), "broad_e"] + minutes(1)

# 방송일시 전처리 후 다시 시작 시각, 시작 분, 종료 시각, 종료 분 변수 재생성
nsshop$hour_s = nsshop$broad_s %>% hour
nsshop$mins_s = nsshop$broad_s %>% minute
nsshop$hour_e = nsshop$broad_e %>% hour
nsshop$mins_e = nsshop$broad_e %>% minute

# 판매가격 결측치를 세일가격으로 채우기
nsshop[is.na(nsshop$price_prior),"price_prior"] = nsshop[is.na(nsshop$price_prior),"price_sale"]

#브랜드명의 [미정의]로 나타나는 결측치 채우기
tt2 = nsshop[nsshop$brand_name=="[미정의]","prod_name"] %>% unique
tmp1 = nsshop[nsshop$brand_name!="[미정의]",c("prod_name","brand_name")] %>% unique
tmp2 = tmp1[tmp1$prod_name %in% tt2,]
colnames(tmp2) = c("prod_name","replace_brand_name")

tmp3 = merge(nsshop, tmp2, by ="prod_name", all.x = T)
tmp3[((tmp3$brand_name=="[미정의]") & (tmp3$prod_name %in% tmp2$prod_name)),"brand_name"] = 
  tmp3[((tmp3$brand_name=="[미정의]") & (tmp3$prod_name %in% tmp2$prod_name)),"replace_brand_name"]
tmp3$replace_brand_name = NULL

nsshop = tmp3; rm(tt2,tmp1,tmp2,tmp3)

# New투표수 변수 만들기
nsshop$my_id = 1:nrow(nsshop)
tmp = nsshop %>% group_by(prod_name) %>% summarise(new_grade_n = grade_n %>% unique %>% sum)
tmp1 = merge(nsshop, tmp, by="prod_name")
tmp2 = tmp1 %>% arrange(prod_name, broad_s) %>% group_by(prod_name) %>%
  summarise( new_grade_n1 = cumsum( new_grade_n/length(new_grade_n) ) )
tmp1$new_grade_n = tmp2$new_grade_n1
nsshop = tmp1 %>% arrange(my_id)
nsshop$my_id = NULL
rm(tmp, tmp1, tmp2)


# 상품별 실적 데이터에 미리 변수명 만들기
raw_data$price_prior = raw_data$price_sale = raw_data$grade_n = raw_data$grade_star =
  raw_data$prod_name = raw_data$brand_name = raw_data$new_grade_n = NA
test_data$price_prior = test_data$price_sale = test_data$grade_n = test_data$grade_star =
  test_data$prod_name = test_data$brand_name = test_data$new_grade_n = NA

# raw data set의 상품별 실적 데이터와 NS shop 데이터 합치기
tmp1 = raw_data
# 매칭이 안되는 행을 저장할 변수 : error_n
error_n = NULL
# 해당 for loop는 상품별 실적 데이터의 하나의 관측치마다 매칭되는 Ns shop 데이터를 찾아서 합치는 loop
for(i in 1:nrow(tmp1)){
  # 종료 방송일시 생성
  tmp_end = tmp1[i,"broad_s"] + minutes(tmp1[i,"period"])
  # 상품별 실적 데이터에서 방송 시간동안 매칭되는 Ns shop 데이터를 tmp에 저장
  tmp = nsshop %>% filter( tmp1[i,"broad_s"] <= broad_s, broad_s <= tmp_end)
  
  # 매칭 시작
  if(nrow(tmp)==1){
    # 매칭되는 Ns shop 데이터가 1개라면 그대로 매칭
    tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
      tmp[1,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")]
    next
  }else if(nrow(tmp)==0){
    # 매칭되는 NS shop 데이터가 없다면 매칭시키지 않고 패스
    next
  }else{
    # 위의 과정에서 걸러지지 않은 것은 매칭되는 NS shop 데이터가 2개 이상인 경우인데 상품명을 살펴서 1:1로 매칭
    tmp1_prod.n = tmp1[i,"p_name"]
    # 해당 상품명을 Ns shop 데이터 상품명 중에 포함하는 개수가 1개인 경우 그대로 매칭
    if( (grepl(tmp1_prod.n, tmp$prod_name, fixed=T) %>% sum) == 1 ){
      tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
        tmp[grepl(tmp1_prod.n, tmp$prod_name, fixed=T), c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
    }else{
      # 해당 상품명을 NS shop 데이터 상품명 중에 포함하는 개수가 1개가 아니므로 다양한 방식으로 상품명을 고쳐서 확인
      # 1) "(문자 or 숫자)" 를 제외하고 오른쪽 빈칸을 제외한 상품명 생성
      prod.n1 = tmp1_prod.n %>% gsub("[\\(][[:alnum:]]+[\\)]","",.) %>% trimws(x = ., which = "right")
      # 2) "(문자 or 숫자)" 를 제외하고 모든 빈칸을 제외한 상품명 생성
      prod.n2 = tmp1_prod.n %>% gsub("[\\(][[:alnum:]]+[\\)]","",.) %>% gsub(" ","",.)
      # 3) "특수문자" 를 제외하고 모든 빈칸을 제외한 상품명 생성
      prod.n3 = tmp1_prod.n %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.)
      # 4) "특수문자" 를 제외하고 모든 빈칸을 제외하고 모든 영어를 제외한 상품명 생성
      prod.n4 = tmp1_prod.n %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.) %>% gsub("[A-z]","",.)
      
      # 위와 같은 과정을 NS shop 데이터에도 적용한 후 앞으로 비교하여 1:1 매칭되는지 확인
      ns_prod.n2 = gsub(" ","",tmp$prod_name)
      ns_prod.n3 = tmp$prod_name %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.)
      ns_prod.n4 = tmp$prod_name %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.) %>% gsub("[A-z]","",.)
      
      # 각 경우 마다 상품명 매칭 확인
      if( (grepl(prod.n1 , tmp$prod_name, fixed=T) %>% sum) == 1 ){
        # 1) "(문자 or 숫자)" 를 제외하고 오른쪽 빈칸을 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name", "prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(prod.n1 , tmp$prod_name, fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl( prod.n2 , ns_prod.n2 , fixed=T) %>% sum) == 1 ){
        # 2) "(문자 or 숫자)" 를 제외하고 모든 빈칸을 제외한 상품명 매칭 확인 
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl( prod.n2 , ns_prod.n2 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                         "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl(  prod.n3 , ns_prod.n3 , fixed=T) %>% sum) == 1 ){
        # 3) "특수문자" 를 제외하고 모든 빈칸을 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(  prod.n3 , ns_prod.n3 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl(  prod.n4 , ns_prod.n4 , fixed=T) %>% sum) == 1 ){
        # 4) "특수문자" 를 제외하고 모든 빈칸을 제외하고 모든 영어를 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(  prod.n4 , ns_prod.n4 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else{
        # 나머지 error들 확인하여 직접 수작업 상품명 수정 후 nsshop_data_after.csv,
        # 2020 빅콘테스트 데이터분석분야-챔피언리그_2019년 실적데이터_after.csv,
        # 2020 빅콘테스트 데이터분석분야-챔피언리그_2020년 6월 판매실적예측데이터(평가데이터)_after.csv 에 저장
        error_n = c(error_n,i) 
      }
    }
  }
}
# for loop에서 사용한 임시변수들 제거 및 raw_data에 합친 데이터 저장
rm(tmp_end,tmp,tmp1_prod.n,ns_prod.n2,ns_prod.n3,ns_prod.n4,prod.n1,prod.n2,prod.n3,prod.n4)
raw_data = tmp1; rm(tmp1, error_n, i)

# test data set의 상품별 실적 데이터와 NS shop 데이터 합치기
tmp1 = test_data
# 매칭이 안되는 행을 저장할 변수 : error_n
error_n = NULL
# 해당 for loop는 상품별 실적 데이터의 하나의 관측치마다 매칭되는 Ns shop 데이터를 찾아서 합치는 loop
for(i in 1:nrow(tmp1)){
  # 종료 방송일시 생성
  tmp_end = tmp1[i,"broad_s"] + minutes(tmp1[i,"period"])
  # 상품별 실적 데이터에서 방송 시간동안 매칭되는 Ns shop 데이터를 tmp에 저장
  tmp = nsshop %>% filter( tmp1[i,"broad_s"] <= broad_s, broad_s <= tmp_end)
  
  # 매칭 시작
  if(nrow(tmp)==1){
    # 매칭되는 Ns shop 데이터가 1개라면 그대로 매칭
    tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
      tmp[1,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")]
    next
  }else if(nrow(tmp)==0){
    # 매칭되는 NS shop 데이터가 없다면 매칭시키지 않고 패스
    next
  }else{
    # 위의 과정에서 걸러지지 않은 것은 매칭되는 NS shop 데이터가 2개 이상인 경우인데 상품명을 살펴서 1:1로 매칭
    tmp1_prod.n = tmp1[i,"p_name"]
    # 해당 상품명을 Ns shop 데이터 상품명 중에 포함하는 개수가 1개인 경우 그대로 매칭
    if( (grepl(tmp1_prod.n, tmp$prod_name, fixed=T) %>% sum) == 1 ){
      tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
        tmp[grepl(tmp1_prod.n, tmp$prod_name, fixed=T), c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
    }else{
      # 해당 상품명을 NS shop 데이터 상품명 중에 포함하는 개수가 1개가 아니므로 다양한 방식으로 상품명을 고쳐서 확인
      # 1) "(문자 or 숫자)" 를 제외하고 오른쪽 빈칸을 제외한 상품명 생성
      prod.n1 = tmp1_prod.n %>% gsub("[\\(][[:alnum:]]+[\\)]","",.) %>% trimws(x = ., which = "right")
      # 2) "(문자 or 숫자)" 를 제외하고 모든 빈칸을 제외한 상품명 생성
      prod.n2 = tmp1_prod.n %>% gsub("[\\(][[:alnum:]]+[\\)]","",.) %>% gsub(" ","",.)
      # 3) "특수문자" 를 제외하고 모든 빈칸을 제외한 상품명 생성
      prod.n3 = tmp1_prod.n %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.)
      # 4) "특수문자" 를 제외하고 모든 빈칸을 제외하고 모든 영어를 제외한 상품명 생성
      prod.n4 = tmp1_prod.n %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.) %>% gsub("[A-z]","",.)
      
      # 위와 같은 과정을 NS shop 데이터에도 적용한 후 앞으로 비교하여 1:1 매칭되는지 확인
      ns_prod.n2 = gsub(" ","",tmp$prod_name)
      ns_prod.n3 = tmp$prod_name %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.)
      ns_prod.n4 = tmp$prod_name %>% gsub("[[:punct:]]","",.) %>% gsub(" ","",.) %>% gsub("[A-z]","",.)
      
      # 각 경우 마다 상품명 매칭 확인
      if( (grepl(prod.n1 , tmp$prod_name, fixed=T) %>% sum) == 1 ){
        # 1) "(문자 or 숫자)" 를 제외하고 오른쪽 빈칸을 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name", "prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(prod.n1 , tmp$prod_name, fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl( prod.n2 , ns_prod.n2 , fixed=T) %>% sum) == 1 ){
        # 2) "(문자 or 숫자)" 를 제외하고 모든 빈칸을 제외한 상품명 매칭 확인 
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl( prod.n2 , ns_prod.n2 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                         "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl(  prod.n3 , ns_prod.n3 , fixed=T) %>% sum) == 1 ){
        # 3) "특수문자" 를 제외하고 모든 빈칸을 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(  prod.n3 , ns_prod.n3 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else if( (grepl(  prod.n4 , ns_prod.n4 , fixed=T) %>% sum) == 1 ){
        # 4) "특수문자" 를 제외하고 모든 빈칸을 제외하고 모든 영어를 제외한 상품명 매칭 확인
        tmp1[i,c("brand_name","prod_name","grade_star","grade_n","new_grade_n","price_sale","price_prior")] =
          tmp[grepl(  prod.n4 , ns_prod.n4 , fixed=T) , c("brand_name","prod_name","grade_star","grade_n",
                                                          "new_grade_n","price_sale","price_prior")]
        
      }else{
        # 나머지 error들 확인하여 직접 수작업 상품명 수정 후 nsshop_data_after.csv,
        # 2020 빅콘테스트 데이터분석분야-챔피언리그_2019년 실적데이터_after.csv,
        # 2020 빅콘테스트 데이터분석분야-챔피언리그_2020년 6월 판매실적예측데이터(평가데이터)_after.csv 에 저장
        error_n = c(error_n,i) 
      }
    }
  }
}
# for loop에서 사용한 임시변수들 제거 및 test_data에 합친 데이터 저장
rm(tmp_end,tmp,tmp1_prod.n,ns_prod.n2,ns_prod.n3,ns_prod.n4,prod.n1,prod.n2,prod.n3,prod.n4)
test_data = tmp1; rm(tmp1, error_n, i)

#nsshop 데이터 제거
rm(nsshop)
}

{
# 공휴일, 기상, 소비자 심리지수 데이터 불러오기
holi_data = read.csv(paste0(ex_path,"holi_data.csv"), stringsAsFactors = F)
weather_data = read.csv(paste0(ex_path,"weather.csv"), stringsAsFactors = F)
csi_data = read.csv(paste0(ex_path,"CSI.csv"), stringsAsFactors = F)

################ 공휴일 데이터 : holi_data
# 방송일시에서 날짜를 뽑아 공휴일에 해당되면 1 아니면 0으로 변수 생성
holi = grepl(paste0(holi_data$date,collapse = "|"),
             raw_data$broad_s %>% as.character %>% strsplit(" ") %>% sapply(.,"[",1) %>% gsub("-","",.))
# 방송일시에서 일요일일 때 0인 경우를 1로 수정
holi = holi | (weekdays(raw_data$broad_s)=="일요일")
# 숫자형 변수로 변경 후 데이터에 변수 대입
holi = as.numeric(holi)
raw_data$holiday = holi; rm(holi)

#위의 작업과 동일하게 test set도 공휴일 변수 생성
holi = grepl(paste0(holi_data$date,collapse = "|"),
             test_data$broad_s %>% as.character %>% strsplit(" ") %>% sapply(.,"[",1) %>% gsub("-","",.))
holi = holi | (weekdays(test_data$broad_s)=="일요일")
holi = as.numeric(holi)
test_data$holiday = holi; rm(holi)

################ 기상 데이터 : weather_data
# 날씨 변수 전처리
# (1) 강수량
weather_data[(weather_data$precipitation==0)&(!is.na(weather_data$precipitation)), "precipitation"] = 0.025
weather_data[is.na(weather_data$precipitation), "precipitation"] = 0
# (2) 풍속
weather_data[(weather_data$wind_speed==0)&(!is.na(weather_data$wind_speed)), "wind_speed"] = 0.025
weather_data[is.na(weather_data$wind_speed), "wind_speed"] = 0
# (3) 일조량
weather_data[(weather_data$sunshine==0)&(!is.na(weather_data$sunshine)), "sunshine"] = 0.025
weather_data[is.na(weather_data$sunshine), "sunshine"] = 0
# (4) 일사량
weather_data[(weather_data$insolation==0)&(!is.na(weather_data$insolation)), "insolation"] = 0.0025
weather_data[is.na(weather_data$insolation), "insolation"] = 0

# 지역별 데이터를 서울_강수량, ..., 대전_일사량 등으로 데이터 변환
city_name = c("서울","부산","대구","인천","광주","대전")
tmp_wheather = data.frame(date = weather_data[weather_data$region=="서울","date"])
for(i in 1:length(city_name)){
  tmp = weather_data[weather_data$region==city_name[i],-(1:2)]
  colnames(tmp) = c(paste0(city_name[i],colnames(tmp)) )

  tmp_wheather = cbind(tmp_wheather, tmp)
}
rm(tmp, city_name)
# 데이터 변환 후 각 지역을 알파벳 대문자로 각 기상 변수를 숫자로 변수명 변환
colnames(tmp_wheather) = c( "date", paste0( rep(c("A","B","C","D","E","F"),each=8),rep(1:8,6) ) )

# 기상 데이터 PCA(주성분 분석) 시행 및 결과
pca_wheather = prcomp(tmp_wheather[,-1], center = T, scale = T)
summary(pca_wheather)
# PCA 9개로 축약
pca_wt = pca_wheather$rotation[,1:9]
# PCA 점수로 변환
tmp1 = NULL
for(i in 1:ncol(pca_wt)){
  tmp = scale(tmp_wheather[,-1]) *
    matrix(rep(pca_wt[,i]), nrow=nrow(tmp_wheather), ncol=ncol(tmp_wheather)-1, byrow=T)
  tmp1 = cbind(tmp1, tmp %>% apply(.,1,sum))
}
# 변수명 PCA1~9로 저장 후 최종 기상데이터 생성
colnames(tmp1) = paste0("PCA",1:ncol(pca_wt))
weather_pca_data = data.frame(date=tmp_wheather$date, tmp1)
rm(pca_wheather, pca_wt, tmp, tmp_wheather, tmp1, weather_data)
# 최종 기상데이터 변수형 정의 및 병합할 때 사용할 변수 생성
weather_pca_data$date = ymd_hm(weather_pca_data$date)
weather_pca_data$tmp_merge = weather_pca_data$date %>% as.character %>% strsplit(":") %>%
  sapply(.,"[",1) %>%gsub("-","",.) %>% gsub(" ","",.)
weather_pca_data$date = NULL

# 최종 기상데이터와 train 상품별 실적 데이터 병합
raw_data$tmp_merge =
  raw_data$broad_s %>% as.character %>% strsplit(":") %>% sapply(.,"[",1) %>% gsub("-","",.) %>% gsub(" ","",.)
raw_data = merge(raw_data, weather_pca_data, by="tmp_merge")
raw_data$tmp_merge = NULL

# 최종 기상데이터와 test 상품별 실적 데이터 병합
test_data$tmp_merge =
  test_data$broad_s %>% as.character %>% strsplit(":") %>% sapply(.,"[",1) %>% gsub("-","",.) %>% gsub(" ","",.)
test_data = merge(test_data, weather_pca_data, by="tmp_merge")
test_data$tmp_merge = NULL

################ 소비자 심리지수 데이터 : csi_data
# 변수명 전처리
tmp_name = names(csi_data) %>% gsub("월|X","",.)
colnames(csi_data) = NULL
# 최종 소비자 심리지수 데이터 생성
csi_data = data.frame(tmp_name,csi_data %>% unlist);rm(tmp_name)
colnames(csi_data) = c("date","csi")
# 최종 소비자 심리지수 데이터와 train 상품별 실적 데이터 병합
raw_data$tmp_merge =
  raw_data$broad_s %>% as.character %>% strsplit(" ") %>% sapply(.,"[",1) %>% gsub("-","",.) %>% substr(.,1,6)
raw_data = merge(x=raw_data, y=csi_data, by.x="tmp_merge", by.y="date")
raw_data$tmp_merge = NULL
# 최종 소비자 심리지수 데이터와 test 상품별 실적 데이터 병합
test_data$tmp_merge =
  test_data$broad_s %>% as.character %>% strsplit(" ") %>% sapply(.,"[",1) %>% gsub("-","",.) %>% substr(.,1,6)
test_data = merge(x=test_data, y=csi_data, by.x="tmp_merge", by.y="date")
test_data$tmp_merge = NULL

rm(holi_data, weather_pca_data, csi_data)
}

############################## 방송시간 관련변수 생성 ##############################
#1. 월, 일, 시각, 분, 요일 변수 생성(month, day, hour, minute, weekdays)
raw_data$month = month(raw_data$broad_s)
raw_data$day = day(raw_data$broad_s)
raw_data$hour = hour(raw_data$broad_s)
raw_data$minute = minute(raw_data$broad_s)
raw_data$weekdays = weekdays(raw_data$broad_s)

test_data$month = month(test_data$broad_s)
test_data$day = day(test_data$broad_s)
test_data$hour = hour(test_data$broad_s)
test_data$minute = minute(test_data$broad_s)
test_data$weekdays = weekdays(test_data$broad_s)

#2. 방송 상품수, 노출시간 변수 생성(with_p_num, ex_period)
raw_data = raw_data %>% group_by(broad_s) %>% summarise(with_p_num = length(period)) %>% merge(raw_data,., by="broad_s")
raw_data$ex_period = raw_data$period/raw_data$with_p_num

test_data = test_data %>% group_by(broad_s) %>% summarise(with_p_num = length(period)) %>% merge(test_data,., by="broad_s")
test_data$ex_period = test_data$period/test_data$with_p_num


############################## 다른 파생 변수 생성 ##############################
#1. 지불방식 변수 생성( pay_method(X / 무 / 일) )
pay_method = rep(0,nrow(raw_data))
pay_method[ grepl("무이자",raw_data$p_name) ] = 1
pay_method[ grepl("일시불",raw_data$p_name) ] = 2
raw_data$pay_method = factor(pay_method); rm(pay_method)

pay_method = rep(0,nrow(test_data))
pay_method[ grepl("무이자",test_data$p_name) ] = 1
pay_method[ grepl("일시불",test_data$p_name) ] = 2
test_data$pay_method = factor(pay_method); rm(pay_method)

#2. 브랜드 제품 노출수, 브랜드 방송 노출수(brand_power1, brand_power2)
# 브랜드 판매액 평균, 누적합(brand_sales, brand_sales1)
# 브랜드 판매수 평균, 누적합(brand_amount, brand_amount1) 변수 생성
# train + test set 합쳐서 변수 생성
tmp = rbind(raw_data, test_data)
tmp$broad_s = as.character(tmp$broad_s)

# 변수명 정의
brand_sales = NA; brand_sales1 = NA
brand_amount = NA; brand_amount1 = NA
brand_power1 = 0
brand_power2 = 0

for(i in 2:nrow(tmp)){
  tmp1 = tmp[i,c("brand_name","broad_s")] %>% unlist
  # 각 관측치 이전의 브랜드명이 같은 데이터들 읽고 그 중 결측치 제거
  tmp2 = tmp[1:(i-1),] %>% filter(brand_name==tmp1[1], broad_s!=tmp1[2]) %>% arrange(broad_s)
  tmp3 = tmp2 %>% filter(!is.na(amount))
  # 해당 제품 수는 brand_power1에 저장, 해당 방송 수는 brand_power2에 저장
  brand_power1 = c(brand_power1, nrow(tmp2))
  brand_power2 = c(brand_power2, tmp2 %>% select(broad_s) %>% unlist %>% unique %>% length)
  
  if(nrow(tmp3)==0){
    # 이전 관측치가 존재하지 않으면 결측치
    brand_sales = c(brand_sales, NA)
    brand_sales1 = c(brand_sales1, NA)
    brand_amount = c(brand_amount, NA)
    brand_amount1 = c(brand_amount1, NA)

  }else{
    # 이전 관측치가 존재한다면 각 해당 변수에 맞게 생성
    brand_sales = c(brand_sales, mean(tmp3$sales) )
    brand_sales1 = c(brand_sales1, sum(tmp3$sales) )
    brand_amount = c(brand_amount, mean(tmp3$amount) )
    brand_amount1 = c(brand_amount1, sum(tmp3$amount) )
  }
  # if(i %% 1000 == 2){print( paste0(i," : ", Sys.time()) )}
}
# 최종 브랜드 데이터 생성 : tmp_data1
tmp$broad_s = ymd_hms(tmp$broad_s)
tmp_data1 = cbind(tmp,brand_sales,brand_sales1,brand_amount,brand_amount1,brand_power1,brand_power2)
rm(tmp,tmp1,tmp2,tmp3,brand_sales,brand_sales1,brand_amount,brand_amount1,brand_power1,brand_power2)

#3. 취급액_t1, 취급액_t2, 취급액_t평균, n번째 판매 변수 생성 ( Y_t1, Y_t2, Y_t_mean, nth_sale)
# 초기에 해당 변수를 생성할 때 상품수를 기준으로 하였기 때문에 과거실적 변수는 상품수(amount)로 계산된다.
# 후에 취급액은 상품수에 가격만을 곱하여 Y_t1.sales, Y_t2.sales, Y_t_mean.sales 변수로 재생성할 예정이다.
tmp = rbind( raw_data, test_data )

# 변수명 정의
Y_t1 = NA
Y_t2 = NA
Y_t_mean = NA
nth_sale = 1

for(i in 2:nrow(tmp)){
  # 해당 관측치의 상품명(상품별 실적 데이터), 상품코드, 가격, 상품명(NS shop 데이터)을 확인하여 매칭
  tmp1 = tmp[i,c("p_name","p_code","price","prod_name")] %>% unlist
  # (1) 상품명(상품별 실적 데이터), 가격이 동일한 경우
  tmp2 = tmp[1:(i-1),] %>% filter(p_name==tmp1[1],price==as.numeric(tmp1[3])) %>% arrange(broad_s)
  # (2) 상품명(NS shop 데이터), 가격이 동일한 경우
  tmp2.1 = tmp[1:(i-1),] %>% filter(prod_name==tmp1[4],price==as.numeric(tmp1[3])) %>% arrange(broad_s)
  # (3) 상품코드, 가격이 동일한 경우
  tmp3 = tmp[1:(i-1),] %>% filter(p_code==as.numeric(tmp1[2]),price==as.numeric(tmp1[3])) %>% arrange(broad_s)
  # 위의 3가지 데이터를 모두 종합하고 반복되는 행 제거
  tmp4 = unique( rbind(tmp2,tmp2.1,tmp3) )
  # 취급액 및 판매수 결측치 제거 후 최종 과거 실적 데이터 생성 : tmp5
  tmp5 = tmp4 %>% filter(!is.na(amount))

  if(nrow(tmp5)==0){
    # 과거 실적 데이터가 없는 경우
    nth_sale = c(nth_sale, nrow(tmp4)+1 )
    Y_t1 = c(Y_t1, NA)
    Y_t2 = c(Y_t2, NA)
    Y_t_mean = c(Y_t_mean, NA)

  }else if(nrow(tmp5)==1){
    # 과거 실적이 1개인 경우
    nth_sale = c(nth_sale, nrow(tmp4)+1 )
    Y_t1 = c(Y_t1, tmp5[1,"amount"] )
    Y_t2 = c(Y_t2, NA)
    Y_t_mean = c(Y_t_mean, mean(tmp5[,"amount"]))

  }else{
    # 과거 실적이 2개 이상인 경우
    nth_sale = c(nth_sale, nrow(tmp4)+1 )
    Y_t1 = c(Y_t1, tmp5[2,"amount"] )
    Y_t2 = c(Y_t2, tmp5[1,"amount"] )
    Y_t_mean = c(Y_t_mean, mean(tmp5[,"amount"]))
  }
  if(i %% 1000 == 2){print( paste0(i," : ", Sys.time()) )}
}
# 최종 과거실적 데이터 생성 : tmp_data2
tmp_data2 = cbind(tmp, nth_sale, Y_t1, Y_t2, Y_t_mean)
rm(tmp,tmp1,tmp2,tmp2.1,tmp3,tmp4,tmp5, nth_sale, Y_t1, Y_t2, Y_t_mean)

##############################
# 앞서 생성한 최종 브랜드 데이터 및 최종 과거실적 데이터를 상품별 실적 데이터와 결합
tt1 = cbind(rbind( raw_data, test_data ) ,
            tmp_data1[,c("brand_sales","brand_sales1","brand_amount","brand_amount1","brand_power1","brand_power2")],
            tmp_data2[, c("nth_sale","Y_t1","Y_t2","Y_t_mean")] )
tt2 = tt1 %>% arrange(broad_s)
# 상품별 실적 데이터의 취급액 변수명이 혼동되지 않기 위해 변수명을 sales -> prod_sales로 변환
# 상품별 실적 데이터의 판매수 변수명이 혼동되지 않기 위해 변수명을 amount -> prod_amount로 변환
colnames(tt2)[colnames(tt2)=="sales"] = "prod_sales"
colnames(tt2)[colnames(tt2)=="amount"] = "prod_amount"

####### 상품명 관련 변수 생성 : word_18k, word_TV
# 18K_단어변수, TV_단어변수 
tt2$word_18k = ( grepl("18k",tt2$p_name)|grepl("18k",tt2$prod_name) ) %>% as.numeric
tt2$word_TV = ( grepl("TV",tt2$p_name) | grepl("TV",tt2$prod_name) )  %>% as.numeric

####### 최종 상품별 실적 데이터 생성 완료 및 raw data set 과 test data set 으로 구분
final_raw_data = tt2 %>% filter(broad_s < ymd(20200102))
final_test_data = tt2 %>% filter(broad_s > ymd(20200102))

rm(raw_data, test_data)
rm(tmp_data1,tmp_data2)
rm(tt1, tt2)
# 최종 상품별 실적 데이터 저장
write.csv(final_raw_data, paste0(final_path, "final_raw_data3.csv"), row.names = F)
write.csv(final_test_data, paste0(final_path, "final_test_data3.csv"), row.names = F)

########### 최종 상품별 실적 데이터 -> 최종 분당 실적 데이터
# 훈련데이터의 최종 분당 실적 데이터 생성 : raw_data_prop
# 훈련데이터의 분당 실적 데이터에서 필요한 변수만 남기고 생성
tmp1 = raw_data_sale[,c("my_group","broad_time","ex_minute","m_code",
                        "p_code","p_name","p_group","price","sales","amount")]
# 상품별 실적 데이터에서 사용하지 않을 변수 지정 후 분당 실적 데이터에 합치기
no_use_var = c("m_code","broad_s","p_code","p_name","p_group","price")
tmp2 = final_raw_data[,!(colnames(final_raw_data) %in% no_use_var)]
tmp3 = merge(tmp1,tmp2,by="my_group") %>% arrange(broad_time)
raw_data_prop = tmp3
rm(tmp1,tmp2,tmp3,no_use_var)

# 시험 데이터의 분당 실적 데이터에서 필요한 변수만 남기고 생성
tmp1 = test_data_sale[,c("my_id","my_group","broad_time","ex_minute","m_code",
                         "p_code","p_name","p_group","price","sales","amount")]
# 상품별 실적 데이터에서 사용하지 않을 변수 지정 후 분당 실적 데이터에 합치기
no_use_var = c("m_code","broad_s","p_code","p_name","p_group","price")
tmp2 = final_test_data[,!(colnames(final_test_data) %in% no_use_var)]
tmp3 = merge(tmp1,tmp2,by="my_group") %>% arrange(my_id)
test_data_prop = tmp3
rm(tmp1,tmp2,tmp3,no_use_var)

## 방송시간 비율, 방송시간 누적 비율 변수 생성 : period_prop, period_cum_prob
tmp = raw_data_prop
# 변수 생성 시 바뀌게 될 관측치 순서를 위하여 my_id 변수 생성
tmp$my_id = 1:nrow(tmp)

# 방송시간 비율 변수 생성 : 노출시간/방송시간
tmp$period_prop = tmp$ex_minute/tmp$period
# my_group 변수 및 방송시간에 따라 정렬 후 my_group 변수에 맞게 그룹화 =>
# 노출(분) 변수에 누적합 함수(cumsum)를 적용하여 누적 방송시간 변수 생성 : total_ex_minute
tmp = tmp %>% arrange(my_group, broad_time)
tmp1 = tmp %>% group_by(my_group) %>% summarise(cumsum(ex_minute)) %>% as.data.frame
colnames(tmp1) = c("my_group", "total_ex_minute")
# 방송시간 누적 비율 변수 생성 : 누적 방송시간 / 방송시간
tmp = cbind( tmp , total_ex_minute = tmp1$total_ex_minute )
tmp$period_cum_prop = tmp$total_ex_minute/tmp$period
tmp$total_ex_minute = NULL
# 관측치 순서를 그대로 하기 위하여 만든 my_id 변수로 배열
tmp = tmp %>% arrange(my_id)
raw_data_prop = tmp
rm(tmp, tmp1)

## 위의 작업과 동일하게 test set도 방송시간 비율, 방송시간 누적 비율 변수 생성 : period_prop, period_cum_prob
tmp = test_data_prop
tmp$my_id = 1:nrow(tmp)

tmp$period_prop = tmp$ex_minute/tmp$period
tmp = tmp %>% arrange(my_group, broad_time)
tmp1 = tmp %>% group_by(my_group) %>% summarise(cumsum(ex_minute)) %>% as.data.frame
colnames(tmp1) = c("my_group", "total_ex_minute")
tmp = cbind( tmp , total_ex_minute = tmp1$total_ex_minute )
tmp$period_cum_prop = tmp$total_ex_minute/tmp$period
tmp$total_ex_minute = NULL
tmp = tmp %>% arrange(my_id)

test_data_prop = tmp
rm(tmp, tmp1)

# 최종 분당 실적 데이터 저장
write.csv(raw_data_prop, paste0(final_path,"raw_data_prop6.csv") , row.names = F)
write.csv(test_data_prop, paste0(final_path,"test_data_prop6.csv") , row.names = F)

#############이전의 작업은 전처리 및 파생변수을 생성 => 최종 분당, 상품별 실적 데이터 생성 및 저장
#############이후의 작업은 해당 최종 데이터로 모델링 및 제안, 활용방안에 사용

######################### save -> final
# 분당 실적 데이터 읽기
raw_data_prop = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/raw_data_prop6.csv", stringsAsFactors = F)
test_data_prop = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/test_data_prop6.csv", stringsAsFactors = F)

# 시작시각이 2시인 경우 전체 데이터 중 2개이므로 제거
raw_data_prop = raw_data_prop %>% filter(hour!=2)

# 변수형 변환
raw_data_prop$broad_time = ymd_hms(raw_data_prop$broad_time); test_data_prop$broad_time = ymd_hms(test_data_prop$broad_time)
raw_data_prop$pay_method = factor(raw_data_prop$pay_method);  test_data_prop$pay_method = factor(test_data_prop$pay_method)

# 앞서 언급한 과거실적 데이터에서 판매수로 계산하였으므로 가격을 곱하여 취급액으로 계산된 과거실적 관련 변수 생성
raw_data_prop$Y_t1.sales = raw_data_prop$Y_t1*raw_data_prop$price
raw_data_prop$Y_t2.sales = raw_data_prop$Y_t2*raw_data_prop$price
raw_data_prop$Y_t_mean.sales = raw_data_prop$Y_t_mean*raw_data_prop$price

test_data_prop$Y_t1.sales = test_data_prop$Y_t1*test_data_prop$price
test_data_prop$Y_t2.sales = test_data_prop$Y_t2*test_data_prop$price
test_data_prop$Y_t_mean.sales = test_data_prop$Y_t_mean*test_data_prop$price

# 로그-취급액 변수 생성(판매액이 0인 경우는 가격대비 작은 값(0.5x가격)을 대입하여 로그 변환)
raw_data_prop$log_sales = log(raw_data_prop$sales);
raw_data_prop[raw_data_prop$log_sales==-Inf,"log_sales"] = log(raw_data_prop[raw_data_prop$log_sales==-Inf,"price"]*0.5)

############################## character -> factor ##############################
# train_data와 test_data를 훈련데이터 및 실제 예측할 데이터로 구분
# character변수를 factor로 변환
train_data = raw_data_prop
train_data$p_group = factor(train_data$p_group)
train_data$month = factor(train_data$month)
train_data$day = factor(train_data$day)
train_data$hour = factor(train_data$hour)
train_data$minute = factor(train_data$minute)
train_data$weekdays = factor(train_data$weekdays)

test_data = test_data_prop
test_data$p_group = factor(test_data$p_group)
test_data$month = factor(test_data$month)
test_data$day = factor(test_data$day)
test_data$hour = factor(test_data$hour)
test_data$minute = factor(test_data$minute)
test_data$weekdays = factor(test_data$weekdays)

# 사용할 변수 정의(PPT 참고)
# (1) NA_var : 전체 데이터에서 사용할 변수
NA_var = c("price","hour","weekdays","p_group","with_p_num","ex_period","pay_method","grade_star","new_grade_n",
           "holiday","PCA2",
           "brand_power2",
           "period_prop","period_cum_prop",
           "word_18k","word_TV")
# (2) NA_var2 : 1번 데이터에서 사용할 변수
NA_var2 = c(NA_var,"brand_sales","brand_amount","brand_amount1")
# (3) NA_var3 : 2번 데이터에서 사용할 변수
NA_var3 = c(NA_var2, "Y_t1.sales", "Y_t_mean.sales")
# (4) NA_var4 : 3번 데이터에서 사용할 변수
NA_var4 = c(NA_var3, "Y_t2.sales")

# 앞서 정의한 전체 데이터의 변수들 중 결측치인 관측치는 제거
train_data = train_data[(is.na(train_data[,NA_var]) %>% apply(.,1,sum))==0, ]

# 훈련데이터를 훈련데이터, 검증데이터로 구분(train, valid)
train = train_data[as.numeric(as.character(train_data$month))<=9,]
valid = train_data[as.numeric(as.character(train_data$month))>9,]

############################## modeling ##############################
# 각 모델별로 예측값을 저장할 변수 할당 : ensemble_pred
ensemble_pred = data.frame(id = 1:nrow(valid))

############## 회귀분석 ##############
# 전체 데이터로 적합한 회귀분석
lm_tmp1 = lm(log_sales~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
              holiday+PCA2+
              brand_power2+
              period_prop+period_cum_prop+
              word_18k+word_TV, data = train )

# 1번 데이터로 적합한 회귀분석
lm_tmp4 = lm(log_sales~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               period_prop+period_cum_prop+
               word_18k+word_TV, data = train )

# 2번 데이터로 적합한 회귀분석
lm_tmp5 = lm(log_sales~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               period_prop+period_cum_prop+
               word_18k+word_TV+
               Y_t1.sales+Y_t_mean.sales, data = train )

# 3번 데이터로 적합한 회귀분석
lm_tmp6 = lm(log_sales~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               period_prop+period_cum_prop+
               word_18k+word_TV+
               Y_t1.sales+Y_t2.sales+Y_t_mean.sales, data = train )

# 리스트 자료 형태를 이용하여 4개의 회귀 모델을 저장 : lm_list
lm_list = list(lm_tmp1,lm_tmp4,lm_tmp5,lm_tmp6)
rm(lm_tmp1,lm_tmp4,lm_tmp5,lm_tmp6)

# 각 해당 회귀 모델의 수정된 결정계수 값 확인
lm_list %>% sapply(., function(x){ summary(x)$adj.r.squared }) %>% print

# 회귀 예측값 생성
# 3번데이터로 적합한 회귀모델의 예측값을 대입 후 남은 결측치 중 2번 데이터 모델의 예측값을 대입
# 남은 결측치 중 1번 데이터 모델의 예측값을 대입 후 남은 결측치 중 전체 데이터 모델의 예측값을 대입
lm_pred = lm_list %>% sapply(., function(x) { predict(x,valid) })
pred = NULL
pred = lm_pred[,4]
for(i in 3:1){
  pred[is.na(pred)] = lm_pred[is.na(pred),i]
}
# 로그 변환 후 예측한 모델이므로 exp을 취해주어야한다.
pred = exp(pred)
ensemble_pred$lm_pred = pred

############## Xgboost ##############

#######대략 40시간 정도 소요되므로 주석처리 -> hyper parameter 결과값 사용
# customization한 xgboost의 경우 최적화에 문제가 발생하여 평가함수가 MAPE인 경우는 최적화를 하지 못함
## 3-cross validation을 통한 hyper parameter 최적화 함수 생성
# create_cv = function(tmp, eval_name){
#   # Regression Task 생성
#   traintask = makeRegrTask(data = train[,c(tmp,"log_sales")], target = "log_sales")
#   validtask = makeRegrTask(data = valid[,c(tmp,"log_sales")], target = "log_sales")
#   # factor 변수 1-0 핫코딩
#   traintask = createDummyFeatures(obj = traintask)
#   validtask = createDummyFeatures(obj = validtask)
#   # regression xgboost 사용
#   lrn = makeLearner("regr.xgboost",predict.type = "response")
#   # 목적함수는 reg:linear, 평가기준은 rmse, 최대 반복 횟수는 10000번으로 고정
#   lrn$par.vals = list( objective="reg:linear", eval_metric = eval_name, nrounds=10000L)
#   # 최적화할 hyper parameter들의 최소, 최대를 지정
#   params <- makeParamSet( makeDiscreteParam("booster",values = "gbtree"),
#                           makeIntegerParam("max_depth",lower = 3L,upper = 10L),
#                           makeNumericParam("gamma",lower = 0, upper = 3),
#                           makeIntegerParam("early_stopping_rounds",lower = 3L,upper = 10L),
#                           makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
#                           makeNumericParam("subsample",lower = 0.5,upper = 1),
#                           makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
#                           makeNumericParam("eta",lower = 0.01, upper = 1)
#   )
#   # 3-cross validation 사용
#   rdesc <- makeResampleDesc("CV", iters = 3L)
#   # 해당 hyper parmeter 구간들 중 임시로 100개를 추출하여 사용
#   ctrl <- makeTuneControlRandom(maxit = 100L)
#   # seed 지정 후 hyper parameter 최적화 진행
#   set.seed(123456)
#   mytune = tuneParams(learner = lrn, task = traintask, resampling = rdesc,
#                       measures = rmse, par.set = params, control = ctrl, show.info = FALSE)
#   return(mytune)
# }
# 
# #### hyper parameter 최적화 결과 : 코드 밑에 결과 정리
# rmse_xgb1 = create_cv(NA_var, "rmse");rmse_xgb1
# # max_depth = 7, gamma = 0.992, early_stopping_rounds = 11, min_child_weight = 7.35,
# # subsample = 0.98, colsample_bytree = 0.538, eta = 0.0487
# rmse_xgb2 = create_cv(NA_var2, "rmse");rmse_xgb2
# # max_depth = 7, gamma = 0.992, early_stopping_rounds = 11, min_child_weight = 7.35,
# # subsample = 0.98, colsample_bytree = 0.538, eta = 0.0487
# rmse_xgb3 = create_cv(NA_var3, "rmse");rmse_xgb3
# # max_depth = 5, gamma = 1.11, early_stopping_rounds = 11, min_child_weight = 3.52,
# # subsample = 0.923, colsample_bytree = 0.548, eta = 0.00613
# rmse_xgb4 = create_cv(NA_var4, "rmse");rmse_xgb4
# # max_depth = 7, gamma = 0.992, early_stopping_rounds = 11, min_child_weight = 7.35,
# # subsample = 0.98, colsample_bytree = 0.538, eta = 0.0487
# 
# 
# mae_xgb1 = create_cv(NA_var, "mae");mae_xgb1
# # max_depth = 8, gamma = 1.36, early_stopping_rounds = 11, min_child_weight = 7.51,
# # subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261
# mae_xgb2 = create_cv(NA_var2, "mae");mae_xgb2
# # max_depth = 8, gamma = 1.36, early_stopping_rounds = 11, min_child_weight = 7.51,
# # subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261
# mae_xgb3 = create_cv(NA_var3, "mae");mae_xgb3
# # max_depth = 8, gamma = 1.36, early_stopping_rounds = 11, min_child_weight = 7.51,
# # subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261
# mae_xgb4 = create_cv(NA_var4, "mae");mae_xgb4
# # max_depth = 8, gamma = 1.36, early_stopping_rounds = 11, min_child_weight = 7.51,
# # subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261

##### xgb.train 함수를 이용하여 모델 적합
# 앞선 회귀모델을 이용하여 model.matrix에 사용할 formula 생성
form = lm_list %>% sapply(.,formula)

# xgboost 모델에 사용할 TRAIN set(xgb.DMatrix object) 생성: dtrain (list)
# 각각에 전체, 1번, 2번, 3번 데이터에 관하여 저장
dtrain = lapply(form, function(x){
  tmp = model.matrix(x, data = train)[,-1]
  # model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장 : tmp_label
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train) %in% tmp_label)
  # 취급액이 0인 경우 가격대비 작은 값(판매가격X0.5)을 대입하여 로그 변환 후 사용
  tmp1 = train
  tmp1[tmp1$sales==0,"sales"] = 0.5*tmp1[tmp1$sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$sales[tmp_label])) )
  })

# model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장하기 위한 list : dtrain_ind
dtrain_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = train)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train) %in% tmp_label)
  return(tmp_label)
  })

# xgboost 모델에 사용할 VALID set(xgb.DMatrix object) 생성 : dvalid (list)
# 각각에 전체, 1번, 2번, 3번 데이터에 관하여 저장(과정은 위의 TRAIN set 과 동일)
dvalid = lapply(form, function(x){
  tmp = model.matrix(x, data = valid)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(valid) %in% tmp_label)
  tmp1 = valid
  tmp1[tmp1$sales==0,"sales"] = 0.5*tmp1[tmp1$sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$sales[tmp_label])) )
  })

# model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장하기 위한 list : dvalid_ind
dvalid_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = valid)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(valid) %in% tmp_label)
  return(tmp_label)
  })

# dtrain 과 dvalid로 watchlist 작성
watchlist = list()
for(i in 1:length(dtrain)){
  watchlist[[i]] = list(train = dtrain[[i]], eval = dvalid[[i]])
}

# custom MAE Metric for XGBoost
xg_eval_mape <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err = mape(y, yhat)
  return (list(metric = "mape", value = err))
}
# custom MAPE Objective function for XGBoost(fair loss 기준) - 자세한 사항은 PPT 참고
amo.fairobj2 <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  grad = grad / labels
  hess <- con ^ 2 / (abs(x) + con) ^ 2
  hess = hess / labels
  return(list(grad = grad, hess = hess))
}

## 앞에서 찾은 각 모델마다 최적 hyper parameter 지정
# (1) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - RMSE
#  1) 전체, 1번, 3번 데이터 셋
param_rmse.1 = list(booster="gbtree", max_depth = 7, gamma = 0.992,
                    min_child_weight = 7.35, subsample = 0.98, colsample_bytree = 0.538, eta = 0.048,
                      objective = "reg:linear", eval_metric = "rmse")
#  2) 2번 데이터 셋
param_rmse.2 = list(booster="gbtree", max_depth = 5, gamma = 1.11,
                    min_child_weight = 3.52, subsample = 0.923, colsample_bytree = 0.548, eta = 0.00613,
                    objective = "reg:linear", eval_metric = "rmse")

# (2) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAE
#  1) 전체, 1번, 2번, 3번 데이터 셋
param_mae = list(booster="gbtree", max_depth = 8, gamma = 1.36,
                 min_child_weight = 7.51, subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261,
                 objective = "reg:linear", eval_metric = "mae")

# customization한 xgboost의 경우 최적화에 문제가 발생하여 앞에서 언급된 hyper parameter들 중 가장 좋은 것으로 선택
# (3) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAPE
#  1) 전체, 1번, 2번, 3번 데이터 셋
param_mape = list(booster="gbtree", max_depth = 7, gamma = 0.992,
                  min_child_weight = 7.35, subsample = 0.98, colsample_bytree = 0.538, eta = 0.048,
                  objective = "reg:linear", eval_metric = xg_eval_mape)

# (4) 목적함수(Objective) - MAPE, 평가기준(eval_metric) - MAPE
#  1) 전체, 1번, 2번, 3번 데이터 셋
param_my_mape = list(booster="gbtree", max_depth = 8, gamma = 1.36,
                     min_child_weight = 7.51, subsample = 0.972, colsample_bytree = 0.569, eta = 0.0261,
                     objective = amo.fairobj2, eval_metric = xg_eval_mape)

set.seed(1234567)
### Xgboost 모델 적합
# (1) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - RMSE
xgb.rmse_list = list()
for(i in 1:2){
  xgb.rmse_list[[i]] = xgb.train(params = param_rmse.1, data = dtrain[[i]], watchlist = watchlist[[i]],
                            verbose=1, print_every_n = 100, nround = 10000, early_stopping_rounds = 11)
}
xgb.rmse_list[[3]] = xgb.train(params = param_rmse.2, data = dtrain[[3]], watchlist = watchlist[[3]],
                               verbose=1, print_every_n = 100, nround = 10000, early_stopping_rounds = 11)
xgb.rmse_list[[4]] = xgb.train(params = param_rmse.1, data = dtrain[[4]], watchlist = watchlist[[4]],
                               verbose=1, print_every_n = 100, nround = 10000, early_stopping_rounds = 11)

# (2) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAE
xgb.mae_list = list()
for(i in 1:4){
  xgb.mae_list[[i]] = xgb.train(params = param_mae, data = dtrain[[i]], watchlist = watchlist[[i]],
                            verbose=1, print_every_n = 100, nround = 10000, early_stopping_rounds = 11)
}
# (3) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAPE
xgb.mape_list = list()
for(i in 1:4){
  xgb.mape_list[[i]] = xgb.train(params = param_mape, data = dtrain[[i]], watchlist = watchlist[[i]],
                             verbose=1, print_every_n = 100, nround = 10000, maximize = F, early_stopping_rounds = 11)
}
# (4) 목적함수(Objective) - MAPE, 평가기준(eval_metric) - MAPE
xgb.my_list = list()
for(i in 1:4){
  xgb.my_list[[i]] = xgb.train(params = param_my_mape, data = dtrain[[i]], watchlist = watchlist[[i]],
                                 verbose=1, print_every_n = 100, nround = 10000, early_stopping_rounds = 11  ,maximize = F)
}

# 검증 데이터로 예측
pred1 = pred2 = pred3 = pred4 = NULL
pred1 = predict(xgb.rmse_list[[1]], dvalid[[1]]);pred2 = predict(xgb.mae_list[[1]], dvalid[[1]])
pred3 = predict(xgb.mape_list[[1]], dvalid[[1]]);pred4 = predict(xgb.my_list[[1]], dvalid[[1]])
for(i in 2:3){
  pred1[dvalid_ind[[i]]] = predict(xgb.rmse_list[[i]], dvalid[[i]])
  pred2[dvalid_ind[[i]]] = predict(xgb.mae_list[[i]], dvalid[[i]])
  pred3[dvalid_ind[[i]]] = predict(xgb.mape_list[[i]], dvalid[[i]])
  pred4[dvalid_ind[[i]]] = predict(xgb.my_list[[i]], dvalid[[i]])
}
pred1 = exp(pred1); pred2 = exp(pred2); pred3 = exp(pred3); pred4 = exp(pred4)

ensemble_pred$xgb_rmse_pred = pred1
ensemble_pred$xgb_mae_pred = pred2
ensemble_pred$xgb_mape_pred = pred3
ensemble_pred$xgb_my_pred = pred4

# 결과 확인
s_mape_ind = (valid$sales==0)
apply(ensemble_pred[,-1], 2, function(x){ rmse(valid$sales[!s_mape_ind], x[!s_mape_ind])  })
apply(ensemble_pred[,-1], 2, function(x){ mae(valid$sales[!s_mape_ind], x[!s_mape_ind])  })
apply(ensemble_pred[,-1], 2, function(x){ mape(valid$sales[!s_mape_ind], x[!s_mape_ind])  })

# 평균 앙상블 결과 확인
pred = ensemble_pred[,-1] %>% select(xgb_rmse_pred, xgb_mae_pred, xgb_mape_pred, xgb_my_pred) %>%
  apply(., 1, mean)
rmse(valid$sales[!s_mape_ind], pred[!s_mape_ind])
mae(valid$sales[!s_mape_ind], pred[!s_mape_ind])
mape(valid$sales[!s_mape_ind], pred[!s_mape_ind])

##### importance plot 저장
for(j in 1:4){
  tmp_importance = xgb.importance(feature_names = attr(dtrain[[j]],".Dimnames")[[2]], model = xgb.rmse_list[[j]])
  plot_tmp = xgb.ggplot.importance(tmp_importance, measure = "Gain", rel_to_first = TRUE, top_n = 15, n_clusters = 5)
  ggsave(paste0(ggsave_path,"xgboost_reg_rmse_importance",j,".jpg"), plot = plot_tmp)
  rm(tmp_importance, plot_tmp)
  
  tmp_importance = xgb.importance(feature_names = attr(dtrain[[j]],".Dimnames")[[2]], model = xgb.mae_list[[j]])
  plot_tmp = xgb.ggplot.importance(tmp_importance, measure = "Gain", rel_to_first = TRUE, top_n = 15, n_clusters = 5)
  ggsave(paste0(ggsave_path,"xgboost_reg_mae_importance",j,".jpg"), plot = plot_tmp)
  rm(tmp_importance, plot_tmp)
  
  tmp_importance = xgb.importance(feature_names = attr(dtrain[[j]],".Dimnames")[[2]], model = xgb.mape_list[[j]])
  plot_tmp = xgb.ggplot.importance(tmp_importance, measure = "Gain", rel_to_first = TRUE, top_n = 15, n_clusters = 5)
  ggsave(paste0(ggsave_path,"xgboost_reg_mape_importance",j,".jpg"), plot = plot_tmp)
  rm(tmp_importance, plot_tmp)
  
  tmp_importance = xgb.importance(feature_names = attr(dtrain[[j]],".Dimnames")[[2]], model = xgb.my_list[[j]])
  plot_tmp = xgb.ggplot.importance(tmp_importance, measure = "Gain", rel_to_first = TRUE, top_n = 15, n_clusters = 5)
  ggsave(paste0(ggsave_path,"xgboost_my_mape_importance",j,".jpg"), plot = plot_tmp)
  rm(tmp_importance, plot_tmp)
}

############# 실제 test 데이터에 대한 예측값 생성 (2020년 6월)
# 전체 TRAIN set 생성 : dtrain
dtrain = lapply(form, function(x){
  tmp = model.matrix(x, data = train_data)[,-1]
  # model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장 : tmp_label
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train_data) %in% tmp_label)
  # 취급액이 0인 경우 가격대비 작은 값(판매가격X0.5)을 대입하여 로그 변환 후 사용
  tmp1 = train_data
  tmp1[tmp1$sales==0,"sales"] = 0.5*tmp1[tmp1$sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$sales[tmp_label])) )
})

# model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장하기 위한 list : dtrain_ind
dtrain_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = train_data)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train_data) %in% tmp_label)
  return(tmp_label)
})

# 전체 TEST set 생성 : dtest

tt1 = (form[[1]] %>% as.character %>% nth(3) %>% paste0("~",.) %>% formula %>% model.matrix(., data = test_data))[,-1]
tt2 = row.names(tt1) %>% as.numeric
tt2 = which(row.names(test_data) %in% tt2)
tmp1 = test_data

dtest = lapply(form, function(x){
  tmp = (x %>% as.character %>% nth(3) %>% paste0("~",.) %>% formula %>% model.matrix(., data = test_data))[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(test_data) %in% tmp_label)
  tmp1 = test_data
  tmp1$sales = exp(1)
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$sales[tmp_label])) )
})

# model.matrix 함수 사용시 결측치를 자동 제외하므로 결측치를 제외한 행의 index 저장하기 위한 list : dtest_ind
dtest_ind = lapply(form, function(x){
  tmp = (x %>% as.character %>% nth(3) %>% paste0("~",.) %>% formula %>% model.matrix(., data = test_data))[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(test_data) %in% tmp_label)
  return(tmp_label)
})

# dtrain 과 dvalid로 watchlist 작성
watchlist = list()
for(i in 1:length(dtrain)){
  watchlist[[i]] = list(train = dtrain[[i]], eval = dtest[[i]])
}

### 실제 test set을 예측할 Xgboost 모델 적합
# (1) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - RMSE
xgb.rmse_list_final = list()
for(i in 1:2){
  xgb.rmse_list_final[[i]] = xgb.train(params = param_rmse.1, data = dtrain[[i]], watchlist = watchlist[[i]],
                                 verbose=1, print_every_n = 100, nround = xgb.rmse_list[[i]]$best_iteration)
}
xgb.rmse_list_final[[3]] = xgb.train(params = param_rmse.2, data = dtrain[[3]], watchlist = watchlist[[3]],
                               verbose=1, print_every_n = 100, nround = xgb.rmse_list[[3]]$best_iteration)
xgb.rmse_list_final[[4]] = xgb.train(params = param_rmse.1, data = dtrain[[4]], watchlist = watchlist[[4]],
                               verbose=1, print_every_n = 100, nround = xgb.rmse_list[[4]]$best_iteration)

# (2) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAE
xgb.mae_list_final = list()
for(i in 1:4){
  xgb.mae_list_final[[i]] = xgb.train(params = param_mae, data = dtrain[[i]], watchlist = watchlist[[i]],
                                verbose=1, print_every_n = 100, nround = xgb.mae_list[[i]]$best_iteration)
}
# (3) 목적함수(Objective) - reg:linear, 평가기준(eval_metric) - MAPE
xgb.mape_list_final = list()
for(i in 1:4){
  xgb.mape_list_final[[i]] = xgb.train(params = param_mape, data = dtrain[[i]], watchlist = watchlist[[i]],
                                 verbose=1, print_every_n = 100, nround = xgb.mape_list[[i]]$best_iteration, maximize = F)
}
# (4) 목적함수(Objective) - MAPE, 평가기준(eval_metric) - MAPE
xgb.my_list_final = list()
for(i in 1:4){
  xgb.my_list_final[[i]] = xgb.train(params = param_my_mape, data = dtrain[[i]], watchlist = watchlist[[i]],
                               verbose=1, print_every_n = 100, nround = xgb.my_list[[i]]$best_iteration, maximize = F)
}

test_pred = data.frame(my_id = 1:nrow(test_data) )
# 실제 시험 데이터 예측
pred1 = pred2 = pred3 = pred4 = NULL
pred1 = predict(xgb.rmse_list_final[[1]], dtest[[1]]);pred2 = predict(xgb.mae_list_final[[1]], dtest[[1]])
pred3 = predict(xgb.mape_list_final[[1]], dtest[[1]]);pred4 = predict(xgb.my_list_final[[1]], dtest[[1]])
for(i in 2:3){
  pred1[dtest_ind[[i]]] = predict(xgb.rmse_list_final[[i]], dtest[[i]])
  pred2[dtest_ind[[i]]] = predict(xgb.mae_list_final[[i]], dtest[[i]])
  pred3[dtest_ind[[i]]] = predict(xgb.mape_list_final[[i]], dtest[[i]])
  pred4[dtest_ind[[i]]] = predict(xgb.my_list_final[[i]], dtest[[i]])
}
pred1 = exp(pred1); pred2 = exp(pred2); pred3 = exp(pred3); pred4 = exp(pred4)

test_pred$xgb_rmse_pred = pred1
test_pred$xgb_mae_pred = pred2
test_pred$xgb_mape_pred = pred3
test_pred$xgb_my_pred = pred4

# 최종 앙상블 값 계산 후 상품군 무형이 아닌 관측값에 대입
real_test_pred = test_pred[,-1] %>% select(xgb_rmse_pred, xgb_mae_pred, xgb_mape_pred, xgb_my_pred) %>%
  apply(., 1, mean)

real_test = read.csv(paste0(after_path,"/2020 빅콘테스트 데이터분석분야-챔피언리그_2020년 6월 판매실적예측데이터(평가데이터).csv"))
real_test[real_test$상품군!="무형", "취급액"] = real_test_pred 
real_test[real_test$상품군=="무형", "취급액"] = 0

# 실제로 제출할 파일 저장
write.csv(real_test, paste0(final_path,"평가 데이터.csv"), row.names = F)

