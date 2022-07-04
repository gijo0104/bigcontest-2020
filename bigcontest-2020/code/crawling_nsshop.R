#install.packages("rvest")
library(rvest)
library(lubridate)
library(dplyr)

candi_date = (ymd("20190101")+days(0:546)) %>% as.character

nsshop_url = paste0("http://www.nsmall.com/TComLiveBrodcastingList?tab_gubun=1&tab_Week=1&tab_bord=0&selectDay=",candi_date,"#goToLocation")

train_data = NULL

for(j in 1:length(candi_date)){
  tmp = read_html(nsshop_url[j])
  tmp1 = tmp %>% html_nodes('.air')
  
  #rowspan except e_Item
  rowspan = (tmp1 %>% as.character %>% strsplit("\"") %>% sapply(.,"[",4) %>% as.numeric)-1
  et2 = tmp %>% html_nodes('tbody') %>% html_nodes('tr') %>% grepl("e_Item al",.) %>% which
  if(length(et2)!=0){
    et1 = tmp %>% html_nodes('tbody') %>% html_nodes('tr') %>% grepl("class=\"air\"",.) %>% which
    et3 = NULL
    for(i in 1:length(et2)){
      et3 = c(et3, which(et1<=et2[i]) %>% max)
    }
    rowspan[et3] = rowspan[et3]-1
  }
  sum(rowspan)
  # broad_time, broad_name
  broad_time = tmp1 %>% html_nodes('em') %>% html_text() %>% rep(., rowspan)
  broad_name = tmp1 %>% html_nodes('.txt') %>% html_text() %>% rep(., rowspan)

  # prod_name
  prod_name = tmp %>% html_nodes('.al') %>% html_nodes('.ptImg') %>% html_attrs %>% sapply(.,"[",3)
  prod_name = prod_name %>% strsplit("\'") %>% sapply(.,"[",4)
  prod_name = prod_name[!(prod_name %>% grepl("http",.))]
  names(prod_name) = NULL
  length(prod_name)
  
  # grade_star, grade_n
  grade_star = tmp %>% html_nodes('.star') %>% html_text() %>% gsub("%","",.) %>% as.numeric
  grade_n = tmp %>% html_nodes('.review') %>% html_text() %>% gsub("건","",.) %>% gsub(",","",.) %>% as.numeric
  
  # price_sale, price_prior
  price_prior_n = tmp %>% html_nodes('.ar') %>% as.character %>% grepl("txt_line",.)
  price_prior = tmp %>% html_nodes('.ar') %>% html_nodes('.txt_line') %>% html_text() %>% gsub("원","",.) %>% gsub(",","",.) %>% as.numeric
  price_sale = tmp %>% html_nodes('.ar') %>% html_nodes('.fc1') %>% html_text() %>% gsub("원","",.) %>% gsub(",","",.) %>% as.numeric
  if(length(price_sale)!=sum(rowspan)){
    price_prior_x = tmp %>% html_nodes('.ar') %>% as.character %>% grepl("pic_order",.)
    tmp3 = rep(NA,sum(rowspan)); tmp3[!price_prior_x]=price_sale; tmp3[price_prior_x]=-1
    price_sale = tmp3
    rm(price_prior_x, tmp3)
  }
  
  # data frame
  tmp2 = data.frame(date = candi_date[j],broad_time=broad_time, broad_name=broad_name, prod_name=prod_name,
                    grade_star=grade_star, grade_n=grade_n, price_sale=price_sale, price_prior = NA)
  tmp2[price_prior_n,"price_prior"] = price_prior
  
  train_data = rbind(train_data, tmp2)
  rm(broad_name, broad_time, et2, grade_n, grade_star, price_prior, price_prior_n, price_sale, prod_name, rowspan, tmp, tmp1, tmp2)
  if(j%%10==1){print(paste0(j," : ",Sys.time()))}
}

# write.csv(train_data,"C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/External Data/nsshop_data.csv",row.names=F)

