library(rvest)
library(lubridate)
library(dplyr)
library(httr)
library(beepr)

super_gsub = function(x){
  return( gsub("\t","",x) %>% gsub("\r","",.) %>% gsub("\n","",.) %>% gsub(" ","",.) %>% gsub(",","",.) )
}

nielsen_date = (ymd("20181231")+days(0:548)) %>% as.character %>% gsub("-","",.)

# sub_menu : 1-지상파, 3-케이블 / area : 00-전국, 01-수도권
nielsen_url1 = paste0("https://www.nielsenkorea.co.kr/tv_terrestrial_day.asp?menu=Tit_1&sub_menu=1_1&area=00&begin_date=",nielsen_date)
nielsen_url2 = paste0("https://www.nielsenkorea.co.kr/tv_terrestrial_day.asp?menu=Tit_1&sub_menu=1_1&area=01&begin_date=",nielsen_date)
nielsen_url3 = paste0("https://www.nielsenkorea.co.kr/tv_terrestrial_day.asp?menu=Tit_1&sub_menu=3_1&area=00&begin_date=",nielsen_date)
nielsen_url4 = paste0("https://www.nielsenkorea.co.kr/tv_terrestrial_day.asp?menu=Tit_1&sub_menu=3_1&area=01&begin_date=",nielsen_date)

nielsen_data = NULL
none_data = NULL

for(i in 414:length(nielsen_date)){
  ##### [nielsen_url1] 지상파 - 전국 : 1.가구시청률TOP20, 2.시청자수TOP20
  if( (GET(nielsen_url1[i]) %>% status_code())==500 ){
    tmp3 = NULL
    none_data = rbind( none_data, c(1, nielsen_date[i]) )
    print(c(1, nielsen_date[i]))
  }else{
    tmp = read_html(nielsen_url1[i]) %>% html_nodes('.ranking_tb')
    tmp1.1 = tmp[[1]]
    tmp1.2 = tmp[[2]]
    
    rank_channel = tmp1.1 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.1 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.1 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.1 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp2 = cbind(region = "nation", title="rating",rank_channel,program_name,percent)
    rm(rank_channel, program_name, percent)
    
    rank_channel = tmp1.2 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.2 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.2 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.2 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp3 = rbind( tmp2, cbind(region = "nation", title="viewers",rank_channel,program_name,percent) )
    colnames(tmp3) = c("region", "title", "rank", "channel", "program", "value")
    rm(tmp, tmp1.1, tmp1.2, rank_channel, program_name, percent, tmp2)
  }
  
  ##### [nielsen_url2] 지상파 - 수도권 : 1.가구시청률TOP20, 2.시청자수TOP20
  if( (GET(nielsen_url2[i]) %>% status_code())==500 ){
    tmp5 = NULL
    none_data = rbind( none_data, c(2, nielsen_date[i]) )
    print(c(2, nielsen_date[i]))
  }else{
    tmp = read_html(nielsen_url2[i]) %>% html_nodes('.ranking_tb')
    tmp1.1 = tmp[[1]]
    tmp1.2 = tmp[[2]]
    
    rank_channel = tmp1.1 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.1 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.1 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.1 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp2 = cbind(region = "metro", title="rating",rank_channel,program_name,percent)
    rm(rank_channel, program_name, percent)
    
    rank_channel = tmp1.2 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.2 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.2 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.2 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp4 = rbind( tmp2, cbind(region = "metro", title="viewers",rank_channel,program_name,percent) )
    colnames(tmp4) = c("region", "title", "rank", "channel", "program", "value")
    rm(tmp, tmp1.1, tmp1.2, rank_channel, program_name, percent, tmp2)
    
    tmp5 = data.frame(date = nielsen_date[i], rbind(tmp3, tmp4))
    rm(tmp3, tmp4)
  }
  
  ##### [nielsen_url3] 케이블 - 전국 : 1.가구시청률TOP20, 2.시청자수TOP20
  if( (GET(nielsen_url3[i]) %>% status_code())==500 ){
    tmp3 = NULL
    none_data = rbind( none_data, c(3, nielsen_date[i]) )
    print(c(3, nielsen_date[i]))
  }else{
    tmp = read_html(nielsen_url3[i]) %>% html_nodes('.ranking_tb')
    tmp1.1 = tmp[[1]]
    tmp1.2 = tmp[[2]]
    
    rank_channel = tmp1.1 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.1 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.1 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.1 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp2 = cbind(region = "nation", title="rating",rank_channel,program_name,percent)
    rm(rank_channel, program_name, percent)
    
    rank_channel = tmp1.2 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.2 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.2 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.2 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp3 = rbind( tmp2, cbind(region = "nation", title="viewers",rank_channel,program_name,percent) )
    colnames(tmp3) = c("region", "title", "rank", "channel", "program", "value")
    rm(tmp, tmp1.1, tmp1.2, rank_channel, program_name, percent, tmp2)
  }
  
  
  ##### [nielsen_url4] 케이블 - 수도권 : 1.가구시청률TOP20, 2.시청자수TOP20
  if( (GET(nielsen_url4[i]) %>% status_code())==500 ){
    tmp6 = NULL
    none_data = rbind( none_data, c(4, nielsen_date[i]) )
    print(c(4, nielsen_date[i]))
  }else{
    tmp = read_html(nielsen_url4[i]) %>% html_nodes('.ranking_tb')
    tmp1.1 = tmp[[1]]
    tmp1.2 = tmp[[2]]
    
    rank_channel = tmp1.1 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.1 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.1 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.1 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp2 = cbind(region = "metro", title="rating",rank_channel,program_name,percent)
    rm(rank_channel, program_name, percent)
    
    rank_channel = tmp1.2 %>% html_nodes(".tb_txt_center") %>% html_text %>% super_gsub() %>% matrix(., ncol=2, byrow=T)
    program_name = tmp1.2 %>% html_nodes(".tb_txt") %>% html_text %>% super_gsub() %>% as.matrix(.,ncol=1)
    percent = c( tmp1.2 %>% html_nodes(".percent") %>% html_text %>% super_gsub(),
                 tmp1.2 %>% html_nodes(".percent_g") %>% html_text %>% super_gsub() ) %>% as.matrix(.,ncol=1)
    tmp4 = rbind( tmp2, cbind(region = "metro", title="viewers",rank_channel,program_name,percent) )
    colnames(tmp4) = c("region", "title", "rank", "channel", "program", "value")
    rm(tmp, tmp1.1, tmp1.2, rank_channel, program_name, percent, tmp2)
    
    tmp6 = data.frame(date = nielsen_date[i], rbind(tmp3, tmp4))
    rm(tmp3, tmp4)
  }
  
  tmp7 = rbind(tmp5, tmp6)
  rm(tmp5, tmp6)
  
  nielsen_data = rbind(nielsen_data, tmp7)
  
  if(i%%10==1){ paste0(i," : ", Sys.time()) %>% print }
}

# beep()
# write.csv(nielsen_data,"C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/External Data/nielsen_data.csv",row.names=F)

rm( super_gsub,nielsen_date,nielsen_url1,nielsen_url2,nielsen_url3,nielsen_url4,nielsen_data,none_data,tmp7,ex_path )
