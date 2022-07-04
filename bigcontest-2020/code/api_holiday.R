library(dplyr)
library(lubridate)
library(reshape)
library(rvest)

holi_key = "HPt0yjBAA4%2BVmjd5GIwSoLfZmegr7NBUuxcDQ1zrgMDg47w2HPqGRbgDR8CO3lOZs6YFCZf1fpzNlrks0NRzHA%3D%3D"
holi_url = c( paste0("http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?solYear=2019&solMonth=",
                     sprintf("%02d",1:12),"&ServiceKey=", holi_key),
              paste0("http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?solYear=2020&solMonth=",
                     sprintf("%02d",1:6),"&ServiceKey=", holi_key) )
holi_data = NULL
for(i in 1:length(holi_url)){
  tmp = cbind( holi_url[i] %>% read_html() %>% html_nodes("datename") %>% html_text(),
               holi_url[i] %>% read_html() %>% html_nodes("locdate") %>% html_text() )
  holi_data = rbind( holi_data, tmp )
}
colnames(holi_data) = c("date_name", "date")

write.csv(holi_data, "C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/External Data/holi_data.csv", row.names = F)