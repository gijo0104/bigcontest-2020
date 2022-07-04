# 제안 및 활용방안을 제시하는 코드

# install.packages("clue")
# 라이브러리
library(dplyr)
library(lubridate)
library(reshape)
library(rvest)
library(RColorBrewer)
library(corrplot)
library(Metrics)
library(clue)
library(xgboost)
library(partitions)
library(iterpc)

# 데이터 불러오기
final_raw_data = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/final_raw_data3.csv", stringsAsFactors = F)
final_test_data = read.csv("C:/Users/user/Desktop/2020빅콘테스트-챔피언리그/data/final/final_test_data3.csv", stringsAsFactors = F)

# 자료형 변환
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

# 훈련, 검증 데이터 생성
standard_date = "2019093005"
valid_date = "2019113005"
# 최적화할 주간 시작 날짜 지정
test_date = "2019120205"

#42~60 제외하고 모두 그대로 고정
NA_var = c("price","hour","weekdays","p_group","with_p_num","ex_period","pay_method","grade_star","new_grade_n",
           "holiday","PCA2","nth_sale",
           "brand_power2",
           "word_TV")
# NA_var2 = c(NA_var,"brand_sales","brand_amount","brand_amount1")
# NA_var3 = c(NA_var2, "Y_t1", "Y_t_mean")
# NA_var4 = c(NA_var3, "Y_t2")

# final_raw_data %>% dim
# sum(( is.na(final_raw_data[,NA_var]) %>% apply(.,1,sum))!=0);sum(( is.na(final_raw_data[,NA_var]) %>% apply(.,1,sum))==0)
# sum(( is.na(final_raw_data[,NA_var2]) %>% apply(.,1,sum))!=0);sum(( is.na(final_raw_data[,NA_var2]) %>% apply(.,1,sum))==0)
# sum(( is.na(final_raw_data[,NA_var3]) %>% apply(.,1,sum))!=0);sum(( is.na(final_raw_data[,NA_var3]) %>% apply(.,1,sum))==0)
# sum(( is.na(final_raw_data[,NA_var4]) %>% apply(.,1,sum))!=0);sum(( is.na(final_raw_data[,NA_var4]) %>% apply(.,1,sum))==0)
# c(12546, 11912, 10789, 9524)/12807
# rm(NA_var2, NA_var3, NA_var4)
# final_raw_data$p_group %>% table

train = final_raw_data %>% filter( ymd_h(standard_date) > broad_s); dim(train)
train[(train$prod_sales==0), "prod_sales"] = train[(train$prod_sales==0), "price"]*1

valid = final_raw_data %>% filter( ymd_h(standard_date) <= broad_s, broad_s < ymd_h(valid_date)); dim(valid)
valid[(valid$prod_sales==0), "prod_sales"] = valid[(valid$prod_sales==0), "price"]*1

test = final_raw_data %>% filter( ymd_h(test_date) <= broad_s, broad_s < ymd_h(test_date) + days(7)); dim(test)
# test = final_raw_data %>% filter( month(broad_s) > 9)

train = train[apply(is.na(train[,NA_var]),1,sum)==0,]
valid = valid[apply(is.na(valid[,NA_var]),1,sum)==0,]
test = test[apply(is.na(test[,NA_var]),1,sum)==0,]
dim(train); dim(valid); dim(test)

fix_date = (test %>% filter((period<=40)|(71<=period)))$broad_s
fix_data = test[ (test$period<=40) | (71<=test$period), ]

candi_date = (test %>% filter(!(broad_s %in% fix_date)) )$broad_s %>% unique
candi_data = (test %>% filter(!(broad_s %in% fix_date)) )

candi_date1 = candi_data %>% select(broad_s, hour, weekdays, holiday) %>% unique

ensemble_pred = data.frame(id = 1:nrow(test))

############## Regression Analysis ##############
lm_tmp1 = lm(log(prod_sales)~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+nth_sale+
               brand_power2+
               +word_TV, data = rbind(train,valid) )
# summary(lm_tmp1)

lm_tmp4 = lm(log(prod_sales)~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+nth_sale+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               word_TV, data = rbind(train,valid))
# summary(lm_tmp4)

lm_tmp5 = lm(log(prod_sales)~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+nth_sale+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               word_TV+
               price:Y_t1+price:Y_t_mean, data = rbind(train,valid))
# summary(lm_tmp5)

lm_tmp6 = lm(log(prod_sales)~price+hour+weekdays+p_group+with_p_num+ex_period+pay_method+grade_star+new_grade_n+
               holiday+PCA2+nth_sale+
               brand_power2+brand_sales+brand_amount+brand_amount1+
               word_TV+
               price:Y_t1+price:Y_t2+price:Y_t_mean, data = rbind(train,valid))
# summary(lm_tmp6)

lm_list = list(lm_tmp1,lm_tmp4,lm_tmp5,lm_tmp6)
rm(lm_tmp1,lm_tmp4,lm_tmp5,lm_tmp6)
lm_list %>% sapply(., function(x){ summary(x)$adj.r.squared })
# lm_list %>% lapply(., summary)

lm_pred = lm_list %>% sapply(., function(x) { predict(x,test) })
real_pred = NULL
real_pred = lm_pred[,4]
for(i in 3:1){
  real_pred[is.na(real_pred)] = lm_pred[is.na(real_pred),i]
}
real_pred = exp(real_pred)

ensemble_pred$lm_pred = real_pred

s_mape_ind = (test$prod_sales==0)
mape(test$prod_sales[!s_mape_ind], real_pred[!s_mape_ind])
rmse(test$prod_sales[!s_mape_ind], real_pred[!s_mape_ind])

############## Xgboost ##############
form = lm_list %>% sapply(.,formula)
# form = lm_step_list %>% sapply(.,formula)

# cv <- xgb.cv(data = dtrain[[1]], nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse","auc"),
#              max_depth = list(1,2,3), eta = 1, objective = "binary:logistic")

dtrain = lapply(form, function(x){
  tmp = model.matrix(x, data = train)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train) %in% tmp_label)
  tmp1 = train
  # tmp1[tmp1$prod_sales==0,"prod_sales"] = 0.5*tmp1[tmp1$prod_sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$prod_sales[tmp_label])) )
})

dtrain_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = train)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(train) %in% tmp_label)
  return(tmp_label)
})

dvalid = lapply(form, function(x){
  tmp = model.matrix(x, data = valid)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(valid) %in% tmp_label)
  tmp1 = valid
  # tmp1[tmp1$prod_sales==0,"prod_sales"] = 0.5*tmp1[tmp1$prod_sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$prod_sales[tmp_label])) )
})

dvalid_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = valid)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(valid) %in% tmp_label)
  return(tmp_label)
})

dtest = lapply(form, function(x){
  tmp = model.matrix(x, data = test)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(test) %in% tmp_label)
  tmp1 = test
  # tmp1[tmp1$prod_sales==0,"prod_sales"] = 0.5*tmp1[tmp1$prod_sales==0,"price"]
  return(tmp %>% xgb.DMatrix(., label = log(tmp1$prod_sales[tmp_label])) )
})

dtest_ind = lapply(form, function(x){
  tmp = model.matrix(x, data = test)[,-1]
  tmp_label = row.names(tmp) %>% as.numeric
  tmp_label = which(row.names(test) %in% tmp_label)
  return(tmp_label)
})

watchlist = list()
for(i in 1:length(dtrain)){
  watchlist[[i]] = list(train = dtrain[[i]], eval = dvalid[[i]])
}

xg_eval_mape <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err = mape(y, yhat)
  return (list(metric = "mape", value = err))
}

# custom MAE Metric for XGBoost
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

amm_mape <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mape(elab, epreds)
  return(list(metric = "amm_mape", value = err))
}

param_rmse = list(booster="gbtree", max_depth = 4, eta = 0.01, subsample = 0.5,
                  objective = "reg:linear", eval_metric = "rmse")

param_mae = list(booster="gbtree", max_depth = 4, eta = 0.01, subsample = 0.5,
                 objective = "reg:linear", eval_metric = "mae")

param_mape = list(booster="gbtree", max_depth = 4, eta = 0.01, subsample = 0.5,
                  objective = "reg:linear", eval_metric = xg_eval_mape)

param_my_mae = list(booster="gbtree",max_depth = 4, eta = 0.01, subsample = 0.5,
                    objective = amo.fairobj2, eval_metric = amm_mape)

set.seed(1234567)
# rm(xgb.rmse_list, xgb.mae_list, xgb.mape_list, xgb.my_list)
xgb.rmse_list = list()
for(i in 1:4){
  xgb.rmse_list[[i]] = xgb.train(params = param_rmse, data = dtrain[[i]], watchlist = watchlist[[i]],
                                 verbose=1, print_every_n = 100, nround = 10000,early_stopping_rounds = 10) #  ,maximize = F)
}
xgb.mae_list = list()
for(i in 1:4){
  xgb.mae_list[[i]] = xgb.train(params = param_mae, data = dtrain[[i]], watchlist = watchlist[[i]],
                                verbose=1, print_every_n = 100, nround = 10000,early_stopping_rounds = 10) #  ,maximize = F)
}
xgb.mape_list = list()
for(i in 1:4){
  xgb.mape_list[[i]] = xgb.train(params = param_mape, data = dtrain[[i]], watchlist = watchlist[[i]],
                                 verbose=1, print_every_n = 100, nround = 10000,early_stopping_rounds = 10  ,maximize = F)
}
xgb.my_list = list()
for(i in 1:4){
  xgb.my_list[[i]] = xgb.train(params = param_my_mae, data = dtrain[[i]], watchlist = watchlist[[i]],
                               verbose=1, print_every_n = 100, nround = 10000,early_stopping_rounds = 10  ,maximize = F)
}

pred1 = pred2 = pred3 = pred4 = NULL
pred1 = predict(xgb.rmse_list[[1]], dtest[[1]]);pred2 = predict(xgb.mae_list[[1]], dtest[[1]]);
pred3 = predict(xgb.mape_list[[1]], dtest[[1]]);pred4 = predict(xgb.my_list[[1]], dtest[[1]]);
for(i in 2:4){
  pred1[dtest_ind[[i]]] = predict(xgb.rmse_list[[i]], dtest[[i]])
  pred2[dtest_ind[[i]]] = predict(xgb.mae_list[[i]], dtest[[i]])
  pred3[dtest_ind[[i]]] = predict(xgb.mape_list[[i]], dtest[[i]])
  pred4[dtest_ind[[i]]] = predict(xgb.my_list[[i]], dtest[[i]])
}
pred1 = exp(pred1); pred2 = exp(pred2); pred3 = exp(pred3); pred4 = exp(pred4)
ensemble_pred$xgb_rmse_pred = pred1; ensemble_pred$xgb_mae_pred = pred2
ensemble_pred$xgb_mape_pred = pred3; ensemble_pred$xgb_my_pred = pred4

apply(ensemble_pred[,-1], 2, function(x){ mape(test$prod_sales[!s_mape_ind], x[!s_mape_ind])  }) %>% sort
pred = ensemble_pred[,-1] %>% select(xgb_rmse_pred, xgb_mae_pred, xgb_mape_pred, xgb_my_pred) %>%
  apply(., 1, mean)
s_mape_ind = (test$prod_sales==0)
mape(test$prod_sales[!s_mape_ind], pred[!s_mape_ind])

############################ 방송편성 최적화(헝가리안 알고리즘) ############################
############## Xgboost ##############
Hungary_data = NULL
for(i in 1:nrow(candi_data)){
  pred_data = cbind(candi_date1, candi_data[i, !(colnames(candi_data) %in% c("broad_s","hour","weekdays","holiday"))])
  
  dpred = lapply(form, function(x){
    tmp = model.matrix(x, data = pred_data)[,-1]
    tmp_label = row.names(tmp) %>% as.numeric
    tmp_label = which(row.names(pred_data) %in% tmp_label)
    tmp1 = pred_data
    # tmp1[tmp1$prod_sales==0,"prod_sales"] = 0.5*tmp1[tmp1$prod_sales==0,"price"]
    return(tmp %>% xgb.DMatrix(., label = log(tmp1$prod_sales[tmp_label])) )
  })
  
  dpred_ind = lapply(form, function(x){
    tmp = model.matrix(x, data = pred_data)[,-1]
    tmp_label = row.names(tmp) %>% as.numeric
    tmp_label = which(row.names(pred_data) %in% tmp_label)
    return(tmp_label)
  })
  
  pred1 = pred2 = pred3 = pred4 = NULL
  pred1 = predict(xgb.rmse_list[[1]], dpred[[1]]); pred2 = predict(xgb.mae_list[[1]], dpred[[1]])
  pred3 = predict(xgb.mape_list[[1]], dpred[[1]]); pred4 = predict(xgb.my_list[[1]], dpred[[1]])
  for(i in 2:4){
    if( length(dpred_ind[[i]])==0 ){
      next;
    }
    pred1[dpred_ind[[i]]] = predict(xgb.rmse_list[[i]], dpred[[i]])
    pred2[dpred_ind[[i]]] = predict(xgb.mae_list[[i]], dpred[[i]])
    pred3[dpred_ind[[i]]] = predict(xgb.mape_list[[i]], dpred[[i]])
    pred4[dpred_ind[[i]]] = predict(xgb.my_list[[i]], dpred[[i]])
  }
  pred1 = exp(pred1); pred2 = exp(pred2); pred3 = exp(pred3); pred4 = exp(pred4)
  
  Hungary_data = rbind(Hungary_data, cbind(pred1, pred2, pred3, pred4) %>% apply(., 1, mean) )
}
Hungary_data = Hungary_data %>% as.data.frame %>% mutate(broad_s = candi_data$broad_s, p_name = candi_data$p_name)
# colnames(Hungary_data)[-c(ncol(Hungary_data)-1,ncol(Hungary_data))] =
#   paste0("X",colnames(Hungary_data)[-c(ncol(Hungary_data)-1,ncol(Hungary_data))])
var_name = colnames(Hungary_data)[-c(ncol(Hungary_data)-1,ncol(Hungary_data))]

j = 2
Hungary_data_broad = Hungary_data
while( j <= nrow(Hungary_data_broad) ){
  if(Hungary_data_broad[j-1,"broad_s"]==Hungary_data_broad[j,"broad_s"]){
    Hungary_data_broad[j-1,var_name] = Hungary_data_broad[j-1,var_name] + Hungary_data_broad[j,var_name]
    tt1 = (Hungary_data_broad[j-1,"p_name"] %>% strsplit(., " | & "))[[1]]
    tt2 = (Hungary_data_broad[j,"p_name"] %>% strsplit(.," "))[[1]]
    Hungary_data_broad[j-1,"p_name"] = paste0(Hungary_data_broad[j-1,"p_name"]," & ",tt2[!(tt2 %in% tt1)] %>% paste0(., collapse =" ") )
    Hungary_data_broad = Hungary_data_broad[-j,]
  }else{
    j = j+1
  }
}
# Hungary_data %>% dim
# test %>% dim
# candi_data %>% dim

Hungary_result = Hungary_data_broad[,-c(ncol(Hungary_data_broad)-1,ncol(Hungary_data_broad))] %>%
  as.matrix %>% solve_LSAP(., maximum = T)

Hungary_final = data.frame( broad_s = Hungary_data_broad$broad_s[Hungary_result], p_name = Hungary_data_broad$p_name,
                            prod_sales = Hungary_data_broad[cbind(seq_along(Hungary_result), Hungary_result)] %>% as.numeric)

dim()
Hungary_final %>% dim

Hungary_value = sum(real_pred[test$broad_s %in% fix_data$broad_s]) + sum( Hungary_final$prod_sales )

real_pred = ensemble_pred[,-1] %>% select(xgb_rmse_pred, xgb_mae_pred, xgb_mape_pred, xgb_my_pred) %>%
  apply(., 1, mean)

c(Hungary_value, real_pred %>% sum, test$prod_sales %>% sum)

############################ 최적 노출시간 극대화 ############################
tt1 = c((row.names(Hungary_data_broad) %>% as.numeric %>% diff),
        (candi_data %>% nrow) - (row.names(Hungary_data_broad) %>% as.numeric %>% max) + 1)
candi_data1 = candi_data
candi_data1$broad_s = rep( Hungary_final$broad_s, tt1)
candi_data1$hungary_pred = rep( Hungary_final$prod_sales, tt1)
candi_data1$hour = hour(candi_data1$broad_s) %>% factor(., levels = c(0:2, 6:23))
candi_data1$weekdays = weekdays(candi_data1$broad_s) %>% factor

candi_data1$date = candi_data1$broad_s %>% as.character %>% strsplit(.," ") %>% sapply(.,"[",1)
candi_data1$my_id = 1:nrow(candi_data1)
candi_data1$holiday = NULL

candi_data1 = data.frame(date = candi_data$broad_s %>% as.character %>% strsplit(.," ") %>% sapply(.,"[",1),
                         holiday = candi_data$holiday) %>% unique %>% merge(candi_data1, ., by = "date") %>% arrange(my_id)
candi_data1$date = NULL
candi_data1$my_id = NULL

candi_date2 = (candi_data1$broad_s %>% table)[(candi_data1$broad_s %>% table) >= 2] %>% names %>% ymd_hms
(candi_data1$broad_s %>% table)[(candi_data1$broad_s %>% table) >= 2] %>% sum

max_period_data = NULL
for(i in 1:length(candi_date2)){
  tt1 = candi_date2[i]
  tmp1 = candi_data1[candi_data1$broad_s==tt1,]
  broad_time = tmp1$period[1]
  if( nrow(tmp1)*10 >= broad_time ){
    tmp1$max_period = tmp1$hungary_pred
    max_period_data = rbind(max_period_data, tmp1)
    paste0(i," : No at least 10 minutes") %>% print; next
  }
  free_time = broad_time - nrow(tmp1)*10
  tt2 = restrictedparts(free_time, nrow(tmp1) )
  
  ex_period_candi = tt2 %>% apply(., 2, function(x){
    table(x) %>% iterpc(., labels = names(.), ordered = TRUE) %>% getall() %>% t %>% as.numeric })
  if( "list" %in% ( ex_period_candi %>% class ) ){
    ex_period_candi = ex_period_candi %>% unlist
  }else{
    ex_period_candi = ex_period_candi %>% as.numeric
  }
  ex_period_candi = ex_period_candi + 10
  tmp1$ex_period = NULL
  tmp2 = cbind(tmp1, ex_period = ex_period_candi)
  
  dpred = lapply(form, function(x){
    tmp = model.matrix(x, data = tmp2)[,-1]
    tmp_label = row.names(tmp) %>% as.numeric
    tmp_label = which(row.names(tmp2) %in% tmp_label)
    tmp1 = tmp2
    # tmp1[tmp1$prod_sales==0,"prod_sales"] = 0.5*tmp1[tmp1$prod_sales==0,"price"]
    return(tmp %>% xgb.DMatrix(., label = log(tmp1$prod_sales[tmp_label])) )
  })
  
  dpred_ind = lapply(form, function(x){
    tmp = model.matrix(x, data = tmp2)[,-1]
    tmp_label = row.names(tmp) %>% as.numeric
    tmp_label = which(row.names(tmp2) %in% tmp_label)
    return(tmp_label)
  })
  
  pred1 = pred2 = pred3 = pred4 = NULL
  pred1 = predict(xgb.rmse_list[[1]], dpred[[1]]); pred2 = predict(xgb.mae_list[[1]], dpred[[1]])
  pred3 = predict(xgb.mape_list[[1]], dpred[[1]]); pred4 = predict(xgb.my_list[[1]], dpred[[1]])
  for(j in 2:4){
    if( length(dpred_ind[[j]])==0 ){
      next;
    }
    pred1[dpred_ind[[j]]] = predict(xgb.rmse_list[[j]], dpred[[j]])
    pred2[dpred_ind[[j]]] = predict(xgb.mae_list[[j]], dpred[[j]])
    pred3[dpred_ind[[j]]] = predict(xgb.mape_list[[j]], dpred[[j]])
    pred4[dpred_ind[[j]]] = predict(xgb.my_list[[j]], dpred[[j]])
  }
  pred1 = exp(pred1); pred2 = exp(pred2); pred3 = exp(pred3); pred4 = exp(pred4)
  pred = cbind(pred1, pred2, pred3, pred4) %>% apply(., 1, mean)
  
  tmp3 = data.frame(pred ,id = rep(1:( length(pred)/nrow(tmp1) ), each = nrow(tmp1)) ) %>%
    group_by(id) %>% summarise(value = sum(pred))

  tmp1$ex_period = ex_period_candi[( which.max(tmp3$value)*nrow(tmp1) - nrow(tmp1)+1 ) : ( which.max(tmp3$value)*nrow(tmp1) )]
  tmp1$max_period = max(tmp3$value)
  max_period_data = rbind(max_period_data, tmp1)
  print(paste0(i,"-hungary : ",tmp1$hungary_pred %>% unique,", max_period : ",tmp1$max_period %>% unique))
}
max_period_data %>% dim

max_period_data1 = max_period_data %>% group_by(broad_s) %>%
  summarise(prod_sales = sum(prod_sales), hungary_pred = unique(hungary_pred), max_period = unique(max_period)) %>% 
  as.data.frame

tt1 = candi_data1 %>% filter(!(broad_s %in% candi_date2)) %>% select(hungary_pred) %>% sum

max_period_value = sum(real_pred[test$broad_s %in% fix_data$broad_s]) + tt1 + sum( max_period_data1$max_period )

####result
c(max_period_value, Hungary_value, real_pred %>% sum, test$prod_sales %>% sum) %>% print



