# install the required packages first

require(jsonlite)
require(httr)
require(data.table)
require(xts)
require(forecast)
require(ggplot2)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group6"
p_word = "HarNGafZYHupCK6x"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)
#data_temiz <- data[product_content_id==32939029 & basket_count>-1]


dates <- seq(as.Date("2019-04-30"), length = uniqueN(data$event_date), by = "days")

dates <- seq(as.Date("2019-04-30"), length = nrow(data)/8, by = "days")

#dates <- seq(as.Date("2019-04-30"), length = 395, by = "days")


tayt <- xts(data[product_content_id==31515569],order.by=dates)
disfirca <- xts(data[product_content_id==32939029],order.by=dates)
mont <- xts(data[product_content_id==3904356],order.by=dates)
mendil <- xts(data[product_content_id==4066298],order.by=dates)
bikini <- xts(data[product_content_id==5926527],order.by=dates)
kulaklik <- xts(data[product_content_id==6676673],order.by=dates)
supurge <- xts(data[product_content_id==7061886],order.by=dates)
yuztemizleyici <- xts(data[product_content_id==85004],order.by=dates)

#autoplot(tayt, facets = TRUE)
#as.numeric(tayt[,"category_sold"])

#price elasticity
#tayt_price_vector <- log(as.numeric(tayt$price>0))
#tayt_sold_vector <- log(as.numeric(tayt$price>0))
#tayt_price_elast <- data.frame(log(tayt_sold_vector),log(tayt_price_vector))
#colnames(tayt_price_elast) <- c("log_sales","log_price")
#tayt_price_elasticity <- lm("log_sales","log_price",data=tayt_price_elast)

#arima model, error test, yarin icin forecast
test_tayt<-tail(tayt,7)
tayt_start <- window(tayt,start="2020-01-05")
#tayt_basket <- tayt_basket_psu [,"favored_count">0]
tayt_fav_duration<- window(tayt,start="2020-01-02",end=tayt[nrow(tayt)-3]$event_date)
#tayt_duzgun_sold <- tayt_sold_duration[,"favored_count">0]
tayt_sold_fav <- auto.arima(as.numeric(tayt_start$sold_count), xreg = as.numeric(tayt_fav_duration$favored_count))
checkresiduals(tayt_sold_fav)
yarin_tayt_fav <- forecast(tayt_sold_fav, xreg = as.numeric(test_tayt$favored_count))

tayt_son<-tayt_start[-179]
tayt_sonn <- tayt_son[as.numeric(tayt_son$price)>0]

tayt_sold_visit<-auto.arima(as.numeric(tayt_start$sold_count),xreg=as.numeric(tayt_fav_duration$visit_count))
checkresiduals(tayt_sold_visit)

yarin_tayt_visit<-forecast(tayt_sold_visit,xreg=as.numeric(test_tayt$visit_count))
summary(yarin_tayt_visit)

#regressor is the mean of basket of 3-5 days 
ziyaret_tayt<-tail(tayt,5)
mean_test_basket_tayt<-head(ziyaret_tayt,3)
mean_basket_tayt<-mean(as.numeric(mean_test_basket_tayt$basket_count))
tayt_basket_start<-window(tayt,start="2020-01-28")
tayt_basket<-window(tayt,start="2020-01-25")
tayt_basket_head<-head(tayt_basket,nrow(tayt_basket_start))
tayt_sold_basket<-auto.arima(as.numeric(tayt_basket_start$sold_count),xreg=as.numeric(tayt_basket_head$basket_count))
checkresiduals(tayt_sold_basket)
#lag 0.1dan kucuk kontrol,normal dagilima yakin, white noise kontrol
yarin_tayt_basket<-forecast(tayt_sold_basket,xreg=mean_basket_tayt)

#AIC ler karsilastirildi kucuk olana daha yuksek weight verildi, lag 0.1dan kucuk kontrol,normal dagilima yakin, white noise kontrol
mixed_yarin_tayt<-(0.40*yarin_tayt_basket$mean + 0.30 * yarin_tayt_visit$mean[1] +0.30*yarin_tayt_fav$mean[1])

fc <- mixed_yarin_tayt[1]
#fc <- as.numeric(yarin_tayt_basket$mean[1])

#yarin_tayt_f
#residual iyi model,5ten sonrasi daha iyi tahmin oluyor
disfirca_baslangic<- window(disfirca,start="2019-11-21")
#View(disfirca_baslangic), if price is same before and after it is used, if not the mean of the two days is used for price
#Assumption olarak bir onceki gun ile bir sonraki gun fiyat ayniysa o fiyat gecerli, degilse ikisinin ortalamasi
disfirca_baslangic$price["2019-11-21"] <- "99.95"
disfirca_baslangic$price["2019-11-22"] <- "99.95"
disfirca_baslangic$price["2019-11-30"] <- (as.numeric(disfirca_baslangic$price["2019-11-29"]) + as.numeric(disfirca_baslangic$price["2019-12-01"])) / 2
disfirca_baslangic$price["2019-12-04"] <- "129.90"
disfirca_baslangic$price["2019-12-08"] <- (as.numeric(disfirca_baslangic$price["2019-12-07"]) + as.numeric(disfirca_baslangic$price["2019-12-09"])) / 2
disfirca_baslangic$price["2019-12-21"] <- (as.numeric(disfirca_baslangic$price["2019-12-20"]) + as.numeric(disfirca_baslangic$price["2019-12-22"])) / 2
disfirca_pricefix<-disfirca_baslangic

#disfirca_baslangic_train<- window(disfirca,start="2019-11-21",end="2020-03-20")
#disfirca_baslangic_test<-window(disfirca,start="2020-03-21")
#disfirca_sold <- auto.arima(as.numeric(disfirca_baslangic$sold_count), xreg = as.numeric(disfirca_baslangic$visit_count))
#checkresiduals(disfirca_sold)
#yarin_disfirca <- forecast(disfirca_sold, xreg = as.numeric(disfirca_baslangic$visit_count), h = 2)

#accuracy(yarin_disfirca,disfirca_baslangic_train)
#autoplot(yarin_disfirca)
#yarin_disfirca$mean
#fc <- c(fc, yarin_disfirca$mean[48])
#autoplot(forecast(tbats(disfirca_baslangic[,"sold_count"])))

test_disfirca<-tail(disfirca,7)

#modeli son bir haftanın favorilere eklenme sayisina gore 1 haftalik satis tahmini modeli olusturuyor
#disfirca forecast by favored_count of last week
disfirca_sold_fav <- auto.arima(as.numeric(disfirca_pricefix$sold_count), xreg = as.numeric(disfirca_pricefix$favored_count))
autoplot(disfirca_sold_fav)
summary(disfirca_sold_fav)
checkresiduals(disfirca_sold_fav)
yarin_disfirca_fav<- forecast(disfirca_sold_fav, xreg = as.numeric(test_disfirca$favored_count)) 
autoplot(yarin_disfirca_fav)
yarin_disfirca_fav$mean

#modeli son bir haftanın ziyaret sayisina gore 1 haftalik satis tahmini modeli olusturuyor
#better than the one above, #disfirca forecast by visit_count of last week
disfirca_sold_visit <- auto.arima(as.numeric(disfirca_pricefix$sold_count), xreg = as.numeric(disfirca_pricefix$visit_count))
autoplot(disfirca_sold_visit)
summary(disfirca_sold_visit)
checkresiduals(disfirca_sold_visit)
yarin_disfirca_visit<- forecast(disfirca_sold_visit, xreg = as.numeric(test_disfirca$visit_count)) 
autoplot(yarin_disfirca_visit)

#worst of the 3 , #, p value so low
disfirca_sold_price <- auto.arima(as.numeric(disfirca_pricefix$sold_count), xreg = as.numeric(disfirca_pricefix$price))
autoplot(disfirca_sold_price)
summary(disfirca_sold_price)
checkresiduals(disfirca_sold_price)
yarin_disfirca_price<- forecast(disfirca_sold_price, xreg = as.numeric(test_disfirca$price)) 
autoplot(yarin_disfirca_price)

#regressor is the mean of basket of added 3,4 or 5 days ago, 24yle baslama nedeni satisa ciktigi gun
ziyaret_disfirca<-tail(disfirca,5)
mean_test_basket_disfirca<-head(ziyaret_disfirca,3)
mean_basket_disfirca<-mean(as.numeric(mean_test_basket_disfirca$basket_count))
disfirca_basket_start<-window(disfirca,start="2020-01-27")
disfirca_basket<-window(disfirca,start="2020-01-24")
disfirca_basket_head<-head(disfirca_basket,nrow(disfirca_basket_start))
disfirca_sold_basket<-auto.arima(as.numeric(disfirca_basket_start$sold_count),xreg=as.numeric(disfirca_basket_head$basket_count))
checkresiduals(disfirca_sold_basket)
yarin_disfirca_basket<-forecast(disfirca_sold_basket,xreg=mean_basket_disfirca)

#mixed forecast of disfirca,Aic ye gore weight
yarin_disfirca <- 0.35 * yarin_disfirca_visit$mean[1]  + 0.2 * yarin_disfirca_fav$mean[1] +  0.1 * yarin_disfirca_price$mean[1] + 0.35 * yarin_disfirca_basket$mean
#summary(yarin_disfirca)

fc <- c(fc, yarin_disfirca[1])
#fc <- c(fc, as.numeric(yarin_disfirca_visit$mean[1]))


#forecast train set of last 3 months sfircaand test setof last week,using fav count as regressor in the model, giving 0 forecast makes sense
#talep hizli degisip bahar sonrasi yaz boyunca ayni kaldigi icin boyle tercih edildi
train_mont<-tail(mont,90)
test_mont<-tail(mont,7)
mont_sold_fav <- auto.arima(as.numeric(train_mont$sold_count), xreg = as.numeric(train_mont$favored_count))
checkresiduals(mont_sold_fav)
yarin_mont_fav <- forecast(mont_sold_fav, xreg = as.numeric(test_mont$sold_count))
autoplot(yarin_mont_fav)
yarin_mont_fav$mean

#fc <- c(fc, as.numeric(yarin_mont_fav$mean[1]))
fc <- c(fc, as.numeric(yarin_mont_fav$mean[1]))
#fc <- c(fc, as.numeric(mont$sold_count[387]))


#model sacma tahmin veriyor,en mantiklisi verinin basladigi(-1 siz) tarihden itibaren xts alindi 
#menidl satisa baslangic
mendil_start <- window(mendil,start="2019-09-09")
mendil_start$price["2019-10-12"] <- (as.numeric(mendil_start$price["2019-10-11"]) + as.numeric(mendil_start$price["2019-10-13"])) / 2
test_mendil<-tail(mendil,7)


#by visit, AICc high, lag issue
ziyaret_mendil<-tail(mendil_start,5)
mean_test_visit_mendil<-head(ziyaret_mendil,3)
mean_visit_mendil<-mean(as.numeric(mean_test_visit_mendil$visit_count))
mendil_visit_start<-window(mendil_start,start="2019-12-16")
mendil_visit<-window(mendil_start,start="2019-12-13")
mendil_visit_head<-head(mendil_visit,nrow(mendil_visit_start))
mendil_sold_visit <- auto.arima(as.numeric(mendil_visit_start$sold_count), xreg = as.numeric(mendil_visit_head$visit_count))
autoplot(mendil_sold_visit)
summary(mendil_sold_visit)
checkresiduals(mendil_sold_visit)
yarin_mendil_visit<- forecast(mendil_sold_visit, xreg = mean_visit_mendil) 
yarin_mendil_visit$mean

#by price,rezalet
mendil_sold_price <- auto.arima(as.numeric(mendil_start$sold_count), xreg = as.numeric(mendil_start$price))
autoplot(mendil_sold_price)
summary(mendil_sold_price)
checkresiduals(mendil_sold_price)
yarin_mendil_price<- forecast(mendil_sold_price, xreg = as.numeric(test_mendil$price)) 
autoplot(yarin_mendil_price)

#by fav count of 3-5 days ago, good, favori giris 13 aralık
ziyaret_mendil<-tail(mendil_start,5)
mean_test_fav_mendil<-head(ziyaret_mendil,3)
mean_fav_mendil<-mean(as.numeric(mean_test_fav_mendil$favored_count))
mendil_fav_start<-window(mendil_start,start="2019-12-16")
mendil_fav<-window(mendil_start,start="2019-12-13")
mendil_fav_head<-head(mendil_fav,nrow(mendil_fav_start))
mendil_sold_fav <- auto.arima(as.numeric(mendil_fav_start$sold_count), xreg = as.numeric(mendil_fav_head$favored_count))
autoplot(mendil_sold_fav)
summary(mendil_sold_fav)
checkresiduals(mendil_sold_fav)
yarin_mendil_fav<- forecast(mendil_sold_fav, xreg = mean_fav_mendil) 
yarin_mendil_fav$mean

#by basket count of 3-5 days ago,best model,basket verisi giris 24 ocak
ziyaret_mendil<-tail(mendil_start,5)
mean_test_basket_mendil<-head(ziyaret_mendil,3)
mean_basket_mendil<-mean(as.numeric(mean_test_basket_mendil$basket_count))
mendil_basket_start<-window(mendil_start,start="2020-01-27")
mendil_basket<-window(mendil_start,start="2020-01-24")
mendil_basket_head<-head(mendil_basket,nrow(mendil_basket_start))
mendil_sold_basket<-auto.arima(as.numeric(mendil_basket_start$sold_count),xreg=as.numeric(mendil_basket_head$basket_count))
checkresiduals(mendil_sold_basket)
yarin_mendil_basket<-forecast(mendil_sold_basket,xreg=mean_basket_mendil)

yarin_mendil <- 0.34 * yarin_mendil_visit$mean[1]  + 0.33 * yarin_mendil_basket$mean[1] + 0.33 * yarin_mendil_fav$mean[1]

fc <- c(fc,yarin_mendil[1])
#fc <- c(fc, as.numeric(yarin_mendil_visit$mean[1]))





#by fav, residual check okay, AIcc value of 805





#seasonality effect visit_countta daha iyi ama neagtif veriyor price birakildi, cok seasonal olduğü icin son ay kullanildi
bikini_start<-window(bikini,start="2020-05-15")
bikini_sold <- auto.arima(as.numeric(bikini_start$sold_count), xreg = as.numeric(bikini_start$visit_count))
checkresiduals(bikini_sold)
test_bikini<-tail(bikini,7)
yarin_bikini <- forecast(bikini_sold, xreg = as.numeric(test_bikini$visit_count))
yarin_bikini$mean
autoplot(yarin_bikini)
#fc <- c(fc, yarin_bikini$mean[2])
fc <- c(fc, yarin_bikini$mean[1])



test_kulaklik<-tail(kulaklik,7)
kulaklik_start<-window(kulaklik,start="2019-11-15")
kulaklik_debut<-window(kulaklik,start="2019-06-20")
kulaklik_debut$price["2019-08-11"] <- "165.00"
kulaklik_debut$price["2019-08-12"] <- "165.00"
kulaklik_debut$price["2019-12-12"] <- "126.00"
#better, #kulaklik forecast by visit_count of last week,AICc=2895,checkresidual ok
kulaklik_sold_visit <- auto.arima(as.numeric(kulaklik_start$sold_count), xreg = as.numeric(kulaklik_start$visit_count))
autoplot(kulaklik_sold_visit)
summary(kulaklik_sold_visit)
checkresiduals(kulaklik_sold_visit)
yarin_kulaklik_visit<- forecast(kulaklik_sold_visit, xreg = as.numeric(test_kulaklik$visit_count)) 
autoplot(yarin_kulaklik_visit)

#worst of the 3 , #kulaklik forecast by visit_count of last week,aicc so high,p-value so low
kulaklik_sold_price <- auto.arima(as.numeric(kulaklik_debut$sold_count), xreg = as.numeric(kulaklik_debut$price))
autoplot(kulaklik_sold_price)
summary(kulaklik_sold_price)
checkresiduals(kulaklik_sold_price)
yarin_kulaklik_price<- forecast(kulaklik_sold_price, xreg = as.numeric(test_kulaklik$price)) 
autoplot(yarin_kulaklik_price)

#kulaklik forecast by favored_count of last week,AIcc=2166,residual not normally
ziyaret_kulaklik<-tail(kulaklik,5)
mean_test_fav_kulaklik<-head(ziyaret_kulaklik,3)
mean_fav_kulaklik<-mean(as.numeric(mean_test_fav_kulaklik$favored_count))
kulaklik_fav_start<-window(kulaklik,start="2019-12-16")
kulaklik_fav<-window(kulaklik,start="2019-12-13")
kulaklik_fav_head<-head(kulaklik_fav,nrow(kulaklik_fav_start))
kulaklik_sold_fav <- auto.arima(as.numeric(kulaklik_fav_start$sold_count), xreg = as.numeric(kulaklik_fav_head$favored_count))
autoplot(kulaklik_sold_fav)
summary(kulaklik_sold_fav)
checkresiduals(kulaklik_sold_fav)
yarin_kulaklik_fav<- forecast(kulaklik_sold_fav, xreg = mean_fav_kulaklik) 
yarin_kulaklik_fav$mean


#regressor is the mean of basket of 3-5 days ,AIc lowest, residual almost normal, basket veri baslangic 24 ocak
ziyaret_kulaklik<-tail(kulaklik,5)
mean_test_basket_kulaklik<-head(ziyaret_kulaklik,3)
mean_basket_kulaklik<-mean(as.numeric(mean_test_basket_kulaklik$basket_count))
kulaklik_basket_start<-window(kulaklik,start="2020-01-27")
kulaklik_basket<-window(kulaklik,start="2020-01-24")
kulaklik_basket_head<-head(kulaklik_basket,nrow(kulaklik_basket_start))
kulaklik_sold_basket<-auto.arima(as.numeric(kulaklik_basket_start$sold_count),xreg=as.numeric(kulaklik_basket_head$basket_count))
checkresiduals(kulaklik_sold_basket)
yarin_kulaklik_basket<-forecast(kulaklik_sold_basket,xreg=mean_basket_kulaklik)

#mixed forecast of kulaklik
yarin_kulaklik <- 0.3 * yarin_kulaklik_visit$mean[1]   +  0.05 * yarin_kulaklik_price$mean[1] + 0.35 * yarin_kulaklik_basket$mean[1] + 0.3 * yarin_kulaklik_fav$mean[1]
#summary(yarin_kulaklik)

fc <- c(fc, yarin_kulaklik[1])
#fc <- c(fc,yarin_kulaklik_basket$mean[1])

#start and price fix for supurge
supurge_start<-window(supurge,start="2019-07-27")
supurge_start$price["2019-09-14"] <- (as.numeric(mendil_start$price["2019-09-13"]) + as.numeric(mendil_start$price["2019-10-17"])) / 2
supurge_start$price["2019-09-15"] <- (as.numeric(mendil_start$price["2019-09-13"]) + as.numeric(mendil_start$price["2019-10-17"])) / 2
supurge_start$price["2019-09-16"] <- (as.numeric(mendil_start$price["2019-09-13"]) + as.numeric(mendil_start$price["2019-10-17"])) / 2
supurge_start$price["2019-12-01"] <- (as.numeric(mendil_start$price["2019-11-30"]) + as.numeric(mendil_start$price["2019-12-03"])) / 2
supurge_start$price["2019-12-02"] <- (as.numeric(mendil_start$price["2019-11-30"]) + as.numeric(mendil_start$price["2019-12-03"])) / 2
supurge_start$price["2019-12-04"] <- (as.numeric(mendil_start$price["2019-12-06"]) + as.numeric(mendil_start$price["2019-12-03"])) / 2
supurge_start$price["2019-12-05"] <- (as.numeric(mendil_start$price["2019-12-06"]) + as.numeric(mendil_start$price["2019-12-03"])) / 2
test_supurge<-tail(supurge_start,7)

#by visit, AIcc=2407, residual okay
supurge_sold_visit <- auto.arima(as.numeric(supurge_start$sold_count), xreg = as.numeric(supurge_start$visit_count))
autoplot(supurge_sold_visit)
summary(supurge_sold_visit)
checkresiduals(supurge_sold_visit)
yarin_supurge_visit<- forecast(supurge_sold_visit, xreg = as.numeric(test_supurge$visit_count)) 
autoplot(yarin_supurge_visit)

#by price, residual not normally distributed, high AIcc
supurge_sold_price <- auto.arima(as.numeric(supurge_start$sold_count), xreg = as.numeric(supurge_start$price))
autoplot(supurge_sold_price)
summary(supurge_sold_price)
checkresiduals(supurge_sold_price)
yarin_supurge_price<- forecast(supurge_sold_price, xreg = as.numeric(test_supurge$price)) 
autoplot(yarin_supurge_price)

#by favored of 3-5 ago, residual not  normally, AIcc=2458 , veri girisi start duzeltme
ziyaret_supurge<-tail(supurge,5)
mean_test_fav_supurge<-head(ziyaret_supurge,3)
mean_fav_supurge<-mean(as.numeric(mean_test_fav_supurge$favored_count))
supurge_fav_start<-window(supurge_start,start="2019-09-30")
supurge_fav<-window(supurge_start,start="2019-09-27")
supurge_fav_head<-head(supurge_fav,nrow(supurge_fav_start))
supurge_sold_fav <- auto.arima(as.numeric(supurge_fav_start$sold_count), xreg = as.numeric(supurge_fav_head$favored_count))
autoplot(supurge_sold_fav)
summary(supurge_sold_fav)
checkresiduals(supurge_sold_fav)
yarin_supurge_fav<- forecast(supurge_sold_fav, xreg = mean_fav_supurge) 

#by basket of 3-5 days ago
ziyaret_supurge<-tail(supurge,5)
mean_test_basket_supurge<-head(ziyaret_supurge,3)
mean_basket_supurge<-mean(as.numeric(mean_test_basket_supurge$basket_count))
supurge_basket_start<-window(supurge_start,start="2019-09-30")
supurge_basket<-window(supurge_start,start="2019-09-27")
supurge_basket_head<-head(supurge_basket,nrow(supurge_basket_start))
supurge_sold_basket<-auto.arima(as.numeric(supurge_basket_start$sold_count),xreg=as.numeric(supurge_basket_head$basket_count))
checkresiduals(supurge_sold_basket)
summary(supurge_sold_basket)
yarin_supurge_basket<-forecast(supurge_sold_basket,xreg=mean_basket_supurge)


yarin_supurge <- 0.3 * yarin_supurge_visit$mean[1]   +  0.05 * yarin_supurge_price$mean[1] + 0.3 * yarin_supurge_basket$mean[1] + 0.35 * yarin_supurge_fav$mean[1]


#fc <- c(fc, yarin_supurge$lower[1])
fc <- c(fc, yarin_supurge[1])

#price fix for yzutemizleyici
yuztemizleyici$price["2019-05-08"] <- (as.numeric(yuztemizleyici$price["2019-05-07"]) + as.numeric(yuztemizleyici$price["2019-05-10"])) / 2
yuztemizleyici$price["2019-05-09"] <- (as.numeric(yuztemizleyici$price["2019-05-07"]) + as.numeric(yuztemizleyici$price["2019-05-10"])) / 2
yuztemizleyici$price["2019-06-26"] <- (as.numeric(yuztemizleyici$price["2019-06-25"]) + as.numeric(yuztemizleyici$price["2019-05-27"])) / 2
yuztemizleyici$price["2019-07-13"] <- (as.numeric(yuztemizleyici$price["2019-07-12"]) + as.numeric(yuztemizleyici$price["2019-05-14"])) / 2
yuztemizleyici$price["2019-09-01"] <- (as.numeric(yuztemizleyici$price["2019-08-31"]) + as.numeric(yuztemizleyici$price["2019-09-02"])) / 2
test_yuztemizleyici<-tail(yuztemizleyici,7)

#by visit, high AICc
yuztemizleyici_sold_visit <- auto.arima(as.numeric(yuztemizleyici$sold_count), xreg = as.numeric(yuztemizleyici$visit_count))
autoplot(yuztemizleyici_sold_visit)
summary(yuztemizleyici_sold_visit)
checkresiduals(yuztemizleyici_sold_visit)
yarin_yuztemizleyici_visit<- forecast(yuztemizleyici_sold_visit, xreg = as.numeric(test_yuztemizleyici$visit_count)) 
autoplot(yarin_yuztemizleyici_visit)

#by price, worst, residuals bad
yuztemizleyici_sold_price <- auto.arima(as.numeric(yuztemizleyici$sold_count), xreg = as.numeric(yuztemizleyici$price))
autoplot(yuztemizleyici_sold_price)
summary(yuztemizleyici_sold_price)
checkresiduals(yuztemizleyici_sold_price)
yarin_yuztemizleyici_price<- forecast(yuztemizleyici_sold_price, xreg = as.numeric(test_yuztemizleyici$price)) 
autoplot(yarin_yuztemizleyici_price)

#by favored 3-5 days ago,Aicc 2412
ziyaret_yuztemizleyici<-tail(yuztemizleyici,5)
mean_test_fav_yuztemizleyici<-head(ziyaret_yuztemizleyici,3)
mean_fav_yuztemizleyici<-mean(as.numeric(mean_test_fav_yuztemizleyici$favored_count))
yuztemizleyici_fav_start<-window(yuztemizleyici,start="2019-09-30")
yuztemizleyici_fav<-window(yuztemizleyici,start="2019-09-27")
yuztemizleyici_fav_head<-head(yuztemizleyici_fav,nrow(yuztemizleyici_fav_start))
yuztemizleyici_sold_fav <- auto.arima(as.numeric(yuztemizleyici_fav_start$sold_count), xreg = as.numeric(yuztemizleyici_fav_head$favored_count))
autoplot(yuztemizleyici_sold_fav)
summary(yuztemizleyici_sold_fav)
checkresiduals(yuztemizleyici_sold_fav)
yarin_yuztemizleyici_fav<- forecast(yuztemizleyici_sold_fav, xreg = mean_fav_yuztemizleyici) 

#by basket of 3-5 days ago
ziyaret_yuztemizleyici<-tail(yuztemizleyici,5)
mean_test_basket_yuztemizleyici<-head(ziyaret_yuztemizleyici,3)
mean_basket_yuztemizleyici<-mean(as.numeric(mean_test_basket_yuztemizleyici$basket_count))
yuztemizleyici_basket_start<-window(yuztemizleyici,start="2019-09-30")
yuztemizleyici_basket<-window(yuztemizleyici,start="2019-09-27")
yuztemizleyici_basket_head<-head(yuztemizleyici_basket,nrow(yuztemizleyici_basket_start))
yuztemizleyici_sold_basket<-auto.arima(as.numeric(yuztemizleyici_basket_start$sold_count),xreg=as.numeric(yuztemizleyici_basket_head$basket_count))
checkresiduals(yuztemizleyici_sold_basket)
summary(yuztemizleyici_sold_basket)
yarin_yuztemizleyici_basket<-forecast(yuztemizleyici_sold_basket,xreg=mean_basket_yuztemizleyici)


yarin_yuztemizleyici <- 0.06 * yarin_yuztemizleyici_visit$mean[1]   +  0.04 * yarin_yuztemizleyici_price$mean[1] + 0.45 * yarin_yuztemizleyici_basket$mean[1] + 0.45 * yarin_yuztemizleyici_fav$mean[1]



fc <- c(fc, yarin_yuztemizleyici[1])
#yuztemizleyici[yuztemizleyici$price=="-1.00"]$price <- mean(as.numeric(yuztemizleyici$price),NA.rm=t)
#yuztemizleyici_sold <- auto.arima(as.numeric(yuztemizleyici$sold_count), xreg = as.numeric(yuztemizleyici$price))
#checkresiduals(yuztemizleyici_sold)
#yarin_yuztemizleyici <- forecast(yuztemizleyici_sold, xreg = as.numeric(yuztemizleyici$price), h = 2)
#autoplot(yarin_yuztemizleyici)
#fc <- c(fc, yarin_yuztemizleyici$mean[1])



predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=fc]


send_submission(predictions, token, url=subm_url, submit_now=T)

