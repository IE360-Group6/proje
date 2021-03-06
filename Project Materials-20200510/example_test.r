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

tayt_basket_psu <- window(tayt,start="2019-10-01", end = "2020-05-19")
#tayt_duzgun_basket <- tayt_basket_psu [,"basket_count">0]
#tayt_sold_psu <- window(tayt,start="2019-11-20",end="2020-05-19")
#tayt_duzgun_sold <- tayt_sold_psu [,"basket_count">0]
#tayt_sold <- auto.arima(as.numeric(tayt_duzgun_sold$sold_count), xreg = as.numeric(tayt_duzgun_basket$basket_count))
#checkresiduals(tayt_sold)
#yarin_tayt <- forecast(tayt_sold, xreg = as.numeric(tayt_duzgun_basket$basket_count), h = 2)
tayt_son<-tayt_basket_psu[-179]
tayt_sonn <- tayt_son[as.numeric(tayt_son$price)>0]
yarin_tayt_visit<-forecast(auto.arima(as.numeric(tayt_sonn$sold_count),xreg=as.numeric(tayt_sonn$visit_count)),xreg=as.numeric(tayt_sonn$visit_count), h = 15)
checkresiduals(auto.arima(as.numeric(tayt_sonn$sold_count),xreg=as.numeric(tayt_sonn$visit_count)))
#autoplot(yariave<-forecast(auto.arima(as.numeric(tayt_sonn$sold_count),xreg=as.numeric(tayt_sonn$favored_count)),xreg=as.numeric(tayt_sonn$favored_count)))
#checkresiduals(auto.arima(as.numeric(tayt_sonn$sold_count),xreg=as.numeric(tayt_sonn$favored_count)))
#autoplot(yarin_tayt_fave)

summary(yarin_tayt_visit)
#summary(yarin_tayt_fave)

#yarin_tayt$mean
fc_tayt <- yarin_tayt_visit$mean[-c(1:207)]

for_tayt_test = window(tayt, start = "2020-05-20", end = as.Date(as.Date("2020-05-20")+14))
for_tayt_test = for_tayt_test$sold_count

mae_tayt <- mean(abs(fc_tayt-as.numeric(for_tayt_test$sold_count)))/15
mape_tayt <- 100*mean(abs((fc_tayt-as.numeric(for_tayt_test$sold_count))/as.numeric(for_tayt_test$sold_count)))/15



#yarin_tayt_f
#residual iyi model,5ten sonrasi daha iyi tahmin oluyor
disfirca_baslangic<- window(disfirca,start="2019-11-21")
#View(disfirca_baslangic)
df <- disfirca_baslangic[as.numeric(disfirca_baslangic$price)>0]



#disfirca_baslangic_train<- window(disfirca,start="2019-11-21",end="2020-03-20")
#disfirca_baslangic_test<-window(disfirca,start="2020-03-21")
disfirca_sold <- auto.arima(as.numeric(disfirca_baslangic$sold_count), xreg = as.numeric(disfirca_baslangic$visit_count))
checkresiduals(disfirca_sold)
yarin_disfirca <- forecast(disfirca_sold, xreg = as.numeric(disfirca_baslangic$visit_count), h = 2)

#accuracy(yarin_disfirca,disfirca_baslangic_train)
autoplot(yarin_disfirca)
yarin_disfirca$mean
fc <- c(fc, yarin_disfirca$mean[6])
#autoplot(forecast(tbats(disfirca_baslangic[,"sold_count"])))


disfirca_sold <- auto.arima(as.numeric(df$sold_count), xreg = as.numeric(df$favored_count))
checkresiduals(disfirca_sold)
yarin_disfirca <- forecast(disfirca_sold, xreg = as.numeric(df$favored_count), h = 2) 
 
autoplot(yarin_disfirca)
yarin_disfirca$mean
#$fc <- c(fc, yarin_disfirca$mean[5])



#bu mevsim mont satilmaz forecast seasonal effect almasa da mantikli forecast veriyor onumuz icin
mont_sold <- auto.arima(as.numeric(mont$sold_count), xreg = as.numeric(mont$ty_visits))
checkresiduals(mont_sold)
yarin_mont <- forecast(mont_sold, xreg = as.numeric(mont$ty_visits), h = 2)
autoplot(yarin_mont)
yarin_mont$mean

fc <- c(fc, as.numeric(mont$sold_count[nrow(mont)]))

#fc <- c(fc, as.numeric(mont$sold_count[387]))


#model sacma tahmin veriyor,en mantiklisi verinin basladigi(-1 siz) tarihden itibaren xts alindi 
mendil_available <- window(mendil,start="2019-09-09")
mendil_sold <- auto.arima(as.numeric(mendil_available$sold_count), xreg = as.numeric(mendil_available$price))
checkresiduals(mendil_sold)
yarin_mendil <- forecast(mendil_sold, xreg = as.numeric(mendil_available$price), h = 2)
autoplot(yarin_mendil)
yarin_mendil$mean
fc <- c(fc,yarin_mendil$mean[1])

#seasonality effect visit_countta daha iyi ama neagtif veriyor price birakildi
bikini_sold <- auto.arima(as.numeric(bikini$sold_count), xreg = as.numeric(bikini$price))
checkresiduals(bikini_sold)
yarin_bikini <- forecast(bikini_sold, xreg = as.numeric(bikini$price), h = 2)
yarin_bikini$mean
autoplot(yarin_bikini)
fc <- c(fc, yarin_bikini$mean[7])

kulaklik_sold <- auto.arima(as.numeric(kulaklik$sold_count), xreg = as.numeric(kulaklik$price))
checkresiduals(kulaklik_sold)
yarin_kulaklik <- forecast(kulaklik_sold, xreg = as.numeric(kulaklik$price), h = 2)
autoplot(yarin_kulaklik)
yarin_kulaklik$mean
fc <- c(fc, yarin_kulaklik$mean[1])

supurge_sold <- auto.arima(as.numeric(supurge$sold_count), xreg = as.numeric(supurge$price))
checkresiduals(supurge_sold)
yarin_supurge <- forecast(supurge_sold, xreg = as.numeric(supurge$price), h = 2)
autoplot(yarin_supurge)
fc <- c(fc, yarin_supurge$lower[1])

yuztemizleyici_sold <- auto.arima(as.numeric(yuztemizleyici$sold_count), xreg = as.numeric(yuztemizleyici$price))
checkresiduals(yuztemizleyici_sold)
yarin_yuztemizleyici <- forecast(yuztemizleyici_sold, xreg = as.numeric(yuztemizleyici$price), h = 2)
autoplot(yarin_yuztemizleyici)
fc <- c(fc, yarin_yuztemizleyici$mean[1])


predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=fc]


send_submission(predictions, token, url=subm_url, submit_now=T)
    
