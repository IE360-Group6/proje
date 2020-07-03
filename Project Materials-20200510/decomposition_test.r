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

#######################

#class(data)

products = unique(data$product_content_id)

tayt = data[product_content_id == products[1]]
tayt = tayt[order(event_date)]
#visit_count
#sold_tayt=zoo(tayt[,list(sold_count, visit_count, basket_count, favored_count)],tayt$event_date)
#plot(sold_tayt)


disfirca = data[product_content_id == products[2]]
disfirca = disfirca[order(event_date)]
#visit_count & basket_count
#sold_disfirca=zoo(disfirca[,list(sold_count, visit_count, basket_count, favored_count)],disfirca$event_date)
#plot(sold_disfirca)


mont = data[product_content_id == products[3]]
mont = mont[order(event_date)]
#visit_count | favored_count
#sold_mont=zoo(mont[,list(sold_count, visit_count, basket_count, favored_count)],mont$event_date)
#plot(sold_mont)


mendil = data[product_content_id == products[4]]
mendil = mendil[order(event_date)]
#visit_count
#sold_mendil=zoo(mendil[,list(sold_count, visit_count, basket_count, favored_count)],mendil$event_date)
#plot(sold_mendil)


bikini = data[product_content_id == products[5]]
bikini = bikini[order(event_date)]
#favored_count??
#sold_bikini=zoo(bikini[,list(sold_count, visit_count, basket_count, favored_count)],bikini$event_date)
#plot(sold_bikini)


kulaklik = data[product_content_id == products[6]]
kulaklik = kulaklik[order(event_date)]
#visit_count & (?)favored_count
#sold_kulaklik=zoo(kulaklik[,list(sold_count, visit_count, basket_count, favored_count)],kulaklik$event_date)
#plot(sold_kulaklik)


supurge = data[product_content_id == products[7]]
supurge = supurge[order(event_date)]
#basket_count + visit_cpunt ??
#sold_supurge=zoo(supurge[,list(sold_count, visit_count, basket_count, favored_count)],supurge$event_date)
#plot(sold_supurge)


yuztemizleyici = data[product_content_id == products[8]]
yuztemizleyici = yuztemizleyici[order(event_date)]
#visit_count + basket_count ??
#sold_yuztemizleyici=zoo(yuztemizleyici[,list(sold_count, visit_count, basket_count, favored_count)],yuztemizleyici$event_date)
#plot(sold_yuztemizleyici)

#######################
#TAYT - Trend - No Seasonality

fc_tayt <- -1
for(i in 1:7){
    for_tayt = tayt[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_tayt = for_tayt[event_date > "2019-10-01" & event_date < as.Date(as.Date("2020-05-18") + i)]
    for_tayt[,time_index:=1:.N]
    #head(for_tayt)    
    #tail(for_tayt)
    
    trend_tayt = lm(sold_count~time_index, data = for_tayt)
    summary(trend_tayt)
    trend_tayt_component = trend_tayt$fitted
    for_tayt[,lr_trend:=trend_tayt_component]
    #matplot(for_tayt[,list(sold_count, lr_trend)], type = "l")
    
    for_tayt[,detr_sc:=sold_count-lr_trend]
    detr_for_tayt = for_tayt[,list(detr_sc, event_date, time_index, price, visit_count, favored_count, basket_count)]
    
    y_tayt = ts(detr_for_tayt$detr_sc, freq = 7)
    t_tayt = ts(for_tayt$lr_trend, freq = 7)

    fc_y_tayt = forecast(y_tayt,1)
    fc_t_tayt = forecast(t_tayt,1)
    
    fc_tayt <- c(fc_tayt, (fc_y_tayt$mean[1]+fc_t_tayt$mean[1]))
    
}

fc_tayt <- fc_tayt[-1]
fc_tayt

for_tayt_test = tayt[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_tayt_test = for_tayt_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_tayt <- mean(abs(fc_tayt-for_tayt_test))
mape_tayt <- 100*mean(abs((fc_tayt-for_tayt_test)/for_tayt_test))


#######################
#DISFIRCA - Trend - No Seasonality

fc_disfirca <- -1
for(i in 1:7){
    for_disfirca = disfirca[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_disfirca = for_disfirca[event_date > "2019-12-01" & event_date < as.Date(as.Date("2020-05-18") + i)]
    for_disfirca[,time_index:=1:.N]
    #head(for_disfirca)    
    
    trend_disfirca = lm(sold_count~time_index, data = for_disfirca)
    summary(trend_disfirca)
    trend_disfirca_component = trend_disfirca$fitted
    for_disfirca[,lr_trend:=trend_disfirca_component]
    #matplot(for_disfirca[,list(sold_count, lr_trend)], type = "l")
    
    for_disfirca[,detr_sc:=sold_count-lr_trend]
    detr_for_disfirca = for_disfirca[,list(detr_sc, event_date, time_index, price, visit_count, favored_count, basket_count)]
    
    y_disfirca = ts(detr_for_disfirca$detr_sc, freq = 7)
    #plot(y_disfirca)
    fc_y_disfirca = forecast(y_disfirca,1)
    t_disfirca = ts(for_disfirca$lr_trend, freq = 7)
    fc_t_disfirca = forecast(t_disfirca,1)
    
    fc_disfirca <- c(fc_disfirca, fc_y_disfirca$mean[1]+fc_t_disfirca$mean[1])
}

fc_disfirca <- fc_disfirca[-1]
fc_disfirca


for_disfirca_test = disfirca[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_disfirca_test = for_disfirca_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_disfirca <- mean(abs(fc_disfirca-for_disfirca_test))
mape_disfirca <- 100*mean(abs((fc_disfirca-for_disfirca_test)/for_disfirca_test))



#######################
#MONT - No Trend - No Seasonality

fc_mont <- -1
for(i in 1:7){
    for_mont = mont[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_mont = for_mont[event_date < as.Date(as.Date("2020-05-18") + i)]
    for_mont[,time_index:=1:.N]
    
    y_mont = ts(for_mont$sold_count, freq = 360)
    
    f_mont=fourier(y_mont, K=10)
    #str(f_mont)
    #matplot(f_mont[1:360,1:2],type='l')
    
    fit_mont=lm(y_mont~f_mont)
    summary(fit_mont)
    
    deseason_mont=y_mont-coef(fit_mont)[1]
    #plot(deseason_mont[1:(360*2)],type='l')
    
    fc_y_mont_deseas = forecast(deseason_mont,1)
    fc_mont <- c(fc_mont, fc_y_mont_deseas$mean[1])
}

fc_mont <- fc_mont[-1]
fc_mont

for_mont_test = mont[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_mont_test = for_mont_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

#gercek veriler 0 oldugu icin mape=inf
mae_mont <- mean(abs(fc_mont-for_mont_test))
mape_mont <- 100*mean(abs((fc_mont-for_mont_test)/for_mont_test))

#######################
#MENDIL - No Trend - No Seasonality

fc_mendil <- -1
for(i in 1:7){
    for_mendil = mendil[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_mendil = for_mendil[event_date > "2019-09-10" & event_date < as.Date(as.Date("2020-05-18") + i)]
    for_mendil[,time_index:=1:.N]
    #tail(for_mendil)
    y_mendil = ts(for_mendil$sold_count, freq = 7)
    
    fc_y_mendil <- forecast(y_mendil,1)
    fc_mendil <- c(fc_mendil, fc_y_mendil$mean[1])
}

fc_mendil <- fc_mendil[-1]
fc_mendil


for_mendil_test = mendil[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_mendil_test = for_mendil_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_mendil <- mean(abs(fc_mendil-for_mendil_test))
mape_mendil <- 100*mean(abs((fc_mendil-for_mendil_test)/for_mendil_test))

#######################
#BIKINI - No Trend - Seasonality

fc_bikini <- -1
for(i in 1:7){
    for_bikini = bikini[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    for_bikini = for_bikini[event_date < as.Date(as.Date("2020-05-18") + i)]
    for_bikini[,time_index:=1:.N]
    tail(for_bikini)
    
    y_bikini = ts(for_bikini$sold_count, freq = 360)
    
    f_bikini=fourier(y_bikini, K=10)
    #str(f_bikini)
    #matplot(f_bikini[1:360,1:2],type='l')
    
    fit_bikini=lm(y_bikini~f_bikini)
    summary(fit_bikini)
    
    deseason_bikini=y_bikini-coef(fit_bikini)[1]
    #plot(deseason_bikini[1:(360*2)],type='l')
    
    fc_y_bikini_deseas = forecast(deseason_bikini,1)
    
    fc_bikini = c(fc_bikini, fc_y_bikini_deseas$mean[1])
}

fc_bikini <- fc_bikini[-1]
fc_bikini

for_bikini_test = bikini[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_bikini_test = for_bikini_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

#gercek veriler 0 oldugu icin mape=inf
mae_bikini <- mean(abs(fc_bikini-for_bikini_test))
mape_bikini <- 100*mean(abs((fc_bikini-for_bikini_test)/for_bikini_test))

#######################
#KULAKLIK - Trend - No Seasonality

fc_kulaklik <- -1
for(i in 1:7){
    for_kulaklik = kulaklik[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_kulaklik = for_kulaklik[event_date > "2019-06-20" & event_date < as.Date(as.Date("2020-05-18") + i)]
    for_kulaklik[,time_index:=1:.N]
    #tail(for_kulaklik)
    
    trend_kulaklik = lm(sold_count~time_index, data = for_kulaklik)
    summary(trend_kulaklik)
    trend_kulaklik_component = trend_kulaklik$fitted
    for_kulaklik[,lr_trend:=trend_kulaklik_component]
    #matplot(for_kulaklik[,list(sold_count, lr_trend)], type = "l")
    
    for_kulaklik[,detr_sc:=sold_count-lr_trend]
    detr_for_kulaklik = for_kulaklik[,list(detr_sc, event_date, time_index, price, visit_count, favored_count, basket_count)]
    
    y_kulaklik = ts(detr_for_kulaklik$detr_sc, freq = 7)
    #plot(y_kulaklik)
    fc_y_kulaklik = forecast(y_kulaklik,1)
    t_kulaklik = ts(for_kulaklik$lr_trend, freq = 7)
    fc_t_kulaklik = forecast(t_kulaklik,1)
    
    fc_kulaklik = c(fc_kulaklik, fc_y_kulaklik$mean[1] + fc_t_kulaklik$mean[1])
}

fc_kulaklik <- fc_kulaklik[-1]
fc_kulaklik


for_kulaklik_test = kulaklik[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_kulaklik_test = for_kulaklik_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_kulaklik <- mean(abs(fc_kulaklik-for_kulaklik_test))
mape_kulaklik <- 100*mean(abs((fc_kulaklik-for_kulaklik_test)/for_kulaklik_test))


#######################
#SUPURGE - Trend - No Seasonality

fc_supurge <- -1
for(i in 1:7){
    for_supurge = supurge[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    
    for_supurge = for_supurge[event_date > "2019-07-25" & event_date < as.Date(as.Date("2020-05-18") + i)]
    for_supurge[,time_index:=1:.N]
    #tail(for_supurge)
    
    trend_supurge = lm(sold_count~time_index, data = for_supurge)
    summary(trend_supurge)
    trend_supurge_component = trend_supurge$fitted
    for_supurge[,lr_trend:=trend_supurge_component]
    #matplot(for_supurge[,list(sold_count, lr_trend)], type = "l")
    
    for_supurge[,detr_sc:=sold_count-lr_trend]
    detr_for_supurge = for_supurge[,list(detr_sc, event_date, time_index, price, visit_count, favored_count, basket_count)]
    
    y_supurge = ts(detr_for_supurge$detr_sc, freq = 7)
    #plot(y_supurge)
    fc_y_supurge = forecast(y_supurge,1)
    t_supurge = ts(for_supurge$lr_trend, freq = 7)
    fc_t_supurge = forecast(t_supurge,1)
    
    fc_supurge = c(fc_supurge, fc_y_supurge$mean[1] + fc_t_supurge$mean[1])
}

fc_supurge <- fc_supurge[-1]
fc_supurge


for_supurge_test = supurge[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_supurge_test = for_supurge_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_supurge <- mean(abs(fc_supurge-for_supurge_test))
mape_supurge <- 100*mean(abs((fc_supurge-for_supurge_test)/for_supurge_test))


#######################
#YUZTEMIZLEYICI - Trend - No Seasonality

fc_yuztemizleyici <- -1
for(i in 1:7){
    for_yuztemizleyici = yuztemizleyici[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
    for_yuztemizleyici = for_yuztemizleyici[event_date < as.Date(as.Date("2020-05-18") + i)]
    for_yuztemizleyici[,time_index:=1:.N]
    #tail(for_yuztemizleyici)
    
    trend_yuztemizleyici = lm(sold_count~time_index, data = for_yuztemizleyici)
    summary(trend_yuztemizleyici)
    trend_yuztemizleyici_component = trend_yuztemizleyici$fitted
    for_yuztemizleyici[,lr_trend:=trend_yuztemizleyici_component]
    #matplot(for_yuztemizleyici[,list(sold_count, lr_trend)], type = "l")
    
    for_yuztemizleyici[,detr_sc:=sold_count-lr_trend]
    detr_for_yuztemizleyici = for_yuztemizleyici[,list(detr_sc, event_date, time_index, price, visit_count, favored_count, basket_count)]
    
    y_yuztemizleyici = ts(detr_for_yuztemizleyici$detr_sc, freq = 7)
    #plot(y_yuztemizleyici)
    fc_y_yuztemizleyici = forecast(y_yuztemizleyici,1)
    t_yuztemizleyici = ts(for_yuztemizleyici$lr_trend, freq = 7)
    fc_t_yuztemizleyici = forecast(t_yuztemizleyici, 1)
    
    fc_yuztemizleyici = c(fc_yuztemizleyici, fc_y_yuztemizleyici$mean[1] + fc_t_yuztemizleyici$mean[1])
}

fc_yuztemizleyici <- fc_yuztemizleyici[-1]
fc_yuztemizleyici


for_yuztemizleyici_test = yuztemizleyici[,list(sold_count, event_date, price, visit_count, favored_count, basket_count)]
for_yuztemizleyici_test = for_yuztemizleyici_test[event_date >= "2020-05-20" & event_date < as.Date(as.Date("2020-05-20")+7)]$sold_count

mae_yuztemizleyici <- mean(abs(fc_yuztemizleyici-for_yuztemizleyici_test))
mape_yuztemizleyici <- 100*mean(abs((fc_yuztemizleyici-for_yuztemizleyici_test)/for_yuztemizleyici_test))


#######################


predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=fc]

send_submission(predictions, token, url=subm_url, submit_now=F)

