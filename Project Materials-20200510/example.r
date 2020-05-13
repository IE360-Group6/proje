# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

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
library(forecast)
library(ggplot2)
library(zoo)
library(xts)

?subset
str(data)
data$event_date=as.Date(data$event_date)
filtered_data<-data[product_content_id == 32939029]
filtered_data<-filtered_data[event_date >= "2019-04-30"]
filtered_data[,dates:=as.Date(event_date)]
disfircasi<-xts(filtered_data$sold_count,order.by = filtered_data$dates)
disfircasi2<-xts(filtered_data$category_sold,order.by = filtered_data$dates)
auto.arima(disfircasi)
fit_disfircasi<-auto.arima(disfircasi,xreg = disfircasi2)
fc<-forecast(fit_disfircasi,xreg = disfircasi2)
autoplot(fc)

ggplot(filtered_data,aes(x=filtered_data$dates,y=filtered_data$sold_count))+geom_point()


predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)
    
