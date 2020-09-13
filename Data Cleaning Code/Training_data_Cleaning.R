library(stringr)
library(dplyr)

airbnb_train_x_description<-read.csv("airbnb_train_x_des.csv",stringsAsFactors = FALSE)
airbnb_train_y<-read.csv("airbnb_train_y.csv")

airbnb_cleaned=airbnb_train_x_description[,-which(names(airbnb_train_x_description)%in%c("X","market","description","Perc_Contribution","Dominant_Topic","Unnamed: 0","level_0","Unnamed: 0.1","index","access","city","city_name","country","country_code","host_acceptance_rate","host_listings_count","host_location","host_name","host_neighbourhood","host_response_rate","host_since","jurisdiction_names","latitude","longitude" , "name","neighborhood_overview","neighbourhood" , "notes"  , "smart_location"  , "space" , "square_feet","state", "street",  "summary"  , "transit" , "zipcode","experiences_offered"))]

##'host_is_superhost', 'host_has_profile_pic', 'host_identity_verified'
airbnb_cleaned['host_is_superhost']=ifelse(airbnb_cleaned['host_is_superhost']=='t',1,0)
airbnb_cleaned['host_has_profile_pic']=ifelse(airbnb_cleaned['host_has_profile_pic']=='t',1,0)
airbnb_cleaned['host_identity_verified']=ifelse(airbnb_cleaned[,'host_identity_verified']=='t',1,0)
##'house_rules'
airbnb_cleaned['house_rules']=ifelse(airbnb_cleaned['house_rules']=='',0,1)
##'interaction'
airbnb_cleaned['interaction']=ifelse(airbnb_cleaned['interaction']=='',0,1)
##'bed_type'---No change
##'cancellation_policy'---No change
##'number_of_amenities'
airbnb_cleaned$number_of_amenities = str_count(airbnb_cleaned$amenities, '"')/2
##'host_verifications'
airbnb_cleaned$number_of_host_verifications = str_count(airbnb_cleaned$host_verifications, "'")/2

#"instant_bookable","is_business_travel_ready","is_location_exact",'license','maximum_nights','minimum_nights','monthly_price'
airbnb_cleaned["instant_bookable"] = ifelse(airbnb_cleaned["instant_bookable"]=='t',1,0)
airbnb_cleaned["is_business_travel_ready"] = ifelse(airbnb_cleaned['is_business_travel_ready']=='','Unknown',ifelse(airbnb_cleaned["is_business_travel_ready"]=='t',1,0))
airbnb_cleaned["is_location_exact"] = ifelse(airbnb_cleaned['is_location_exact']=='','Unknown',ifelse(airbnb_cleaned["is_location_exact"]=='t',1,0))
airbnb_cleaned['license'] = ifelse(airbnb_cleaned['license']=='',0,1)
airbnb_cleaned['monthly_price'] = ifelse(airbnb_cleaned['monthly_price']=='',0,1)
airbnb_cleaned$maximum_nights[is.na(airbnb_cleaned$maximum_nights)]=1125
airbnb_cleaned$minimum_nights[is.na(airbnb_cleaned$minimum_nights)]=2
#price get rid of "$" and change into numeric type
airbnb_cleaned$price=str_replace(airbnb_cleaned$price,"[,]","")
airbnb_cleaned$price=as.numeric(word(airbnb_cleaned$price,2,sep=fixed('$')))
#require_guest_phone_verification
airbnb_cleaned$require_guest_phone_verification=ifelse(airbnb_cleaned$require_guest_phone_verification=='t',1,0)
#require_guest_profile_picture
airbnb_cleaned$require_guest_profile_picture=ifelse(airbnb_cleaned$require_guest_profile_picture=='t',1,0)
#requires_license
airbnb_cleaned$requires_license=ifelse(airbnb_cleaned$requires_license=='t',1,0)
#security_deposit
airbnb_cleaned$security_deposit=str_replace(airbnb_cleaned$security_deposit,"[,]","")
airbnb_cleaned$security_deposit=as.numeric(word(airbnb_cleaned$security_deposit,2,sep=fixed('$')))
airbnb_cleaned$security_deposit[is.na(airbnb_cleaned$security_deposit)]=0
#weekly_price
airbnb_cleaned$weekly_price=ifelse(airbnb_cleaned$weekly_price=='',0,1)
#host_response_time
airbnb_cleaned$host_response_time[airbnb_cleaned$host_response_time=='']='Unknown'

## deal with column "cleaning fee"
airbnb_cleaned$cleaning_fee[airbnb_cleaned$cleaning_fee=='']=0
airbnb_cleaned$cleaning_fee=str_replace(airbnb_cleaned$cleaning_fee,"[,]","")
airbnb_cleaned$cleaning_fee=as.numeric(word(airbnb_cleaned$cleaning_fee,2,sep=fixed('$')))
airbnb_cleaned$cleaning_fee[is.na(airbnb_cleaned$cleaning_fee)]=0

## first_review
now_time=Sys.Date()
airbnb_cleaned$first_review_days=word(as.Date(now_time) - as.Date(airbnb_cleaned$first_review))
airbnb_cleaned$first_review_days = as.numeric(airbnb_cleaned$first_review_days)

##host_about
airbnb_cleaned$length_host_about=str_count(airbnb_cleaned$host_about, " ")+1
airbnb_cleaned$host_about_num=ifelse(airbnb_cleaned$length_host_about>20,1,0)


#extra_people
airbnb_cleaned$extra_people=str_replace(airbnb_cleaned$extra_people,"[,]","")
airbnb_cleaned$extra_people=as.numeric(word(airbnb_cleaned$extra_people,2,sep=fixed('$')))
airbnb_cleaned$cate_extra_people=ifelse(airbnb_cleaned$extra_people==0,1,ifelse(airbnb_cleaned$extra_people<=30,2,ifelse(airbnb_cleaned$extra_people<=60,3,ifelse(airbnb_cleaned$extra_people<=100,4,5))))

#beds
airbnb_cleaned$beds=as.numeric(airbnb_cleaned$beds)
airbnb_cleaned$beds[is.na(airbnb_cleaned$beds)]=0
#bathrooms
airbnb_cleaned$bathrooms=as.numeric(airbnb_cleaned$bathrooms)
airbnb_cleaned$bathrooms[is.na(airbnb_cleaned$bathrooms)]=0
#bedrooms
airbnb_cleaned$bedrooms=as.numeric(airbnb_cleaned$bedrooms)
airbnb_cleaned$bedrooms[is.na(airbnb_cleaned$bedrooms)]=0
#property_type
airbnb_cleaned$property_type[airbnb_cleaned$property_type=='']='Apartment'

##Topic
airbnb_cleaned$Topic_Keywords[airbnb_cleaned$Topic_Keywords=='']='Unknown'

#host_total_listings_counts
airbnb_cleaned$host_total_listings_count=as.numeric(airbnb_cleaned$host_total_listings_count)


##Drop Duplicate Column
airbnb_cleaned=airbnb_cleaned[ , -which(names(airbnb_cleaned) %in% c('amenities','extra_people','first_review','length_host_about','host_about','host_verifications'))]


##Bind dataset
airbnb_cleaned=cbind(airbnb_cleaned,airbnb_train_y[,3])
colnames(airbnb_cleaned)[41]='high_booking_rate'

##Drop NA
airbnb_cleaned=na.omit(airbnb_cleaned)

summary(is.na(airbnb_cleaned))

##Write csv
write.csv(airbnb_cleaned,file='airbnb_train_final.csv')
