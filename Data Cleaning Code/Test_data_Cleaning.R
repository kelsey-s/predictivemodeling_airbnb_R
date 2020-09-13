library(stringr)
library(dplyr)

airbnb_test_x_des=read.csv('airbnb_test_des.csv',stringsAsFactors =FALSE)
airbnb_cleaned=airbnb_test_x_des[,-which(names(airbnb_test_x_des) %in% c("X","access",'description','experiences_offered','description',"city","city_name","country","country_code","host_acceptance_rate","host_listings_count","host_location","host_name","host_neighbourhood","host_response_rate","host_since","jurisdiction_names","latitude","longitude" , "name","neighborhood_overview","neighbourhood" , "notes"  , "smart_location"  , "space" , "square_feet","state", "street",  "summary"  , "transit" , "zipcode"))]

##'host_is_superhost', 'host_has_profile_pic', 'host_identity_verified'
airbnb_cleaned[which(airbnb_cleaned$host_is_superhost==''),'host_is_superhost']='f'
airbnb_cleaned['host_is_superhost']=ifelse(airbnb_cleaned['host_is_superhost']=='t',1,0)

airbnb_cleaned[which(airbnb_cleaned$host_has_profile_pic==''),'host_has_profile_pic']='f'
airbnb_cleaned['host_has_profile_pic']=ifelse(airbnb_cleaned['host_has_profile_pic']=='t',1,0)

airbnb_cleaned[which(airbnb_cleaned$host_identity_verified==''),'host_identity_verified']='f'
airbnb_cleaned['host_identity_verified']=ifelse(airbnb_cleaned[,'host_identity_verified']=='t',1,0)

##'house_rules'
airbnb_cleaned['house_rules']=ifelse(airbnb_cleaned$house_rules=='',0,1)

##'interaction'
airbnb_cleaned['interaction']=ifelse(airbnb_cleaned$interaction=='',0,1)

##'bed_type'
airbnb_cleaned[which(airbnb_cleaned$bed_type==''),'bed_type']='Real Bed'

##'cancellation_policy'
airbnb_cleaned[which(airbnb_cleaned$cancellation_policy==''),'cancellation_policy']='strict'

##'number_of_amenities'
airbnb_cleaned$number_of_amenities = str_count(airbnb_cleaned$amenities, '"')/2
airbnb_cleaned['number_of_amenities'][is.na(airbnb_cleaned['number_of_amenities'])]=9

##'host_verifications'
airbnb_cleaned$number_of_host_verifications = str_count(airbnb_cleaned$host_verifications, "'")/2
airbnb_cleaned['number_of_host_verifications'][is.na(airbnb_cleaned['number_of_host_verifications'])]=0

#"instant_bookable",,
airbnb_cleaned[which(airbnb_cleaned$instant_bookable==''),'instant_bookable']='f'
airbnb_cleaned["instant_bookable"] = ifelse(airbnb_cleaned["instant_bookable"]=='t',1,0)

#"is_business_travel_ready"
airbnb_cleaned["is_business_travel_ready"] = ifelse(airbnb_cleaned$is_business_travel_ready=='','Unknown',ifelse(airbnb_cleaned["is_business_travel_ready"]=='t',1,0))

#is_location_exact
airbnb_cleaned["is_location_exact"] = ifelse(airbnb_cleaned$is_location_exact=='','Unknown',ifelse(airbnb_cleaned["is_location_exact"]=='t',1,0))

#'license'
airbnb_cleaned['license'] = ifelse(airbnb_cleaned$license=='',0,1)

#'monthly_price'
airbnb_cleaned['monthly_price'] = ifelse(airbnb_cleaned['monthly_price']=='',0,1)

#price get rid of "$" and change into numeric type
##convert na to 0 ---not sure
airbnb_cleaned$price=str_replace(airbnb_cleaned$price,"[,]","")
airbnb_cleaned$price=as.numeric(word(airbnb_cleaned$price,2,sep=fixed('$')))
airbnb_cleaned[is.na(airbnb_cleaned['price']),'price']=0

#require_guest_phone_verification
airbnb_cleaned[airbnb_cleaned['require_guest_phone_verification']=='','require_guest_phone_verification']='f'
airbnb_cleaned$require_guest_phone_verification=ifelse(airbnb_cleaned$require_guest_phone_verification=='f',0,1)

#require_guest_profile_picture
airbnb_cleaned[airbnb_cleaned['require_guest_profile_picture']=='','require_guest_profile_picture']='f'
airbnb_cleaned$require_guest_profile_picture=ifelse(airbnb_cleaned$require_guest_profile_picture=='f',0,1)

#requires_license
airbnb_cleaned[airbnb_cleaned['requires_license']=='','requires_license']='f'
airbnb_cleaned$requires_license=ifelse(airbnb_cleaned$requires_license=='f',0,1)


#security_deposit
airbnb_cleaned$security_deposit=str_replace(airbnb_cleaned$security_deposit,"[,]","")
airbnb_cleaned$security_deposit=as.numeric(word(airbnb_cleaned$security_deposit,2,sep=fixed('$')))
airbnb_cleaned$security_deposit[is.na(airbnb_cleaned$security_deposit)]=0

#weekly_price
airbnb_cleaned$weekly_price=ifelse(airbnb_cleaned$weekly_price=='',0,1)

#host_response_time
airbnb_cleaned$host_response_time[airbnb_cleaned$host_response_time=='']='Unknown'

## deal with column "cleaning fee"
airbnb_cleaned$cleaning_fee=str_replace(airbnb_cleaned$cleaning_fee,"[,]","")
airbnb_cleaned$cleaning_fee=as.numeric(word(airbnb_cleaned$cleaning_fee,2,sep=fixed('$')))
airbnb_cleaned$cleaning_fee[is.na(airbnb_cleaned$cleaning_fee)]=0

## first_review
now_time=Sys.Date()
airbnb_cleaned$first_review_days=word(as.Date(now_time) - as.Date(airbnb_cleaned$first_review,'%m/%d/%Y'))
airbnb_cleaned$first_review_days = as.numeric(airbnb_cleaned$first_review_days)
airbnb_cleaned$first_review_days[is.na(airbnb_cleaned$first_review_days)]=0

##host_about
airbnb_cleaned$length_host_about=str_count(airbnb_cleaned$host_about, " ")+1
airbnb_cleaned$host_about_num=ifelse(airbnb_cleaned$length_host_about>20,1,0)
airbnb_cleaned[which(is.na(airbnb_cleaned$host_about_num)),"host_about_num" ]=0

#extra_people
airbnb_cleaned$extra_people=str_replace(airbnb_cleaned$extra_people,"[,]","")
airbnb_cleaned$extra_people=as.numeric(word(airbnb_cleaned$extra_people,2,sep=fixed('$')))
airbnb_cleaned$cate_extra_people=ifelse(airbnb_cleaned$extra_people==0,1,ifelse(airbnb_cleaned$extra_people<=30,2,ifelse(airbnb_cleaned$extra_people<=60,3,ifelse(airbnb_cleaned$extra_people<=100,4,5))))
airbnb_cleaned$cate_extra_people[is.na(airbnb_cleaned$cate_extra_people)]=1

#Beds, Bathroom, bedrooms
airbnb_cleaned$bathrooms=as.numeric(airbnb_cleaned$bathrooms)
airbnb_cleaned$bathrooms[is.na(airbnb_cleaned$bathrooms)]=1

airbnb_cleaned$bedrooms=as.numeric(airbnb_cleaned$bedrooms)
airbnb_cleaned$bedrooms[is.na(airbnb_cleaned$bedrooms)]=1

airbnb_cleaned$beds[is.na(airbnb_cleaned$beds)]=1

##Maximum&minimum
airbnb_cleaned$maximum_nights[is.na(airbnb_cleaned$maximum_nights)]=1125
airbnb_cleaned$minimum_nights[is.na(airbnb_cleaned$minimum_nights)]=2

#accommodates 
airbnb_cleaned$accommodates[is.na(airbnb_cleaned$accommodates)]=3

#availability_30 availability_365 availability_60 availability_90
airbnb_cleaned$availability_30[is.na(airbnb_cleaned$availability_30)]=11
airbnb_cleaned$availability_365[is.na(airbnb_cleaned$availability_365)]=168
airbnb_cleaned$availability_60[is.na(airbnb_cleaned$availability_60)]=25
airbnb_cleaned$availability_90[is.na(airbnb_cleaned$availability_90)]=41

#guests_included 
airbnb_cleaned$guests_included[is.na(airbnb_cleaned$guests_included)]=2

#host_total_listings_count
airbnb_cleaned$host_total_listings_count[is.na(airbnb_cleaned$host_total_listings_count)]=10

#property_type&room_type
airbnb_cleaned$property_type[airbnb_cleaned$property_type=='']='Apartment'
airbnb_cleaned$room_type[airbnb_cleaned$room_type=='']='Entire home/apt'

#Topic_Keywords
airbnb_cleaned$Topic_Keywords[airbnb_cleaned$Topic_Keywords=='']='place, family, couple, traveler, business, perfect, love, solo, adventurer, friend'

##Drop Duplicate Column
airbnb_cleaned=airbnb_cleaned[ , -which(names(airbnb_cleaned) %in% c('amenities','extra_people','market','extra_people','first_review','host_about','length_host_about','host_verifications'))]

summary(is.na(airbnb_cleaned))
##Write csv
write.csv(airbnb_cleaned,file='airbnb_test_final.csv')
