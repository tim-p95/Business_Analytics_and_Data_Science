#Tim Peschenz
#Matrikelnr.: 569405
#E-Mail: tim.peschenz@gmail.com

#working directory
wd = "C:/Users/timpe_000/Documents/02 Master/Semester 1/04 Business Analytics and Data Science/Kaggle Competition"
setwd(wd)

#read data
library(readr)
known = data.frame(read_csv("BADS_WS1819_known.csv"))
unknown = data.frame(read_csv("BADS_WS1819_unknown.csv"))



##order_item_id
known1 = data.frame("order_item_id" = seq(1:100000))
unknown1 = data.frame("order_item_id" = seq(1:50000))

##return
known1$return = as.numeric(known$return)


##order_date
known1$order_date = as.Date(known$order_date)
unknown1$order_date = as.Date(unknown$order_date)


#extracting order_day
library(dplyr)
known1$order_day = weekdays(as.Date(known1$order_date))
order_weekday = data.frame(table(known1$order_day))
colnames(order_weekday) = c("weekday", "freq")
order_weekday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_day), FUN = sum)[,2]
order_weekday$return_prob = order_weekday$returns/order_weekday$freq

known1$order_day = NULL
rm(order_weekday)


#extracting order_month
known1$order_month = as.factor(format(known1$order_date, "%m"))
order_month = data.frame(table(known1$order_month))
colnames(order_month) = c("month", "freq")
order_month$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_month), FUN = sum)[,2]
order_month$return_prob = order_month$returns/order_month$freq

rm(order_month)
unknown1$order_month = as.factor(format(unknown1$order_date, "%m"))


#extracting order_quarter
known1$order_quarter = as.factor(substr(quarters(known1$order_date), 2, 2))
levels(known1$order_quarter)
order_quarter = data.frame(table(known1$order_quarter))
colnames(order_quarter) = c("quarter", "freq")
order_quarter$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_quarter), FUN = sum)[,2]
order_quarter$return_prob = order_quarter$returns/order_quarter$freq

rm(order_quarter)
unknown1$order_quarter = as.factor(substr(quarters(unknown1$order_date), 2, 2))


#extracting order_year
known1$order_year = as.factor(format(known1$order_date, "%Y"))
order_year = data.frame(table(known1$order_year))
colnames(order_year) = c("year", "freq")
order_year$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_year), FUN = sum)[,2]
order_year$return_prob = order_year$returns/order_year$freq

rm(order_year)
unknown1$order_year = as.factor(format(unknown1$order_date, "%Y"))


#extracting month day of ordering (numeric)
known1$order_monthday = as.factor(format(known1$order_date, "%d"))
order_monthday = data.frame(table(known1$order_monthday))
colnames(order_monthday) = c("monthday", "freq")
order_monthday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_monthday), FUN = sum)[,2]
order_monthday$return_prob = order_monthday$returns/order_monthday$freq

cor(as.numeric(known1$order_monthday), known1$return)

known1$order_monthday = NULL
rm(order_monthday)


#order_date as factor
known1$order_factor = as.factor(known1$order_date)
order_factor = data.frame(table(known1$order_factor))
colnames(order_factor) = c("order_factor", "freq")
order_factor$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$order_factor), FUN = sum)[,2]
order_factor$return_prob = order_factor$returns/order_factor$freq

known1$order_factor = NULL
rm(order_factor)


##delivery_date
known1$delivery_date = known$delivery_date
unknown1$delivery_date = unknown$delivery_date


#delivery_date_missing
known1$delivery_date_missing = 0
known1$delivery_date_missing[which(is.na(known1$delivery_date))] = 1

unknown1$delivery_date_missing = 0
unknown1$delivery_date_missing[which(is.na(unknown1$delivery_date))] = 1


#delivery_time
known1$delivery_time = unclass(known1$delivery_date) - unclass(known1$order_date)
#known1$delivery_time[which(is.na(known1$delivery_time))] = median(known1$delivery_time, na.rm = TRUE)
median(known1$delivery_time, na.rm = TRUE)
mean(known1$delivery_time, na.rm = TRUE)

knownc = known1[complete.cases(known1), ]
cor(knownc$return, knownc$delivery_time)
rm(knownc)

sum(known1$return[which(known1$delivery_time < 0)])/length(known1$return[which(known1$delivery_time < 0)])

known1$delivery_time_negative = 0
known1$delivery_time_negative[which(known1$delivery_time < 0)] = 1
known1$delivery_time[which(known1$delivery_time < 0)] = NA

sum(is.na(known1$delivery_time))
sum(known1$delivery_date_missing)

median(known1$delivery_time, na.rm = TRUE)
mean(known1$delivery_time, na.rm = TRUE)

cor(known1$delivery_time[which(is.na(known1$delivery_time) == FALSE)],known1$return[which(is.na(known1$delivery_time) == FALSE)])

#for unknown
unknown1$delivery_time = unclass(unknown1$delivery_date) - unclass(unknown1$order_date)

unknown1$delivery_time_negative = 0
unknown1$delivery_time_negative[which(unknown1$delivery_time < 0)] = 1
unknown1$delivery_time[which(unknown1$delivery_time < 0)] = NA

sum(is.na(unknown1$delivery_time))
sum(unknown1$delivery_date_missing)

median(unknown1$delivery_time, na.rm = TRUE)
mean(unknown1$delivery_time, na.rm = TRUE)


#extracting delivery_day
library(dplyr)
known1$delivery_day = weekdays(as.Date(known1$delivery_date))
known1$delivery_day[which(is.na(known1$delivery_day))] = "missing"
known1$delivery_day = as.factor(known1$delivery_day)

delivery_weekday = data.frame(table(known1$delivery_day))
colnames(delivery_weekday) = c("weekday", "freq")
delivery_weekday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_day), FUN = sum)[,2]
#delivery_weekday = delivery_weekday[-3, ]
delivery_weekday$return_prob = delivery_weekday$returns/delivery_weekday$freq

known1$delivery_day = NULL
rm(delivery_weekday)


#extracting delivery_month
known1$delivery_month = format(known1$delivery_date, "%m")
known1$delivery_month[which(is.na(known1$delivery_month))] = "missing"
known1$delivery_month = as.factor(known1$delivery_month)

delivery_month = data.frame(table(known1$delivery_month))
colnames(delivery_month) = c("month", "freq")
delivery_month$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_month), FUN = sum)[,2]
delivery_month$return_prob = delivery_month$returns/delivery_month$freq

rm(delivery_month)
unknown1$delivery_month = format(unknown1$delivery_date, "%m")
unknown1$delivery_month[which(is.na(unknown1$delivery_month))] = "missing"
unknown1$delivery_month = as.factor(unknown1$delivery_month)


#extracting delivery_quarter
known1$delivery_quarter = substr(quarters(known1$delivery_date), 2, 2)
known1$delivery_quarter[which(known1$delivery_quarter == "N")] = "missing"
known1$delivery_quarter = as.factor(known1$delivery_quarter)

levels(known1$delivery_quarter)
delivery_quarter = data.frame(table(known1$delivery_quarter))
colnames(delivery_quarter) = c("quarter", "freq")
delivery_quarter$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_quarter), FUN = sum)[,2]
delivery_quarter$return_prob = delivery_quarter$returns/delivery_quarter$freq

rm(delivery_quarter)
unknown1$delivery_quarter = substr(quarters(unknown1$delivery_date), 2, 2)
unknown1$delivery_quarter[which(unknown1$delivery_quarter == "N")] = "missing"
unknown1$delivery_quarter = as.factor(unknown1$delivery_quarter)


#extracting delivery_year
known1$delivery_year = format(known1$delivery_date, "%Y")
known1$delivery_year[which(is.na(known1$delivery_year))] = "missing"
known1$delivery_year = as.factor(known1$delivery_year)

delivery_year = data.frame(table(known1$delivery_year))
colnames(delivery_year) = c("year", "freq")
delivery_year$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_year), FUN = sum)[,2]
delivery_year$return_prob = delivery_year$returns/delivery_year$freq

rm(delivery_year)
unknown1$delivery_year = format(unknown1$delivery_date, "%Y")
unknown1$delivery_year[which(is.na(unknown1$delivery_year))] = "missing"
unknown1$delivery_year = as.factor(unknown1$delivery_year)


#extracting month day of delivery (numeric)
known1$delivery_monthday = format(known1$delivery_date, "%d")
known1$delivery_monthday[which(is.na(known1$delivery_monthday))] = "missing"
known1$delivery_monthday = as.factor(known1$delivery_monthday)

delivery_monthday = data.frame(table(known1$delivery_monthday))
colnames(delivery_monthday) = c("monthday", "freq")
delivery_monthday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_monthday), FUN = sum)[,2]
delivery_monthday$return_prob = delivery_monthday$returns/delivery_monthday$freq

cor(as.numeric(known1$delivery_monthday), known1$return)

knownc = known1[-which(known1$delivery_monthday == "missing"), ]
cor(knownc$return, as.numeric(knownc$delivery_monthday))
rm(knownc)

rm(delivery_monthday)
known1$delivery_monthday = NULL


#delivery_dates as factor
known1$delivery_factor = as.factor(known1$delivery_date)
levels(known1$delivery_factor) = c(as.character(levels(known1$delivery_factor)), "missing")
known1$delivery_factor[which(is.na(known1$delivery_factor))] = "missing"

delivery_factor = data.frame(table(known1$delivery_factor))
colnames(delivery_factor) = c("delivery_date", "freq")
delivery_factor$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$delivery_factor), FUN = sum)[,2]
delivery_factor$return_prob = delivery_factor$returns/delivery_factor$freq

known1$delivery_factor = NULL
rm(delivery_factor)


## item_id
library(dplyr)
#id as integer
known1$item_id = as.integer(known$item_id)
unknown1$item_id = as.integer(unknown$item_id)

cor(known1$item_id, known1$return)


# frequency of item_id
i_id1 = known1 %>% add_count(item_id)
known1$item_id_freq = i_id1$n
rm(i_id1)

i_id2 = unknown1 %>% add_count(item_id)
unknown1$item_id_freq = i_id2$n
rm(i_id2)



## item_size
library(dplyr)
known1$item_size = as.factor(known$item_size)
unknown1$item_size = as.factor(unknown$item_size)

item_size = data.frame(table(known1$item_size))
colnames(item_size) = c("item_size", "freq")
item_size$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$item_size), FUN = sum)[,2]
item_size$return_prob = item_size$returns/item_size$freq

item_size_unknown = data.frame(table(unknown1$item_size))

#comparing levels
size_levels_k = data.frame("levels" = as.character(levels(known1$item_size)))
size_levels_u = data.frame("levels" = as.character(levels(unknown1$item_size)))

size_levels_k$in_unknown = size_levels_k$levels %in% size_levels_u$levels
size_levels_u$in_known = size_levels_u$levels %in% size_levels_k$levels

#change levels to other: c("105", "2+", "2932", "58", "76", "80", "95", "3132", "4034", "49") sum: 23 observations
known1$item_size = as.character(known1$item_size)
known1$item_size[known1$item_size %in% c("105", "2+", "2932", "58", "76", "80", "95", "3132", "4034", "49")] = "other"
known1$item_size = as.factor(known1$item_size)

unknown1$item_size = as.character(unknown1$item_size)
unknown1$item_size[unknown1$item_size %in% c("105", "2+", "2932", "58", "76", "80", "95", "3132", "4034", "49")] = "other"
unknown1$item_size = as.factor(unknown1$item_size)

#check result
size_levels_k = data.frame("levels" = as.character(levels(known1$item_size)))
size_levels_u = data.frame("levels" = as.character(levels(unknown1$item_size)))

size_levels_k$in_unknown = size_levels_k$levels %in% size_levels_u$levels
size_levels_u$in_known = size_levels_u$levels %in% size_levels_k$levels

rm(item_size); rm(item_size_unknown); rm(size_levels_k); rm(size_levels_u)


#combine same size with different names
library(dplyr)

old_sizes <- c('m','M','l','L','xl','XL','43+','37+','36+','XXL','xxl','s','S','XS','xs','XXXL','xxxl','8+','8','6+','6','9+','9','4','4+','2','2+','7','7+','5','5+','39+','38+','10','10+','11','11+','40+','21','20','42+','41+','12+','3+','45+','44+','46+')
new_sizes <- c(38,38,42,42,46,46,43,37,36,50,50,36,36,34,34,54,54,36,36,34,34,38,38,32,32,30,30,36,36,33,33,39,38,38,38,39,39,40,48,48,42,41,12,3,45,44,46)

size_df = data.frame(unique(rbind(data.frame("size" = levels(as.factor(known1$item_size))), data.frame("size" = levels(as.factor(unknown1$item_size))))))
size_df$numeric = as.numeric(as.character(size_df$size))

size_df$new_size = size_df$size
size_df$id_num = seq(1:nrow(size_df))
filtered = data.frame()

for( i in 1:length(old_sizes)) {
  filtered = size_df %>%
    filter(size == old_sizes[i])
  
  size_df$new_size[filtered$id_num] = new_sizes[i]
  filtered = data.frame()
}

size_df$new_size = as.character(size_df$new_size)


#create item_size2 column in known1
known1$item_size2 = 0

filtered = data.frame()

library(dplyr)
for(i in 1:nrow(size_df)) {
  filtered = known1 %>%
    filter(item_size == size_df$size[i])
  
  known1$item_size2[filtered$order_item_id] = size_df$new_size[i]
  filtered = data.frame()
}

known1$item_size2 = as.factor(known1$item_size2)

head(known1$item_size, 20)
head(known1$item_size2, 20)

#unknown
unknown1$item_size2 = 0

filtered = data.frame()

for(i in 1:nrow(size_df)) {
  filtered = unknown1 %>%
    filter(item_size == size_df$size[i])
  
  unknown1$item_size2[filtered$order_item_id] = size_df$new_size[i]
  filtered = data.frame()
}

unknown1$item_size2 = as.factor(unknown1$item_size2)

rm(size_df); rm(filtered); rm(new_sizes); rm(old_sizes)

#checking for level equality

size_levels_k = data.frame("levels" = as.character(levels(known1$item_size2)))
size_levels_u = data.frame("levels" = as.character(levels(unknown1$item_size2)))

size_levels_k$in_unknown = size_levels_k$levels %in% size_levels_u$levels
size_levels_u$in_known = size_levels_u$levels %in% size_levels_k$levels

rm(size_levels_k); rm(size_levels_u)

# add frequency of item size
s_id1 = known1 %>% add_count(item_size)
known1$item_size_freq = s_id1$n
rm(s_id1)

s_id2 = unknown1 %>% add_count(item_size)
unknown1$item_size_freq = s_id2$n
rm(s_id2)




## item_color
# checking for level equality
library(dplyr)
known1$item_color = as.factor(known$item_color)
unknown1$item_color = as.factor(unknown$item_color)

item_color = data.frame(table(known1$item_color))
colnames(item_color) = c("item_color", "freq")
item_color$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$item_color), FUN = sum)[,2]
item_color$return_prob = item_color$returns/item_color$freq

item_color_unknown = data.frame(table(unknown1$item_color))

#comparing levels
color_levels_k = data.frame("levels" = as.character(levels(known1$item_color)))
color_levels_u = data.frame("levels" = as.character(levels(unknown1$item_color)))

color_levels_k$in_unknown = color_levels_k$levels %in% color_levels_u$levels
color_levels_u$in_known = color_levels_u$levels %in% color_levels_k$levels

#change levels to other: c("amethyst", "opal", "perlmutt", "vanille", "cortina mocca") sum: 7 observations
known1$item_color = as.character(known1$item_color)
known1$item_color[known1$item_color %in% c("amethyst", "opal", "perlmutt", "vanille", "cortina mocca")] = "other"
known1$item_color = as.factor(known1$item_color)

unknown1$item_color = as.character(unknown1$item_color)
unknown1$item_color[unknown1$item_color %in% c("amethyst", "opal", "perlmutt", "vanille", "cortina mocca")] = "other"
unknown1$item_color = as.factor(unknown1$item_color)

#check result
color_levels_k = data.frame("levels" = as.character(levels(known1$item_color)))
color_levels_u = data.frame("levels" = as.character(levels(unknown1$item_color)))

color_levels_k$in_unknown = color_levels_k$levels %in% color_levels_u$levels
color_levels_u$in_known = color_levels_u$levels %in% color_levels_k$levels

rm(item_color); rm(item_color_unknown); rm(color_levels_k); rm(color_levels_u)


#aggregate different color levels to base colors
all_color_levels = data.frame(unique(rbind(data.frame("color" = levels(as.factor(known1$item_color))), data.frame("color" = levels(as.factor(unknown1$item_color))))))

color_check = function(x){
  if(x %in% c("avocado", "jade","mint","khaki","oliv", "dark oliv","olive", "green")){
    return("green")
  }else if(x %in% c("opal", "cobalt blue","baltic blue", "dark navy","darkblue","aqua","blau", "navy", "azure", "dark denim","aquamarine", "denim","turquoise", "blue", "petrol")){
    return("blue")
  }else if(x %in% c("perlmutt", "white")){
    return("white")
  }else if(x %in% c("amethyst", "currant purple" , "pallid", "magenta", "aubergine","berry", "purple")){
    return("purple")
  }else if(x %in% c("crimson", "dark garnet", "bordeaux","apricot","coral","mango","terracotta", "red")){
    return("red")
  }else if(x %in% c("apricot","coral","mango", "orange")){
    return("orange")
  }else if(x %in% c("lemon", "vanille","curry", "yellow", "gold")){
    return("yellow")
  }else if(x %in% c("aviator", "graphite", "ash", "anthracite","dark grey", "basalt", "iron", "grey", "silver")){
    return("grey")
  }else if(x %in% c("copper coin", "caramel", "brwon", "kanel", "habana", "mahagoni", "cognac", "mocca","cortina mocca", "brown")){
    return("brown")
  }else if(x %in% c("ebony", "black")){
    return("black")
  }else if(x %in% c("antique pink", "fuchsia", "pink", "hibiscus")){
    return("pink")
  }else if(x %in% c("creme","almond", "champagner", "ivory", "ingwer","ecru", "beige", "ocher")){
    return("beige")
  }else if(x %in% c("?","ancient","curled","floral","nature", "striped", "stained", "other")){
    return("other")
  }else{
    return("other")
  }
}

#for known and unknown
known1$item_color2 = sapply(known1$item_color, FUN = color_check)
known1$item_color2 = as.factor(known1$item_color2)

unknown1$item_color2 = sapply(unknown1$item_color, FUN = color_check)
unknown1$item_color2 = as.factor(unknown1$item_color2)

head(known1$item_color2, 20); head(known1$item_color, 20)
head(unknown1$item_color2, 20); head(unknown1$item_color, 20)


#check agan for level equality
#comparing levels
color_levels_k = data.frame("levels" = as.character(levels(known1$item_color2)))
color_levels_u = data.frame("levels" = as.character(levels(unknown1$item_color2)))

color_levels_k$in_unknown = color_levels_k$levels %in% color_levels_u$levels
color_levels_u$in_known = color_levels_u$levels %in% color_levels_k$levels

rm(color_levels_k); rm(color_levels_u); rm(all_color_levels); rm(color_check)


#include frequencies of colors
c_id1 = known1 %>% add_count(item_color)
known1$item_color_freq = c_id1$n
rm(c_id1)

c_id2 = unknown1 %>% add_count(item_color)
unknown1$item_color_freq = c_id2$n
rm(c_id2)




## brand_id
#include brand_id as integer value
known1$brand_id = as.integer(known$brand_id)
unknown1$brand_id = as.integer(unknown$brand_id)


#include brand_id frequencies
b_id1 = known1 %>% add_count(as.factor(brand_id))
known1$brand_id_freq = b_id1$n
rm(b_id1)

b_id2 = unknown1 %>% add_count(as.factor(brand_id))
unknown1$brand_id_freq = b_id2$n
rm(b_id2)



#item_price
#include item_price as numeric variable
known1$item_price = as.numeric(known$item_price)
unknown1$item_price = as.numeric(unknown$item_price)




## user_id
#include user_id as integer value
known1$user_id = as.integer(known$user_id)
unknown1$user_id = as.integer(unknown$user_id)


#include brand_id frequencies
u_id1 = known1 %>% add_count(as.factor(user_id))
known1$user_id_freq = u_id1$n
rm(u_id1)

u_id2 = unknown1 %>% add_count(as.factor(user_id))
unknown1$user_id_freq = u_id2$n
rm(u_id2)



## user_title
#include user_title as factor
known1$user_title = as.factor(known$user_title)
unknown1$user_title = as.factor(unknown$user_title)

# compare titles
user_title = data.frame(table(known1$user_title))
colnames(user_title) = c("title", "freq")
user_title$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$user_title), FUN = sum)[,2]
user_title$return_prob = user_title$returns/user_title$freq

rm(user_title)


#include binary variable for female
known1$female = 0
known1$female[which(known1$user_title == "Mrs")] = 1

unknown1$female = 0
unknown1$female[which(unknown1$user_title == "Mrs")] = 1



## user_dob
known1$user_dob = as.Date(known$user_dob)
unknown1$user_dob = as.Date(unknown$user_dob)


#user_dob_missing
known1$user_dob_missing = 0
known1$user_dob_missing[which(is.na(known1$user_dob))] = 1

unknown1$user_dob_missing = 0
unknown1$user_dob_missing[which(is.na(unknown1$user_dob))] = 1


#age
age_df = data.frame("user_dob" = known$user_dob)
age_df$user_dob = as.Date(age_df$user_dob, format = "%Y-%m-%d")

age_df$age = round((unclass(as.Date("2018-01-01", format = "%Y-%m-%d")) - unclass(age_df$user_dob))/365, 1)
mean_age = round(mean(age_df$age[which(age_df$age < 80)], na.rm = TRUE), 0)

#check for differences in missing age
age_df$age[which(is.na(age_df$age))] = "missing"

age_check = data.frame(table(as.factor(age_df$age)))
colnames(age_check) = c("age", "freq")
age_df$return = as.numeric(known1$return)
age_check$returns = aggregate(as.numeric(age_df$return), by = list(Category = age_df$age), FUN = sum)[,2]
#delivery_weekday = delivery_weekday[-3, ]
age_check$return_prob = age_check$returns/age_check$freq

rm(age_check)

age_df$age[which(age_df$age == "missing")] = NA
age_df$age[which(is.na(age_df$age))] = mean_age
age_df$age = sapply(as.numeric(age_df$age), FUN = function(x) round(x, 0))

known1$age = age_df$age

rm(age_df)


#unknown
age_df = data.frame("user_dob" = unknown$user_dob)
age_df$user_dob = as.Date(age_df$user_dob, format = "%Y-%m-%d")

age_df$age = round((unclass(as.Date("2018-01-01", format = "%Y-%m-%d")) - unclass(age_df$user_dob))/365, 1)
mean_age = round(mean(age_df$age[which(age_df$age < 80)], na.rm = TRUE), 0)

age_df$age[which(is.na(age_df$age))] = mean_age
age_df$age = sapply(as.numeric(age_df$age), FUN = function(x) round(x, 0))

unknown1$age = age_df$age

rm(age_df); rm(mean_age)


#yob
#year of birth
known1$user_yob = as.numeric(format(as.Date(known1$user_dob, format = "%Y-%m-%d"), "%Y"))
unknown1$user_yob = as.numeric(format(as.Date(unknown1$user_dob, format = "%Y-%m-%d"), "%Y"))


#age category
#user_yob with categories and na's category
user_yob2 = data.frame("user_yob" = as.numeric(format(as.Date(known$user_dob, format = "%Y-%m-%d"), "%Y")))
user_yob2$return = known$return
user_yob2$na_check = sapply(user_yob2$user_yob, is.na)

library(dplyr)
na_user_yob2 = user_yob2 %>%
  filter(na_check == TRUE)

table(na_user_yob2$return)
user_yob2$na_check = NULL

categories_yob = data_frame("category_no" = seq(1:10))
categories_yob$category = c("< 1940", "1940 - 1949", "1950 - 1959", "1960 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1999","2000 - 2009", "> 2009", "NA")

user_yob2$category_no = 0
user_yob2$category = ""

yob_cat = function(x){
  if(is.na(x)){
    return(10)
  }else if(x < 1940){
    return(1)
  }else if(x <= 1949){
    return(2)
  }else if(x <= 1959){
    return(3)
  }else if(x <= 1969){
    return(4)
  }else if(x <= 1979){
    return(5)
  }else if(x <= 1989){
    return(6)
  }else if(x <= 1999){
    return(7)
  }else if(x <= 2009){
    return(8)
  }else if(x > 2009){
    return(9)
  }else{
    return(10)
  }
}

user_yob2$category_no = sapply(user_yob2$user_yob, FUN = yob_cat)
user_yob2$id_numeric = seq(1:nrow(user_yob2))

filtered = data.frame()

system.time(
  for(i in 1:10){
    filtered = filter(user_yob2, category_no == i)
    user_yob2[filtered$id_numeric, ]$category = categories_yob$category[i]
    filtered = data.frame()
  }
)

known1$yob_cat = factor(user_yob2$category, levels = c("NA", "< 1940", "1940 - 1949", "1950 - 1959", "1960 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009", "> 2009"))

#unknown data:
user_yob2_unknown = data.frame("user_yob" = as.numeric(format(as.Date(unknown$user_dob, format = "%Y-%m-%d"), "%Y")))

user_yob2_unknown$category_no = 0
user_yob2_unknown$category = ""

user_yob2_unknown$category_no = sapply(user_yob2_unknown$user_yob, FUN = yob_cat)
user_yob2_unknown$id_numeric = seq(1:nrow(user_yob2_unknown))

filtered = data.frame()

system.time(
  for(i in 1:10){
    filtered = filter(user_yob2_unknown, category_no == i)
    user_yob2_unknown[filtered$id_numeric, ]$category = categories_yob$category[i]
    filtered = data.frame()
  }
)

unknown1$yob_cat = factor(user_yob2_unknown$category, levels = c("NA", "< 1940", "1940 - 1949", "1950 - 1959", "1960 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009", "> 2009"))

rm(user_yob2_unknown); rm(user_yob2); rm(categories_yob); rm(filtered); rm(na_user_yob2); rm(yob_cat)


#extracting birth_day
library(dplyr)
known1$birth_day = weekdays(as.Date(known1$user_dob))
known1$birth_day[which(is.na(known1$birth_day))] = "missing"
known1$birth_day = as.factor(known1$birth_day)

birth_day = data.frame(table(known1$birth_day))
colnames(birth_day) = c("weekday", "freq")
birth_day$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$birth_day), FUN = sum)[,2]
birth_day$return_prob = birth_day$returns/birth_day$freq

known1$birth_day = NULL
rm(birth_day)


#extracting birth_month
known1$birth_month = format(known1$user_dob, "%m")
known1$birth_month[which(is.na(known1$birth_month))] = "missing"
known1$birth_month = as.factor(known1$birth_month)

birth_month = data.frame(table(known1$birth_month))
colnames(birth_month) = c("month", "freq")
birth_month$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$birth_month), FUN = sum)[,2]
birth_month$return_prob = birth_month$returns/birth_month$freq

known1$birth_month = NULL
rm(birth_month)


#extracting birth_quarter
known1$birth_quarter = substr(quarters(known1$user_dob), 2, 2)
known1$birth_quarter[which(known1$birth_quarter == "N")] = "missing"
known1$birth_quarter = as.factor(known1$birth_quarter)

levels(known1$birth_quarter)
birth_quarter = data.frame(table(known1$birth_quarter))
colnames(birth_quarter) = c("quarter", "freq")
birth_quarter$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$birth_quarter), FUN = sum)[,2]
birth_quarter$return_prob = birth_quarter$returns/birth_quarter$freq

known1$birth_quarter = NULL
rm(birth_quarter)


#extracting month day of birth (numeric)
known1$birth_monthday = format(known1$user_dob, "%d")
known1$birth_monthday[which(is.na(known1$birth_monthday))] = "missing"
known1$birth_monthday = as.factor(known1$birth_monthday)

birth_monthday = data.frame(table(known1$birth_monthday))
colnames(birth_monthday) = c("monthday", "freq")
birth_monthday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$birth_monthday), FUN = sum)[,2]
birth_monthday$return_prob = birth_monthday$returns/birth_monthday$freq

cor(as.numeric(known1$birth_monthday), known1$return)

knownc = known1[-which(known1$birth_monthday == "missing"), ]
cor(knownc$return, as.numeric(knownc$birth_monthday))
rm(knownc)

rm(birth_monthday)
known1$birth_monthday = NULL


#delivery_dates as factor
known1$dob_factor = as.factor(known1$user_dob)
levels(known1$dob_factor) = c(as.character(levels(known1$dob_factor)), "missing")
known1$dob_factor[which(is.na(known1$dob_factor))] = "missing"

dob_factor = data.frame(table(known1$dob_factor))
colnames(dob_factor) = c("birth_date", "freq")
dob_factor$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$dob_factor), FUN = sum)[,2]
dob_factor$return_prob = dob_factor$returns/dob_factor$freq

known1$dob_factor = NULL
rm(dob_factor)



## user_state
known1$user_state = as.factor(known$user_state)
unknown1$user_state = as.factor(unknown$user_state)

user_states = data.frame(table(known1$user_state))
colnames(user_states) = c("user_state", "freq")
user_states$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$user_state), FUN = sum)[,2]
user_states$return_prob = user_states$returns/user_states$freq 

rm(user_states)


#include binary variable for east and west germany
east = c("Brandenburg", "Mecklenburg-Western Pomerania", "Saxony", "Thuringia", "Saxony-Anhalt")

known1$east = 0
known1$east[which(known1$user_state %in% east)] = 1

unknown1$east = 0
unknown1$east[which(unknown1$user_state %in% east)] = 1

rm(east)



## user_reg_date

known1$user_reg_date = as.Date(known$user_reg_date)
unknown1$user_reg_date = as.Date(unknown$user_reg_date)


#extracting reg_day
library(dplyr)
known1$reg_day = weekdays(as.Date(known1$user_reg_date))
reg_day = data.frame(table(known1$reg_day))
colnames(reg_day) = c("reg_day", "freq")
reg_day$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_day), FUN = sum)[,2]
reg_day$return_prob = reg_day$returns/reg_day$freq

known1$reg_day = NULL
rm(reg_day)


#extracting reg_month
known1$reg_month = as.factor(format(known1$user_reg_date, "%m"))
reg_month = data.frame(table(known1$reg_month))
colnames(reg_month) = c("month", "freq")
reg_month$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_month), FUN = sum)[,2]
reg_month$return_prob = reg_month$returns/reg_month$freq

rm(reg_month)
unknown1$reg_month = as.factor(format(unknown1$user_reg_date, "%m"))


#extracting reg_quarter
known1$reg_quarter = as.factor(substr(quarters(known1$user_reg_date), 2, 2))
levels(known1$reg_quarter)
reg_quarter = data.frame(table(known1$reg_quarter))
colnames(reg_quarter) = c("quarter", "freq")
reg_quarter$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_quarter), FUN = sum)[,2]
reg_quarter$return_prob = reg_quarter$returns/reg_quarter$freq

rm(reg_quarter)
unknown1$reg_quarter = as.factor(substr(quarters(unknown1$user_reg_date), 2, 2))


#extracting order_year
known1$reg_year = as.factor(format(known1$user_reg_date, "%Y"))
reg_year = data.frame(table(known1$reg_year))
colnames(reg_year) = c("year", "freq")
reg_year$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_year), FUN = sum)[,2]
reg_year$return_prob = reg_year$returns/reg_year$freq

rm(reg_year)
unknown1$reg_year = as.factor(format(unknown1$user_reg_date, "%Y"))


#extracting month day of registration (numeric)
known1$reg_monthday = as.factor(format(known1$user_reg_date, "%d"))
reg_monthday = data.frame(table(known1$reg_monthday))
colnames(reg_monthday) = c("monthday", "freq")
reg_monthday$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_monthday), FUN = sum)[,2]
reg_monthday$return_prob = reg_monthday$returns/reg_monthday$freq

cor(as.numeric(known1$reg_monthday), known1$return)

known1$reg_monthday = NULL
rm(reg_monthday)


#user_reg_date as factor
known1$reg_factor = as.factor(known1$user_reg_date)
reg_factor = data.frame(table(known1$reg_factor))
colnames(reg_factor) = c("reg_factor", "freq")
reg_factor$returns = aggregate(as.numeric(known1$return), by = list(Category = known1$reg_factor), FUN = sum)[,2]
reg_factor$return_prob = reg_factor$returns/reg_factor$freq

known1$reg_factor = NULL
rm(reg_factor)



## return
#set return to last position
known1$return = NULL
known1$return = known$return



##compare colums of known1 and unknown1
columns = cbind(colnames(known1), c(colnames(unknown1),"0"))
rm(columns)



#read saved files
wd = "C:/Users/timpe_000/Documents/02 Master/Semester 1/04 Business Analytics and Data Science/Kaggle Competition"
setwd(wd)

library(readr)
known = data.frame(read_csv("BADS_WS1819_known.csv"))
unknown = data.frame(read_csv("BADS_WS1819_unknown.csv"))

known1 = data.frame(read_csv("known1.csv"))
unknown1 = data.frame(read_csv("unknown1.csv"))

known1$X1 = NULL
unknown1$X1 = NULL


#change to correct format
known1$order_item_id = as.integer(known1$order_item_id); unknown1$order_item_id = as.integer(unknown1$order_item_id)
known1$order_date = as.Date(known1$order_date); unknown1$order_date = as.Date(unknown1$order_date)
known1$order_month = as.factor(known1$order_month); unknown1$order_month = as.factor(unknown1$order_month)
known1$order_quarter = as.factor(known1$order_quarter); unknown1$order_quarter = as.factor(unknown1$order_quarter)
known1$order_year = as.factor(known1$order_year); unknown1$order_year = as.factor(unknown1$order_year)

known1$delivery_date = as.Date(known1$delivery_date); unknown1$delivery_date = as.Date(unknown1$delivery_date)
known1$delivery_date_missing = as.factor(known1$delivery_date_missing); unknown1$delivery_date_missing = as.factor(unknown1$delivery_date_missing)
known1$delivery_time = as.numeric(known1$delivery_time); unknown1$delivery_time = as.numeric(unknown1$delivery_time)
known1$delivery_time_negative = as.factor(known1$delivery_time_negative); unknown1$delivery_time_negative = as.factor(unknown1$delivery_time_negative)
known1$delivery_month = as.factor(known1$delivery_month); unknown1$delivery_month = as.factor(unknown1$delivery_month)
known1$delivery_quarter = as.factor(known1$delivery_quarter); unknown1$delivery_quarter = as.factor(unknown1$delivery_quarter)
known1$delivery_year = as.factor(known1$delivery_year); unknown1$delivery_year = as.factor(unknown1$delivery_year)

known1$item_id = as.integer(known1$item_id); unknown1$item_id = as.integer(unknown1$item_id)
known1$item_id_freq = as.numeric(known1$item_id_freq); unknown1$item_id_freq = as.numeric(unknown1$item_id_freq)

known1$item_size = as.factor(known1$item_size); unknown1$item_size = as.factor(unknown1$item_size)
known1$item_size2 = as.factor(known1$item_size2); unknown1$item_size2 = as.factor(unknown1$item_size2)
known1$item_size_freq = as.numeric(known1$item_size_freq); unknown1$item_size_freq = as.numeric(unknown1$item_size_freq)

known1$item_color = as.factor(known1$item_color); unknown1$item_color = as.factor(unknown1$item_color)
known1$item_color2 = as.factor(known1$item_color2); unknown1$item_color2 = as.factor(unknown1$item_color2)
known1$item_color_freq = as.numeric(known1$item_color_freq); unknown1$item_color_freq = as.numeric(unknown1$item_color_freq)

known1$brand_id = as.integer(known1$brand_id); unknown1$brand_id = as.integer(unknown1$brand_id)
known1$brand_id_freq = as.numeric(known1$brand_id_freq); unknown1$brand_id_freq = as.numeric(unknown1$brand_id_freq)

known1$item_price = as.numeric(known1$item_price); unknown1$item_price = as.numeric(unknown1$item_price)

known1$user_id = as.integer(known1$user_id); unknown1$user_id = as.integer(unknown1$user_id)
known1$user_id_freq = as.numeric(known1$user_id_freq); unknown1$user_id_freq = as.numeric(unknown1$user_id_freq)

known1$user_title = as.factor(known1$user_title); unknown1$user_title = as.factor(unknown1$user_title)
known1$female = as.factor(known1$female); unknown1$female = as.factor(unknown1$female)

known1$user_dob = as.Date(known1$user_dob); unknown1$user_dob = as.Date(unknown1$user_dob)
known1$user_dob_missing = as.factor(known1$user_dob_missing); unknown1$user_dob_missing = as.factor(unknown1$user_dob_missing)
known1$age = as.numeric(known1$age); unknown1$age = as.numeric(unknown1$age)
known1$user_yob = as.numeric(known1$user_yob); unknown1$user_yob = as.numeric(unknown1$user_yob)
known1$yob_cat = as.factor(known1$yob_cat); unknown1$yob_cat = as.factor(unknown1$yob_cat)

known1$user_state = as.factor(known1$user_state); unknown1$user_state = as.factor(unknown1$user_state)
known1$east = as.factor(known1$east); unknown1$east = as.factor(unknown1$east)

known1$user_reg_date = as.Date(known1$user_reg_date); unknown1$user_reg_date = as.Date(unknown1$user_reg_date)
known1$reg_month = as.factor(known1$reg_month); unknown1$reg_month = as.factor(unknown1$reg_month)
known1$reg_quarter = as.factor(known1$reg_quarter); unknown1$reg_quarter = as.factor(unknown1$reg_quarter)
known1$reg_year = as.factor(known1$reg_year); unknown1$reg_year = as.factor(unknown1$reg_year)

known1$return = as.factor(known1$return)


##save files
#write.csv(known1, file = "known1.csv")
#write.csv(unknown1, file = "unknown1.csv")


## splitting tknown data into train and test set
set.seed(12)
n = nrow(known1)
sample.size <- ceiling(n*0.7)
samp = sample(n, sample.size)
train = known1[samp,]
test = known1[-samp,]


### gradient boosting model ###

library(caret)
library(mlr)

#prepare data
known1$user_dob = NULL
known1$user_yob = NULL
known1$yob_cat = NULL

unknown1$user_dob = NULL
unknown1$user_yob = NULL
unknown1$yob_cat = NULL

trainc = known1[which(complete.cases(known1)), ]
testc = unknown1[which(complete.cases(unknown1)), ]

trainc$order_item_id = NULL
#trainc$order_date = NULL
#trainc$user_title = NULL
trainc$delivery_date_missing = NULL
trainc$delivery_time_negative = NULL
trainc$user_dob_missing = NULL
trainc$item_size = NULL
#trainc$item_size2 = NULL
trainc$item_color = NULL
#trainc$female = NULL
trainc$user_yob = NULL
trainc$user_dob = NULL
#trainc$user_reg_date = NULL
#trainc$order_quarter = NULL
trainc$reg_quarter = NULL
trainc$delivery_quarter = NULL
trainc$east = NULL
trainc$order_year = NULL
trainc$delivery_year = NULL
trainc$yob_cat = NULL
trainc$reg_year = NULL
trainc$delivery_month = NULL
trainc$female = NULL
trainc$reg_month = NULL
trainc$item_size_freq = NULL
trainc$item_color_freq = NULL


trainc$order_date = unclass(trainc$order_date)
trainc$delivery_date = unclass(trainc$delivery_date)
trainc$user_reg_date = unclass(trainc$user_reg_date)

unknown_ids = testc$order_item_id
testc$order_item_id = NULL
#testc$order_date = NULL
#testc$user_title = NULL
testc$delivery_date_missing = NULL
testc$delivery_time_negative = NULL
testc$user_dob_missing = NULL
testc$item_size = NULL
#testc$item_size2 = NULL
testc$item_color = NULL
#testc$female = NULL
testc$user_yob = NULL
testc$user_dob = NULL
#testc$user_reg_date = NULL
#testc$order_quarter = NULL
testc$reg_quarter = NULL
testc$delivery_quarter = NULL
testc$east = NULL
testc$order_year = NULL
testc$delivery_year = NULL
testc$yob_cat = NULL
testc$reg_year = NULL
testc$delivery_month = NULL
testc$female = NULL
testc$reg_month = NULL
testc$item_size_freq = NULL
testc$item_color_freq = NULL

testc$order_date = unclass(testc$order_date)
testc$delivery_date = unclass(testc$delivery_date)
testc$user_reg_date = unclass(testc$user_reg_date)


# Prepare the mlr task
# create dummy variables
train_dummy <- mlr::createDummyFeatures(trainc, target="return")
test_dummy <- mlr::createDummyFeatures(testc)

task <- makeClassifTask(data = train_dummy, target = "return", positive = "1")
library("xgboost")

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob",  
                           par.vals = list("verbose" = 0,
                                           "early_stopping_rounds"=10)) 

# Set tuning parameters
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.1),
  makeIntegerParam("nrounds", lower=40, upper=300),
  makeIntegerParam("max_depth", lower=2, upper=6),
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 1),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.9)
)

# Random TUne COntrol
tuneControl <- makeTuneControlRandom(maxit=10, tune.threshold = FALSE)

# 3 fold cross validation
rdesc <- makeResampleDesc(method = "CV", iters = 3, stratify = TRUE)
library("parallelMap")
library(parallel)
parallelStartSocket(3, level = "mlr.tuneParams")
set.seed(123) 

RNGkind("L'Ecuyer-CMRG")

clusterSetRNGStream(iseed = 1234567)
# Tune parameters
xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
# Extract optimal parameter values
xgb.tuning$x

#optimal parameters for submission were:
#eta = 0.07174785
#nrounds = 289
#max_depth = 6
#gamma = 0
#colsample_bytree = 1
#min_child weight = 1
#subsamle = 0.9

# set optimal hyperparameters
xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
xgb.learner

# Train the model 
model_library <- mlr::train(xgb.learner, task = task)

# prediction
pred <- predict(model_library, newdata = test_dummy, simplify=FALSE)

sum(as.numeric(as.character(pred$data$response)))/length(pred$data$response)

prediction = data.frame("prediction" = pred$data$prob.1, "id" = unknown_ids)

unknown1$prediction_class = 0

#replace predictions in full data where delivery date is not equal to 0
j = 1
for(i in prediction$id){
  unknown1$prediction_class[i] = as.numeric(as.character(prediction$prediction[j]))
  j = j + 1
}

#check percentage of return
sum(as.numeric(as.character(unknown1$prediction_class)))/nrow(unknown1)

head(unknown_ids,30)
head(prediction$prediction,30)
head(unknown1$prediction_class,30)

#create data frame
prediction_xgb = data.frame("order_item_id" = unknown$order_item_id, "return" = unknown1$prediction_class)

#save prediction
write.csv(prediction_xgb, file = "prediction_xgb6.csv", row.names = FALSE)
