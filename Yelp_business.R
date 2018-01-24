## Downloaded business and reviews json files from Yelp website

## Since the files are huge, I didnt add those files in the project

## Code to load the json files

# business <- as_data_frame(flatten(stream_in(file("business.json"))))
# saveRDS(business, file = "data/business.rds")

## Since the reviews file is 3.5 GB loaded, used llply and set .parallel parameter to TRUE to use parallel backend provided by foreach

# library('jsonlite')
# library('plyr')
# reviews <- llply("data/review.json", function(x) stream_in(file(x),pagesize = 10000),.parallel = TRUE)
# saveRDS(reviews, file = "data/review.rds")


## Loading the RDS files saves earlier from streaming the json files. Loading from RDS is faster compared to loading entire JSON file, 
## so it is advised to save to RDS and load it whenver required to save time

business <- readRDS("data/business.rds")
reviews <- data.frame(readRDS("data/review.rds"))

glimpse(reviews)
#### Data Prepation ####

library(tidyverse)
#Extract categories of business 
categories_table <- business %>% 
  select(-starts_with("hours"), -starts_with("attribute")) %>% 
  unnest(categories) %>%
  select(business_id, name, state, categories)

#Top ranked categories (double count)
categories_table %>% select(categories, name) %>% 
  group_by(categories) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:15)

#filter out all the restaurants rows in business table
restaurant_business <- categories_table %>% 
  filter(categories == "Restaurants") %>% # reactive({}) for shiny app
  select(business_id) %>% 
  inner_join(.,business, by = "business_id") 

#extract all the review related to restaurants 
business_review <- categories_table %>% 
  filter(categories == "Restaurants") %>% 
  select(business_id, name) %>% 
  inner_join(.,reviews, by = "business_id")

names(reviews)


##
#Select restaurants with more then 100 reviews
qplot(restaurant_business$review_count, geom = "histogram") + xlim(0,2000)
mean(restaurant_business$review_count > 100)

restaurant_business <- restaurant_business %>% 
  filter(restaurant_business$review_count > 100)

business_review <- restaurant_business %>% #-------# change the filter argument to subset the review data
  filter(state =='NV') %>% 
  select(business_id, name) %>% 
  inner_join(.,reviews, by = "business_id")


#### Data Cleaning ####

#NA count table for restaurant_business table
na_count_rb <- data.frame(col = names(restaurant_business), 
                          na_count = colSums(is.na(restaurant_business))) %>%  
  mutate(percentage = na_count/nrow(restaurant_business), 
         percentage_cut = cut(percentage, breaks =c(0, 0.25, 0.50, 0.75, 100), 
                              labels = c("<0.25", "0.26-0.50" ,"0.51-0.75", "0.76-1.00")))

na_count_br <- data.frame(col = names(business_review), 
                          na_count = colSums(is.na(business_review))) %>%  
  mutate(percentage = na_count/nrow(business_review),  
         percentage_cut = cut(percentage, breaks =c(0, 0.25, 0.50, 0.75, 100), 
                              labels = c("<0.25", "0.26-0.50" ,"0.51-0.75", "0.76-1.00")))

#filter out variables in business table with more than 50% NA
restaurant_business <- 
  subset(restaurant_business, select = -c(which(na_count_rb$percentage>0.5), attributes.BusinessAcceptsBitcoin)) %>% 
  select(-starts_with("hours"))


#na pattern plot for vairbales have more than 25% NA
library(VIM)
restaurant_business[,40:ncol(restaurant_business)] %>%
  aggr(., col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
       labels=(names(restaurant_business[,13:ncol(restaurant_business)])), cex.axis=.7, gap=3, 
       ylab=c("Histogram of missing data <0.75","Pattern")) #no label


#Collapse parking attributes into one column
parking <- restaurant_business %>% 
  select(starts_with("attributes.BusinessParking")) %>% 
  apply(., 1,paste, collapse = "-")


library(stringr)
restaurant_business <- restaurant_business %>% 
  select(-starts_with("attributes.BusinessParking")) %>% 
  mutate(parking = ifelse(str_detect(parking, "TRUE"), TRUE, FALSE))


#Assign 0 to all NA value
restaurant_business[is.na(restaurant_business)] <- 0


#Convert chr to factor with levels
restaurant_business<- 
  restaurant_business %>% 
  mutate(attributes.NoiseLevel = factor(attributes.NoiseLevel, 
                                        levels = c(0, "quiet", "average",  "loud", "very_loud")),
         attributes.RestaurantsAttire = factor(attributes.RestaurantsAttire, 
                                               levels= c(0, "casual", "dressy", "formal")),
         attributes.WiFi = factor(attributes.WiFi, 
                                  levels =c( 0, "no", "free", "paid")), 
         attributes.Alcohol  = factor(attributes.Alcohol , 
                                      levels = c(0, "none", "full_bar", "beer_and_wine")))

which(sapply(restaurant_business, class)== "character")


#### Visualization #### 

#stars vs price
restaurant_business %>% 
  select(business_id, stars, attributes.RestaurantsPriceRange2) %>% 
  filter(!attributes.RestaurantsPriceRange2 == 0) %>% 
  mutate(price_range = factor(attributes.RestaurantsPriceRange2, levels = c(1,2,3,4),
                              labels = c("$", "$$","$$$","$$$$")),
         stars = factor(stars, levels = c(seq(1,5,0.5)))) %>%  
  group_by(price_range, stars) %>% 
  summarise(count = n ()) %>% 
  ggplot(aes(x = price_range, y = count, fill = stars)) + 
  geom_bar(stat = "identity",position=position_fill()) + 
  ggtitle("Price Range vs Star Levels") + 
  ylab("Stars Proportions") + 
  xlab("Price Range")



#Top average stars categories 
#vertical line represent the overal average rating of all categories
restaurant_business %>% unnest(categories) %>%
  filter(!attributes.RestaurantsPriceRange2 == 0) %>% 
  select(business_id, stars, categories, attributes.RestaurantsPriceRange2) %>% 
  mutate(price_range = factor(attributes.RestaurantsPriceRange2, levels = c(1,2,3,4),
                              labels = c("$", "$$","$$$","$$$$"))) %>% 
  group_by(categories, price_range) %>% 
  summarise(count = n(), average_stars = mean(stars)) %>% 
  arrange(price_range, desc(average_stars)) %>% 
  filter(!categories == "Restaurants"|categories == "Food", !count < 100) %>% #get rid of count < 100 categories
  ggplot(aes(x = reorder(categories,average_stars), y = average_stars), group = price_range) + 
  geom_point(stat ="identity",aes(size = count, col = price_range)) + 
  geom_hline(aes(yintercept = mean(restaurant_business$stars), col = "red")) + 
  facet_grid(~price_range) +   
  coord_flip() # working on to only show top 15.........


#Worst categories in terms of average rating
restaurant_business %>% unnest(categories) %>% 
  select(business_id, stars, categories) %>% 
  group_by(categories) %>% 
  summarise(count = n(), average_stars = mean(stars)) %>% 
  filter(!categories == "Restaurants"|categories == "Food", !count < 100) %>% 
  arrange(average_stars) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(categories,average_stars), y = average_stars)) + 
  geom_point(stat ="identity",aes(size = count)) + 
  geom_hline(aes(yintercept = mean(restaurant_business$stars), col = "red"))+
  coord_flip() 


#correlation with stars 
cor_stars <- restaurant_business %>% 
  select(stars,review_count, attributes.RestaurantsPriceRange2:parking) %>% sapply(.,as.numeric) %>% 
  as.data.frame() %>% cor(.) %>% as.data.frame()#%>% select(stars)

#heatmap
cor_stars %>%
  mutate(colname = rownames(cor_stars),
         col_neg_posi = ifelse(stars >0, 1, 0)) %>% 
  filter(!stars == 1) %>% 
  ggplot(aes(x =reorder(colname, stars), y = stars, fill = col_neg_posi)) +
  geom_bar(stat = "identity") + 
  coord_flip() 

#top_10 correlaion heatmap
top_10_cor <- cor_stars %>% 
  mutate(colname = rownames(cor_stars)) %>% 
  arrange(desc(abs(stars))) %>% slice(1:10)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


restaurant_business %>% 
  select((which(names(restaurant_business) %in% top_10_cor$colname))) %>% 
  sapply(.,as.numeric) %>% cor(.) %>%   
  get_upper_tri() %>% melt(.,na.rm = TRUE) %>% 
  ggplot(aes(Var2, Var1, fill = round(value, 3)))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+
  theme_minimal()+
  geom_text(aes(Var2, Var1, label = round(value,3)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 9, barheight = 3,
                               title.position = "top", title.hjust = 0.5))



#### Text Mining ####

library(scales)
library(forcats)
library(tibble)
library(tidytext)
library(tm)



#Convert text to DTM
shiny_categories <- restaurant_business %>% 
  unnest(categories) %>% select(business_id, name, stars,categories)


text_mining <- restaurant_business %>% #-------# change the filter argument to subset the review data
  filter(state =='NV', review_count > 500) %>% 
  select(business_id, name) %>% 
  inner_join(.,reviews, by = "business_id") %>%
  select(name, business_id, review_id, text)
  

#mutate doc_id = review_id which is the name VCorpus function will recognise 
text_mining <- text_mining %>% rename(doc_id = review_id)

review_text <- VCorpus(DataframeSource(select(text_mining,doc_id,text)))


#Remove punctuation , stopword and....
DTM.all <- DocumentTermMatrix(review_text,
                              control=list(removePunctuation=TRUE,
                                           wordLengths=c(3, Inf),
                                           stopwords=TRUE,
                                           stemming=TRUE,
                                           removeNumbers=TRUE
                              ))

print(DTM.all)

DTM.all.sp <- removeSparseTerms(DTM.all,0.995)

print(DTM.all.sp)

## Most frequent words for best rated restaurants

text.tidy <- tidy(DTM.all.sp)
term.count <- text.tidy %>%
  group_by(term) %>%
  summarize(n.total=sum(count)) %>%
  arrange(desc(n.total))


term.count %>% 
  slice(1:30) %>%
  ggplot(aes(x=fct_reorder(term,n.total),y=n.total)) + geom_bar(stat='identity') + 
  coord_flip() + xlab('Counts') + ylab('')+ggtitle('Most Frequent Terms')

term.count.pop <- term.count %>%
  slice(5:100) 

## To print the word cloud of most frequent terms
wordcloud(term.count.pop$term, term.count.pop$n.total, scale=c(5,.5))




#### Prediction ####

set.seed(42)

pred_df <- restaurant_business


## Considering that rating of 4 and above implies that business will be successful, so converting the data to 1's and 0's for >=4 and <4 respectively
pred_df$stars <- ifelse(pred_df$stars>=4, 1, 0)
pred_df$stars <- as.factor(pred_df$stars)

pred_df[, 1:7] <- NULL

temp <- pred_df %>% 
  unnest(categories) %>% select(categories) %>%
  unique() %>% slice(1:10)

result <- matrix(NA, nrow(pred_df), nrow(temp))
for(i in 1:nrow(temp)){
  result[,i] <- ifelse(str_detect(pred_df$categories, as.character(temp[i, ])),1,0)
}

colnames(result) <- c(temp$categories)

pred_df <- cbind(pred_df, result)

pred_df$categories <- NULL
names(pred_df)[names(pred_df) == 'Chicken Wings'] <- "Chicken_wings"
names(pred_df)[names(pred_df) == 'American (Traditional)'] <- "American_traditional"
names(pred_df)[names(pred_df) == 'Specialty Food'] <- "Specialty_food"
names(pred_df)[names(pred_df) == 'Breakfast & Brunch'] <- "Breakfast_brunch"

colnames(pred_df)

## Dividing the data in train and test sets to train Random forest model and test the model
sample <- sample.int(n= nrow(pred_df), size = floor(0.75*nrow(pred_df)), replace=F)
train <- pred_df[sample, ]
test <- pred_df[-sample, ]

library(randomForest)

fit <- randomForest(
  stars ~ .,
  data = train,
  importance = TRUE,
  mtry = 12,
  ntree = 100,
  type = "classification"
)


Prediction <- data.frame(prediction = predict(fit, test))
y <- cbind(test, Prediction)
temp_pred <- data.frame(y$stars, y$prediction)

tab <- table(temp_pred)
acc <- (tab[1,1]+tab[2,2])/sum(tab)

## We get about 83.5% accurate predictions on whether the restarant will be success or failure based on the 47 variables including
## location and other features like type of business, parking facilities, cuisine of restaurant, etc


