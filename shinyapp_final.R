library(shiny)
library(plotly)


shiny_categories <- restaurant_business %>% unnest(categories) %>% select(categories)

ui <- tagList(
        shinythemes::themeSelector(),
        navbarPage("Yelp Data Exploration", id="nav",
                 tabPanel("Visualization",
                          sidebarLayout(
                                  sidebarPanel(
                                          selectInput("state","Choose a state:",
                                                      c("WI", "QC", "AZ", "NV", "ON", "PA", 
                                                        "EDH", "OH", "NC", "IL", "SC","BW"), selected = "NV"),
                                          selectInput("city_name", "Choose a city", 
                                                      c(unique(c(restaurant_business$city, "ALL"))), selected = "Las Vegas" )
                                  ),
                                mainPanel(
                                          tabsetPanel(
                                                  tabPanel("Price vs Rating ", plotOutput("price_vs_stars"), height = "1000px"),
                                                  tabPanel("Rating vs Categories", plotOutput("rate_vs_categories")),
                                                  tabPanel("Correlation Analysis", plotOutput("cor_analysis")),
                                                  tabPanel("Correlation Heatmap", plotOutput("cor_heat_map"))

                                          )
                                  )
                          )
 #### Text mining ####                     
                 ),
                 tabPanel("Text Mining",
                          sidebarLayout(
                                  sidebarPanel(
                                          selectInput("categori", "Select a categories", 
                                                      c(unique(shiny_categories$categories)),selected =  "Korean"),
                                          sliderInput("text_mine_stars", "Choose a rating level", 
                                                      min = 1, max = 5, step = 0.5, value = 3.5),
                                          sliderInput("text_mine_count", "Number of review to choose from each resturant:", 
                                                      min = 0, max = 1000, step = 10, value = 100)
                                  ),
                                  mainPanel(
                                          tabsetPanel(
                                                  tabPanel("Word Cloud", plotOutput("text_mine_wordcloud")),
                                                  tabPanel("Sentiment Score", plotOutput("text_mine_sentiment_score")),
                                                  tabPanel("Kmean Analysis", plotOutput("text_mine_kmean")))
                                               
                                          )
                                  )
                          )
                         
                )
) 



#### server ####

server <- function(input, output, session) {
        dat <- reactive({restaurant_business %>%
                                 unnest(categories) %>%
                                 filter(state == input$state, 
                                        city == input$city_name)})
        
        dat_present <- reactive({restaurant_business %>%
                        unnest(categories) %>%
                        filter(state == input$state)})
        
        output$price_vs_stars <- renderPlot({
                dat() %>% 
                        select(business_id, stars, attributes.RestaurantsPriceRange2) %>% 
                        filter(!attributes.RestaurantsPriceRange2 == 0) %>% 
                        mutate(price_range = factor(attributes.RestaurantsPriceRange2, levels = c(1,2,3,4),
                                                    labels = c("$", "$$","$$$","$$$$")),
                               stars = factor(stars, levels = c(seq(1,5,0.5)))) %>%  
                        group_by(price_range, stars) %>% 
                        summarise(count = n ()) %>% 
                        ggplot(aes(x = price_range, y = count, fill = stars)) + 
                        geom_bar(stat = "identity",position=position_fill()) +
                        theme(axis.text=element_text(size=14),
                              axis.title=element_text(size=14,face="bold")) + 
                        ggtitle("Price Range vs Star Levels") + 
                        ylab("Stars Proportions") + 
                        xlab("Price Range")
                
        }, height = 600)
        
        output$rate_vs_categories <- renderPlot({
                rate_vs_categories <- dat_present() %>% unnest(categories) %>%
                        filter(!attributes.RestaurantsPriceRange2 == 0) %>% 
                        select(business_id, stars, categories, attributes.RestaurantsPriceRange2) %>% 
                        mutate(price_range = factor(attributes.RestaurantsPriceRange2, levels = c(1,2,3,4),
                                                    labels = c("$", "$$","$$$","$$$$"))) %>% 
                        group_by(categories, price_range) %>% 
                        summarise(count = n(), average_stars = mean(stars)) %>% 
                        arrange(price_range, desc(average_stars)) %>% 
                        filter(!categories == "Restaurants"|categories == "Food", !count < 20) %>% #get rid of count < 100 categories
                        ggplot(aes(x = reorder(categories,average_stars), y = average_stars), group = price_range) + 
                        geom_point(stat ="identity",aes(size = count, col = price_range)) + 
                        geom_hline(aes(yintercept = mean(restaurant_business$stars), col = "red")) + 
                        facet_grid(~price_range) +   
                        coord_flip() +
                        theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold")) 
                
                print(rate_vs_categories)
                
        }, height = 700)
        
#### correlation analysis ####
        cor_stars <- reactive({dat() %>% 
                select(stars,review_count, attributes.RestaurantsPriceRange2:parking) %>% sapply(.,as.numeric) %>% 
                as.data.frame() %>% cor(.) %>% as.data.frame()
        })
        
        
        output$cor_analysis <- renderPlot({
                #cor_bar plot
                cor_stars() %>%
                        mutate(colname = rownames(cor_stars()),
                               col_neg_posi = ifelse(stars >0, 1, 0)) %>% 
                        filter(!stars == 1) %>% 
                        ggplot(aes(x =reorder(colname, stars), y = stars, fill = col_neg_posi)) +
                        geom_bar(stat = "identity") + 
                        coord_flip() 
                
        }, height = 700)
        
        
        #heatmap
        top_10_cor <- reactive({cor_stars() %>%          #top_10 correlaion heatmap
                mutate(colname = rownames(cor_stars())) %>% 
                arrange(desc(abs(stars))) %>% slice(1:10)
        })
                
        get_upper_tri <- function(cormat){
                cormat[lower.tri(cormat)]<- NA
                return(cormat)
        }
        
        output$cor_heat_map <- renderPlot({
                dat() %>%
                        select((which(names(dat()) %in% top_10_cor()$colname))) %>% 
                        sapply(.,as.numeric) %>% cor(.) %>%   
                        get_upper_tri() %>% melt(.,na.rm = TRUE) %>% 
                        ggplot(aes(Var2, Var1, fill = round(value, 3)))+ 
                        geom_tile(color = "white")+
                        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                                             name="Pearson\nCorrelation")+
                        theme_minimal()+ 
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                        geom_text(aes(Var2, Var1, label = round(value,3)), color = "black", size = 5) +
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
                        theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold"))+ 
                        guides(fill = guide_colorbar(barwidth = 9, barheight = 3,
                                                     title.position = "top", title.hjust = 0.5))
        }, height = 600)
        output

#### Text Mining ####                
        text_mining_dat <- reactive({ 
                dat() %>% unnest(categories) %>% 
                select(business_id, name, state, city, postal_code, stars, review_count,latitude, longitude, categories) %>% 
                filter(state == input$state, 
                       city == input$city_name,
                       categories == input$categori, 
                       stars > input$text_mine_stars,
                       review_count > 100) %>% slice(1:input$text_mine_count) %>% 
                left_join(.,business_review, by = "business_id") %>% 
                rename(doc_id = review_id, name = name.x, stars = stars.x) %>% 
                group_by(name) %>% filter(row_number()== c(1:50))
                })
        
        review_t <-reactive({VCorpus(DataframeSource(select(text_mining_dat(), doc_id, text)))}) 
        #Remove punctuation , stopword and....
        DTM.best <- reactive({
                DocumentTermMatrix(review_t(),
                         control=list(removePunctuation=TRUE,
                                      wordLengths=c(4, 15),
                                      stopwords=TRUE,
                                      stemming=TRUE,
                                      removeNumbers=TRUE))
        })
        
        DTM.best.sp <- reactive({removeSparseTerms(DTM.best(),0.995)})
        
        review_tidy <- reactive({tidy(DTM.best.sp())})
        
        
        term.count <- reactive({
                review_tidy() %>%
                group_by(term) %>%
                summarize(n.total=sum(count)) %>%
                arrange(desc(n.total)) %>% slice(5:500)
        })
        
        
        output$text_mine_wordcloud <- renderPlot({
                wordcloud(term.count()$term, term.count()$n.total, scale=c(5,.5), random.order=FALSE, rot.per=0.35, 
                          colors=brewer.pal(8, "Dark2"))
                
        },height = 600)
        
        # 
        AFINN <- reactive({sentiments %>%  
                filter(lexicon == "AFINN") %>%
                select(word, afinn_score = score)
        })
                
        sentiments_dat <- reactive({text_mining_dat() %>% 
                select(doc_id, stars, date, name) %>% 
                rename(document = doc_id) %>% 
                mutate(date = lubridate::as_date(date)) %>% 
                inner_join(review_tidy(), by = "document")
        })
        
        
        output$text_mine_sentiment_score <- renderPlot({
                sent_score <- sentiments_dat() %>%
                        inner_join(AFINN(), by = c("term" = "word")) %>%
                        group_by(month(date),name) %>%
                        summarize(sentiment = mean(afinn_score)) %>% 
                        group_by(`month(date)`, name) %>% 
                        summarise(avg = mean(sentiment)) %>% 
                        slice(1:3) %>% 
                        ggplot(aes(x = `month(date)` , y = avg, group = name, col = name)) + 
                        geom_line(stat = "identity") + 
                        theme(axis.text=element_text(size=12),
                              axis.title=element_text(size=14,face="bold")) 
                
                sent_score
                
        })
        
        
        #kmean
        kmean_dat <- reactive({DTM.best.sp() %>% as.matrix() %>% as.data.frame()%>% 
                cbind(restaurant_name = text_mining_dat()$name)})
        
        kmean_datx  <-reactive({aggregate(.~restaurant_name, kmean_dat(), mean)})
        
        kmean_daty <- reactive ({kmean_datx() %>% `rownames<-`(c(kmean_datx()$restaurant_name))})
        
        km.res <- reactive({kmean_daty() %>% select(-restaurant_name) %>%  kmeans(.,5)})
        
        output$text_mine_kmean <- renderPlot({
                fviz_cluster(km.res(),
                             data = select(kmean_daty(), -restaurant_name),
                             ellipse.type = "convex",
                             palette = "jco",
                             ggtheme = theme_minimal())
        }, height = 600)
        
}        
 
shinyApp(ui, server)
