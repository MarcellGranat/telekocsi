library(rvest)
library(tidyverse)
library(RSelenium)
load('initial_scrape.RData')

df <-
  initial_df %>% 
  select(start_city, destination) %>% 
  filter(!duplicated(str_c(start_city, destination))) %>% 
  apply(1,  function(x) {
    sort(c(x[1], x[2]))
  }) %>% 
  t %>% 
  data.frame() %>% 
  set_names('start_city', 'destination')%>% 
  filter(!duplicated(str_c(start_city, destination))) %>% 
  tibble()


distances <- list()
rD <- rsDriver(verbose = TRUE, 
               port=48473L, 
               chromever = '88.0.4324.27',
               check = TRUE)

remDr <- rD$client

i <- 1

while (i <= nrow(df)) {
  
  try(remDr$acceptAlert(), T)
  Sys.sleep(3)
  remDr$navigate('https://www.mapdevelopers.com/distance_from_to.php')
  Sys.sleep(6)
  
  from <- remDr$findElement(using = 'css selector',"#fromInput")
  to <- remDr$findElement(using = 'css selector',"#toInput")
  web_button <- remDr$findElement(using = 'css selector',".link_button")
  page <- T
  while (!is.null(page)) {
    page <- NULL
    page <- tryCatch({
      from$clearElement()
      from$sendKeysToElement(list(pull(df[i, 1]), key="enter"))
      to$clearElement()
      to$sendKeysToElement(list(pull(df[i, 2]), key="enter"))
      web_button$clickElement()
      Sys.sleep(1)
      remDr$getPageSource()[[1]]
      
    },
    error = function(e) {
      message(paste('error: ', i)); NULL
    })
    
    
    if (!is.null(page)) {
      distance <- page %>% 
        read_html() %>% 
        html_nodes('#status') %>% 
        html_text()
      driving <- page %>% 
        read_html() %>% 
        html_nodes('#driving_status') %>% 
        html_text()
      
      distances[[i]] <- list(
        source = df[i, ], driving, distance
      )
      
      print(i)
      print(distance)
      print(driving)
      i <- i + 1
    }
    
    if (i %% 100 == 0) {
      
      save(list = c('distances'), file = paste0('distances', i, '.RData'))
    }
  }
}


save(list = c('distances'), file = paste0('distances.RData'))

message('done!')

tcltk::tkmessageBox(title = "Job done!",
                    message = paste('Job done at', Sys.time()),
                    icon = "info", type = "ok")
