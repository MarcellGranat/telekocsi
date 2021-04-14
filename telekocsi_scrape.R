library(tidyverse)
library(rvest)
library(parallel)

source <- tibble(date = paste0('2021-04-', 15:21), n_page = rep(1000, 7)) %>% 
  apply(1, function(x) str_c('https://utitars.oszkar.com/date-', x[1],'/hely-0/pageexact-', 0:as.numeric(x[2]), '#exact')) %>% 
  reduce(c) %>% 
  str_remove_all('pageexact-0#exact')

f_initial_df <- function(url) {
  tryCatch({
  page <- read_html(url) 
  tibble(
  date = gsub('.*date-', '', url) %>% 
    gsub(pattern = '/hely.*', replacement = ''),
  url = page %>% 
    html_nodes('.span8 a') %>% 
    html_attr('href'),
  info = page %>% 
    html_nodes('.span8 a') %>% 
    html_text,
  driver = page %>%
    html_nodes('img+ .rating-star-value') %>%
    html_text,
  drivers_rating = page %>%
    html_nodes('br+ .rating-star-value') %>%
    html_text %>% 
    str_remove_all('\\D') %>% 
    as.numeric() %>% 
    {ifelse(. > 9, . / 10, .)}
  )},
  error = function(e) {message(paste('error: ', url)); return(NULL)})
}


cl <- makeCluster(7)
clusterExport(cl, list("source", "f_initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(tidyverse))

initial_df <- parLapply(cl = cl, X = source, fun = f_initial_df)
initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind)

stopCluster(cl)

initial_df <- initial_df %>% 
  mutate(
    info = str_remove_all(info, '\\\r') %>% 
      str_remove_all('\\\n') %>% 
      str_remove_all('\\\t'),
    price = gsub(' Ft.*', '', info) %>% 
      gsub(pattern = ' €.*', replacement = '') %>% 
      as.numeric(),
    price_currency = ifelse(str_detect(info, '€'), 'euro', 'forint'),
    seat = gsub('.*Ft', '', info) %>% 
      gsub(pattern = '.*€', replacement = '') %>% 
      gsub(pattern = ' hely.*', replacement = '') %>%
      as.numeric(),
    seat = ifelse(str_detect(info, 'Betelt'), 0, seat),
    start_time = gsub('.* hely', '', info) %>% 
      gsub(pattern = '.*Betelt', replacement = '') %>% 
      gsub(pattern = ' .*', replacement = ''),
    arrival_time = gsub('.* ', '', info) %>% 
      str_remove_all('-ig'),
    start_city = gsub('.*:\\d\\d \\d\\d:\\d\\d', '', info) %>% 
      gsub(pattern = '  - .*', replacement = '') %>% 
      gsub(pattern = '\\d* megálló.*', replacement = '__') %>% 
      gsub(pattern = ' .__', replacement = '') %>% 
      {ifelse(str_detect(., 'Bp.'), 'Budapest', .)},
    destination = gsub('.*  - ', '', info) %>% 
      gsub(pattern = '  \\d{2}:\\d{2}', replacement = '') %>% 
      gsub(pattern = '\\d* megálló.*', replacement = '__') %>% 
      gsub(pattern = ' .__', replacement = '') %>% 
      {ifelse(str_detect(., 'Bp.'), 'Budapest', .)}
  )

save(list = c('initial_df'), file = 'initial_scrape.RData')

message('done!')

tcltk::tkmessageBox(title = "Job done!",
                    message = paste('Job done at', Sys.time()),
                    icon = "info", type = "ok")

