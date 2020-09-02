
test_params <- list(
  expand = NULL,
  season = NULL,
  locale = 'en-us',
  page = 1,
  size = 20
)
test_url <- 'https://api.overwatchleague.com/matches'

test_f_next_page_query <- function(req_content, current_page_query = list()){
  current_page <- current_page_query$page
  total_pages <- req_content$totalPages

  if(is_empty(current_page)) return(NULL)
  if(is_empty(total_pages)) return(NULL)

  if(current_page >= total_pages) return(NULL)

  current_page_query$page <- current_page + 1
  return(compact(current_page_query))
}

owl_matches <- GET_paginated_content(
  url = test_url,
  query = test_params,
  f_next_page_query = test_f_next_page_query,
  verbose = T
)

owl_matches_ls <- owl_matches %>%
  reduce(
    ~c(.x, .y$content),
    .init = c()
  )

