library(rlang)
library(httr)
library(dplyr)
library(purrr)
library(jsonlite)
library(wrapr)

GET_paginated_content <- function(
    url,
    query = list(),
    #describe_next_page = function(req_content, current_page_meta = as.list(match.call(expand.dots = F))) {return(list(0L))}
    f_next_page_url = function(req_content, current_page_url = NULL){return(NULL)},
    f_next_page_query = function(req_content, current_page_query = list()){return(list())},
    verbose = F,
    ...
  ) {

  result_pages <- list()

  previous_page_meta <- list(0L)
  next_page_meta <- list(
    url = url,
    query = compact(query) %?% list(0L)
  )

  tryCatch({
    while (!is_empty(compact(next_page_meta)) & !identical(previous_page_meta, next_page_meta)) {
      if (is_empty(next_page_meta$url)) break
      if (is.na(next_page_meta$url)) break

      page_meta <- next_page_meta

      if (verbose) {
        print(paste0(
          'fetching from', page_meta$url,
          if(!is_empty(page_meta$query)) {
            toJSON(page_meta$query, pretty = F, auto_unbox = T)
          }
        ))
      }

      current_page <- httr::GET(
        url = page_meta$url,
        query = compact(page_meta$query),
        ...
      )

      page_content <- current_page %>%
        content()

      result_pages <- append(result_pages, list(page_content))

      previous_page_meta <- page_meta
      next_page_meta <- page_meta %>%
        modifyList(describe_next_page(page_content, page_meta, f_next_page_url, f_next_page_query), keep.null = F)

      print(toJSON(next_page_meta, pretty = F, auto_unbox = T))
    }

    return(result_pages)

  }, error = function(e) {
    print(e)
    stop(e)
  })
}
