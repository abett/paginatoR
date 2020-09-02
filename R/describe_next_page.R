describe_next_page <- function(current_page_content, current_page_meta, f_next_page_url = ~NULL, f_next_page_query = ~list()) {
  next_page_meta <- list()
  if(missing('current_page_content')) return(next_page_meta)
  if(missing('current_page_meta')) return(next_page_meta)
  if(missing('f_next_page_url') & missing('f_next_page_query')) return(next_page_meta)

  if (is.function(f_next_page_url)) {
    np_url <- f_next_page_url(current_page_content, current_page_url = current_page_meta$url)
    if (!is_empty(np_url)) next_page_meta$url <- np_url
  }

  if (is.function(f_next_page_query)) {
    np_query <- f_next_page_query(current_page_content, current_page_query = current_page_meta$query)
    if (is.list(np_query)) next_page_meta$query <- np_query
  }

  return(purrr::compact(next_page_meta))
}
