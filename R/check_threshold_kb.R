#' Check Algolia Threshold KB
#'
#' @param index_list  as retrieved by algoliar::read_index_list()
#' @param threshold_kb  (default: 10), 10 KB is threshold for free Algolia usage,
#' 20 KB for paid for details: https://www.algolia.com/doc/faq/basics/is-there-a-size-limit-for-my-index-records/
#' @return index_list respecting Algolia threshold KB, by reducing the "content"
#' length accordingly
#' @importFrom stringr str_remove_all str_sub str_length
#' @importFrom utils object.size
#' @export
check_threshold_kb <- function(index_list,
                               threshold_kb = 10) {

  objects_size_in_bytes <- as.numeric(stringr::str_remove_all(
    unlist(lapply(index_list, function(x) {
      format(utils::object.size(x), units = "b")})), pattern = "\\sbytes"))

  objects_larger_threshold_kb <- threshold_kb < objects_size_in_bytes / 1024

  if(any(objects_larger_threshold_kb)) {

    for(large_object_idx in which(objects_larger_threshold_kb)) {

      objects_size_in_bytes[large_object_idx]


    bytes_to_reduce <- ceiling(
      1024 * (objects_size_in_bytes[large_object_idx] / 1024 - threshold_kb)
      )

    strings_to_keep <- stringr::str_length(
      index_list[[large_object_idx]]$content) -  bytes_to_reduce

    index_list[[large_object_idx]]$content <- stringr::str_sub(
      index_list[[large_object_idx]]$content,
      start = 1,
      end = strings_to_keep)}

  }

  index_list

}
