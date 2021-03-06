#' Create Algolia Configuration
#'
#' @param path_index_json path to index.json (default: "public/index.json")
#' @param app_id app_id Application ID. This is your unique application identifier.
#' It's used to identify you when using Algolia's API. (see: create a API key
#' with valid scope in the Algolia Dashboard), (default: Sys.getenv("algolia_app_id"))
#' @param api_key api_key (default: Sys.getenv("algolia_api_key"),
#' <https://www.algolia.com/doc/api-client/methods/api-keys/>
#' @param index_name name of Algolia index name (default:
#' Sys.getenv("algolia_index_name"))
#' @seealso <https://www.algolia.com/doc/guides/getting-started/quick-start/tutorials/getting-started-with-the-dashboard/>
#' @return list with algolia config
#' @importFrom httr add_headers
#' @export
#' @examples
#' \dontrun{
#' ########################################################################
#' ### Option A) Interactive R Session
#' ########################################################################
#'
#' config <- create_config(path_index_json = "path_to_your_index.json",
#'                         app_id = "your_algolia_application_id",
#'                         api_key = "your_algolia_api_key",
#'                         index_name = "your_algolia_index_name")
#'
#' ########################################################################
#' ### Option B) Environment variable
#' ########################################################################
#'
#' Sys.setenv(algolia_app_id = "your_algolia_application_id",
#'            algolia_api_key = "your_algolia_api_key",
#'            algolia_index_name = "your_algolia_index_name")
#'
#'
#' ### With default path to index.json ("public/index.json")
#' ### (i.e standard output path after running blogdown::build_site() for
#' ### theme hugo-academic (https://github.com/gcushen/hugo-academic/)
#'
#' config <- create_config()
#'
#' ### With user defined path or url to index.json "public/index.json"
#'
#' config <- create_config(path_index_json = "https://mhugoacademicsite.de/index.json")
#'
#'}
create_config <- function(path_index_json = "public/index.json",
                          app_id = Sys.getenv("algolia_app_id"),
                          api_key = Sys.getenv("algolia_api_key"),
                          index_name = Sys.getenv("algolia_index_name")) {


  list("path_index_json" = path_index_json,
       "index_name" = index_name,
       "app_id" = app_id,
       "api_key" = api_key,
       "api_url" = sprintf("https://%s.algolia.net/1/indexes/%s",
                           app_id,
                           index_name),
       "api_config" = httr::add_headers("X-Algolia-Application-Id" = app_id,
                                       "X-Algolia-API-Key" = api_key))

}

#' Algolia "GET"
#'
#' @param config config as retrieved by create_config() (default:
#' create_config())
#' @export
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' algolia_get()
#'}
algolia_get <- function (config = create_config()) {

  httr::GET(url = config$api_url, config = config$api_config)
}

#' Get Algolia "content"
#'
#' @param config config as retrieved by create_config() (default:
#' create_config())
#' @return data.frame with content of Algolia index
#' @export
#' @importFrom httr content GET
#' @examples
#' \dontrun{
#' algolia_content()
#'}
algolia_content <- function (config = create_config()) {

response <- httr::content(x = algolia_get(config),
                          as="text")
jsonlite::fromJSON(response)
}


#' Delete Algolia Index
#'
#' @param config config as retrieved by create_config() (default:
#' create_config())
#' @return deletes "index_name" (config$index_name) at Algolia URL (config$api_url)
#' in case that provided API Key has sufficient rights!
#' @export
#' @importFrom httr DELETE
#' @seealso  <https://www.algolia.com/doc/api-client/methods/api-keys/>
#' @examples
#' \dontrun{
#' algolia_content()
#'}
algolia_delete_index <- function(config = create_config()) {

  httr::DELETE(url = config$api_url,
               config = config$api_config)
}

#' Helper Function: Check If Path or URL
#'
#' @param path path/url to be checked
#' @return TRUE (if existing) or FALSE (if not existing)
#' @importFrom fs file_exists
#' @importFrom RCurl url.exists
#' @examples
#' \dontrun{
#' check_if_path_or_url(path = "public/index.json")
#' }
#' @keywords internal
#' @noRd
check_if_path_or_url <- function(path) {

  path_arg_name <- deparse(substitute(path))
  is_file <- as.logical(fs::file_exists(path))
  is_url <- RCurl::url.exists(path)

  if(!(is_file | is_url))  {
    msg <- sprintf(paste("Argmuent '%s' = '%s' is neither an existing",
                         "'path' or 'url'!\n Please fix it by providing a",
                         "correct one with an 'index.json'"),
                   path_arg_name, path)
    stop(msg)
  }
}


#' Read Index.Json
#'
#' @param path path or url to index.json (default: "public/index.json")
#' @return imported index.json
#' @importFrom jsonlite fromJSON toJSON
#' @export
#' @examples
#' \dontrun{
#' index_json <- read_index_json(path = "public/index.json")
#' }
#'
read_index_json <- function(path = "public/index.json") {

  check_if_path_or_url(path)

  jsonlite::toJSON(jsonlite::fromJSON(path),
                   auto_unbox = TRUE,
                   pretty = TRUE)

}

#' Read Index.Json As List
#'
#' @param path path or url to index.json (default: "public/index.json")
#' @param encoding encoding (default: "UTF-8")
#' @param ... additional arguments passed to jsonlite::read_json()
#' @return imported index.json as list
#' @importFrom jsonlite read_json
#' @export
#' @examples
#' \dontrun{
#' index_list <- read_index_list(path = "public/index.json")
#' }
#'
read_index_list <- function(path = "public/index.json",
                            encoding = "UTF-8",
                            ...) {

  check_if_path_or_url(path)

  jsonlite::read_json(path, encoding = encoding, ...)

}



#' Create addObject list
#' @description prepare json for API endpoint "addObject"
#' @param index_list as retrieved by read_index_list()
#' @return list as required for API endpoint "addObject"
#' @keywords internal
#' @noRd
#' @seealso <https://www.algolia.com/doc/api-reference/api-methods/add-objects/>
#' @examples
#' \dontrun{
#' index_list <- read_index_list(path = "public/index.json")
#' create_addObject(index_list)
#' }
create_addObject <- function(index_list) {

  lapply(seq_along(index_list), function(i)  {
  sel <- index_list[[i]]
  if(grepl(pattern = "^(\\s)?index.utf8.md", sel$summary)) {
    sel$summary <- ""
  }
  if(grepl(pattern = "^(\\s)?index.utf8.md", sel$content)) {
    sel$content <- ""
  }


  list(action = "addObject",
       body = sel)}
  )
}

#' Pepare Batch Json
#'
#' @param config config as retrieved by create_config() (default:
#' create_config())
#' @param threshold_kb  (default: 10), 10 KB is threshold for free Algolia usage,
#' 20 KB for paid for details: https://www.algolia.com/doc/faq/basics/is-there-a-size-limit-for-my-index-records/
#' @param encoding encoding (default: "UTF-8")
#' @param ... additional arguments passed to jsonlite::read_json()
#' @examples
#' \dontrun{
#' batch_json <- prepare_batch_json()
#' }
#' @export
prepare_batch_json <- function(config = create_config(),
                               threshold_kb = 10,
                               encoding = "UTF-8", ...) {


  index_list <- read_index_list(path = config$path_index_json,
                                encoding = encoding, ...)

  index_list <- check_threshold_kb(index_list, threshold_kb = threshold_kb)

  requests <- list("requests" = create_addObject(index_list))

  jsonlite::toJSON(requests,
                 auto_unbox = TRUE,
                 pretty = TRUE)
}



#' Performs Algolia Post Batch
#'
#' @param config config as retrieved by create_config() (default:
#' create_config())
#' @param threshold_kb  (default: 10), 10 KB is threshold for free Algolia usage,
#' 20 KB for paid for details: https://www.algolia.com/doc/faq/basics/is-there-a-size-limit-for-my-index-records/
#'
#' @return performs batch operation at provided Algolia url (config$api_url)
#' in case of sufficient rights of the provided API key (see:
#' <https://www.algolia.com/doc/api-client/methods/api-keys/>)
#' @export
#' @importFrom httr content POST
#' @seealso <https://www.algolia.com/doc/rest-api/search/#batch-write-operations>
#' @examples
#' \dontrun{
#' algolia_post_batch()
#' }
algolia_post_batch <- function(config = create_config(),
                               threshold_kb = 10) {

  batch_json <- prepare_batch_json(config, threshold_kb)

  tmp <- httr::POST(sprintf("%s/batch", config$api_url),
                    config = config$api_config,
                    body = batch_json,
                    verbose = TRUE)

  httr::content(tmp)

}


