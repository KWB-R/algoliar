# devtools::install_github("KWB-R/kwb.orcid")
#
#install.packages("remotes", repos = "https://cloud.r-project.org")
# Workaround for issue https://github.com/KWB-R/kwb.pkgbuild/issues/21
# Requires that R 64bit is used on Windoows
remotes::install_github("kwb-r/kwb.pkgbuild", INSTALL_opts =  c('--no-multiarch'))

library(magrittr)

package <- "algoliar"

#
# author <- list(
#   name = "Michael Rustler",
#   orcid = kwb.orcid::get_kwb_orcids()["Michael Rustler"],
#   url = "https://mrustl.de"
# )

pkg <- list(name = "algoliar",
title = "Simple Access to Algolia REST API (https://www.algolia.com/doc/rest-api/search/)",
desc = paste0("Simple Access to Algolia REST API ",
"(https://www.algolia.com/doc/rest-api/search/)."))



setwd(package_dir)

kwb.pkgbuild::use_pkg_skeleton(pkg$name)
kwb.pkgbuild::use_pkg(pkg = pkg,
  version = "0.0.0.9000",
  stage = "experimental"
)




deps$remotes <- stringr::str_split(deps$depends, ", ", simplify = TRUE) %>%
  as.character() %>% stringr::str_subset("^kwb")
desc::desc_add_remotes(stringr::str_c("github::", deps$remotes))



usethis::use_r("function")

pkg_dependencies <- c("digest", "kwb.fakin", "kwb.utils", "yaml")

sapply(pkg_dependencies, usethis::use_package)

# And now, let's do the first commit and upload everything to GitHub
# (manually)...
