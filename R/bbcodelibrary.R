#' R interface to the QCP code library at Bitbucket
#'
#' Access scripts directly from R using the Bitbucket REST API
#'
#' @examples
#' \dontrun{
#' new_project("~/yourdir/yoursubdir") ## Creates new project
#' ls_scripts() ## List all scripts available in the QCP code library
#' code_library() ## See information about all available scripts
#' info_scripts(c("AUC.R", "snap.R"), c("Description", "Author")) ## Get specific information about selected scripts
#' preview_script("AUC.R") ## Preview script from the code library
#' copy_script("AUC.R") ## Copy script from code library into local Scripts directory
#' submit_script("AUC2.R", message="A new scripts - Robert Gray") ## Submit a new script to the code library
#'
#' }
#' @docType package
#' @name bbcodelibrary

NULL


#' Set bbcodelibrary options
set_bbcodelibrary_opts <- function(){
  if(is.null(getOption("mode"))) options("mode"="cloud")

}
