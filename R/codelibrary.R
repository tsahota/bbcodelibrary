#' @importFrom magrittr %>%
#' @export
magrittr::'%>%'
#' Get most recent commit hash
#'
#' Finds the hash of the most recent commit
#'
get_mrc_hash <- function(){
  apigethash()
}

#' List scripts in BB code library
#' @param language character. Default = all. List scripts of a certain folder or all scripts
#' @export
ls_scripts <- function(language="all"){
    mrc.hash <- get_mrc_hash()
    apigetscriptnames(mrc.hash, language)
}

#' List information about scripts
#'
#' Displays information about selected scripts
#'
#' @param files vector string of file names/paths
#' @param fields vector string of field tags to display
#' @param viewer logical indicating if Rstudio viewer should be used. Default = TRUE
#' @param silent logical run in quiet mode (default=FALSE)
#' @export
info_scripts <- function(files,fields=c("Description"), viewer=TRUE, silent=FALSE){
  if (length(fields) > 0) {
    res <- lapply(files, function(file.name) {
      ## per file
      if(length(grep("/", file.name))==0){
        ext <- sub(".*[.]", "", file.name)
        if(ext=="R"){
          file.name <- file.path("R", file.name)
        }else if (ext=="mod" || ext=="scm"){
          file.name <- file.path("NONMEM", file.name)
        }else{
          stop("File type not recognised. Enter the full path.")
        }
      }
      mrc.hash <- get_mrc_hash()
      s <- apigetscriptcontent(mrc.hash, file.name)
      field.vals <- as.data.frame(lapply(fields, function(field){
        if(length(grep(field, s))!=0){
          line <- s[grepl(field,s)]
          gsub(paste0(".*", field, ": ", ""), "", line)
        }else{
          as.character(NA)
        }
      }))
      names(field.vals) <- fields

      field.vals

    })
    res <- do.call(rbind, res)
  }else{res <- data.frame(row.names = seq_along(files))
  }
  res
  d <- cbind(data.frame(NAME=basename(files),TYPE=sub(".*[.]", "", files)),res)

  if (!silent) {
    if (viewer)
      get("View")(d, "available files")
  }
  invisible(d)
}

#' Preview  file from the BB code library
#'
#' @param name character indicating script in the code library to preview
#'
#' @export
preview_script <- function(name){

  if(length(name)>1) stop("can only preview one file at a time")
  ##if name is not full path, we need the full path
  if(length(grep("/", name))==0){
    ext <- sub(".*[.]", "", name)
    if(ext=="R"){
      fullname <- file.path("R", name)
    }else if (ext=="mod" || ext=="scm"){
      fullname <- file.path("NONMEM", name)
    }else{
      stop("File type not recognised. Try entering the full path.")
    }
  }else{
    fullname <- name
  }
  mrc.hash <- get_mrc_hash()

  s0 <- apigetscriptcontent(mrc.hash,fullname)

  tp <- tempfile()
  writeLines(s0, tp)
  file.show(tp, title=name)

}

#' Copy script from the BB code library to project directory
#'
#' Will search code library and copy script and dependencies into scripts directory.
#' Script will also be stamped with source location, time, user information and the commit hash at the current repository head
#'
#' @param from character. file name or path of file to copy
#' @param to character. file name file to create
#' @param stamp_copy logical. Create a commented timestamp at beginning of file
#' @param overwrite logical. Overwrite 'to' file if exists?
#' @param comment_char character. Comment character
#' @param proj_path character. Default = current working directory. path to tidyproject
#' @export
copy_script <- function(from, to, stamp_copy = TRUE, overwrite = FALSE,
                           comment_char = "#", proj_path = ".") {
  tidyproject::check_if_tidyproject(proj_path)
  if (missing(from))
    stop("need \"from\" argument")
  onlyfrom <- missing(to)
  if (missing(to))
    to <- basename(from)
  if (to != basename(to))
    stop("name must not be a path")
  to_path <- file.path(tidyproject::scripts_dir(proj_path), to)  ## destination path
  if (file.exists(to_path) & !overwrite)
    stop(paste(to_path, "already exists. Rerun with overwrite = TRUE"))

  ##if name is not full path, we need the full path
  if(length(grep("/", from))==0){
    ext <- sub(".*[.]", "", from)
    if(ext=="R"){
      from_path <- file.path("R", from)
    }else if (ext=="mod" || ext=="scm"){
      from_path <- file.path("NONMEM", from)
    }else{
      stop("File type not recognised. Try entering the full path.")
    }
  }else{
    from_path <- from
  }
  mrc.hash <- get_mrc_hash()

  s0 <- apigetscriptcontent(mrc.hash, from_path)
  ## modify text at top of 'from_path'
  if (stamp_copy){
    topmatter <- c(paste0(comment_char, comment_char, " Copied from Bitbucket code library ",
                  from_path), paste0("##  (",
                  Sys.time(), ") by ", Sys.info()["user"]),
                  paste0("## Commit hash: ", mrc.hash))
    s <- append(topmatter, s0)
  }else{
    s <- s0
  }
  writeLines(s, to_path)
  tidyproject::setup_file(to_path)
}

#' Show Code Library
#'
#' @param extn vector string of extensions to include
#' @param fields character vector of fields to extract
#' @param viewer logical indicating if viewer should be used to display results (default=FALSE)
#' @param silent logical indicating if messages should be silenced (default=FALSE)
#' @param return_info logical (default = FALSE). Return data.frame of results (FALSE= returns file names)
#' @export
code_library <- function(extn = NULL, fields = "Description", viewer = TRUE, silent = FALSE,
                            return_info = FALSE) {

  files <- ls_scripts()
  if (!is.null(extn)) {
    file_match <- paste0("\\.(", extn, ")$")
    files <- files[grepl(file_match, files)]
  }

  if (viewer == FALSE & !return_info) {
    return(files)
  }
  info <- info_scripts(files, fields = fields, viewer = viewer, silent = silent)

  if (return_info) {
    if (silent)
      return_ob <- invisible(info) else return_ob <- info
  } else {
    return_ob <- files
  }
  return_ob
}


#' Check for updates on a file
#'
#' Check if a locally copied script has since been updated in the BB code library by using hashes to see the diff
#'
#' @param from character. file name to check
#' @param proj_path character. Default = current working directory. path to tidyproject
#' @export
check_for_updates<- function(from, proj_path="."){
  tidyproject::check_if_tidyproject(proj_path)
  if (missing(from))
    stop("need \"from\" argument")
  #get hash from script
  from_path <- file.path(tidyproject::scripts_dir(proj_path), from)
  local.hash <- gsub(".*Commit hash: |\r.*", "", readr::read_file(from_path))
  if(nchar(local.hash)!=40)
    stop("File commit hash not found. Check that file was copied from the code library and stamped with a commit hash.")
  hash.mrc <- get_mrc_hash()
  #find if it has changed
  changed <- apicheckupdates(mrc.hash, local.hash)
  if(changed){
    print(paste0("The script ", from, " has not changed since it was copied locally."))
  } else {
    print(paste0("The script ", from, " has changed since it was copied locally. Please access Bitbucket Cloud in the browser to see exactly what these changes were."))
  }
}

#' Submit a file via pull request
#'
#' Makes a fork of the BB code library with the QCP user account, then commits the file to that fork, then makes a pull request back to the BB code library
#' Pull request is made with the message
#'
#' @param from character. file name or path of file to submit
#' @param message character. The message to make the pull request with. Include name of user
#' @param overwrite logical. Overwrite the file if it exists in code library already?
#' @param proj_path character. Default = current working directory. path to tidyproject
#' @param forkname character. Default = qcpuserfork. Name of forked repository. Current time is added to name to prevent duplicates
#'
#' @export
submit_script <- function(from, message, language="R", overwrite=FALSE, proj_path=".", forkname="qcpuserfork"){

  tidyproject::check_if_tidyproject(proj_path)
  if (missing(from))
    stop("need \"from\" argument")
  if(missing(message))
    stop("need \"message\" argument")
  from_path <- file.path(tidyproject::scripts_dir(proj_path), from)
  #check if file exists in the code library already
  ls.files <- ls_scripts()
  inlib <- isTRUE(length(grep(from,ls.files))!=0)
  if(inlib && !overwrite)
    stop(paste(from, " already exists in the library. To modify it, rerun with overwrite = TRUE"))

  file.contents <- readr::read_file(from_path)
  if(inlib && overwrite)
    if(length(grep("Copied from Bitbucket code library",file.contents))!=0)
      message("Script is stamped - removing stamp lines before submission. Please check this was done correctly.")
  tp <- tempfile()
  writeLines(readLines(from_path)[-(1:3)],tp)
  file.contents <- readr::read_file(tp)

  #fork the repository
  time <- gsub(":","", Sys.time()) %>%
    substr(., nchar(.)-5, nchar(.))
  forkname <- paste0(forkname ,"at", time)
  apimakefork(forkname)

  #Now, add the new file to this forked repository
  apicommitfile(forkname, fullname, file.contents, message)

  #Finally, make a pull request back to the main code library
  prmade <- apimakepullrequest()
  if(prmade){
    message("File successfully submitted!")
  }
}

