#' @import httr

#BB repo username/slug
c.username.slug <- "qcpadmin/qcpcodelibrary"
c.base.url.1 <- file.path("https://api.bitbucket.org/1.0/repositories", c.username.slug)
c.base.url.2 <- file.path("https://api.bitbucket.org/2.0/repositories", c.username.slug)
s.base.url <- "https://stash.rd.astrazeneca.net/rest/api/1.0/projects/~kpcm550/repos/qcptest2"

#Cloud QCP user username and pw
c.username <- "qcpuser"
c.pw <- "Huddersfield2018"
#Server username and pw
s.slug <- ""
s.username <- ""
s.pw <- ""

options("mode"="cloud")

#' Get most recent commit hash through the api
#'
apigethash <- function(){

  if(getOption("mode")=="cloud"){
    r.commit.list <- httr::GET(file.path(c.base.url.2, "commits/master"),
                       authenticate(c.username, c.pw))
    stop_for_status(r.commit.list)
    mrc.hash <- content(r.commit.list, simplifyVector= FALSE)[[2]][[1]]$hash
  } else if (getOption("mode")=="server"){
    r.commit.list <- httr::GET(file.path(s.base.url, "commits/master"),
                       authenticate(s.username, s.pw))
    stop_for_status(r.commit.list)
    mrc.hash <- content(r.commit.list)$id
  } else {
    stop("Invalid mode. Set option mode to cloud or server.")
  }
  mrc.hash
}

#' Get a list of scripts from the BB code library through the api
#' @param mrc.hash character. Hash of the most recent commit
#' @param language character. Default = all. List scripts of a certain folder or all scripts
#'
apigetscriptnames <- function(mrc.hash, language="all"){

  if(getOption("mode")=="cloud"){

    if(language=="all"){
      api.list <- file.path(c.base.url.1,"src", mrc.hash, "/")
    } else {
      api.list <- file.path(c.base.url.1, "src", mrc.hash, language, "/")
    }
    r.ls <- httr::GET(api.list, authenticate(c.username,c.pw))
    stop_for_status(r.ls)
    ls.all <- jsonlite::fromJSON(content(r.ls, "text"), simplifyVector = FALSE)

    if(language=="all"){
      #if looking at all directories we get a list of directories:
      dirs <- ls.all$directories
      ls.files.json <- lapply(dirs, function(x)
        httr::GET(file.path(c.base.url.1, "src" ,mrc.hash, x, "/"), authenticate(c.username, c.pw)))
      ls.files.all <- lapply(ls.files.json, function(x)
        jsonlite::fromJSON(content(x, "text"), simplifyVector = FALSE))
      ls.names <- lapply(ls.files.all, function(y)
        lapply(y[[4]], function(x)
          x$path))
      ls.names <- unlist(ls.names)
    } else {
      ls.names <- unlist(lapply(ls.all[[4]], function(x)
        x$path))
    }
  } else if(getOption("mode")=="server"){

    if(language=="all"){
      api.list <- file.path(s.base.url, paste0("files?", mrc.hash, "&limit=1000"))
    }else{
      api.list <- file.path(s.base.url, "files", paste0(language, "?", mrc.hash, "&limit=1000"))
    }
    r.ls <- httr::GET(api.list, authenticate(s.username,s.pw))
    stop_for_status(r.ls)
    ls.names <- unlist(content(r.ls)$values)
  }else {
    stop("Invalid mode. Set option mode to cloud or server.")
  }
  ls.names
}

#' Get the content of a single scripts
#'
#' @param mrc.hash character. Hash of the most recent commit
#' @param file.name character. The full path of the file to get contents of
#'
apigetscriptcontent <- function(mrc.hash, file.name){

  if(getOption("mode")=="cloud"){

    from.api.url <- file.path(c.base.url.1, "raw", mrc.hash, file.name)
    r.get <- httr::GET(from.api.url, authenticate(c.username,c.pw))
    stop_for_status(r.get)
    cont <- content(r.get)
    if(length(cont)>0){
      s<- unlist(strsplit(cont, "\n"))
    } else{
      s <- ""
    }
  } else if(getOption("mode")=="server"){

    from.api.url <- paste0(file.path(s.base.url,"browse",file.name),"?",mrc.hash,"&raw")
    r.get <- httr::GET(from.api.url, authenticate(s.username,s.pw))
    stop_for_status(r.get)
    s <-   unlist(lapply(content(r.get)$lines, function(line){
      line$text
    }))
  }else {
    stop("Invalid mode. Set option mode to cloud or server.")
  }
  s
}

#' Check if a file has been updates since it was copied
#'
#' @param mrc.hash character. Hash of the most recent commit
#' @param local.hash character. Hash from the copied file, corresponding to the most recent commit when it was copied
#' @param file character. The full path of the the file to check
apicheckupdates <- function(mrc.hash, local.hash, file){

  if(getOption("mode")=="cloud"){
    hash.concat <- paste(mrc.hash, local.hash, sep="..")
    api.diff <- file.path(c.base.url.2, "diff", hash.concat)
    r.diff <- httr::GET(api.diff, authenticate(c.username, c.pw))
    stop_for_status(r.diff)
    diff.result <- content(r.diff, encoding="UTF-8")
    if(length(grep(file,diff.result))==0){
       FALSE
    }else{
       TRUE
    }
  }else if(getOption("mode")=="server"){
    ##if name is not full path, we need the full path
    if(length(grep("/", file))==0){
      ext <- sub(".*[.]", "", file)
      if(ext=="R"){
        fullname <- file.path("R", file)
      }else if (ext=="mod" || ext=="scm"){
        fullname <- file.path("NONMEM", file)
      }else{
        stop("File type not recognised. Try entering the full path.")
      }
    }else{
      fullname <- file
    }
    api.diff <-paste0(file.path(s.base.url., "diff", fullname), "?since=", local.hash, "&until=", mrc.hash)
    r.diff <- httr::GET(api.diff, authenticate(username, pw))
    stop_for_status(r.diff)
    if(length(content(r.diff)$diffs)==0){
      FALSE
    }else{
      TRUE
    }
  }else{
      stop("Invalid mode. Set option mode to cloud or server.")
    }

}

#' Make a new fork of the code library
#'
#' @param forkname character. The name to give the new fork
#'
apimakefork <- function(forkname){

  if(getOption("mode")=="cloud"){

    body.fk <- list("name"=forkname)
    r.fk <- httr::POST(file.path(c.base.url.1, "fork"),body=body.fk, encode="form", authenticate(c.username,c.pw))
    stop_for_status(r.fk)

  }else if(getOption("mode")=="server"){

    body.fk <- list("name"=forkname)
    r.fk <- httr::POST(s.base.url ,body=body.fk, encode="json", authenticate(s.username,s.pw))
    stop_for_status(r.fk)

  } else {
    stop("Invalid mode. Set option mode to cloud or server.")
  }
}

#' Commit a file to a forked repository
#'
#' @param forkname character Name of the fork to make the commit to
#' @param filename character Name of the file to commit or change
#' @param contents character Contents of the new file
#' @param message character Commit message, include your name
#'
apicommitfile <- function(forkname, filename, contents, message){

  if(getOption("mode")=="cloud"){
    api.cmt <- file.path("https://api.bitbucket.org/2.0/repositories", c.username, forkname,"src")
    body.cmt <-list("message"=message)
    body.cmt[[filename]] <- contents
    r.cmt <- httr::POST(api.cmt, body = body.cmt, encode = "form", authenticate(c.username,c.pw))
    if(r.cmt$status_code==403)
      Sys.sleep(5)
    r.cmt <- httr::POST(api.cmt, body = body.cmt, encode = "form", authenticate(c.username,c.pw))
    stop_for_status(r.cmt)


  }else if(getOption("mode")=="server"){

    stop("Committing files with Bitbucket Server is not currently working. Check back later.")
    #api.put <- file.path(s.base.url, "browse", "R/testE.R")
    #body.put <- list("message"="A test message", "branch"="master", "content"=httr::upload_file(tp))
    #r.put <- httr::PUT(api.put, body = body.put, encode = "multipart", authenticate(s.username, s.pw))

  }else{
    stop("Invalid mode. Set option mode to cloud or server.")
  }
}

#' Make a pull request through the API
#'
#' Make a pull request from a fork to the main code library repository
#'
#' @param forkname character. The name of the fork to make the request from
#' @param message character. A message explaining what the pull request is for which goes as the title of the pull request
#'
apimakepullrequest <- function(forkname, message){

  if(getOption("mode")=="cloud"){

    api.pr <- file.path(c.base.url.2, "pullrequests")
    repo.name <- file.path(c.username, forkname)
    body.pr = list("source"=list("repository"=list("type"="repository", "full_name"=repo.name),
                                 "branch"=list("name"="master")),
                   "title"=message)
    r.pr <- httr::POST(api.pr, body=body.pr,encode = "json", authenticate(c.username,c.pw))
    if(r.pr$status_code==403)
      Sys.sleep(5)
    r.pr <- httr::POST(api.pr, body=body.pr,encode = "json", authenticate(c.username,c.pw))

    stop_for_status(r.pr)
    if(r.pr$status_code<300){
      TRUE
    }
  } else if(getOption("mode")=="server"){

    api.pr <- file.path(s.base.url, "pull-requests")
    body.pr = list("title"=message,
                   "toRef"=list("id"="master" ,"repository"=list("project"=list("key"=paste0("~",s.username)),"slug"=s.slug)),
                   "fromRef"=list("id"="master", "repository"=list("project"=list("key"=paste0("~", s.username)),"slug"=forkname))
                   )
    r.pr <- httr::POST(api.pr, body=body.pr, encode="json", authenticate(s.username, s.pw))
    stop_for_status(r.pr)
    if(r.pr$status_code<300){
      TRUE
    }
  } else{
    stop("Invalid mode. Set option mode to cloud or server.")
  }
}


#' Permanently delete a fork
#' Use for cleaning up forks made after files have been committed
#'
#' @param forkname character. Name of the fork to delete
#'
#'@export
apideletefork <- function(forkname){

  if(getOption("mode")=="cloud"){

    api.d <- file.path("https://api.bitbucket.org/2.0/repositories", c.username, forkname)
    r.d <- httr::DELETE(api.d, authenticate(c.username, c.pw))
    stop_for_status(r.d)
    if(status_code(r.d)==204){
      message("Fork successfully deleted!")
    }
  } else if(getOption("mode")=="server") {

    api.d <- file.path("https://stash.rd.astrazeneca.net/rest/api/1.0/projects", paste0("~", s.username), "repos",forkname)
    r.d <- httr::DELETE(api.d, authenticate(s.username, s.pw))
    stop_for_status(r.d)
    if(status_code(r.d)==202){
      message("Fork successfully deleted!")
    }
  } else {
    stop("Invalid mode. Set option mode to cloud or server.")

  }
}
