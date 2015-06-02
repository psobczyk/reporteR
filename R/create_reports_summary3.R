#' Create a summary file
#' 
#' @export
create_reports_summary3 <- function(directory = "./", file_name = "toc") {
  knitting_env <- new.env()
  assign("directory", directory, knitting_env)
  knitr:::knit(input = system.file("markdown_report.Rmd", package = "reporteR"), 
               output = paste0(file_name, ".md"), envir = knitting_env, quiet = TRUE)
  output <-  paste0(getwd(), "/", directory, file_name, ".html")
  rmarkdown::render(input = system.file("markdown_report.Rmd", package = "reporteR"),
                    output_format = "html_document", output_file = output,
                    encoding="UTF-8", quiet = TRUE)
}

#' Create a summary table
#' 
#' @export
create_files_table <- function(directory) {
  repo_files <- list.files(path = directory, recursive = TRUE)
  
  sapply(repo_files, function(single_file) {
    all_lines <- readLines(paste0(directory, single_file))
    title <- pattern_lines("^title:", all_lines)
    abstract <- pattern_lines("^abstract:", all_lines)
    sources <- pattern_lines("^source\\(", all_lines)
    c(title, abstract, sources)
  })
}
    
#' Create a summary table
#' 
#' @export
create_files_table_DT <- function(directory) {
  library(DT)
  library(xtable)
  files <- list.files(path = directory, pattern = "*.html$", recursive = TRUE)
  
  df <- data.frame()
  for(file in files){
    fileRmd <- paste0(substr(file, 1, nchar(file)-5), ".Rmd")
    if(file.exists(fileRmd)){
      all_lines <- readLines(fileRmd)
      source_lines <- grep("^abstract:", all_lines, fixed = FALSE)
      single_line <- all_lines[source_lines]
      if(length(single_line)>0){
        abstract <- strsplit(single_line, 'abstract: ', fixed = TRUE)[[1]][2]
      } else{
        abstract <- ""
      }
      source_lines <- grep("^title:", all_lines, fixed = FALSE)
      single_line <- all_lines[source_lines]
      if(length(single_line)>0){
        title <- strsplit(single_line, 'title: ', fixed = TRUE)[[1]][2]
      } else{
        title <- "None"
      }
    } else{
      abstract <- ""
      title <- "None"
    }
    names(abstract) <- "abstract"
    names(title) <- "title"
    df <- rbind(df, cbind(file.info(file), fileRmd, abstract, title))
  }
  
  df$mtime <- format(df$mtime, "%Y-%m-%d %H:%M")
  df$ctime <- format(df$ctime, "%Y-%m-%d")
  df$atime <- format(df$atime, "%Y-%m-%d")
  df$size <- paste(trimws(format(df$size/1024, digits = 1)), "kB")
  df$ctime <- NULL
  df$atime <- NULL
  df$mode <- NULL
  df$isdir <- NULL
  df$uid <- NULL
  df$gid <- NULL
  df$uname <- NULL
  df$grname <- NULL
  df$Source <-  paste0('<a href="', getwd(), '/', df$fileRmd, '">', "Download", '</a>')
  df$fileRmd <- NULL
  df$FileName <- rownames(df)
  colnames(df) <- c("Size", "Modification time", "Abstract", "Title", "Source", "Filename")
  rownames(df) <- paste0('<a href="', rownames(df), '">', df$Title, '</a>')
  df <- df[c("Abstract", "Source", "Filename", "Modification time", "Size")]
  datatable(df, options = list(iDisplayLength = 5), escape = FALSE)
}


pattern_lines <- function(pattern, all_lines) {
  source_lines <- grep(pattern, all_lines, fixed = FALSE)
  source_file_names <- if(length(source_lines) > 0) {
    vapply(all_lines[source_lines], function(single_line)
      strsplit(strsplit(single_line, pattern, fixed = TRUE)[[1]][[2]],
               '\"')[[1]][[1]], "a")
  } else {
    ""
  }
}
