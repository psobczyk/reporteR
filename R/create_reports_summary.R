#' Create a summary file
#' 
#' @export
create_reports_summary <- function(directory = "./", file_name = "toc") {
  knitting_env <- new.env()
  assign("directory", paste0(getwd(), "/", directory), knitting_env)
  output <-  paste0(getwd(), "/", directory, file_name, ".html")
  rmarkdown::render(input = system.file("markdown_report.Rmd", package = "reporteR"),  
                    output_format = "html_document", output_file = output,
                    encoding="UTF-8", quiet = TRUE, envir = knitting_env)
}

#' Create a summary table
#' 
#' @export
create_files_table <- function(directory) {
  repo_files <- list.files(path = directory, recursive = TRUE)
  sapply(repo_files, function(single_file) {
    all_lines <- readLines(paste0(directory, "/", single_file))
    title <- pattern_lines("^title:", all_lines)
    abstract <- pattern_lines("^abstract:", all_lines)
    sources <- pattern_lines("^source\\(", all_lines)
    c(title, abstract, sources)
  })
}
    
#' Create a summary table with DT package
#' 
#' @export
create_files_table_DT <- function(directory) {
  library(DT)
  setwd(directory)
  files <- list.files(path = directory, pattern = "*.html$", recursive = TRUE)  
  df <- data.frame()
  for(file in files){
    fileRmd <- paste0(substr(file, 1, nchar(file)-5), ".Rmd")
    if(file.exists(fileRmd)){
      all_lines <- readLines(fileRmd)
      abstract <- pattern_line("^abstract:", all_lines)
      title <- pattern_line("^title:", all_lines)
      if(title==" ") title <- "None"
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
  df$Source <-  paste0('<a href="', df$fileRmd, '">', "Download", '</a>')
  df$fileRmd <- NULL
  df$FileName <- rownames(df)
  df$FileName <- gsub(x= df$FileName, "/", "/\n")
  colnames(df) <- c("Size", "Modification time", "Abstract", "Title", "Source", "Filename")
  rownames(df) <- paste0('<a href="', rownames(df), '">', df$Title, '</a>')
  df <- df[c("Abstract", "Modification time", "Size", "Source", "Filename")]
  datatable(df, options = list(iDisplayLength = 5), escape = FALSE)
}


#' Create a summary table of source files with DT package
#' 
#' @export
create_files_table_DT_source <- function(directory){
  library(DT)
  setwd(directory)
  files <- list.files(path = directory, pattern = "*.R$", recursive = TRUE)
  df <- data.frame()
  for(file in files){
    df <- rbind(df, file.info(file))
  }
  if(nrow(df)>0){
    df$mtime <- format(df$mtime, "%Y-%m-%d %H:%M")
    df$ctime <- format(df$ctime, "%Y-%m-%d")
    df$atime <- format(df$atime, "%Y-%m-%d")
    df$ctime <- NULL
    df$atime <- NULL
    df$mode <- NULL
    df$isdir <- NULL
    df$uid <- NULL
    df$gid <- NULL
    df$uname <- NULL
    df$grname <- NULL
    colnames(df) <- c("Size", "Modification time")
    rownames(df) <-paste0('<a href="', rownames(df), '">', rownames(df), '</a>')
    datatable(df, options = list(iDisplayLength = 5), escape = FALSE)
  }
}

pattern_line <- function(pattern, all_lines) {
  source_lines <- grep(pattern, all_lines, fixed = FALSE)
  if(length(source_lines) > 0) {
    vapply(all_lines[source_lines], function(single_line)
      strsplit(single_line, pattern, fixed = FALSE)[[1]][[2]], "a")
  } else {
    "None"
  }
}


pattern_lines <- function(pattern, all_lines) {
  source_lines <- grep(pattern, all_lines, fixed = FALSE)
  source_file_names <- if(length(source_lines) > 0) {
    vapply(all_lines[source_lines], function(single_line)
      strsplit(strsplit(single_line, pattern, fixed = FALSE)[[1]][[2]],
               '"')[[1]][[1]], "a")
  } else {
    "None"
  }
}
