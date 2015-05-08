#' Create a summary file
#' 
#' @export
create_reports_summary3 <- function(directory, file_name = "toc") {
  knitting_env <- new.env()
  assign("directory", directory, knitting_env)
  knitr:::knit(input = system.file("markdown_report.Rmd", package = "reporteR"), 
               output = paste0(file_name, ".md"), envir = knitting_env, quiet = TRUE)
}

#' Create a summary table
#' 
#' @export
create_files_table <- function(directory) {
  repo_files <- list.files(path = directory, recursive = TRUE)
  
  sapply(repo_files, function(single_file) {
    all_lines <- readLines(paste0(directory, single_file))
    title <- pattern_lines("^title:")
    abstract <- pattern_lines("^abstract:")
    sources <- pattern_lines("^source\\(")
    c(title, abstract, sources)
  })
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