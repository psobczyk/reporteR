
#' Create a summary file
#' 
#' 
create_reports_summary <- function(directory, type="html"){
  file.create("toc.Rmd")
  currentDir <- getwd()
  newDir <- directory
  text <- "
```{r, echo=FALSE, warning=FALSE}
files <- list.files(path = \".\", pattern = \"*.html$\", recursive = TRUE)

df <- data.frame()
for(file in files){
fileRmd <- paste0(substr(file, 1, nchar(file)-5), \".Rmd\")
if(file.exists(fileRmd)){
  all_lines <- readLines(fileRmd)
  source_lines <- grep(\"^abstract:\", all_lines, fixed = FALSE)
  single_line <- all_lines[source_lines]
  if(length(single_line)>0){
  abstract <- strsplit(single_line, 'abstract: ', fixed = TRUE)[[1]][2]
  } else{
  abstract <- \"\"
  }
  source_lines <- grep(\"^title:\", all_lines, fixed = FALSE)
  single_line <- all_lines[source_lines]
  if(length(single_line)>0){
  title <- strsplit(single_line, 'title: ', fixed = TRUE)[[1]][2]
  } else{
  title <- \"None\"
  }
} else{
  abstract <- \"\"
  title <- \"None\"
}
names(abstract) <- \"abstract\"
names(title) <- \"title\"
df <- rbind(df, cbind(file.info(file), fileRmd, abstract, title))
}

df$mtime <- format(df$mtime, \"%Y-%m-%d %H:%M\")
df$ctime <- format(df$ctime, \"%Y-%m-%d\")
df$atime <- format(df$atime, \"%Y-%m-%d\")
df$size <- paste(trimws(format(df$size/1024, digits = 1)), \"kB\")
df$ctime <- NULL
df$atime <- NULL
df$mode <- NULL
df$isdir <- NULL
df$uid <- NULL
df$gid <- NULL
df$uname <- NULL
df$grname <- NULL
df$Source <-  paste0('[', \"Download\", '](', getwd(), '/', df$fileRmd, ')')
df$fileRmd <- NULL
df$FileName <- rownames(df)
colnames(df) <- c(\"Size\", \"Modification time\", \"Abstract\", \"Title\", \"Source\", \"Filename\")
rownames(df) <- paste0('[', df$Title, '](', getwd(), '/', rownames(df), ')')
df <- df[c(\"Abstract\", \"Source\", \"Filename\", \"Modification time\", \"Size\")]
```

#### Reports

```{r, echo=FALSE, results='asis'}
library(xtable)
print.xtable(xtable(df), type = \"html\", sanitize.rownames.function = function(x) x)
```

----

----

#### Source files

```{r, echo=FALSE, results='asis'}
files <- list.files(path = \".\", pattern = \"*.R$\", recursive = TRUE)

df <- data.frame()
for(file in files){
# all_lines <- readLines(file)
# source_lines <- grep(\"^abstract:\", all_lines, fixed = FALSE)
# single_line <- all_lines[source_lines]
# if(length(single_line)>0){
# abstract <- strsplit(single_line, 'abstract: ', fixed = TRUE)[[1]][2]
# } else{
# abstract <- \"\"
# }
# names(abstract) <- \"abstract\"
# source_lines <- grep(\"^title:\", all_lines, fixed = FALSE)
# single_line <- all_lines[source_lines]
# if(length(single_line)>0){
# title <- strsplit(single_line, 'title: ', fixed = TRUE)[[1]][2]
# } else{
# title <- \"None\"
# }
# names(title) <- \"title\"
# df <- rbind(df, cbind(file.info(file), abstract, title))
df <- rbind(df, file.info(file))
}

if(nrow(df)>0){
df$mtime <- format(df$mtime, \"%Y-%m-%d %H:%M\")
df$ctime <- format(df$ctime, \"%Y-%m-%d\")
df$atime <- format(df$atime, \"%Y-%m-%d\")
df$ctime <- NULL
df$atime <- NULL
df$mode <- NULL
df$isdir <- NULL
df$uid <- NULL
df$gid <- NULL
df$uname <- NULL
df$grname <- NULL
colnames(df) <- c(\"Size\", \"Modification time\")
rownames(df) <- paste0('[', rownames(df), '](', getwd(), '/', rownames(df), ')')
print.xtable(xtable(df), type = \"html\", sanitize.rownames.function = function(x) x)
} else{
print(\"No source files found\")
}
```
  "
  prevText <- paste0("`r library(knitr); opts_knit$set(root.dir = \"", newDir, "\")`\n")
  fileConn<-file("toc.Rmd")
  print(paste(text))
  writeLines(paste(prevText, text), fileConn)
  close(fileConn)
  print(newDir)
  setwd(newDir)
  setwd(currentDir)
  knit("toc.Rmd")
  pandoc("toc.md")
  file.remove("toc.Rmd")
  file.remove("toc.md")
  browseURL("toc.html")
  
  
}