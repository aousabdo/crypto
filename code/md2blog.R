#! /usr/bin/Rscript 
library(knitr)
library(markdown)

initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
other.name <- paste(sep="/", script.basename, "other.R")

#print(script.name)
#print(script.basename)

resource_dir <- paste(script.basename, "/", sep="")

args <- commandArgs(TRUE)
rmd_name <- args[[1]]

base_name <- strsplit(rmd_name, '\\.')[[1]][[1]]
md_name <- paste(base_name, ".md", sep="")
html_name <- paste(base_name, ".html", sep = "")

knit(rmd_name, md_name) # create markdown file 

get.resource <- function(file_name) {
  full_name <- paste(resource_dir, file_name, sep = "")
  readChar(full_name, file.info(full_name)$size)
}

header <- "<html><head>"
open.css <- "<style>"
css <- get.resource("markdown.css")
close.css <- "</style>"
r_syntax_css <- get.resource("r_syntax.css")
r_syntax_js <- get.resource("r_syntax.js" ) 
twitter_code <- get.resource("twitter.snippet")
google_plus_code <- get.resource("google_plus.snippet")
sharing_code <- get.resource("sharing_buttons.html")
ga_tracking <- get.resource("ga_tracking.snippet")

end_header <- "</head><body>"

#fragment <- renderMarkdown(md_name, renderer = 'HTML', renderer.options = getOption('markdown.HTML.options'))

fragment <- markdownToHTML(md_name, options = getOption('markdown.HTML.options'), fragment.only = TRUE)


disqus_code <- get.resource("disqus.snippet")
footer <- "</body></html>"

html <- paste(header,
              open.css,
              css,
              close.css,
              r_syntax_css,
              r_syntax_js,
              twitter_code,
              ga_tracking, 
              end_header,
              fragment,
              sharing_code, 
              disqus_code,
              google_plus_code,
              footer, 
              sep =" ")

fileConn<-file(html_name)
writeLines(c(html), fileConn)
close(fileConn)