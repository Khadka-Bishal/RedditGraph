#load necessary libraries
library(rvest)
library(xml2)

#fetch the webpage content
url <- 'https://striketracker.ilr.cornell.edu/'
webpage <- read_html(url)

#print the prettified HTML content
cat(html_text(html_node(webpage, xpath = "/*")))

#select elements with class 'info-card' and 'date card' and print
info_cards <- html_nodes(webpage, '.info-card')
date_cards <- html_nodes(webpage, '.date.card')

#print 'info-card' elements
cat(html_text(info_cards))