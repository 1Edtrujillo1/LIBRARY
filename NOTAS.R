#general info: https://datatables.net/
#datatable: https://rstudio.github.io/DT/
#column filter: https://rstudio.github.io/DT/008-filter.html
#column filter: https://rstudio.github.io/DT/007-search.html

#' @used Regular Expressions 
 
#(?<=\\[)(.*?)(?=\\]) bring everything between []
#(?<=[.])\\w{0,5} bring everything after a point in a string
#(?<=[.])json bring only de word json after a point in a string
#\\.json$ everything that ends with json
#URLS\.\w{0,5} find URLS follow by any character follow by any word with 0 to 5 letter
#^M.*|URLS.*  find worth that start with M and followed by any character OR the word URLS follow by any character
#^(?!^MATHEMATICS.*).*$ give everythin that is not ^MATHEMATICS.*
#REMOVE A LEVEL OF A LIST A <- unlist(JSON_FILES,recursive = FALSE)