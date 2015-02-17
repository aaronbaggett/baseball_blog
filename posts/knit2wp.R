if (!require("RWordPress"))
install.packages("RWordPress", repos = "http://www.omegahat.org/R", type = "source")
library(RWordPress)
library(knitr)

# Set username and password:
options(WordpressLogin = c(username = "password"), 
  WordpressURL = "http://baseballwithr.wordpress.com/xmlrpc.php")

# Name of Rmd file to knit:
knit2wp(input = "file_name.Rmd", 
        title = "Post Title", 
        categories = c("R", "pitchRx"),
        shortcode = c(TRUE, TRUE),
        publish = FALSE)

# Navigate browser to edit post within WP
browseURL("http://baseballwithr.wordpress.com/wp-admin/edit.php")
