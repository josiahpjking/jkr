#sudo apt-get install r-base r-base-dev
#sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
install.packages(c("curl","RCurl","rsconnect","devtools","roxygen2","tidyverse","conflicted","readxl","rvest","jsonlite","httr","janitor","foreign","furrr","xml2"))

instal.packages(c("lubridate","padr"))

install.packages(c("gghighlight","gganimate","ggridges","cowplot","viridis"))

#rmarkdown stuff
install.packages(c("rmarkdown","knitr","kableExtra","caTools","blogdown"))

#shiny/plotly etc.
install.packages(c("shiny","shinydashboard","plotly","leaflet"))

#lmm things
install.packages(c("lme4","arm","broom","nlme","blme"))

#gam things
install.packages(c("mgcv","itsadug"))

#nnets and multinoms
install.packages("neuralnet")

#bayes
install.packages(c("rstan","rstanarm","tidybayes","rethinking"))

#audio
install.packages(c("tuneR"))

#spatial etc
install.packages(c("spdep"))


devtools::install_github("josiahpjking/jkr")
devtools::install_github("josiahpjking/DasGuptR")
devtools::install_github("dalejbarr/clusterperm")
devtools::install_github("dalejbarr/exchangr")
