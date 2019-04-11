#sudo apt-get install r-base r-base-dev 
#sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
install.packages(c("curl","RCurl","rsconnect","devtools","tidyverse","conflicted"))

#rmarkdown stuff
install.packages(c("rmarkdown","knitr","caTools"))

#shiny/plotly etc.
install.packages(c("shiny","shinydashboard","plotly","leaflet"))

#various things for data 
install.packages(c("rvest","here","rio","jsonlite"))
#rio::install_formats()

#lmm things
install.packages(c("lme4","arm","broom","nlme"))

#gam things
install.packages(c("mgcv","itsadug"))

#zero inflation models?
install.packages(c("pscl","glmmTMB","glmmAdaptive"))


