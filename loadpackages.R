### function to load/install multiple packages
### courtesy of stevenworthington @ https://gist.github.com/stevenworthington/3178163
loadpackages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}