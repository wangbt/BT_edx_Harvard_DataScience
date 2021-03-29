##Sec 1 ##
# Sec1.1
library(dslabs)
library(tidyverse)
library(readxl)
library(readr)

filename <- "murders.csv"
dir <- system.file("extdata",package = "dslabs")
fullpath <- file.path(dir,filename)
file.copy(fullpath,"murders.csv")


dat<- read_csv(filename)


system.file(package ="dslabs")

list.files(path = system.file(package ="dslabs"))

# 5.1.3
wd <- getwd()

# 5.1.4
filename <- "murders.csv"

dir <- system.file("extdata",package = "dslabs")
# provides the full path of the folder containing 
# all the files and directories relevant to the 
# package specified by the package argument.

fullpath <- file.path(dir,filename)


# 5.2
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")

read_lines("murders.csv", n_max = 3)
dat <- read_csv(filename)

View(dat)
data <- read_csv(fullpath)

# 5.3 Excercises
path <- system.file("extdata",package = "dslabs")
file <- list.files(path)
file
data <- read_csv(fullpath)

# 5.4 Downloading files
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/
extdata/murders.csv"
dat <- read_csv(url)
class(dat)
download.file(url,"murders.csv")

# some useful function tempfile, tempdir, useful to 
# download data from internet, not corrupt with data
tmp_filename <- tempfile()
download.file(url,tmp_filename)
dat <- read.csv(tmp_filename)
file.remove(tmp_filename)

filename <- "murders.csv"
dat2 <- read.csv(filename)
class(dat2)
class(dat2$abb)
class(dat2$region)

dat3 <- read.csv(filename,stringsAsFactors = FALSE)
class(dat3)
class(dat3$abb)
class(dat3$region)

# Assesment Q14
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url,col_names  = FALSE)
class(dat)
