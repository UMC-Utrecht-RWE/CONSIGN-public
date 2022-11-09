#Packages needed for CONSIGN


if(!require(RSQLite)){install.packages("RSQLite")}
suppressPackageStartupMessages(library(RSQLite))

if(!require(survival)){install.packages("survival")}
suppressPackageStartupMessages(library(survival))


if(!require(sqldf)){install.packages("sqldf")}
suppressPackageStartupMessages(library(sqldf))

if(!require(sandwich)){install.packages("sandwich")}
suppressPackageStartupMessages(library(sandwich))

if(!require(lmtest)){install.packages("lmtest")}
suppressPackageStartupMessages(library(lmtest))

if(!require(rlist)){install.packages("rlist")}
suppressPackageStartupMessages(library(rlist))

if(!require(stringr)){install.packages("stringr")}
suppressPackageStartupMessages(library(stringr))

if(!require(dplyr)){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))

if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))

if(!require(lubridate)){install.packages("lubridate")}
suppressPackageStartupMessages(library(lubridate))

if(!require(tidyverse)){install.packages("tidyverse")}
suppressPackageStartupMessages(library(tidyverse))

if(!require(tidyselect)){install.packages("tidyselect")}
suppressPackageStartupMessages(library(tidyselect))

if(!require(reshape)){install.packages("reshape")}
suppressPackageStartupMessages(library(reshape))

if(!require(zoo)){install.packages("zoo")}
suppressPackageStartupMessages(library(zoo))
