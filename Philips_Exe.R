# Date: Sep 25, 2018
# Updated On: Sep 25, 2018
# Author: Yiran Jia
# Project Name:  Customer Behavior Segmentation Project 
#
# This code is for Leadership Catalyst Pulling and Cleaning 


# loading packages 
library(dplyr)
library(sjmisc)
library(gsubfn)
library(zoo)
library(ggplot2)
library(modelr)
library(splines)
library(hexbin)
library(stringr)
library(plotly)
library(DT)
library(reshape2)
library(bpa)
library(stringr)
library(tidyverse)
library(scales)
library(rmarkdown)

# input data 
exe <- read.csv("Philips_Exe.csv", stringsAsFactors = FALSE, na.strings="") %>% 
  filter(!is.na(NETWORK_ID)) %>% 
  select(NETWORK_ID, NETWORK_NAME, TITLE)

# function to add new columns based on commnet content
# looking for CMIO, CNIO, CPHO, Clinical CEO
unlock_comment <- function(dataset, col_title, col_work, phrase) {
  namevector <- col_work
  dataset[,namevector] <- NA
  
  change <- function(x) {
    if(grepl(phrase, x[col_title], ignore.case = TRUE)) {
      x[col_work] <- 1
    } else {
      x[col_work] <- 0
    }
  }
  
  answer <- as.data.frame(apply(dataset, 1, change))
  dataset[col_work] <- answer 
  return(dataset)
}

exe <- unlock_comment(exe, "TITLE", "CMIO", "Medical Infor")
exe <- unlock_comment(exe, "TITLE", "CNIO", "Nursing Infor")
exe <- unlock_comment(exe, "TITLE", "CPHO", "Population Health")
exe <- unlock_comment(exe, "TITLE", "CCO", "Chief Clinical Officer")

exe <- exe %>% group_by(NETWORK_ID,  NETWORK_NAME) %>% summarize(sum_cmio = sum(CMIO),
                                                   sum_cnio = sum(CNIO),
                                                   sum_cpho = sum(CPHO),
                                                   sum_cco = sum(CCO))
nrow(exe)
# write.csv(exe, "Philips_Exe_Clean.csv")
