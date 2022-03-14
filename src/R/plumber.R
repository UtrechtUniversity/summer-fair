# Title     : Plumber API
# Created by: Slavc001
# Created on: 03/11/2021

library(plumber)

pr("TutorialAlgorithm.R") %>%
  pr_run(host="0.0.0.0", port=8000)
