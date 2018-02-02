
library(microbenchmark)

setwd('/path/to/working/directory')

######################
### SOURCE FUNCTIONS
######################
source("CHI_CommAreas_Census2010_base.R")
source("CHI_CommAreas_Census2010_dplyr.R")
source("CHI_CommAreas_Census2010_datatable.R")

######################
### EXTRACT
######################
summary(microbenchmark(base_extract_fct, dplyr_extract_fct, dtable_extract_fct))

######################
### RESHAPE
######################
summary(microbenchmark(base_reshape_fct, dplyr_reshape_fct, dtable_reshape_fct))

######################
### SUMMARIZE
######################
summary(microbenchmark(base_summarize_fct, dplyr_summarize_fct, dtable_summarize_fct))

######################
### GRAPHING
######################
summary(microbenchmark(base_graphing_fct, dplyr_graphing_fct, dtable_graphing_fct))

######################
### PYTHON 
### BENCHMARK
######################
txt = system("python3 -W ignore /path/to/working/directory/py_microbenchmark.py", intern=TRUE)

read.table(text=txt, header=TRUE)[-1,]

######################
### EXTRACT BOXPLOT
######################
res <- microbenchmark(base_extract_fct, dplyr_extract_fct, dtable_extract_fct)

boxplot(res,outline=FALSE)

######################
### RESHAPE BOXPLOT
######################
res <- microbenchmark(base_reshape_fct, dplyr_reshape_fct, dtable_reshape_fct)

boxplot(res, outline=FALSE)

######################
### SUMMARIZE BOXPLOT
######################
res <- microbenchmark(base_summarize_fct, dplyr_summarize_fct, dtable_summarize_fct)

boxplot(res,outline=FALSE)

######################
### GRAPHING BOXPLOT
######################
res <- microbenchmark(base_graphing_fct, dplyr_graphing_fct, dtable_graphing_fct)

boxplot(res,outline=FALSE)
