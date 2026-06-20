# ============================================================
# packages.R
# Explicit package declarations for shinyapps.io deployment.
# shinyapps.io scans all R files for library() calls to
# determine which packages to install. This file ensures
# all dependencies are captured.
# ============================================================

library(shiny)
library(bslib)
library(DT)

# tidyverse components (replacing library(tidyverse))
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(ggplot2)
library(forcats)

# NLP + text analysis
library(tidytext)
library(text2vec)

# visualisation
library(ggraph)
library(tidygraph)
library(ggwordcloud)
library(ggrepel)
library(scales)
library(igraph)
library(visNetwork)

# data
library(jsonlite)
library(lubridate)
