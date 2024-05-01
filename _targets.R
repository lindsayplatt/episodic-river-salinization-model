
library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    'accelerometry',
    'arrow',
    'cowplot',
    'dataRetrieval',
    'EnvStats',
    'exactextractr',
    'FlowScreen',
    'GGally', # Needed for `ggcorr()`
    'httr',
    'magick', # Needed for 7_Disseminate and `cowplot::draw_image()`
    'MESS',
    'nhdplusTools',
    'pdp',
    'qs',
    'randomForest',
    'raster',
    'sbtools',
    'scico',
    'sf',
    'tidytext',
    'tidyverse',
    'units',
    'usmap',
    'yaml',
    'zip'
  ), 
  format =  'qs',
  workspace_on_error = TRUE
)

source('1_Download.R')
source('2_Prepare.R')
source('3_Filter.R')
source('4_EpisodicSalinization.R')
source('5_BaseflowSalinization.R')
source('6_DefineCharacteristics.R')
source('7_Disseminate.R')

select <- dplyr::select # The raster pkg keeps overriding this one so make sure this is correct

c(p1_targets, p2_targets, p3_targets,
  p4_targets, p5_targets, p6_targets,
  p7_targets)
