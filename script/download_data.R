#----------------------------------------------------------------------------------------------
#' Packages 

library(tidyverse)
library(httr)
library(magrittr)
# library(here)

#---------------------------------------------------------------------------------------------------------------------
#' Diretório

DATA_ZIP <- 'inst/extdata/zip/'

if (!dir.exists(DATA_ZIP)) {
  message("Creating data directory.")
  dir.create(DATA_ZIP, recursive = TRUE)
}

DATA_DIR  <- 'inst/extdata/'

#----------------------------------------------------------------------------------------------
#' Requests

url <- 'https://servicodados.ibge.gov.br/api/v1/downloads/estatisticas?caminho=Precos_Indices_de_Precos_ao_Consumidor/IPCA/Resultados_por_Subitem&nivel=1'

parsed_url <- GET(url) |> content("parsed")

v_ipca_year_past <- parsed_url |> map_df('children') |> pull(url)
v_ipca_year_today <- parsed_url |> map('url') |> flatten_chr()

# Combine
l_url_ipca  <- c(v_ipca_year_past, v_ipca_year_today)

l_name_file_ipca <-
    gsub("\\Subitem_.*|Subitem.zip","", l_url_ipca)  |> 
    strsplit("_", perl = TRUE) |> 
    map(~tail(.x, 1)) |> 
    map(~paste0('ipca_', .x, '.zip')) |> 
    flatten_chr() 

#* Donwload
map2(
      head(l_url_ipca)
    , head(l_name_file_ipca)
    , ~download.file(.x, destfile =  here::here(DATA_ZIP, .y))
    ) 

#* Unzip
map(
    list.files(here::here(DATA_ZIP), full.names = TRUE)
    , ~unzip(.x, exdir = here::here(DATA_DIR))
    )

#!Remove ZIP
unlink(here::here(DATA_ZIP), recursive = TRUE)

#---------------------------------------------------------------------------------------------------------------------
#' CHeck

# output_path  <- 'data/processed/unzip/' 
# all_raw_files <- list.files('data/raw', full.names = TRUE)
# map(all_raw_files, ~unzip(.x, exdir = output_path))

# #* Check
# all_unzip_files <- list.files('data/processed/unzip', full.names = TRUE)
# length(all_raw_files) == length(all_unzip_files)
