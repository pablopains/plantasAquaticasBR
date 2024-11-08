#' @title Plantas Aquáticas em R - 74 Congresso Nacional de Botânica - Brasília,
#' @author Pablo Hendrigo Alves de Melo
#' @details 
#' pablopains@yahoo.com.br


### Roteiro para prárica ###

### Instalção de programas
# R 4
# https://cran.r-project.org/bin/windows/base/R-4.4.2-win.exe
 
 
# R Tools 4
# https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6335-6327.exe
 
 
# RStudio
# https://download1.rstudio.org/electron/windows/RStudio-2024.09.1-394.exe
 


### Plantas Aquáticas do Brasil

  # https://sites.icb.ufmg.br/plantasaquaticasbrasil/index.htm

  # https://plantasaquaticasbrasil.shinyapps.io/plantasAquaticasBR/

  # https://github.com/pablopains/plantasAquaticasBR

  # github - instalação de pacotes
  # https://github.com/PlantasAquaticasBrasil/plantasAquaticasBR  


### Catálogo de Plantas das UCs do Brasil

  # Catalogo 
  # https://catalogo-ucs-brasil.jbrj.gov.br/
  
  # APPs online
  # https://pablopains.shinyapps.io/catalogoUCsBR_prepare/
    
  # https://pablopains.shinyapps.io/catalogoUCsBR_review/

  # https://pablopains.shinyapps.io/catalogoUCsBR_publication/
  
  # github - instalação de pacotes
  # https://github.com/pablopains/catalogoUCsBR

  # link dados
  # REFLORA
  # https://github.com/pablopains/plantasAquaticasBR/blob/main/minicurso_74C_NBot/REFLORA-Pains_RelatorioConsultaTestemunho.xlsx  

  # JABOT
  # https://github.com/pablopains/plantasAquaticasBR/blob/main/minicurso_74C_NBot/Jabot_Geral_DarwinCore_Pains.csv

  # SPLink
  # https://github.com/pablopains/plantasAquaticasBR/blob/main/minicurso_74C_NBot/speciesLink-20241107185859-0005493_Pains.txt
  # na fonte: https://specieslink.net/search/download/20241107185859-0005493

  # GBIF
  # https://github.com/pablopains/plantasAquaticasBR/blob/main/minicurso_74C_NBot/GBIF_occurrence_Pains.txt
  # na fonte: https://doi.org/10.15468/dl.8wc537


  install.packages('writexl')
  install.packages('tidyverse')
  install.packages('measurements')
  install.packages('readxl')
  install.packages('shinydashboardPlus')
  install.packages( 'writexl')

  
  install.packages('devtools')
  devtools::install_github("pablopains/catalogoUCsBR")
  
  catalogoUCsBR::app_prepare()
  catalogoUCsBR::app_review()
  catalogoUCsBR::app_publication()
    
  
  
### parseGBIF
  
  # APP online
  #  https://pablopains.shinyapps.io/parseGBIF/
  
  # github - instalação de pacotes
  # https://github.com/pablopains/parseGBIF
  
  # link dados
  # https://doi.org/10.15468/dl.8wc537
  
  install.packages('jsonify')
  
  install.packages('devtools')
  devtools::install_github("pablopains/parseGBIF", dependencies = TRUE)

  parseGBIF::parseGBIF_app()
    
  