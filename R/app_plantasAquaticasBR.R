

#' @title Aplicativo do Catálogo de Plantas Aquáticas do Brasil
#' @name plantasAquaticasBR
#' @description Aplicativo do Catálogo de Plantas Aquáticas do Brasil e Flora & Funga do Brasil
#' @return CSV files
#' @author Pablo Hendrigo Alves de Melo
#'        
#' @seealso \code{\link[utils]{download.file}}, \code{\link[utils]{aspell}}
#' 
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import stringr
#' @import lubridate
#' @import jsonlite
#' @import sqldf
#' @import shiny
#' @import shinydashboard
#' @import DT
#' @import rhandsontable
#' @import shinyWidgets
#' @import measurements
#' @import downloader
#' @import writexl
#' 
#' @examples
#' \donttest{
#' app_catalogoPlantasAquaticasBR()
#' }
#' @export
app_PlantasAquaticasBR <- function()
{
  {
    # setwd('C:\\plantasAquaticasBR - github.com\\plantasAquaticasBR')
    
    require(dplyr)
    require(tidyr)
    require(readr)
    require(stringr)
    require(lubridate)
    require(jsonlite)
    require(sqldf)
    require(rvest)
    require(shiny)
    require(shinydashboard)
    require(rhandsontable)
    require(DT)
    require(rhandsontable)
    require(shinyWidgets)
    require(measurements)
    require(downloader)
    require(writexl)
    library(glue)
    
    options(shiny.maxRequestSize=10000*1024^2) 
    
    # cerregar funções 
    {            
      update_wcvp <<- FALSE
      
      {
        get_floraFungaBrasil_v2 <- function(url_source = "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil",
                                            path_results = tempdir)#'C:\\Dados\\APP_GBOT\\data') # if NULL
          
        {  
          
          require(dplyr)
          require(downloader)
          require(stringr)
          # require(plyr)
          
          # criar pasta para salvar raultados do dataset
          path_results <- paste0(path_results,'/FloraFungaBrasil')
          if (!dir.exists(path_results)){dir.create(path_results)}
          
          destfile <- paste0(path_results,"/IPT_FloraFungaBrasil_.zip")
          
          
          # ultima versao
          # destfile <- paste0(path_results,"/",Sys.Date(),'.zip')
          downloader::download(url = url_source, destfile = destfile, mode = "wb") 
          utils::unzip(destfile, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal
          
          
          taxon.file <- paste0(path_results,"/taxon.txt")
          
          # taxon.file <- paste0("C:\\Dados\\APP_GBOT\\data\\FloraFungaBrasil\\taxon.txt")
          
          
          
          # taxon
          fb2020_taxon  <- readr::read_delim(taxon.file, delim = "\t", quote = "") %>% 
            dplyr::select(-id)
          
          ### familia
          # index = fb2020_taxon$taxonRank %in% c("ESPECIE",
          #                                       "SUB_ESPECIE",
          #                                       "VARIEDADE",
          #                                       "FORMA")
          
          index = fb2020_taxon$taxonRank %in% c("ESPECIE",
                                                "SUB_ESPECIE",
                                                "VARIEDADE",
                                                "FORMA",
                                                "FAMILIA",
                                                "GENERO")
          ###
          
          fb2020_taxon  <- fb2020_taxon[index==TRUE,] 
          
          
          scientificName_tmp <- fb2020_taxon$scientificName %>% stringr::str_split(.,pattern = ' ', simplify = TRUE)
          
          
          # carregando especie sem autor
          scientificName <- rep('',nrow(fb2020_taxon))
          
          # scientificName[index==TRUE] <- scientificName_tmp[index==TRUE,1] %>% trimws(.,'right')
          
          index = fb2020_taxon$taxonRank %in% c("ESPECIE")
          
          scientificName[index==TRUE] <-  paste0(scientificName_tmp[index==TRUE,1], ' ', scientificName_tmp[index==TRUE,2]) #%>% trimws(.,'right')
          
          index = fb2020_taxon$taxonRank %in% c("VARIEDADE")
          scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' var. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')
          
          index = fb2020_taxon$taxonRank %in% c("SUB_ESPECIE")
          scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' subsp. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')
          
          index = fb2020_taxon$taxonRank %in% c("FORMA")
          scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' form. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')
          
          fb2020_taxon$scientificNamewithoutAuthorship <- scientificName
          fb2020_taxon$scientificNamewithoutAuthorship_U <- toupper(scientificName)
          
          fb2020_taxon$scientificNameAuthorship_U <- toupper(fb2020_taxon$scientificNameAuthorship)
          
          ### reconhecer genero e familia
          
          fb2020_taxon$genus_U <- toupper(fb2020_taxon$genus)
          
          fb2020_taxon$family_U <- toupper(fb2020_taxon$family)
          
          fb2020_taxon$group <- str_split(fb2020_taxon$higherClassification,';',simplify = TRUE)[,2]
          
          
          ###
          
          return(fb2020_taxon)
          
        }
        
      }
      
      {
        standardize_scientificName <- function(searchedName = 'Alomia angustata (Gardner) Benth. ex Baker')
        {
          
          x <- {}
          
          infrataxa = ''
          # str_squish(x)
          # setdiff(vec1, vec2)
          
          # Transformação padrão GBIF de híbrido para wcvp 
          searchedName_raw <- searchedName
          # searchedName <- gsub('×','x ',searchedName)
          searchedName <- gsub('×','× ',searchedName)
          searchedName_ori <- searchedName
          
          # if(!is.na(taxonRank))
          # {
          #   
          #   searchedName_clear <- ifelse(taxonRank %in% c('GENUS','FAMILY'),word(searchedName,1),
          #                                ifelse(taxonRank=='SPECIES',paste0(word(searchedName,1),' ',word(searchedName,2)),
          #                                       ifelse(taxonRank=='VARIETY',paste0(word(searchedName,1),' ', word(searchedName,2), ' var. ', word(searchedName,4)),
          #                                              ifelse(taxonRank=='SUBSPECIES',paste0(word(searchedName,1),' ', word(searchedName,2), ' subsp. ', word(searchedName,4)),
          #                                                     ifelse(taxonRank=='FORM',paste0(word(searchedName,1),' ', word(searchedName,2), ' f. ', word(searchedName,4)), 
          #                                                            '')))))
          #   
          #   return(list(searchedName = searchedName_raw,
          #               standardizeName = searchedName,
          #               taxonAuthors= taxon_authors))
          #   
          # }
          
          
          
          sp <- str_split(searchedName, ' ', simplify = T)
          padrao <- c('var.', 'subsp.', ' f. ')
          padrao_s <- c('var.', 'subsp.', 'f.')
          
          # Urtica gracilis Aiton subsp. gracilis
          
          if(length(sp)>1)
          {
            # if(any(str_detect(searchedName, padrao))==T)
            if(grepl(padrao[1],searchedName, fixed = T)|grepl(padrao[2],searchedName, fixed = T)|grepl(padrao[3],searchedName, fixed = T) ) 
            {
              ip <- 1
              for(ip in 1:length(padrao))
              {
                # grepl(padrao[ip],'teste var. teste', fixed = T)
                # grepl(padrao[ip],'"Elatostema variabile C.B.Rob."', fixed = T)
                
                # if(str_detect(searchedName, padrao[ip])==TRUE)
                if(grepl(padrao[ip],searchedName, fixed = T)==TRUE)
                {
                  indx <- sp == padrao_s[ip]
                  
                  if(length(sp)>3){if(indx[3]==T){infrataxa <- sp[4]}}
                  if(length(sp)>4){if(indx[4]==T){infrataxa <- sp[5]}}
                  if(length(sp)>5){if(indx[5]==T){infrataxa <- sp[6]}}
                  if(length(sp)>6){if(indx[6]==T){infrataxa <- sp[7]}}
                  if(length(sp)>7){if(indx[7]==T){infrataxa <- sp[8]}}
                  if(length(sp)>8){if(indx[8]==T){infrataxa <- sp[9]}}
                  if(length(sp)>9){if(indx[9]==T){infrataxa <- sp[10]}}
                  if(length(sp)>10){if(indx[10]==T){infrataxa <- sp[11]}}
                  if(length(sp)>11){if(indx[11]==T){infrataxa <- sp[12]}}
                  if(length(sp)>12){if(indx[12]==T){infrataxa <- sp[13]}}
                  
                  if(str_detect(searchedName_raw, '×')==TRUE)
                  {
                    searchedName <- paste0(sp[1], ' × ', sp[3], ifelse(infrataxa=='','',paste0(' ', padrao_s[ip], ' ', infrataxa)))  
                  }else
                  {
                    searchedName <- paste0(sp[1], ' ', sp[2], ' ', padrao_s[ip], ' ', infrataxa)   
                  }
                  
                  
                  
                  break
                  
                }
              }
            }else
            {
              
              if(str_detect(searchedName_raw, '×')==TRUE)
              {
                
                searchedName <- paste0(sp[1], ' × ', sp[3])  
                
              }else
              {
                if((str_sub(sp[2],1,1)==toupper(str_sub(sp[2],1,1)) |
                    str_sub(sp[2],1,1)=="(") )
                {
                  searchedName <- sp[1]
                }else
                {
                  searchedName <- paste0(sp[1], ' ', sp[2])
                } 
              }
            }
          }else
          {
            searchedName <- sp[1]
          }
          
          sp2 <- str_split(searchedName, ' ', simplify = T)
          
          taxon_authors <- str_sub(searchedName_ori, str_locate(searchedName_ori, sp2[length(sp2)])[2]+2, nchar(searchedName_ori))
          # if(length(sp2)>=4){if( paste0(sp2[3], ' ',sp2[4])==taxon_authors){taxon_authors <- ''}}
          
          if(length(sp2)==4 &!is.na(taxon_authors)){if(paste0(sp2[3], ' ',sp2[4])==taxon_authors){taxon_authors <- ''}}
          
          # if( (str_sub(sp[3],1,1)==toupper(str_sub(sp[3],1,1)) | str_sub(sp[3],1,1)=="(") & any(str_detect(searchedName, padrao))==TRUE ){taxon_authors <- ''}
          
          xi <- str_locate(taxon_authors,'\\(')
          xf <- str_locate(taxon_authors,'\\)')
          
          
          if(!is.na(xi)[1] & nchar(taxon_authors) > 0)
          {    
            if(xi[1]==1)
            {
              taxon_authors_last <- str_sub(taxon_authors,xf[2]+ifelse(str_sub(taxon_authors,xf[2]+1,xf[2]+1)==' ',2,1),nchar(taxon_authors))
            }
          }else
          {
            taxon_authors_last <- ''  
          }
          
          if(is.na(taxon_authors)){taxon_authors <- ''}
          
          
          return(list(searchedName = searchedName_raw,
                      standardizeName = searchedName,
                      taxonAuthors= taxon_authors,
                      taxonAuthors_last= taxon_authors_last))
        }
        
        # searchedName = "Acacia plumosa"
        # searchedName = "Furnarius rufus"
        
        # searchedName = "Oncidium cebolleta"
        
        checkName_FloraFungaBrasil <- function(searchedName = 'Alomia angustata',
                                               fb2020="",
                                               if_author_fails_try_without_combinations=TRUE)
        {
          print(searchedName)
          # https://powo.science.kew.org/about-wcvp#unplacednames
          
          x <- {}  
          sp_fb <- standardize_scientificName(searchedName)
          
          if(sp_fb$taxonAuthors != "")
          {
            
            index_author <- 100
            
            index <- fb2020$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) & 
              fb2020$scientificNameAuthorship_U %in% toupper(gsub ("\\s+", "", sp_fb$taxonAuthors ))
            ntaxa <- NROW(fb2020[index==TRUE,])
            
            if(ntaxa == 0 & if_author_fails_try_without_combinations == TRUE)
            {
              index_author <- 50
              index <- fb2020$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) & 
                fb2020$scientificNameAuthorship_U %in% toupper(gsub ("\\s+", "", sp_fb$taxonAuthors_last ))
              ntaxa <- NROW(fb2020[index==TRUE,])
            }
            
            
            if(ntaxa == 0)
            {
              index_author <- 0
              index <- fb2020$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName)
              ntaxa <- NROW(fb2020[index==TRUE,])
            }
            
          }else
          {
            index_author <- 0
            index <- fb2020$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName)
            ntaxa <- NROW(fb2020[index==TRUE,])
          }
          
          if(ntaxa == 0 | sp_fb$standardizeName=="")
          {
            x <- fb2020[index==TRUE,] %>%
              dplyr::add_row()  %>%
              dplyr::mutate(searchedName=searchedName,
                            taxon_status_of_searchedName = "",
                            plant_name_id_of_searchedName = "",
                            taxon_authors_of_searchedName = "",
                            verified_author = index_author,
                            verified_speciesName = 0,
                            searchNotes='Not found')
          }
          
          if(ntaxa == 1)
          {
            verified_speciesName <- 100
            
            id_accept <- ifelse(is.na(fb2020$acceptedNameUsageID[index==TRUE]),'', fb2020$acceptedNameUsageID[index==TRUE])
            
            if((!is.na(fb2020$acceptedNameUsageID[index==TRUE])) &
               (fb2020$taxonID[index==TRUE] != id_accept ))
            {
              
              x <- fb2020[index==TRUE,]
              
              taxon_status_of_searchedName <- fb2020[index==TRUE,]$taxonomicStatus
              plant_name_id_of_searchedName <- fb2020[index==TRUE,]$taxonID
              taxon_authors_of_searchedName <- fb2020[index==TRUE,]$scientificNamewithoutAuthorship
              
              index_synonym <- fb2020$taxonID %in% x$acceptedNameUsageID 
              
              if(sum(index_synonym==TRUE)==1)
              {
                x <- fb2020[index_synonym==TRUE,] %>%
                  dplyr::mutate(searchedName=searchedName,
                                taxon_status_of_searchedName = taxon_status_of_searchedName,
                                plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                verified_author = index_author,
                                verified_speciesName = verified_speciesName,
                                searchNotes= 'Updated')
                
              }else
              {
                x <- fb2020[index==TRUE,] %>%
                  dplyr::mutate(searchedName=searchedName,
                                taxon_status_of_searchedName = taxon_status_of_searchedName,
                                plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                verified_author = index_author,
                                verified_speciesName = verified_speciesName,
                                searchNotes= 'Does not occur in Brazil')
              }
              
            }else
            {
              x <- fb2020[index==TRUE,] %>%
                # dplyr::add_row()  %>%
                dplyr::mutate(searchedName=searchedName,
                              taxon_status_of_searchedName = "",
                              plant_name_id_of_searchedName = "",
                              taxon_authors_of_searchedName = "",
                              verified_author = index_author,
                              verified_speciesName = verified_speciesName,
                              searchNotes=ifelse(is.na(taxonomicStatus),'',taxonomicStatus))
            }
            
          }
          
          if(ntaxa > 1)
          {
            
            taxon_status_of_searchedName <- paste(fb2020[index==TRUE,]$taxonomicStatus, collapse = '|')
            plant_name_id_of_searchedName <- paste(fb2020[index==TRUE,]$taxonID, collapse = '|')
            # taxon_authors_of_searchedName <- paste(paste0(fb2020[index==TRUE,]$taxon_name, ' ',fb2020[index==TRUE,]$taxon_authors), collapse = '|')
            taxon_authors_of_searchedName <- paste(fb2020[index==TRUE,]$scientificNameAuthorship, collapse = '|')
            
            
            # Accepted or Homonyms
            {
              index_status <- fb2020$scientificNamewithoutAuthorship_U %in% toupper(sp_fb$standardizeName) &
                fb2020$taxonomicStatus %in% c( "NOME_ACEITO")
              
              ntaxa_status <- NROW(fb2020[index_status==TRUE,])
              
              if(ntaxa_status == 1)
              {
                
                x <- fb2020[index_status==TRUE,] %>%
                  dplyr::mutate(searchedName=searchedName,
                                taxon_status_of_searchedName = taxon_status_of_searchedName,
                                plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                verified_author = index_author,
                                verified_speciesName = 100/ntaxa,
                                searchNotes=taxonomicStatus)
              }
              else
              {
                
                
                x <- fb2020[1==2,] %>%
                  dplyr::add_row()  %>%
                  dplyr::mutate(searchedName=searchedName,
                                taxon_status_of_searchedName = taxon_status_of_searchedName,
                                plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                verified_author = index_author,
                                verified_speciesName = 0,
                                searchNotes='Homonyms')
                
              }
              
            }
            
          }
          
          # 'Homonyms' ajustar família
          
          if(x$searchNotes == 'Not found' )
          {
            ### reconhecer genero e familia
            # x <-{}
            w1 <- toupper(word(sp_fb$standardizeName))
            
            index <- fb2020$genus_U %in% toupper(w1) & fb2020$taxonRank == 'GENERO' #& !is.na(fb2020$acceptedNameUsageID)
            ntaxa <- NROW(fb2020[index==TRUE,])
            
            g_f <- 'g'
            
            if(ntaxa == 0 )
            {
              index <- fb2020$family_U %in% toupper(w1) & fb2020$taxonRank == 'FAMILIA' #& !is.na(fb2020$acceptedNameUsageID)
              ntaxa <- NROW(fb2020[index==TRUE,])
              g_f <- 'f'
            }    
            
            if(ntaxa == 1)
            {
              verified_speciesName <- 100
              
              id_accept <- ifelse(is.na(fb2020$acceptedNameUsageID[index==TRUE]),'', fb2020$acceptedNameUsageID[index==TRUE])
              
              if((!is.na(fb2020$acceptedNameUsageID[index==TRUE])) &
                 (fb2020$taxonID[index==TRUE] != id_accept ))
              {
                
                x <- fb2020[index==TRUE,]
                
                taxon_status_of_searchedName <- fb2020[index==TRUE,]$taxonomicStatus
                plant_name_id_of_searchedName <- fb2020[index==TRUE,]$taxonID
                taxon_authors_of_searchedName <- fb2020[index==TRUE,]$scientificNamewithoutAuthorship
                
                index_synonym <- fb2020$taxonID %in% x$acceptedNameUsageID 
                
                if(sum(index_synonym==TRUE)==1)
                {
                  x <- fb2020[index_synonym==TRUE,] %>%
                    dplyr::mutate(searchedName=searchedName,
                                  taxon_status_of_searchedName = taxon_status_of_searchedName,
                                  plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                  taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                  verified_author = index_author,
                                  verified_speciesName = verified_speciesName,
                                  searchNotes=  ifelse(g_f=='g', 'Updated_genus', 'Updated_family') )
                  
                }else
                {
                  x <- fb2020[index==TRUE,] %>%
                    dplyr::mutate(searchedName=searchedName,
                                  taxon_status_of_searchedName = taxon_status_of_searchedName,
                                  plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                                  taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                                  verified_author = index_author,
                                  verified_speciesName = verified_speciesName,
                                  searchNotes= 'Does not occur in Brazil')
                }
                
              }else
              {
                x <- fb2020[index==TRUE,] %>%
                  # dplyr::add_row()  %>%
                  dplyr::mutate(searchedName=searchedName,
                                taxon_status_of_searchedName = "",
                                plant_name_id_of_searchedName = "",
                                taxon_authors_of_searchedName = "",
                                verified_author = index_author,
                                verified_speciesName = verified_speciesName,
                                searchNotes=taxonomicStatus)
              }
              
            }
            
            
            if(ntaxa >1)
            {
              
              x <- fb2020[index==TRUE,][1,] %>%
                # dplyr::add_row()  %>%
                dplyr::mutate(taxonID = '',                           
                              acceptedNameUsageID = '',
                              parentNameUsageID = '',         
                              originalNameUsageID = '',          
                              
                              # scientificName = ifelse(g_f=='g', genus, family),                    
                              scientificName = '', 
                              
                              acceptedNameUsage  = '',                
                              parentNameUsage = '',                   
                              namePublishedIn = '',                  
                              namePublishedInYear = '',               
                              higherClassification = '',             
                              # kingdom                           
                              # phylum                           
                              # class                             
                              # order                            
                              # family                            
                              # genus                            
                              specificEpithet = '',                   
                              infraspecificEpithet = '',             
                              
                              # taxonRank = ifelse(g_f=='g',"GENERO", "FAMILIA"),   
                              taxonRank = '',
                              
                              scientificNameAuthorship = '',
                              taxonomicStatus = '',                   
                              nomenclaturalStatus = '',              
                              modified = '',                          
                              bibliographicCitation = '',            
                              references = '',                        
                              scientificNamewithoutAuthorship = ifelse(g_f=='g', genus, family),
                              scientificNamewithoutAuthorship_U = ifelse(g_f=='g', genus_U, family_U),
                              scientificNameAuthorship_U = '',       
                              genus_U,                           
                              family_U) %>%
                dplyr::mutate(searchedName=searchedName,
                              taxon_status_of_searchedName = "",
                              plant_name_id_of_searchedName = "",
                              taxon_authors_of_searchedName = "",
                              verified_author = "",
                              verified_speciesName = "",
                              searchNotes=taxonRank)
            }
            ###
            
          }
          
          colnames(x) <- str_c('fb2020_',colnames(x))
          return(x)
          
        }
        
      }
    }
    
    
    occ_fb2020 <<- {}
    
    {
      {
        
        colunas_fb2020_sel <<- c("fb2020_taxonID",
                                 "fb2020_acceptedNameUsageID",
                                 "fb2020_parentNameUsageID",
                                 "fb2020_originalNameUsageID",
                                 "fb2020_scientificName",
                                 # "fb2020_acceptedNameUsage",
                                 # "fb2020_parentNameUsage",
                                 "fb2020_namePublishedIn",                  
                                 "fb2020_namePublishedInYear",
                                 "fb2020_higherClassification",             
                                 # "fb2020_kingdom",
                                 # "fb2020_phylum",                           
                                 # "fb2020_class",
                                 # "fb2020_order",                            
                                 "fb2020_family",
                                 # "fb2020_genus",                            
                                 "fb2020_specificEpithet",
                                 "fb2020_infraspecificEpithet",             
                                 "fb2020_taxonRank",
                                 "fb2020_scientificNameAuthorship",
                                 "fb2020_taxonomicStatus",
                                 "fb2020_nomenclaturalStatus",              
                                 "fb2020_modified",
                                 "fb2020_bibliographicCitation",
                                 "fb2020_references",
                                 "fb2020_scientificNamewithoutAuthorship",  
                                 "fb2020_scientificNamewithoutAuthorship_U",
                                 "fb2020_searchNotes",
                                 "fb2020_searchedName")
        
        
      }
      
      {
        fb2020_names <- data.frame(stringsAsFactors = FALSE,
                                   
                                   fb2020_taxonID = 0,
                                   fb2020_acceptedNameUsageID = 0,
                                   fb2020_parentNameUsageID = 0,
                                   fb2020_originalNameUsageID = "0",
                                   fb2020_scientificName = "",
                                   # fb2020_acceptedNameUsage = "",
                                   # fb2020_parentNameUsage = "",
                                   fb2020_namePublishedIn = "",                  
                                   fb2020_namePublishedInYear = 0,
                                   fb2020_higherClassification = "",             
                                   # fb2020_kingdom = "",
                                   # fb2020_phylum = "",                           
                                   # fb2020_class = "",
                                   # fb2020_order = "",                            
                                   fb2020_family = "",
                                   # fb2020_genus = "",                            
                                   fb2020_specificEpithet = "",
                                   fb2020_infraspecificEpithet = "",             
                                   fb2020_taxonRank = "",
                                   fb2020_scientificNameAuthorship = "",
                                   fb2020_taxonomicStatus = "",
                                   fb2020_nomenclaturalStatus = "",              
                                   fb2020_modified = lubridate::as_datetime("2021-10-31 21:13:33.77"),
                                   fb2020_bibliographicCitation = "",
                                   fb2020_references = "",
                                   fb2020_scientificNamewithoutAuthorship = "",  
                                   fb2020_scientificNamewithoutAuthorship_U = "",
                                   fb2020_searchNotes = "",
                                   fb2020_searchedName = "")
        
      }
      
    }
    
    # variaveis
    {
      
      colShow <- c('group', 'family', 'scientificName',
                   'taxonRank', 'taxonomicStatus','nomenclaturalStatus',
                   'kingdom','phylum','class','order','genus','specificEpithet','infraspecificEpithet', 'scientificNamewithoutAuthorship', 'scientificNameAuthorship',
                   'taxonID', 'acceptedNameUsageID','acceptedNameUsage','parentNameUsageID','parentNameUsage', 'originalNameUsageID','namePublishedIn','namePublishedInYear',
                   'higherClassification','bibliographicCitation', 'references','modified')
      
      
      colShowName <- c('group', 'family', 'scientificName',
                       'taxonRank', 'taxonomicStatus','nomenclaturalStatus',
                       'kingdom','phylum','class','order','genus','specificEpithet','infraspecificEpithet', 'scientificNamewithoutAuthorship', 'scientificNameAuthorship',
                       'taxonID', 'acceptedNameUsageID','acceptedNameUsage','parentNameUsageID','parentNameUsage', 'originalNameUsageID','namePublishedIn','namePublishedInYear',
                       'higherClassification','bibliographicCitation', 'references','modified')
      
      
      fb2020 <<- get_floraFungaBrasil_v2(path_results = tempdir())
      
      colSearch <- as.list(colnames(fb2020))
      names(colSearch) <- colnames(fb2020)
      i=4
      for(i in 1:length(colSearch))
      {
        colSearch[[names(colSearch)[i]]] <- as.list(fb2020[,names(colSearch)[i]] %>%
                                                      unique() %>% na.omit() %>% as.data.frame() %>% dplyr::arrange_at(., c(names(colSearch)[i])))
      }
      
      # file <- 'data\\especimes.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/especimes.csv'
      especimes <<- readr::read_csv(file, 
                                    locale = readr::locale(encoding = "UTF-8"),
                                    show_col_types = FALSE)
      
      # file <- 'data\\referencia.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/referencia.csv'
      referencia <<- readr::read_csv(file, 
                                     locale = readr::locale(encoding = "UTF-8"),
                                     show_col_types = FALSE)
      
      # file <- 'data\\artigo_Completo.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/artigo_Completo.csv'
      artigo_Completo <<- readr::read_csv(file, 
                                          locale = readr::locale(encoding = "UTF-8"),
                                          show_col_types = FALSE)
      
      # file <- 'data\\autor_Referencia.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/autor_Referencia.csv'
      autor_Referencia <<- readr::read_csv(file, 
                                           locale = readr::locale(encoding = "UTF-8"),
                                           show_col_types = FALSE)
      
      # file <- 'data\\capitulo_Livro.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/capitulo_Livro.csv'
      capitulo_Livro <<- readr::read_csv(file, 
                                         locale = readr::locale(encoding = "UTF-8"),
                                         show_col_types = FALSE)
      
      # file <- 'data\\forma_Biologica.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/forma_Biologica.csv'
      forma_Biologica <<- readr::read_csv(file, 
                                          locale = readr::locale(encoding = "UTF-8"),
                                          show_col_types = FALSE)
      
      # file <- 'data\\habito.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/habito.csv'
      habito <<- readr::read_csv(file, 
                                 locale = readr::locale(encoding = "UTF-8"),
                                 show_col_types = FALSE)
      
      # file <- 'data\\livro.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/livro.csv'
      livro <<- readr::read_csv(file, 
                                locale = readr::locale(encoding = "UTF-8"),
                                show_col_types = FALSE)
      
      # file <- 'data\\tese.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/livro.csv'
      tese <<- readr::read_csv(file, 
                               locale = readr::locale(encoding = "UTF-8"),
                               show_col_types = FALSE)
      
      # file <- 'data\\tipo_Ambiente.csv '
      file <- 'https://raw.githubusercontent.com/pablopains/plantasAquaticasBR/main/data/tipo_Ambiente.csv'
      tipo_Ambiente <<- readr::read_csv(file, 
                                        locale = readr::locale(encoding = "UTF-8"),
                                        show_col_types = FALSE)
      
      especimes <- left_join(especimes %>% dplyr::mutate(codigo_referencia=associatedReference),
                             referencia %>% dplyr::select(codigo_referencia,ano)) %>%
        dplyr::select(-codigo_referencia) %>%
        dplyr::rename(ano_publicacao=ano)
      
      
      # colSearch[['referencia']] <- as.list(c('Tudo',
      #                                        referencia$codigo_referencia))
      
      colSearch2 <- as.list(colnames(especimes))
      names(colSearch2) <- colnames(especimes)
      i=10
      for(i in 1:length(colSearch2))
      {
        x <- data.frame( x = 'Tudo')
        x <- rbind(x$x, 
                   especimes[,names(colSearch2)[i]] %>%
                     unique() %>% na.omit() %>% as.data.frame() %>% dplyr::arrange_at(., c(names(colSearch2)[i])))
        
        
        colSearch2[[names(colSearch2)[i]]] <- as.list(x)
      }
      
      colSearch <- c(colSearch, colSearch2)
      remove(colSearch2)
      
    }
  }
  
  
  #  Tela APP--
  ui <- 
    {
      shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Plantas Aquáticas BR"),
        shinydashboard::dashboardSidebar(width = 0,
                                         collapsed = TRUE
        ),
        
        shinydashboard::dashboardBody(
          
          navbarPage("Catálogo de Plantas Aquáticas do Brasil",
                     tabPanel(icon("home"), 
                              box(title = 'Apresentação',
                                  status = "primary",width = 12,
                                  
                                  helpText('Ferramentas para acessar o repositório de dados do Catálogo de Plantas Aquáticas do Brasil, indexado à Flora & Funga do Brasil!')
                                  
                              )),
                     
                     
                     tabPanel(icon("droplet"),
                              box(title = 'Catálogo de Plantas Aquáticas do Brasil',
                                  status = "primary",width = 12,
                                  
                                  # country	stateProvince	municipality	locality
                                  # wetlandReference
                                  
                                  # basisOfRecord
                                  # collectionCode
                                  # recordedBy
                                  # ano_publicacao
                                  # lifeFormReference	aquaticLifeFormReference	habitatFloraBR	lifeFormFloraBR
                                  # family	genus					scientificName
                                  
                                  fluidRow(column(
                                    width = 12,
                                    
                                    fluidRow(column(
                                      width = 12,
                                      selectInput(inputId = 'tipo_consulta',
                                                  label = 'Para utilizar multipos filtros, considerar o operador lógico:',
                                                  choices = c('E','OU'),
                                                  # multiple = TRUE,
                                                  selected = c('E')),
                                      
                                      
                                    )),
                                    
                                    
                                    wellPanel(
                                      fluidRow(column(
                                        width = 4,
                                        selectInput(inputId = 'brazilianRegion',
                                                    label = 'Região:',
                                                    choices = colSearch[['brazilianRegion']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        selectInput(inputId = 'hydrographicBasin',
                                                    label = 'Bacia hidrográfica:',
                                                    choices = colSearch[['hydrographicBasin']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        selectInput(inputId = 'hydrograficSubbasin',
                                                    label = 'Sub-bacia hidrográfica',
                                                    choices = colSearch[['hydrograficSubbasin']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        
                                        selectInput(inputId = 'country',
                                                    label = 'País:',
                                                    choices = colSearch[['country']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        selectInput(inputId = 'stateProvince',
                                                    label = 'UF:',
                                                    choices = colSearch[['stateProvince']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'municipality',
                                                    label = 'Município:',
                                                    choices = colSearch[['municipality']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'locality',
                                                    label = 'Localidade:',
                                                    choices = colSearch[['locality']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'wetlandReference',
                                                    label = 'Ambiente:',
                                                    choices = colSearch[['wetlandReference']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        
                                        
                                        
                                      ),
                                      
                                      column(
                                        width = 4,
                                        
                                        selectInput(inputId = 'associatedReference',
                                                    label = 'Referência bibliográfica:',
                                                    choices = colSearch[['associatedReference']],
                                                    # multiple = TRUE,
                                                    selected = c('Tuto')),
                                        
                                        # pegar ano da tabela referencia e incluir na tabela de especimes
                                        selectInput(inputId = 'ano_publicacao',
                                                    label = 'Ano publicação:',
                                                    choices = colSearch[['ano_publicacao']],
                                                    # multiple = TRUE,
                                                    selected = c('Tuto')),
                                        
                                        selectInput(inputId = 'basisOfRecord',
                                                    label = 'Tipo de registro:',
                                                    choices = colSearch[['basisOfRecord']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        selectInput(inputId = 'collectionCode',
                                                    label = 'Coleção:',
                                                    choices = colSearch[['collectionCode']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        
                                        selectInput(inputId = 'recordedBy',
                                                    label = 'Coletor:',
                                                    choices = colSearch[['recordedBy']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo'))
                                        
                                      ),
                                      
                                      
                                      column(
                                        width = 4,
                                        
                                        # lifeFormReference	aquaticLifeFormReference	habitatFloraBR	lifeFormFloraBR
                                        # family	genus					scientificName
                                        
                                        # selectInput(inputId = 'family',
                                        #             label = 'Familia Atualizada:',
                                        #             choices = c('Tudo',colSearch[['family']]),
                                        #             multiple = FALSE,
                                        #             # selectize = TRUE,
                                        #             selected = c('Tudo')),
                                        # selectInput(inputId = 'genus',
                                        #             label = 'Gênero:',
                                        #             choices = colSearch[['genus']],
                                        #             # multiple = TRUE,
                                        #             selected = c('Tudo')),
                                        # selectInput(inputId = 'scientificName',
                                        #             label = 'Nome científico Atualizado:',
                                        #             choices = colSearch[['scientificName']],
                                        #             # multiple = TRUE,
                                        #             selected = c('Tudo')),
                                        # 
                                        # selectInput(inputId = 'scientificNameSearch',
                                        #             label = 'Nome científico Referência:',
                                        #             choices = colSearch[['scientificNameSearch']],
                                        #             # multiple = TRUE,
                                        #             selected = c('Tudo')),
                                        
                                        
                                        selectInput(inputId = 'lifeFormReference',
                                                    label = 'Hábito Referência:',
                                                    choices = colSearch[['lifeFormReference']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'aquaticLifeFormReference',
                                                    label = 'Forma biológica Referência:',
                                                    choices = colSearch[['aquaticLifeFormReference']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'habitatFloraBR',
                                                    label = 'Ambiente Flora & Funga do Brasil:',
                                                    choices = colSearch[['habitatFloraBR']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo')),
                                        selectInput(inputId = 'lifeFormFloraBR',
                                                    label = 'Háito Flora & Funga do Brasil:',
                                                    choices = colSearch[['lifeFormFloraBR']],
                                                    # multiple = TRUE,
                                                    selected = c('Tudo'))
                                        
                                        
                                        
                                      )
                                      )),
                                    
                                    fluidRow(
                                      column(
                                        width = 4,
                                        selectInput(inputId = 'filtrar_familia',
                                                    label = 'Filtrar por família:',
                                                    choices = c('Sim','Não'),
                                                    # multiple = TRUE,
                                                    selected = c('Não')),
                                        
                                        br(),
                                        
                                        rHandsontableOutput("hot_familia_key")),
                                      
                                      column(
                                        width = 4,
                                        selectInput(inputId = 'filtrar_genero',
                                                    label = 'Filtrar por gênero:',
                                                    choices = c('Sim','Não'),
                                                    # multiple = TRUE,
                                                    selected = c('Não')),
                                        
                                        br(),
                                        
                                        rHandsontableOutput("hot_genero_key")),
                                      
                                      column(
                                        width = 4,
                                        selectInput(inputId = 'filtrar_sp',
                                                    label = 'Filtrar por espécie:',
                                                    choices = c('Sim','Não'),
                                                    # multiple = TRUE,
                                                    selected = c('Não')),
                                        
                                        br(),
                                        
                                        rHandsontableOutput("hot_sp_key")),
                                      
                                    ),
                                    
                                    br(),
                                    
                                    fluidRow(column(
                                      width = 12, 
                                      wellPanel(
                                        
                                        DT::dataTableOutput('catalogo_sumario_Contents'),
                                        
                                        br(),
                                        br(),
                                        br(),
                                        
                                        DT::dataTableOutput('catalogoEspecimesContents'),
                                      )))
                                    
                                  ))
                                  
                              )),
                     
                     
                     tabPanel(icon("spa"),
                              box(title = 'Flora & Funga do Brasil',
                                  status = "primary",width = 12,
                                  
                                  shiny::tags$a('Projeto Flora do Brasil 2020', href = 'https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil'),
                                  
                                  
                                  wellPanel(
                                    fluidRow(column(
                                      width = 6,
                                      selectInput(inputId = 'group',
                                                  label = 'Grupo:',
                                                  choices = colSearch[['group']],
                                                  # multiple = TRUE,
                                                  selected = c('Angiospermas'))),
                                      column(
                                        width = 6,       
                                        uiOutput("forMenu")
                                        # uiOutput("forMenu2"),
                                      )),
                                    
                                    # selectInput(inputId = 'endemism',
                                    #             label = 'Endemismo BR:',
                                    #             choices = colSearch[['endemism']],
                                    #             multiple = TRUE,
                                    #             selected = c('ESPECIE')
                                    # ),
                                    fluidRow(column(
                                      width = 6,    
                                      selectInput(inputId = 'taxonRank',
                                                  label = 'Nível taxonômico:',
                                                  choices = colSearch[['taxonRank']],
                                                  multiple = TRUE,
                                                  selected = c('ESPECIE')
                                      )),
                                      
                                      column(
                                        width = 6,    
                                        selectInput(inputId = 'taxonomicStatus',
                                                    label = 'Status taxonômico:',
                                                    choices = colSearch[['taxonomicStatus']],
                                                    multiple = TRUE,
                                                    selected = c('NOME_ACEITO')
                                        )))),
                                  
                                  wellPanel(fluidRow(column(
                                    width = 12,    
                                    DT::dataTableOutput('fb2020Contents'))))
                                  
                              )),
                     
                     tabPanel(icon("magnifying-glass"),
                              box(title = 'Busca por espécies',
                                  status = "primary",width = 12,
                                  wellPanel(fluidRow(column(
                                    width = 12,
                                    
                                    textAreaInput(inputId = "Long_Text", 
                                                  label = "Lista de espécies", 
                                                  rows = 10, 
                                                  cols = 1,
                                                  value = ('Aphelandra longiflora;\nHygrophila costata;\nUtricularia breviscapa;'),
                                                  resize = "vertical"),
                                    
                                    br(),
                                    selectInput(inputId = 'taxonomicBackbone',
                                                label = 'Escolha a espinha dorsal taxonômica:',
                                                choices = list('FloraFungaBrasil'),
                                                multiple = TRUE,
                                                selected = c('FloraFungaBrasil')
                                    ),
                                    br(),
                                    actionButton("applyTaxonomicAlignment_Btn", "Aplicar alinhamento taxonômico", icon = icon("play")),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    box(title = 'Resultados da busca em Flora & Funga do Brasil',
                                        status = "primary",width = 12,
                                        DT::dataTableOutput('applyTaxonomicAlignment_Contents'),
                                    ),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    box(title = 'Resultados da busca em Plantas Aquáticas do Brasil',
                                        status = "primary",width = 12,
                                        
                                        DT::dataTableOutput('catalogo_fb2020_sumario_Contents'),
                                        
                                        br(),
                                        br(),
                                        br(),
                                        
                                        DT::dataTableOutput('catalogofb2020_Contents'),
                                    ),
                                    
                                    br(),
                                    downloadButton("downloadData_applyTaxonomicAlignment", "Baixar"),
                                    
                                    
                                  ))
                                  ))),
                     
                     
                     fluidRow(
                       column(width = 12,
                              box(title = 'Copyright ©',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    fluidRow(
                                      
                                      column(
                                        width = 12,
                                        helpText("Administrado pelo Núcleo de Especialistas em Plantas Aquáticas (NEPA)"),
                                        
                                        helpText("Desenvolvido por: Melo, Pablo Hendrigo Alves de"),
                                        
                                        helpText("Versão 1.0.0 de agosto/2024"),
                                        
                                      ))
                                    
                                  )))),
          )
        )
        # }
      )}
  
  #  Server
  server <- function(input, output, session)
  {
    
    # Taxonomic Alignment
    {
      applyTaxonomicAlignment <- 
        eventReactive(input$applyTaxonomicAlignment_Btn,
                      {
                        {
                          
                          
                          
                          x <- data.frame(Ctrl_scientificName=input$Long_Text) 
                          
                          
                          # x <- c('Aphelandra longiflora;\nHygrophila costata;\nUtricularia breviscapa;'),
                          # x <- c('Aphelandra longiflora'),
                          
                          x <- str_split(x,';', simplify = TRUE)
                          index <- x!=''
                          x <- x[index==TRUE]
                          
                          occ_all <- data.frame(Ctrl_scientificName=x) 
                          
                          colnames(occ_all) <- 'Ctrl_scientificName'
                          
                          occ_all$Ctrl_scientificName <- sub('\n','',glue::trim(occ_all$Ctrl_scientificName))
                          
                          name_search_wcvp <- occ_all$Ctrl_scientificName #%>% unique() %>% as.character()
                          # NROW(name_search_wcvp)
                          
                          occ_all <- occ_all %>%
                            dplyr::mutate(fb2020_taxonID = 0,
                                          fb2020_acceptedNameUsageID = 0,
                                          fb2020_parentNameUsageID = 0,
                                          fb2020_originalNameUsageID = 0,
                                          fb2020_scientificName = "",
                                          # fb2020_acceptedNameUsage = "",
                                          # fb2020_parentNameUsage = "",
                                          fb2020_namePublishedIn = "",
                                          fb2020_namePublishedInYear = 0,
                                          fb2020_higherClassification = "",             
                                          # fb2020_kingdom = "",
                                          # fb2020_phylum = "",                     
                                          # fb2020_class = "",
                                          # fb2020_order = "",                       
                                          fb2020_family = "",
                                          # fb2020_genus = "",                      
                                          fb2020_specificEpithet = "",
                                          fb2020_infraspecificEpithet = "",             
                                          fb2020_taxonRank = "",
                                          fb2020_scientificNameAuthorship = "",
                                          fb2020_taxonomicStatus = "",
                                          fb2020_nomenclaturalStatus = "",             
                                          fb2020_modified = lubridate::as_datetime("2021-10-31 21:13:33.77"),
                                          fb2020_bibliographicCitation = "",
                                          fb2020_references = "",
                                          fb2020_scientificNamewithoutAuthorship = "",
                                          fb2020_scientificNamewithoutAuthorship_U = "",
                                          fb2020_searchNotes = "",
                                          fb2020_searchedName = "")
                          
                          fb2020_na <- data.frame(fb2020_taxonID = 0,
                                                  fb2020_acceptedNameUsageID = 0,
                                                  fb2020_parentNameUsageID = 0,
                                                  fb2020_originalNameUsageID = 0,
                                                  fb2020_scientificName = "",
                                                  # fb2020_acceptedNameUsage = "",
                                                  # fb2020_parentNameUsage = "",
                                                  fb2020_namePublishedIn = "",
                                                  fb2020_namePublishedInYear = 0,
                                                  fb2020_higherClassification = "",             
                                                  # fb2020_kingdom = "",
                                                  # fb2020_phylum = "",                     
                                                  # fb2020_class = "",
                                                  # fb2020_order = "",                       
                                                  fb2020_family = "",
                                                  # fb2020_genus = "",                      
                                                  fb2020_specificEpithet = "",
                                                  fb2020_infraspecificEpithet = "",             
                                                  fb2020_taxonRank = "",
                                                  fb2020_scientificNameAuthorship = "",
                                                  fb2020_taxonomicStatus = "",
                                                  fb2020_nomenclaturalStatus = "",             
                                                  fb2020_modified = lubridate::as_datetime("2021-10-31 21:13:33.77"),
                                                  fb2020_bibliographicCitation = "",
                                                  fb2020_references = "",
                                                  fb2020_scientificNamewithoutAuthorship = "",
                                                  fb2020_scientificNamewithoutAuthorship_U = "",
                                                  fb2020_searchNotes = "",
                                                  fb2020_searchedName = "")
                          
                          
                        }
                        
                        withProgress(message = 'Processing...', style = 'notification', value = 0.5, 
                                     {
                                       x <- {}
                                       i <- 3
                                       # i <- 938
                                       
                                       ok <- FALSE
                                       japrocessado <<- rep(FALSE,NROW(name_search_wcvp))
                                       while (i<=NROW(name_search_wcvp) & ok != TRUE)
                                       {
                                         try(
                                           {
                                             
                                             for(i in 1:NROW(name_search_wcvp))
                                             {
                                               if(japrocessado[i]==TRUE){next} # 03-05-2022 para evitar tentar baixar a mesma espécies 2 vezes caso ocorra erro
                                               
                                               sp_tmp <- name_search_wcvp[i]
                                               
                                               if(any(input$taxonomicBackbone %in% c('FloraFungaBrasil'))==TRUE)
                                               {
                                                 x_tmp_fb2020 <- checkName_FloraFungaBrasil(searchedName = sp_tmp,
                                                                                            fb2020 = fb2020)
                                               }else
                                               {
                                                 x_tmp_fb2020 <- fb2020_na
                                               }
                                               
                                               
                                               x <- rbind(x,x_tmp_fb2020[,colunas_fb2020_sel])
                                               
                                               japrocessado[i] <- TRUE
                                             }
                                             
                                             ok <- TRUE
                                           })
                                         
                                         print('reconectar em 2 segundos...')
                                         Sys.sleep(2)
                                       }
                                       
                                       incProgress(100, detail = '100')
                                     })
                        
                        
                        occ_all
                        return(x)
                      })
      
      output$applyTaxonomicAlignment_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                     {
                                                                       shiny::validate(
                                                                         need(NROW(applyTaxonomicAlignment())>0,  "..."))
                                                                       
                                                                       occ_fb2020 <<- applyTaxonomicAlignment()
                                                                       occ_fb2020
                                                                       
                                                                     })
      
      
      count_col_especimes <- function(index){
        
        col_especimes <- c("brazilianRegion",                  "issues",                           "checkFloraBR2020",                
                           "notesFloraBR2020",                 "basisOfRecord",                    "collectionCode",                   "catalogNumber",                   
                           "threatStatus",                     "associatedReference",              "textReference",                    "recordedBy",                      
                           "recordNumber",                     "day",                              "month",                            "year",                            
                           "referenceFamily",                  "referenceGenus",                   "referenceIdentificationQualifier",
                           "referenceSpecificEpithet",         "referenceInfraspecificEpithet",    "referenceScientificnameauthor",    
                           # "typingUpdate"                     "typingUpdateFamily"               "typingUpdateGenus"                "typingUpdateSpecificEpithet"      "typingUpdateInfraspecificEpithet"
                           # "typingUpdateIdentifiedBy"         "typingUpdatedateIdentified"       "herbariumIdentificationUpdate"    "herbariumFamily"                 
                           # "herbariumGenus"                   "herbariumIdentificationQualifier" "herbariumSpecificEpithet"         "herbariumInfraspecificEpithet"   
                           # "herbariumScientificnameauthor"    
                           "identifiedBy",                     "dateIdentified",                   "country",                         
                           "stateProvince",                    "municipality",                     "locality",                         "decimalLatitude",                 
                           "decimalLongitude",                 "altitude",                         "hydrographicBasin",                "hydrograficSubbasin",             
                           "taxonRank",                        "groups",                           "kingdom",                          "phylum",                          
                           "class",                            "order",                            "family",                           "genus",                           
                           "identificationQualifier",          "specificEpithet",                  "infraspecificEpithet",             "scientificNameAuthorship",        
                           "scientificName",                   "lifeFormReference",                "aquaticLifeFormReference",         "habitatFloraBR",                  
                           "lifeFormFloraBR",                  "wetlandReference",                 "classifiedWetlandRefernce",        "wetlandClassification",           
                           "scientificNameSearch",             "ano_publicacao" )
        
        col_especimes <- data.frame(coluna=col_especimes,
                                    quantidade=rep(0,NROW(col_especimes)),
                                    valores=rep('...',NROW(col_especimes)))
        i=1
        for(i in 1:NROW(col_especimes))
        {
          
          df <- data.frame(valores = especimes[index==TRUE,][,col_especimes[i,1]] , stringsAsFactors = FALSE)
          df[,1] <- glue::trim(toupper(df[,1]))
          # df[,1] <- df[,1] %>% dplyr::arrange_all()
          valores <- df[,1] %>% unique()
          col_especimes$quantidade[i]=NROW(valores)
          
          if(NROW(valores)<=200){
            val<-{}
            for(ii in 1:NROW(valores)){
              if(is.null(val)){
                val<-valores[ii]
              }else{
                val<-paste0(val,'; ',valores[ii])}
            }
            col_especimes$valores[i]= val
          }
          
        }
        return(col_especimes)
      }
      
      output$catalogo_fb2020_sumario_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                     {
                                                                       shiny::validate(
                                                                         need(sum(especimes$scientificName %in% applyTaxonomicAlignment()$fb2020_scientificName)>0 &
                                                                                NROW(especimes)>0,  "..."))
                                                                       
                                                                       count_col_especimes(especimes$scientificName %in% applyTaxonomicAlignment()$fb2020_scientificName)
                                                                     })
      
      
      output$catalogofb2020_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                            {
                                                              shiny::validate(
                                                                need(sum(especimes$scientificName %in% applyTaxonomicAlignment()$fb2020_scientificName)>0 &
                                                                       NROW(especimes)>0,  "..."))
                                                              
                                                              # occ_fb2020
                                                              index <- especimes$scientificName %in% applyTaxonomicAlignment()$fb2020_scientificName
                                                              especimes[index==TRUE,]
                                                            })
      
    }
    
    # busca por taxon
    {
      output$hot_familia_key <- renderRHandsontable(
        {
          shiny::validate(
            need(input$filtrar_familia=='Sim',  "..."))
          
          updateSelectInput(session, "filtrar_genero",
                            selected = 'Não')
          
          updateSelectInput(session, "filtrar_sp",
                            selected = 'Não')
          
          
          dt <- especimes %>%
            dplyr::select(family)  %>% 
            dplyr::distinct_at(.,c('family')) %>% 
            dplyr::arrange_at(., c('family'))
          
          
          
          rhandsontable(dt,
                        row_highlight = 1,
                        
                        width = '100%', height = 200,
                        
                        digits = 0,
                        selectionMode = 'single',
                        selectCallback = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
        })
      
      output$hot_genero_key <- renderRHandsontable(
        {
          shiny::validate(
            need(input$filtrar_genero=='Sim',  "..."))
          # need(NROW(data_sel_catalogo())>0,  "..."))
          
          updateSelectInput(session, "filtrar_familia",
                            selected = 'Não')
          
          updateSelectInput(session, "filtrar_sp",
                            selected = 'Não')
          
          
          dt <- especimes %>%
            dplyr::select(genus)  %>% 
            dplyr::distinct_at(.,c('genus')) %>% 
            dplyr::arrange_at(., c('genus'))
          
          
          
          rhandsontable(dt,
                        row_highlight = 1,
                        
                        width = '100%', height = 200,
                        
                        digits = 0,
                        selectionMode = 'single',
                        selectCallback = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
        })
      
      output$hot_sp_key <- renderRHandsontable(
        {
          shiny::validate(
            need(input$filtrar_sp=='Sim',  "..."))
          # need(NROW(data_sel_catalogo())>0,  "..."))
          
          updateSelectInput(session, "filtrar_genero",
                            selected = 'Não')
          
          updateSelectInput(session, "filtrar_familia",
                            selected = 'Não')
          
          
          dt <- especimes %>%
            dplyr::select(scientificName)  %>% 
            dplyr::distinct_at(.,c('scientificName')) %>% 
            dplyr::arrange_at(., c('scientificName'))
          
          
          
          rhandsontable(dt,
                        row_highlight = 1,
                        
                        width = '100%', height = 200,
                        
                        digits = 0,
                        selectionMode = 'single',
                        selectCallback = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
        })
      
      
      ID_familia <- function(input)
      {
        linha <- input$hot_familia_key_select$select$r
        rr <- hot_to_r(input$hot_familia_key)
        
        if ( is.null(linha))
        {
          return(rr[1,1])
        }
        
        if ( linha>NROW(rr))
        {
          return(rr[1,1])
        }else
        {
          return(rr[linha,1])
        }
      }
      
      ID_genero <- function(input)
      {
        linha <- input$hot_genero_key_select$select$r
        rr <- hot_to_r(input$hot_genero_key)
        
        if ( is.null(linha))
        {
          return(rr[1,1])
        }
        
        if ( linha>NROW(rr))
        {
          return(rr[1,1])
        }else
        {
          return(rr[linha,1])
        }
      }
      
      ID_sp <- function(input)
      {
        linha <- input$hot_sp_key_select$select$r
        rr <- hot_to_r(input$hot_sp_key)
        
        if ( is.null(linha))
        {
          return(rr[1,1])
        }
        
        if ( linha>NROW(rr))
        {
          return(rr[1,1])
        }else
        {
          return(rr[linha,1])
        }
      }
      
      
    }
    
    
    # catalogo
    {
      index <- function(){
        index <- rep(TRUE,NROW(especimes)) 
        
        if(input$filtrar_familia=='Sim')
        {
          if(input$tipo_consulta=='E')
          {index <- index & especimes$family %in% ID_familia(input)}
          else
          {index <- index | especimes$family %in% ID_familia(input)}
        }
        
        
        if(input$filtrar_genero=='Sim')
        {
          if(input$tipo_consulta=='E')
          {index <- index & especimes$genus %in% ID_genero(input)}
          else
          {index <- index | especimes$genus %in% ID_genero(input)}
        }
        
        
        if(input$filtrar_sp=='Sim')
        {
          if(input$tipo_consulta=='E')
          {index <- index & especimes$scientificName %in% ID_sp(input)}
          else
          {index <- index | especimes$scientificName %in% ID_sp(input)}
        }
        
        
        if (input$brazilianRegion != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$brazilianRegion %in% input$brazilianRegion}
          else
          {index <- index | especimes$brazilianRegion %in% input$brazilianRegion}
        }
        
        if (input$hydrographicBasin != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$hydrographicBasin %in% input$hydrographicBasin}
          else
          {index <- index | especimes$hydrographicBasin %in% input$hydrographicBasin}
        }
        
        if (input$hydrograficSubbasin != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$hydrograficSubbasin %in% input$hydrograficSubbasin}
          else
          {index <- index | especimes$hydrograficSubbasin %in% input$hydrograficSubbasin}
        }
        if (input$country != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$country %in% input$country}
          else
          {index <- index | especimes$country %in% input$country}
        }
        if (input$stateProvince != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$stateProvince %in% input$stateProvince}
          else
          {index <- index | especimes$stateProvince %in% input$stateProvince}
          
        }
        if (input$municipality != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$municipality %in% input$municipality}
          else
          {index <- index | especimes$municipality %in% input$municipality}
        }
        if (input$locality != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$locality %in% input$locality}
          else
          {index <- index | especimes$locality %in% input$locality}
        }
        if (input$wetlandReference != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$wetlandReference %in% input$wetlandReference}
          else
          {index <- index | especimes$wetlandReference %in% input$wetlandReference}
        }
        if (input$associatedReference != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$associatedReference %in% input$associatedReference}
          else
          {index <- index | especimes$associatedReference %in% input$associatedReference}
        }
        if (input$ano_publicacao != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$ano_publicacao %in% input$ano_publicacao}
          else
          {index <- index | especimes$ano_publicacao %in% input$ano_publicacao}
        }
        if (input$basisOfRecord != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$basisOfRecord %in% input$basisOfRecord}
          else
          {index <- index | especimes$basisOfRecord %in% input$basisOfRecord}
        }
        if (input$collectionCode != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$collectionCode %in% input$collectionCode}
          else
          {index <- index | especimes$collectionCode %in% input$collectionCode}
        }
        if (input$recordedBy != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$recordedBy %in% input$recordedBy}
          else
          {index <- index | especimes$recordedBy %in% input$recordedBy}
        }
        # if (input$family != 'Tudo'){
        #    index <- index & especimes$family %in% input$family
        # }
        # if (input$genus != 'Tudo'){
        #    index <- index & especimes$genus %in% input$genus
        # }
        # 
        # # 			
        if (input$lifeFormReference != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$lifeFormReference %in% input$lifeFormReference}
          else
          {index <- index | especimes$lifeFormReference %in% input$lifeFormReference}
        }
        if (input$aquaticLifeFormReference != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$aquaticLifeFormReference %in% input$aquaticLifeFormReference}
          else
          {index <- index | especimes$aquaticLifeFormReference %in% input$aquaticLifeFormReference}
        }
        if (input$habitatFloraBR != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$habitatFloraBR %in% input$habitatFloraBR}
          else
          {index <- index | especimes$habitatFloraBR %in% input$habitatFloraBR}
        }
        if (input$lifeFormFloraBR != 'Tudo'){
          if(input$tipo_consulta=='E')
          {index <- index & especimes$lifeFormFloraBR %in% input$lifeFormFloraBR}
          else
          {index <- index | especimes$lifeFormFloraBR %in% input$lifeFormFloraBR}
        }
        return(index)
      } 
      
      data_sel_catalogo <- reactive({
        return(especimes[index()==TRUE,])
        
      })
      
      
      output$catalogo_sumario_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                              {
                                                                shiny::validate(
                                                                  need(sum(index())>0 &
                                                                         NROW(especimes)>0,  "..."))
                                                                
                                                                count_col_especimes(index())
                                                              })
      output$catalogoEspecimesContents <- DT::renderDataTable(options = list(
        columnDefs = list(list(className = 'dt-center', targets = 1)),
        pageLength = 5,
        lengthMenu = c(5, 50, 100, 1000),
        scrollX = TRUE,
        dom = "Blfrtip", 
        buttons =
          list("copy", list(
            extend = "collection"
            , buttons = c("csv", "excel", "pdf")
            , text = "Download"
          ))),
        filter = 'top',
        {
          data_sel_catalogo() %>%
            dplyr::arrange_at(., c('brazilianRegion','referenceFamily', 'referenceGenus', 'referenceSpecificEpithet' ))
          
          
        })
      
    }
    
    # fb2020
    {  
      output$fb2020Contents <- DT::renderDataTable(options = list(
        columnDefs = list(list(className = 'dt-center', targets = 1)),
        pageLength = 5,
        lengthMenu = c(5, 50, 100, 1000),
        scrollX = TRUE,
        dom = "Blfrtip", 
        buttons =
          list("copy", list(
            extend = "collection"
            , buttons = c("csv", "excel", "pdf")
            , text = "Download"
          ))),
        filter = 'top',
        {
          data_sel_fb2020() %>% 
            dplyr::select(colShow) %>%
            dplyr::arrange_at(., c('group','family', 'scientificName' ))
          
          
        })
      
      data_sel_fb2020 <- reactive({
        
        if(input$group == 'Algas' ){
          if (input$class_ID1 == 'Tudo'){
            # return(data_sel_quadro_geral())
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # endemism %in% input$endemism &
                                              taxonRank %in% input$taxonRank ))
            
          }else{
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              class %in% input$class_ID1 &
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # endemism %in% input$endemism &
                                              taxonRank %in% input$taxonRank ))
          }
        } else if(input$group == 'Fungos'){
          if (input$order_ID1 == 'Tudo'){
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # (endemism %in% input$endemism | is.na(endemism)) &
                                              taxonRank %in% input$taxonRank ))
          }else{
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              order %in% input$order_ID1 &
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # (endemism %in% input$endemism | is.na(endemism)) &
                                              taxonRank %in% input$taxonRank ))
          }
        } else {
          if (input$family_ID1 == 'Tudo'){
            # return(data_sel_quadro_geral())
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # endemism %in% input$endemism &
                                              taxonRank %in% input$taxonRank ))
            
          }else{
            return(fb2020 %>% dplyr::filter(group == input$group & 
                                              family %in% input$family_ID1 &
                                              # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                              taxonomicStatus %in% input$taxonomicStatus &
                                              # endemism %in% input$endemism &
                                              taxonRank %in% input$taxonRank ))
          }
        }
        
      })
      
      colSearch_sel <- reactive({
        colShow_tmp <-  c("phylum",
                          "class",
                          "order",
                          "family")
        
        colSearch_tmp <- as.list(colShow_tmp)
        names(colSearch_tmp) <- colShow_tmp
        
        data_sel_group <- fb2020 %>% dplyr::filter(group == input$group)
        
        for(i in 1:length(colShow_tmp)){
          
          x <- data_sel_group[,colShow_tmp[i]] %>%
            dplyr::arrange_at(.,colShow_tmp[i]) %>% unique()
          
          colSearch_tmp[[colShow_tmp[i]]] <- as.list(c('Tudo',
                                                       x )) #  %>% sort() #%>% toupper()
        }
        return(colSearch_tmp)
      })
      
      
      # A function to create a block of UI elements this 
      # function can be re-used to create similar blocks
      # of ui elements in different places
      createSelectRadio <- function(id, title){
        
        phylum_ID <- paste0('phylum_ID',id)
        class_ID <- paste0('class_ID',id)
        order_ID <- paste0('order_ID',id)
        family_ID <- paste0('family_ID',id)
        # endemismo_ID <- paste0('endemism_ID',id)
        
        
        if(id==1){
          # selectID <- paste0("myselect", id)
          # radioID <- paste0("myradio", id)
          # checkID <- paste0("mycheck", id)
          
          res <- list(
            if(input$group == 'Algas' ){
              selectInput(inputId = class_ID,
                          label = 'Classe:',
                          choices = colSearch_sel()[['class']])
            } else if(input$group == 'Fungos'){
              selectInput(inputId = order_ID,
                          label = 'Ordem:',
                          choices = colSearch_sel()[['order']])
            } else {
              selectInput(inputId = family_ID,
                          label = 'Família:',
                          choices = colSearch_sel()[['family']])
            })
        }
        
        # if(id==2){
        #    res <- list( 
        #       selectInput(inputId = endemismo_ID,
        #                   label = 'Endemismo:',
        #                   choices = colSearch[['endemism']] )
        #    )
        # }
        
        
        # selectInput(inputId = selectID, label="", choices = sample(LETTERS, 3)),
        # radioButtons(inputId = radioID, label="", choices = sample(letters,3)),
        # checkboxInput(inputId = checkID, label="", value=TRUE)
        
        
        return(res)
      }
      
      output$forMenu    <- renderUI({createSelectRadio(1)})
      
      # output$forMenu2    <- renderUI({createSelectRadio(2)})
    }
    
  }
  
  # shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
  shinyApp(ui = ui, server = server)
}


app_PlantasAquaticasBR()
