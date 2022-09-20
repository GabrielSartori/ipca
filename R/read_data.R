#' @import dplyr  
#' @import lubridate 
#' @import purrr 
#' @import stringr
NULL

EXTDATA_DIR <- system.file("extdata", package = "ipca")
all_files <- list.files(EXTDATA_DIR, full.names = TRUE) 

# all_files <- file.path(EXTDATA_DIR, full.names = TRUE)

# all_files <- file.path(EXTDATA_DIR)

# PATHS <- file.path(EXTDATA_DIR, paste0("*_", region, "_Region_Mobility_Report.csv"))

# Sys.glob(EXTDATA_DIR)


#---------------------------------------------------------------------------------------------------------------------
#* Primeira Tentativa
# Carregando todos os dados

#' IPCA data
#'
#' @return
#' @export
#'
#' @examples
#' ipca
ipca <- function(...){

    df_metadados_excel <- metadados_excel(all_files)
    v_date_excel <- date_excel(df_metadados_excel)
    df_macro <- create_macro()

    #* Convertendo em tibble
    df_ipca <- 
        create_ipca(df_metadados_excel, v_date_excel) |> 
        tidy_ipca() |> 
        macro_ipca(macro = df_macro)

    return(df_ipca)
}

#---------------------------------------------------------------------------------------------------------------------
#* Dados de Meses 

df_mounth  <- function(...){
    df <- 
        data.frame(
            name = as.character(month(ymd(010101) + months(0:11), label = TRUE, abbr = FALSE))
            , number = paste0(1:12)
        )

    return(df)

}

to_latin1 <- function(x){return(`Encoding<-`(x, "latin1"))}
to_accent <- function(...){return(c("á","à","ã","â","é","è","ê","í","ì","î","õ","ó","ò","ô","ú","ù","û", 'ç'))}

#---------------------------------------------------------------------------------------------------------------------
#' Funções de Organização

metadados_excel <- function(df){
    
    nd <- 
        df |> 
        map(~readxl::excel_sheets(.x)) |>
        setNames(df) |> 
        map(~str_extract(.x, "MENSAL SUBITEM IPCA \\(TSO\\)|MENSAL SUBITEM IPCA\\(TSO\\)|MENSAL SUBITEM IPCA|RELATÓRIO"))    |> 
        map(~sort(.x)) |> 
        map(~na.omit(.x))  |> 
        map_df(~head(.x, 1))  |> 
        tidyr::pivot_longer(cols = everything(), names_to = "path", values_to = "sheet")

    return(nd)
    
}

date_excel <- function(df){

    nd <- 
        map2_chr(
            df$path
            , df$sheet
            , ~names(readxl::read_xls(.x, .y, skip = 2, n_max = 1))
            ) |> 
        map_chr(~gsub('MAR\xc7O', 'MARÇO', .x))  |> 
        strsplit(' ')  |> 
        map(~str_remove_all(.x, 'IPCA-15|IPCA15|IPCA|-'))   |> 
        map(~strsplit(.x, ' ')) |> 
        map(~compact(.x))   |> 
        map(function(x) ifelse(x %in% 'DE', '', x)) |> 
        map(~flatten_chr(.x)) |> 
        map(~sort(.x)) |> 
        map(~paste0(.x, collapse = ' ')) |> 
        map_chr(~str_squish(.x))   |> 
        map_chr(~gsub('JANEIRRO', "JANEIRO", .x))
    
    return(nd)
}

create_ipca <- function(df_metadado, v_date){

    df <-  
        map2(
            .x = df_metadado$path
            ,.y = df_metadado$sheet
            ,.f = ~readxl::read_xls(.x, sheet = .y, skip = 3) 
        ) |> 
        setNames(v_date) |> 
        plyr::ldply(.id = "ano_mes") 

    return(df)
}

tidy_ipca  <- function(df){
    
    nd <- 
        df |> 
        tibble() |> 
        tidyr::separate(ano_mes, c('ano', 'mes'), sep = " ")  |> 
        mutate(mes = tolower(mes))  |> 
        inner_join(df_mounth(), by = c('mes' = 'name'))   |> 
        mutate(date = zoo::as.Date(zoo::as.yearmon(paste0(ano, '-', number))))  |> 
        select(-c(ano, mes, number)) |> 
        arrange(date) |> 
           rename_all(recode, 
              ...1 = 'indice'
            , SL = 'c_sao_luis'
            , RB = 'c_rio_branco' 
            , CG = 'c_campo_grande' 
            , SAL = 'c_salvador' 
            , BEL = 'c_belem' 
            , AJU = 'c_aracaju' 
            , RJ = 'c_rio_de_janeiro' 
            , POA = 'c_porto_alegre' 
            , BH = 'c_belo_horizonte' 
            , SP = 'c_sao_paulo' 
            , DF = 'c_distrito_federal' 
            , BEL = 'c_belem' 
            , FOR = 'c_fortaleza' 
            , VIT = 'c_vitoria' 
            , REC = 'c_recife' 
            , SAL = 'c_salvador'
            , CUR = 'c_curitiba'
            , GOI = 'c_goiania'
            , NACIONAL = 'nacional'
        ) |> 
        filter(!is.na(indice)) |> 
        janitor::remove_empty(which = "cols")  |> 
        mutate_at(.vars = vars(-c("indice", "date")), as.numeric)   |>
        mutate(
                is_code_utf8 = map_int(indice, ~xfun:::invalid_utf8(.x) |> length())
                , is_acento_toupper = map_int(indice, ~str_detect(.x, toupper(to_accent())) |> sum())
                , indice_new = ifelse(is_code_utf8 == 1 & is_acento_toupper == 0, to_latin1(indice), indice) 
                , name_ipca = tolower(stringi::stri_trans_general(str = indice_new, id = "Latin-ASCII"))
            )  |> 
        select(
            date, name_ipca, nacional, dplyr::starts_with('c_')
        ) 
    
    return(nd)
}

#---------------------------------------------------------------------------------------------------------------------
#* Macro Ipca
 
create_macro <- function(...){

    #---------------------------------------------------------------------------------------------------------------------
    #' indice Geral

    indice_geral  <- tibble(
        cat_micro = 'Índice Geral'
        , cat_medio = 'Índice Geral'
        , cat_macro =  'Índice Geral'
    ) 

    #---------------------------------------------------------------------------------------------------------------------
    #' Alimentação 
    
    ind_alimentacao <- tibble(
        
        cat_micro = 'Alimentação e Bebidas'
        , cat_medio = 'Alimentação e Bebidas'
    )

    alimentacao_domicilio <- 
        tibble(cat_micro = 
            c(
                    'cereais, legum. e oleaginosas'
                    , 'Cereais, Leguminosas e Oleaginosas'
                    , 'Farinhas, Féculas e Massas'
                    , 'Tubérculos, Raízes e Legumes'
                    , 'Açúcares e Derivados'
                    , 'Hortaliças e Verduras'
                    , 'Frutas'
                    , 'Carnes'
                    , 'Pescados'
                    , 'Carnes e Peixes Industrializados'
                    , 'Aves e Ovos'
                    , 'Leites e Derivados'
                    , 'Panificados'
                    , 'Óleos e Gorduras'
                    , 'Bebidas e Infusões'
                    , 'Enlatados e Conservas'
                    , 'Sal e Condimentos'
            )
        ) |>    
        mutate(cat_medio = 'Alimentação no Domicílio')

    alimentacao_fora <- 
        tibble(
            cat_micro = 'Alimentação Fora do Domicílio'
            , cat_medio = 'Alimentação Fora do Domicílio'
        )

    alimentacao <- 
        bind_rows(ind_alimentacao, alimentacao_domicilio, alimentacao_fora) |> 
        mutate(cat_macro = 'Alimentação e Bebidas')

    

#---------------------------------------------------------------------------------------------------------------------
#'Habitação 
    
    ind_habitacao  <- tibble(
        cat_micro = 'Habitação'
        , cat_medio = 'Habitação'
    )

    encargo_e_manuntencao  <- tibble(
        cat_micro = 
            c(
                  "Aluguel e Taxas"
                , "Reparos"
                , "Artigos de Limpeza"
            )
        , cat_medio = 'Encargos e Manutenção'
    )


    combustivel_e_energia  <- tibble(
        cat_micro = 
            c(
                 'Combustíveis (Domésticos)'
                , 'Energia Elétrica Residencial'
            )
        , cat_medio = 'Combustíveis e Energia'
    )

    energia_eletrica_residencia  <- tibble(
        cat_micro = 'Energia Elétrica Residencial'
        , cat_medio = 'Energia Elétrica Residencial'
    )

    habitacao <- 
        bind_rows(ind_habitacao, encargo_e_manuntencao, combustivel_e_energia, energia_eletrica_residencia) |> 
        mutate(cat_macro = 'Habitação') 

    rm(encargo_e_manuntencao, combustivel_e_energia, energia_eletrica_residencia)

#---------------------------------------------------------------------------------------------------------------------
#' Artigos de Residência
  
   ind_artigo_de_residencia  <- tibble(
        cat_micro =  'Artigos de Residência'
        , cat_medio =  'Artigos de Residência'
    )
  
    moveis_e_utensilios <- tibble(
        cat_micro =  
            c(
                'Mobiliário'
                , 'Utensílios e Enfeites'
                , 'Cama, Mesa e Banho'
            )
        , cat_medio = 'Móveis e Utensílios'
        )

    aparelhos_eletroeletronicos <- tibble(
        cat_micro = 
            c( 
                'Eletrodomésticos e Equipamentos'
                , 'TV, Som e Informática'
            )
        , cat_medio = 'Aparelhos Eletroeletrônicos'
    )

    consertos_e_manutencao  <- tibble(
        cat_micro = 'Consertos e Manutenção'
        , cat_medio = 'Consertos e Manutenção'
    )
    
    artigos_de_residencia <- 
        bind_rows(ind_artigo_de_residencia, moveis_e_utensilios, aparelhos_eletroeletronicos, consertos_e_manutencao) |> 
        mutate(cat_macro = 'Artigos de Residência')

    # RM
    rm(ind_artigo_de_residencia, moveis_e_utensilios, aparelhos_eletroeletronicos, consertos_e_manutencao)

#---------------------------------------------------------------------------------------------------------------------
#'Vestuário

   ind_vestuario <- tibble(
            cat_micro = 'Vestuário'
            , cat_medio =  'Vestuário'
    )

    roupas <- tibble(
            cat_micro = c(
                  'Roupa Masculina'
                , 'Roupa Feminina'
                , 'Roupa Infantil'
            )
            , cat_medio =  'roupas'
    )

    calcados_e_acessorios  <- tibble(
        cat_micro = 'Calçados e Acessórios'
        , cat_medio = 'Calçados e Acessórios'
    )

    joias_e_bijuterias  <- tibble(
        cat_micro = 'Jóias e Bijuterias'
        , cat_medio = 'Jóias e Bijuterias'
    )

    tecidos_e_armarinho  <- tibble(
        cat_micro = 'Tecidos e Armarinho'
        , cat_medio = 'Tecidos e Armarinho'
    )

    vestuario <- 
        bind_rows(ind_vestuario, roupas, calcados_e_acessorios, joias_e_bijuterias, tecidos_e_armarinho) |> 
        mutate(cat_macro = 'Vestuário')

    # RM 
    rm(ind_vestuario, roupas, calcados_e_acessorios, joias_e_bijuterias, tecidos_e_armarinho)

#---------------------------------------------------------------------------------------------------------------------
#' Transportes

    transportes  <- tibble(
        cat_micro = 
            c(  
                'Transportes'
                , 'Transporte Público'
                , 'Veículo Próprio'
                , 'Combustíveis (Veículos)'
            )
        , cat_medio = 'Transportes'
        , cat_macro = 'Transportes'    
        )

#---------------------------------------------------------------------------------------------------------------------
#' Saúde e Cuidados Pessoais    

    
    ind_saude_e_cuidados_pessoais <- tibble(

        cat_micro = 'Saúde e Cuidados Pessoais'
        , cat_medio = 'Saúde e Cuidados Pessoais'
    )
   
    produtos_farmaceuticos_e_oticos <- tibble(
        cat_micro = 
            c(
                'Produtos Farmacêuticos'
                , 'Produtos Óticos'
            )
        , cat_medio = 'Produtos Farmacêuticos e Óticos'
    )

    servicos_de_saude  <- tibble(
            
            cat_micro = 
                c(  
                    'Serviços Médicos e Dentários'
                    , 'Serviços Laboratoriais e Hospitalares'
                    , 'Plano de Saúde'
                )
            , cat_medio = 'Serviços de Saúde'
    )

    cuidado_pessoal <- tibble(
        cat_micro = 'Higiene Pessoal'
        , cat_medio = 'Cuidados Pessoais'
    )

    saude_e_cuidados_pessoais <- 
        bind_rows(ind_saude_e_cuidados_pessoais, produtos_farmaceuticos_e_oticos, servicos_de_saude, cuidado_pessoal) |> 
        mutate(cat_macro = 'Saúde e Cuidados Pessoais')

    rm(produtos_farmaceuticos_e_oticos, servicos_de_saude, cuidado_pessoal)

#---------------------------------------------------------------------------------------------------------------------
#'Despesas Pessoais
#    

    ind_despesas_pessoais  <- tibble(
        cat_micro = 'Despesas Pessoais'
        , cat_medio = 'Despesas Pessoais'
    ) 

    servicos_pessoais <- tibble(
        cat_micro = 'Serviços Pessoais'
        , cat_medio = 'Serviços Pessoais'
    )

    recreacao_fumo_e_filmes <- tibble(
        cat_micro = 
            c(
                'Recreação'
                , 'Fumo'
                , 'Fotografia e Filmagem'
            )
        , cat_medio = 'Recreação, Fumo e Filmes'
    )

    despesas_pessoais <- 
        bind_rows(ind_despesas_pessoais, servicos_pessoais, recreacao_fumo_e_filmes) |> 
        mutate(cat_macro = 'Despesas Pessoais')

    rm(servicos_pessoais, recreacao_fumo_e_filmes)

#---------------------------------------------------------------------------------------------------------------------
#'Educação

    educacao <- tibble(
        cat_micro = 
            c(  
                'Educação'
                , 'Cursos' 
                , 'CURSOS REGULARES'
                , 'Leitura'
                , 'Papelaria'
                , 'Cursos Diversos'
            )
        , cat_medio = 'Cursos, Leitura e Papelaria'
        , cat_macro = 'Educação'
        )

#---------------------------------------------------------------------------------------------------------------------
#'Comunicação

    comunicacao <- tibble(
        cat_micro = 'comunicacao'
        , cat_medio = 'comunicacao'
        , cat_macro = 'comunicacao'
        )

#---------------------------------------------------------------------------------------------------------------------
#'Combinando

    df <- 
        bind_rows(
              indice_geral
            , alimentacao
            , habitacao
            , artigos_de_residencia
            , vestuario
            , transportes
            , saude_e_cuidados_pessoais
            , despesas_pessoais
            , educacao
            , comunicacao
        )

    nd <- 
        df |> 
        mutate_all(tolower) |> 
        mutate_all(stringi::stri_trans_general, id = "Latin-ASCII")

    return(nd)
}

macro_ipca <- function(df, macro){

    nd <- 
        df |> 
        mutate(name_ipca = case_when(name_ipca == 'recreacao, fumo e filmes' ~ 'recreacao e fumo', TRUE ~ name_ipca)) |> # Identificação Manual... 
        left_join(macro, by = c('name_ipca' = 'cat_micro'), keep = TRUE) |> 
        tidyr::fill(c(cat_micro, cat_medio, cat_macro), .direction = 'down')
    
    return(nd)
}


