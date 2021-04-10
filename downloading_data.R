# Initializing ====
#' Link to repository: https://github.com/rfsaldanha/microdatasus/

# Reading libraries ====
library(dplyr)
library(lubridate)
library(magrittr)
library(microdatasus)

# Downloading all the cases ====
df <- fetch_datasus(
  year_start = 2010,
  month_start = 01,
  year_end = 2020,
  month_end = 12,
  information_system = "SIM-DO"
)

df_copy <- df # storing df's content in another object, in case something breaks 

#' As of now, 2nd of April, 2021, the data from 2020 is not available.

# Adjusting all dates ====
format <- "%d%m%Y"
df$DTOBITO <- parse_date_time(df$DTOBITO, format)
df$DTNASC <- parse_date_time(df$DTNASC, format)
df$DTATESTADO <- parse_date_time(df$DTATESTADO, format)
df$DTINVESTIG <- parse_date_time(df$DTINVESTIG, format)
df$DTCADASTRO <- parse_date_time(df$DTCADASTRO, format)
df$DTRECEBIM <- parse_date_time(df$DTRECEBIM, format)
df$DTCADINF <- parse_date_time(df$DTCADINF, format)
df$DTCADINV <- parse_date_time(df$DTCADINV, format)
df$DTRECORIG <- parse_date_time(df$DTRECORIG, format)
df$DTRECORIGA <- parse_date_time(df$DTRECORIGA, format)
df$DTREGCART <- parse_date_time(df$DTREGCART, format)
df$DTCONINV <- parse_date_time(df$DTCONINV, format)
df$DTCONCASO <- parse_date_time(df$DTCONCASO, format)

# Calculating age based on birth and death dates ====
df$idade <- 
  interval(df$DTNASC, df$DTOBITO) /
  duration(num = 1, units = "years")

which(df$idade > 150) # case with an extreme age

#' Let's set this age as `NA`
df$idade[which(df$idade > 150)] <- NA

#' Reassuring that age isn't broken
which(df$idade > 150) #' integer(0)

#' Bringing *idade_calculada* forward
#' Dropping *IDADE*
#' Renaming  *idade_calculada* to *idade*
df %<>% 
  select(idade, everything()) %>% 
  select(- IDADE)

# Excluding columns that'll not be used ====
#' This saves space and makes the .csv smaller
#' 
#' A list with full explanation of variables is available here:
#' https://github.com/rfsaldanha/microdatasus/wiki/Conven%C3%A7%C3%B5es-SIM
#' 
#' The exclusion criteria is the following:
#' 1. Variables that do not have a reliable documentation
#' 2. Variables that contain redundant information
#' 3. Variables that won't contribute to the suicide analysis
df %<>%
  select(
    -c(
      contador,
      ORIGEM,
      TIPOBITO,
      CIRCOBITO,
      HORAOBITO,
      CODBAIRES,
      CODESTAB,
      CODMUNOCOR,
      CODBAIOCOR,
      IDADEMAE,
      PESO,
      NUMERODN,
      CODMUNNATU,
      OBITOGRAV,
      OBITOPUERP,
      EXAME,
      LINHAA,
      LINHAB,
      LINHAC,
      LINHAD,
      LINHAII,
      DTATESTADO,
      ACIDTRAB,
      TPPOS,
      DTINVESTIG,
      DTCADASTRO,
      ATESTANTE,
      DTRECEBIM,
      UFINFORM,
      CB_PRE,
      MORTEPARTO,
      DTCADINF,
      TPOBITOCOR,
      DTCADINV,
      CONTADOR,
      COMUNSVOIM,
      DTRECORIG,
      DTRECORIGA,
      CAUSAMAT,
      ESC2010,
      ESCMAE2010,
      STDOEPIDEM,
      STDONOVA,
      CODMUNCART,
      CODCART,
      NUMREGCART,
      DTREGCART,
      SERIESCFAL,
      ESCMAEAGR1,
      ESCFALAGR1,
      SERIESCMAE,
      SEMAGESTAC,
      TPMORTEOCO,
      EXPDIFDATA,
      DIFDATA,
      DTCONINV,
      DTCONCASO,
      NUDIASOBIN,
      CODMUNNATU,
      ESTABDESCR,
      CRM,
      NUMEROLOTE,
      STCODIFICA,
      CODIFICADO,
      VERSAOSIST,
      VERSAOSCB,
      ATESTADO,
      NUDIASOBCO,
      FONTES,
      TPRESGINFO,
      TPNIVELINV,
      NUDIASINF,
      FONTESINF,
      ALTCAUSA
    )
  )

# Saving as .csv ====
write.csv(df, 'total_datasus_2010_2019.csv')
