library(tidyverse)
library(lubridate)
library(FinancialMath)
library(gridExtra)
library(scales)
library(latexpdf)
library(readxl)
library(parallel)


AMOTIZACIONES <- read_excel("Lineas.xlsx") %>% 
  mutate(primer_pago      = as.Date(primer_pago),
         fecha_desem = as.Date(fecha_desem))

funcion_amortizacion <- function(id,valor_libranza,interes_excel,
                                 plazo,cuota,primer_pago,fecha_desem){
  
  plazo                   <- plazo
  valor_libranza          <- valor_libranza
  interes_mensual         <- interes_excel 
  primer_pago             <- primer_pago
  fecha_desembolso        <- fecha_desem
  credito_id              <- id  
  cuota                   <- cuota
  
  
  
  interes <- pago <- saldo <- fecha <- vector("numeric", plazo)
  
  prestamo <- valor_libranza
  
  for (i in 1:plazo) {
    int         <- prestamo * interes_mensual
    pmt         <- cuota - int
    prestamo    <- prestamo - pmt
    
    interes[i]  <- int
    pago[i]     <- pmt
    saldo[i]    <- prestamo
  }
  
  calendario_de_pagos <- tibble(credito_id,
                                numero_de_pago = 1:plazo,
                                interes,
                                pago,
                                cuota           = interes+pago,
                                saldo) %>% 
    mutate(fecha = seq(from = ymd(primer_pago), by = "month",
                       length.out = plazo)) %>%
    select(fecha, everything()) %>% 
    add_row(credito_id = credito_id, fecha = fecha_desembolso, numero_de_pago = 0, interes = 0, 
            saldo = valor_libranza ,pago = 0,cuota = 0, .before = 1) %>%
    mutate(fecha = if_else(day(fecha) != 30,fecha-2,fecha)) %>% 
    relocate(fecha, .before = numero_de_pago) %>% 
    modify_at(c("interes","pago","cuota","saldo"),
              scales::dollar,largest_with_cents = 1e+06)
  
  calendario_de_pagos <- as_tibble(calendario_de_pagos)
  
}


amortizaciones_totales <- AMOTIZACIONES %>% 
  mutate(tabla_amor = pmap(.l = list(id,valor_libranza,
                                     interes_excel,plazo,cuota,primer_pago,fecha_desem),
                           .f = funcion_amortizacion))


output_pdf <- function(data,id){ 
  
  pdf(paste0("-","Tabla_de_amortizacion",".pdf"),height=11, width=10)
  grid.table(amor1)
  dev.off()
}

amor1 <- amortizaciones_totales$tabla_amor[1] %>%
  as_tibble()

amor1 %>% 
  pivot_wider(names_from = name,values_from = value)

amor1 %>%
  unnest()

amortizaciones_totales %>% 
  select(id,tabla_amor) %>% 
  pmap(.l = list(data = amortizaciones_totales$tabla_amor, id = id),.f = output_pdf)


tabla_de_datos %>% 
  pmap(.l = list(data = tabla_de_datos$tabla_amor,id),.f= output_pdf)


list(data = amortizaciones_totales$tabla_amor, id = id) %>% 
  pmap(.f = output_pdf)


amortizaciones_totales %>% 
  filter(id == 123) %>%
  select(tabla_amor)

amortizaciones_totales %>% 
  select(tabla_amor) %>% 
  unnest(cols = tabla_amor) %>% 
  openxlsx::write.xlsx("amortizaciones.xlsx")




  

