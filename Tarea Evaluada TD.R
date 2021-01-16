
# Cargue la data viajes2.csv que trabajó en la clase 3

library(readr)
library(dplyr)

#data <- read_csv("viajes2.csv")
data <- read_csv("viajes2.csv", locale = locale(encoding = "ISO-8859-1"))


# Cree dos columnas nuevas, una que contenga el dia y otra que contenga el mes

data1 <- data %>% mutate( Dia= as.numeric(substr(Fecha,start = 1, stop = 2))) %>%
                  mutate( Mes= substr(Fecha,start = 4, stop = 5) )

# Cree una columna que contenga la información si el día corresponde a un día de semana (lunes a viernes) o bien corresponde a un fin de semana (sábado o domingo).
# ( Considere que el 1 de mayo fue un día viernes )

dia.semana <- c("Viernes", "Sábado", "Domingo", "Lunes", "Martes", "Miércoles", "Jueves")
dia.semana_mod <- c("Jueves","Viernes", "Sábado", "Domingo", "Lunes", "Martes", "Miércoles", "Jueves")

data2 <- data1 %>% mutate(Dia.Modulo = Dia %% 7) %>% 
                  mutate(Dia.Semana = (ifelse(Dia.Modulo == 1, dia.semana[1], 
                  ifelse(Dia.Modulo == 2, dia.semana[2], 
                  ifelse(Dia.Modulo == 3, dia.semana[3],
                  ifelse(Dia.Modulo == 4, dia.semana[4], 
                  ifelse(Dia.Modulo == 5, dia.semana[5], 
                  ifelse(Dia.Modulo == 6, dia.semana[6],"Jueves")))))))) %>% select(-Dia.Modulo)


data2mod <- data1 %>% mutate(Dia.Modulo = Dia %% 7) %>% 
                      mutate(Dia.Semana = dia.semana_mod[Dia.Modulo + 1]) %>% select(-Dia.Modulo)
                             

# Según el horario de la transacción, cree una columna que indique su clasificación (Punta, Valle, Bajo) según la siguiente tabla

data3 <- data2 %>% mutate(hora = as.character(Hora)) %>% 
                  mutate(Horario = (ifelse((hora >= "07:00:00" & hora <=  "08:59:59") | (hora >= "18:00:00" & hora <=  "19:59:59"), "Punta", 
                                    ifelse((hora >= "09:00:00" & hora <=  "17:59:59") | (hora >= "20:00:00" & hora <=  "20:44:59"), "Valle",
                                    ifelse((hora >= "20:45:00" & hora <=  "22:00:00") | (hora >= "06:00:00" & hora <=  "06:59:59"), "Baja" ,NA))))) %>% 
                  select(-hora)


# Cree una nueva columna que incluya si la transacción fue realizada durante la mañana (00:00 am hasta 11:59 am) o bien durante la tarde (12:00 pm hasta 23:59 pm)

data4 <- data3 %>% mutate(hora = as.character(Hora)) %>% 
                   mutate(Transaccion = (ifelse((hora >= "00:00:00" & hora <=  "11:59:59"), "Mañana", 
                                          ifelse((hora >= "12:00:00" & hora <=  "23:59:59"),"Tarde", NA)))) %>% select(-hora)

# Cree un nuevo dataframe que muestra la cantidad de transacciones por estación según horario Punta, Valle, Bajo. 
# A partir de este dataframe, obtenga las dos estaciones que tienen más transacciones durante el horario Punta.

data5 <- data4 %>% group_by(Nombre_Estacion, Horario) %>% 
                   summarise(conteo = n())

data5 %>% filter(Horario == "Punta") %>% 
          arrange(desc(conteo)) %>% 
          head(2)

# Haga un dataframe resumen de la cantidad de transacciones seguún horario Punta, Valle y Bajo que además esté clasificado según Mañana o Tarde. 
# Si usted tuviera que implementar alguna medida para el atochamiento en una estación durante el horario Punta-Mañana, que estaciones elegiría?

data6 <- data4 %>% group_by(Nombre_Estacion, Horario, Transaccion) %>% 
                   summarise(conteo = n())

data6 %>% filter(Horario == "Punta" & Transaccion == "Mañana") %>% 
          arrange(desc(conteo))

# Haga el mismo estudio anterior, pero quite los fines de semana. Explique si cambia la información, en caso afirmativo de razones de por qué sucede eso?

data7 <- data4 %>% filter(Dia.Semana != "Sábado" & Dia.Semana != "Domingo") %>%
                   group_by(Nombre_Estacion, Horario, Transaccion) %>%
                   summarise(conteo = n())

data7 %>% filter(Horario == "Punta" & Transaccion == "Mañana") %>% 
          arrange(desc(conteo))

