library(tidyverse)

######### 1. Lectura
# Lectura de datos a nivel municipal de México (archivo tipo csv)
df_mxmunicipio <- read_csv("datos/df_municipios.csv")
glimpse(df_mxmunicipio)

# Lee los datos de educación df_edu en la carpeta de datos 
# revisa las primeras entradas con la función glimpse()


######## 2. Filtrar
df_ej <- tibble(sexo = c("mujer", "hombre", "mujer", "mujer", "hombre"), 
                estatura = c(1.65, 1.80, 1.70, 1.60, 1.67))
df_ej

filter(df_ej, sexo == "mujer")
filter(df_ej, estatura > 1.65 & estatura < 1.75)

# Crea un subconjunto de los datos `df_mxmunicipio` que 
# contenga únicamente los municipios de la CDMX (state_abbr es CDMX)

# Los municipios de Nuevo León con más de 200,000 habitantes.

# Los municipios donde más de la mitad la población se autoidentifica como
# afromexicana o parte afromexicana.



######### 3. Seleccionar
df_ej
select(df_ej, sexo)
select(df_ej, -sexo)
select(df_ej, starts_with("s"))
select(df_ej, contains("x"))

# Ve la ayuda de select (?select) y escribe tres
# maneras de seleccionar las variables del estado en los datos df_mxmunicipio.


######## 4. Ordenar
arrange(df_ej, sexo)
arrange(df_ej, desc(estatura))

# Ordena los municipios por población, de mayor a menor.
# ¿Cuáles son los municipios con mayor disparidad de sexo (a total)?
# ¿Cuáles son los municipios con mayor disparidad de sexo (proporcional)?, 
# elimina los municipios con menos de 5000 habitantes y repite.


######## 5. Mutar
mutate(df_ej, estatura_cm = estatura * 100) 
mutate(df_ej, estatura_cm = estatura * 100, estatura_in = estatura_cm * 0.3937) 

# Calcula el porcentaje de población indígena de cada
# municipio y almacenalo en una nueva variable.
# Crea una nueva variable que muestre el cociente entre la población femenina y
# masculina.

######## 6. Summarise y resúmenes por grupo
summarise(df_ej, promedio = mean(estatura))

# Calcula la población total, indígena y afromexicana a total.

######## 7. Separa-aplica-combina 
by_sexo <- group_by(df_ej, sexo)
by_sexo
summarise(by_sexo, promedio = mean(estatura))

# Calcula la población total por estado (primero crea un data.frame agrupado).
# Calcula la población indígena y afromexicana por estado.
# ¿Qué otros resúmenes puedes hacer para explorar los datos?
  
by_metro_area <- group_by(df_mxmunicipio, metro_area)
no_miss <- filter(by_metro_area, !is.na(metro_area))
pop_metro_area <- summarise(no_miss, state = first(state_abbr), 
                            n_municipios = n(), pop_total = sum(pop))
head(pop_metro_area)


