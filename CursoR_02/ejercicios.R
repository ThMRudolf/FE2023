library(tidyverse)

######### 1. Lectura
# Lectura de datos a nivel municipal de México (archivo tipo csv)
df_mxmunicipio <- read_csv("datos/df_municipios.csv")
glimpse(df_mxmunicipio)

# Lee los datos de educación df_edu en la carpeta de datos 
# revisa las primeras entradas con la función glimpse()

df_edu <- read_csv("datos/df_edu.csv")
glimpse(df_edu)

library(readxl)
women_school <- read_excel("datos/Years_in_school_women_25_plus.xlsx", 
                           sheet = "Data")
women_school


######## 2. Filtrar
df_ej <- tibble(sexo = c("mujer", "hombre", "mujer", "mujer", "hombre"), 
                estatura = c(1.65, 1.80, 1.70, 1.60, 1.67))
df_ej

filter(df_ej, sexo == "mujer")
filter(df_ej, estatura > 1.65 & estatura < 1.75)

# Crea un subconjunto de los datos `df_mxmunicipio` que 
# contenga únicamente los municipios de la CDMX (state_abbr es CDMX)
glimpse(df_mxmunicipio)
nrow(df_mxmunicipio)
municipios_cdmx_tbl <- filter(df_mxmunicipio, state_abbr == "CDMX")
municipios_cdmx_tbl
nrow(municipios_cdmx_tbl)

# Los municipios de Nuevo León con más de 200,000 habitantes.

mun_nl_grandes_tbl <- filter(df_mxmunicipio, 
                            state_abbr == "NL" & pop > 200000)
mun_nl_grandes_tbl
# Los municipios donde más de la mitad la población se autoidentifica como
# afromexicana o parte afromexicana.

afromex_tbl <- filter(df_mxmunicipio,
                      afromexican > 0.5 * pop)
afromex_tbl

sqrt(2) ^ 2 == 2
near(sqrt(2) ^ 2, 2)
abs(sqrt(2)^2 - 2) < 1e-15
?near

5 == NA
NA == NA
mean(c(5, 4,  NA))
NA_tbl <- filter(df_edu, is.na(schoolyrs))


######### 3. Seleccionar
df_ej
select(df_ej, sexo)
select(df_ej, -sexo)
select(df_ej, starts_with("s"))
select(df_ej, contains("x"))

# Ve la ayuda de select (?select) y escribe tres
# maneras de seleccionar las variables del estado en los datos df_mxmunicipio.

select(df_mxmunicipio, contains("state"))
select(df_mxmunicipio, state_abbr)
select(df_mxmunicipio, ends_with("abbr"))
select(df_mxmunicipio, - c(municipio_name:metro_area))

# extra
select(df_mxmunicipio, !contains(c("pop", "afro")))
select(df_mxmunicipio, state_abbr:pop)


######## 4. Ordenar
arrange(df_ej, sexo)
arrange(df_ej, estatura)
arrange(df_ej, desc(estatura))

# Ordena los municipios por población, de mayor a menor.
# ¿Cuáles son los municipios con mayor disparidad de sexo (a total)?
# ¿Cuáles son los municipios con mayor disparidad de sexo (proporcional)?, 
# elimina los municipios con menos de 5000 habitantes y repite.

arrange(df_mxmunicipio, desc(pop))
arrange(df_mxmunicipio, desc(abs(pop_male - pop_female)))
arrange(df_mxmunicipio, desc(abs(pop_male - pop_female)/pop))

df_mx_filtrada <-filter(df_mxmunicipio, pop > 5000)
arrange(df_mx_filtrada, 
        desc(abs(pop_male - pop_female)/pop))


######## 5. Mutar
mutate(df_ej, estatura_cm = estatura * 100) 
mutate(df_ej, estatura_cm = estatura * 100, estatura_in = estatura_cm * 0.3937) 

# Calcula el porcentaje de población indígena de cada
# municipio y almacenalo en una nueva variable.
# Crea una nueva variable que muestre el cociente entre la población femenina y
# masculina.
mutate(df_mxmunicipio, pct_indigenous = indigenous/pop*100)
mutate(df_mxmunicipio, pct_female_vs_male = pop_female/pop_male)

mutate(df_mxmunicipio, pct_fem = 100 * pop_female / pop)

######## 6. Summarise y resúmenes por grupo
summarise(df_ej, promedio = mean(estatura))

# Calcula la población total, indígena y afromexicana a total. // resultado siempre en tabla!!
summarise(df_mxmunicipio, pop_total = sum(pop))
summarise(df_mxmunicipio, pop_total_ind = sum(indigenous))
summarise(df_mxmunicipio, pop_total_afro = sum(afromexican))
######## 7. Separa-aplica-combina 
by_sexo <- group_by(df_ej, sexo)
by_sexo
summarise(by_sexo, promedio = mean(estatura))

# Calcula la población total por estado (primero crea un data.frame agrupado).
by_state <-group_by(df_mxmunicipio, state_abbr)
summarise(by_state, sum(pop))

# Calcula la población indígena y afromexicana por estado.
summarise(by_state, sum(indeginous))
summarise(by_state, sum(afromexican))

# ¿Qué otros resúmenes puedes hacer para explorar los datos?


by_metro_area <- group_by(df_mxmunicipio, metro_area)
no_miss <- filter(by_metro_area, !is.na(metro_area))
pop_metro_area <- summarise(no_miss, state = first(state_abbr), 
                            n_municipios = n(), pop_total = sum(pop))
head(pop_metro_area)


library(estcomp)


