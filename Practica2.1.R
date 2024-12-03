install.packages("googledrive")
install.packages("readr")
install.packages("dplyr")
library("readr")
library("googledrive")
library("dplyr")
filegdp <- drive_download(as_id("1KxWlJoX6JefI9dQxJSVe5c3qsf9Fh9Cr"), path = tempfile(), overwrite = TRUE)
fileoecd <- drive_download(as_id("1c3GGzi23SFttM7UF73JL_GPNerg-7HIq"), path = tempfile(), overwrite = TRUE)
filegdp <- read_csv(filegdp$local_path)
fileoecd <- read_csv(fileoecd$local_path)

head(filegdp)
head(fileoecd)

# Filtrar las columnas necesarias del archivo OECD
better_life_filtered <- fileoecd %>%
  select(LOCATION, OBS_VALUE)

# Filtrar los datos del archivo gdp-per-capita-worldbank.csv para obtener solo los países que coincidan y el año 2022
gdp_filtered <- filegdp %>%
  filter(Year == 2022, Code %in% better_life_filtered$LOCATION) %>%
  rename(LOCATION = Code)  # Renombrar la columna 'Code' a 'LOCATION' para que coincida

# Unir los datos filtrados
merged_data <- better_life_filtered %>%
  inner_join(gdp_filtered %>% select(Entity, LOCATION, Year, ny_gdp_pcap_pp_kd), by = "LOCATION")

# Reordenar las columnas y ordenar por ny_gdp_pcap_pp_kd
merged_data <- merged_data %>%
  select(Entity, LOCATION, OBS_VALUE, ny_gdp_pcap_pp_kd, Year) %>%
  arrange(desc(ny_gdp_pcap_pp_kd))  # Ordenar de mayor a menor por ny_gdp_pcap_pp_kd

# Mostrar los datos
print(merged_data)

