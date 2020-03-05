# Header ------------------------------------------------------------------

# 

# PROJECT: Tax Commission - Data Request

# AUTHOR: Mili Chapado
# DATE: 2019-03-26

# PURPOSE: Prepare 2010 median sales data and 2018 housing appreciation index data for visualizations and maps

# DETAILS: 
# 1) Prepare data for mapping - median sale price and HPI for 4 property types by CD
# 2) Create scatterplots and maps 

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(haven)
library(fcr)
library(DBI)
library(scales)
library(sf)
library(ggplot2)
library(viridis)


options(scipen = 999)

con <- fcr::fcdb_connect()

med_sales_price <- tbl(con, "sales_nyc_indicators") %>%
  collect()

hpi <- read_sas("///Index of Housing Price Appreciation/Annual/2018/Output/hpi_results_clean_2018.sas7bdat")

tc_dir <- ""

gis_dir <- ""

nyc_cd <- str_glue("{gis_dir}/Community_Districts/Raw/nycd_18c/nycd.shp") %>% 
  read_sf() %>% 
  st_transform(2263)

# DATA --------------------------------------------------------------------

# 2000 median sales price per CD for 4 property types
msp_00 <- med_sales_price %>%
  filter(year==2000,
         fc_geo_type %in% "cd") %>%
  select(fc_geoid, med_r_1f, volume_1f, med_r_4f, volume_4f, med_r_ot, volume_ot, med_r_cn, volume_cn)
  

# 2018 hpi per cd for 4 property types
geo <- c("cd","sba","boro","ny")
geo_types <- str_c(geo, collapse = "|")

hpi_18 <- hpi %>%
  filter(year == 2018) %>%
  gather(2:596, key = "fc_geo_type", value = "hpi") %>%
  mutate(fc_geo_type = as.character(fc_geo_type),
         cd_alpha = str_sub(fc_geo_type, start = -3),
         prop_type = str_sub(fc_geo_type, start = 5, end = 6),
         fc_geo = str_extract(fc_geo_type, geo_types)) %>%
  filter(fc_geo == "cd", 
         prop_type %in% c("1f", "4f", "ot", "cn")) %>%
  mutate(fc_geoid_1 = str_sub(cd_alpha, end = -3),
         dist = str_sub(cd_alpha, start = 2),
         new_code = recode(
           fc_geoid_1,
           `3` = "1",
           `1` = "2",
           `2` = "3",
           `4` = "4",
           `5` = "5"))%>%
  unite(new, new_code, dist, sep = "") %>%
  select(-cd_alpha, -fc_geoid_1, -year, -fc_geo_type) %>%
  rename(fc_geoid = new)


# Joining HPI and price and splitting by property type
p_1f <- hpi_18 %>%
  filter(prop_type %in% "1f") %>%
  full_join(select(msp_00, med_r_1f, volume_1f, fc_geoid), by = "fc_geoid") %>%
  mutate(cutoff = volume_1f < 10)

p_4f <- hpi_18 %>%
  filter(prop_type %in% "4f") %>%
  full_join(select(msp_00, med_r_4f, volume_4f, fc_geoid), by = "fc_geoid") %>%
  mutate(cutoff = volume_4f < 10)

p_ot <- hpi_18 %>%
  filter(prop_type %in% "ot") %>%
  full_join(select(msp_00, med_r_ot, volume_ot, fc_geoid), by = "fc_geoid") %>%
  mutate(cutoff = volume_ot < 10)

p_cn <- hpi_18 %>%
  filter(prop_type %in% "cn") %>%
  full_join(select(msp_00, med_r_cn, volume_cn, fc_geoid), by = "fc_geoid") %>%
  mutate(cutoff = volume_cn < 10)


# SCATTERPLOTS -----------------------------------------------------------

title <- "Median Sale Prices (2000) vs Housing Price Appreciation (2018)\nby Community District"
caption <- "\nSources: NYC Department of Finance, NYU Furman Center\n \nNotes: Housing price appreciation is indexed with 2000 set to a value of 100.\nCommunity Districts less than 10 sales in 2000 are greyed out."


# Single Family Plot
p_1f %>%
  ggplot(aes(log(med_r_1f), hpi, color=cutoff)) + 
  geom_point() +
  labs(
    title = title,
    subtitle = "Property Type: Single-Family",
    caption = caption,
    x = "Log 2000 Median Sale Prices",
    y = "2018 HPI"
  ) +
  scale_color_manual(values = c("black", "lightgray")) +
  theme_bw() +
  theme(plot.caption = element_text(colour = "grey50", face = "italic", hjust=0, vjust=1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave(
  str_glue("{tc_dir}/plot_1f.png"),
  width = 6.5, height = 5
)


# 2-4 Unit Properties
p_4f %>%
  ggplot(aes(log(med_r_4f), hpi, color=cutoff)) + 
  geom_point() +
  labs(
    title = title,
    subtitle = "Property Type: 2-4 Unit Properties",
    caption = caption,
    x = "Log 2000 Median Sale Prices",
    y = "2018 HPI"
  ) +
  scale_color_manual(values = c("black", "lightgray")) +
  theme_bw() +
  theme(plot.caption = element_text(colour = "grey50", face = "italic", hjust=0, vjust=1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave(
  str_glue("{tc_dir}/plot_4f.png"),
  width = 6.5, height = 5
)


# 5+ Unit Properties 
p_ot %>%
  ggplot(aes(log(med_r_ot), hpi, color=cutoff)) + 
  geom_point() +
  labs(
    title = title,
    subtitle = "Property Type: 5+ Unit Properties",
    caption = caption,
    x = "Log 2000 Median Sale Prices",
    y = "2018 HPI"
  ) +
  scale_color_manual(values = c("black", "lightgray")) +
  theme_bw() +
  theme(plot.caption = element_text(colour = "grey50", face = "italic", hjust=0, vjust=1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave(
  str_glue("{tc_dir}/plot_ot.png"),
  width = 6.5, height = 5
)


# Condos
p_cn %>%
  ggplot(aes(log(med_r_cn), hpi, color=cutoff)) + 
  geom_point() +
  labs(
    title = title,
    subtitle = "Property Type: Condos",
    caption = caption,
    x = "Log 2000 Median Sale Prices",
    y = "2018 HPI"
  ) +
  scale_color_manual(values = c("black", "lightgray")) +
  theme_bw() +
  theme(plot.caption = element_text(colour = "grey50", face = "italic", hjust=0, vjust=1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave(
  str_glue("{tc_dir}/plot_cn.png"),
  width = 6.5, height = 5
)


# MAPS -------------------------------------------------------------------

parks <- c("164", "226", "227", "228", "335", "355", "356", "480", "481", "482", "483", "484", "595")

sources_hpi <- "Sources: NYC Department of Finance, NYU Furman Center\n\nNotes: Housing price appreciation is indexed with 2000 set to a value of 100"
sources_med <- "Sources: NYC Department of Finance, NYU Furman Center\n\nNotes: Outliers that threw off the color scale were individually identified"

nyc_cd_ <- nyc_cd %>%
  filter(!(BoroCD %in% c("164", "226", "227", "228", "335", "355","356", "480", "481", "482", "483", "484", "595")))


### Single Family Maps ###

m_1f <- nyc_cd_ %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_1f, by = c("fc_geoid")) %>% 
  filter(cutoff==FALSE)

m_1f_ <- nyc_cd %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_1f, by = c("fc_geoid")) %>%
  filter(cutoff==TRUE|fc_geoid %in% parks) %>%
  mutate(fill = ifelse(is.na(cutoff), "Parks, airports, and cemeteries", "Fewer than 10 sales in 2000"))

# HPI 
ggplot() +
  geom_sf(data = m_1f, color = "white", size = 0.05, aes(fill = hpi)) +
  geom_sf(data = m_1f_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "2018 HPI") +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2018 Index for Housing Price Appreciation (HPI) by Community District",
    subtitle = "Property Type: Single-Family", 
    caption = sources_hpi) 
  
ggsave(
  str_glue("{tc_dir}/m_1f_hpi.png"),
  width = 12, height = 8
)

# Median Sales Price
ggplot() +
  geom_sf(data = filter(m_1f, fc_geoid != "108"), color = "white", size = 0.05, aes(fill = med_r_1f)) +
  geom_sf(data = m_1f_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  geom_sf(data = filter(m_1f, fc_geoid == "108"), color = "white", size = 0.05, fill = "red") +
  geom_text(data = filter(m_1f, fc_geoid == "108"), aes(label = fc_geoid, x = 996848.4, y = 221627.5), size = 3, color = "white") +
  annotate("text", x = 920000, y = 195000, label = "Outliers:\n   CD108 - $7,572,000", hjust=0, size = 3) + 
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "Median Sale Price", labels=comma) +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2000 Median Sale Prices by Community District",
    subtitle = "Property Type: Single-Family", 
    caption = sources_med) 

ggsave(
  str_glue("{tc_dir}/m_1f_msp.png"),
  width = 12, height = 8
)

### 2-4 Unit Property Maps ###

m_4f <- nyc_cd_ %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_4f, by = c("fc_geoid")) %>% 
  filter(cutoff==FALSE)

m_4f_ <- nyc_cd %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_4f, by = c("fc_geoid")) %>%
  filter(cutoff==TRUE|fc_geoid %in% parks) %>%
  mutate(fill = ifelse(is.na(cutoff), "Parks, airports, and cemeteries", "Fewer than 10 sales in 2000"))

# HPI 
ggplot() +
  geom_sf(data = m_4f, color = "white", size = 0.05, aes(fill = hpi)) +
  geom_sf(data = m_4f_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "2018 HPI") +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2018 Index for Housing Price Appreciation (HPI) by Community District",
    subtitle = "Property Type: 2-4 Unit Properties", 
    caption = sources_hpi) 

ggsave(
  str_glue("{tc_dir}/m_4f_hpi.png"),
  width = 12, height = 8
)

# Median Sales Price
ggplot() +
  geom_sf(data = filter(m_4f, fc_geoid !="102", fc_geoid != "106", fc_geoid != "108"), color = "white", size = 0.05, aes(fill = med_r_4f)) +
  geom_sf(data = m_4f_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  geom_sf(data = filter(m_4f, fc_geoid %in% c("102", "106", "108")), color = "white", size = 0.05, fill = "red") +
  geom_text(data = filter(m_4f, fc_geoid == "102"), aes(label = fc_geoid, x = 984000, y = 206000), size = 3, color = "white") +
  geom_text(data = filter(m_4f, fc_geoid == "106"), aes(label = fc_geoid, x = 990900, y = 210900), size = 3, color = "white", angle = 60) +
  geom_text(data = filter(m_4f, fc_geoid == "108"), aes(label = fc_geoid, x = 996848.4, y = 221627.5), size = 3, color = "white") +
  annotate("text", x = 920000, y = 195000, label = "Outliers:\n   CD102 - $1,611,900\n   CD106 - $1,012,100\n   CD108 - $1,971,700", hjust=0, size = 3) + 
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "Median Sale Price", labels=comma) +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2000 Median Sale Prices by Community District",
    subtitle = "Property Type: 2-4 Unit Properties", 
    caption = sources_med) 

ggsave(
  str_glue("{tc_dir}/m_4f_msp.png"),
  width = 12, height = 8
)


### 5+ Unit Property Maps ###

m_ot <- nyc_cd_ %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_ot, by = c("fc_geoid")) %>% 
  filter(cutoff==FALSE)

m_ot_ <- nyc_cd %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_ot, by = c("fc_geoid")) %>%
  filter(cutoff==TRUE|fc_geoid %in% parks) %>%
  mutate(fill = ifelse(is.na(cutoff), "Parks, airports, and cemeteries", "Fewer than 10 sales in 2000"))

# HPI 
ggplot() +
  geom_sf(data = m_ot, color = "white", size = 0.05, aes(fill = hpi)) +
  geom_sf(data = m_ot_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "2018 HPI") +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2018 Index for Housing Price Appreciation (HPI) by Community District",
    subtitle = "Property Type: 5+ Unit Properties", 
    caption = sources_hpi) 

ggsave(
  str_glue("{tc_dir}/m_ot_hpi.png"),
  width = 12, height = 8
)

# Median Sales Price
ggplot() +
  geom_sf(data = m_ot, color = "white", size = 0.05, aes(fill = med_r_ot)) +
  geom_sf(data = m_ot_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "Median Sale Price", labels=comma) +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2000 Median Sale Prices by Community District",
    subtitle = "Property Type: 5+ Unit Properties", 
    caption = sources_med) 

ggsave(
  str_glue("{tc_dir}/m_ot_msp.png"),
  width = 12, height = 8
)


### Condo Maps ###

m_cn <- nyc_cd_ %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_cn, by = c("fc_geoid")) %>% 
  filter(cutoff==FALSE)

m_cn_ <- nyc_cd %>%
  mutate(fc_geoid = as.character(BoroCD)) %>%
  full_join(p_cn, by = c("fc_geoid")) %>%
  filter(cutoff==TRUE|fc_geoid %in% parks) %>%
  mutate(fill = ifelse(is.na(cutoff), "Parks, airports, and cemeteries", "Fewer than 10 sales in 2000"))

# HPI 
ggplot() +
  geom_sf(data = m_cn, color = "white", size = 0.05, aes(fill = hpi)) +
  geom_sf(data = m_cn_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "2018 HPI") +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2018 Index for Housing Price Appreciation (HPI) by Community District",
    subtitle = "Property Type: Condos", 
    caption = sources_hpi) 

ggsave(
  str_glue("{tc_dir}/m_cn_hpi.png"),
  width = 12, height = 8
)

# Median Sales Price
ggplot() +
  geom_sf(data = m_cn, color = "white", size = 0.05, aes(fill = med_r_cn)) +
  geom_sf(data = m_cn_, color = "white", size = 0.05, fill = "gray45", aes(alpha=fill)) +
  scale_alpha_manual(values = c(0.25, 0.8), name = "") + 
  scale_fill_viridis_c(name = "Median Sale Price", labels=comma) +
  guides(alpha = guide_legend(order=0),
         fill = guide_colorbar(order=1)) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    legend.title = element_text(size=10, face = "bold"),
    plot.title = element_text(size=16, face="bold"),
    plot.subtitle = element_text(size= 12, color = "black"),
    plot.caption = element_text(size=10, color = "grey50", face = "italic", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.15, .70)
  ) +
  labs(
    title = "2000 Median Sale Prices by Community District",
    subtitle = "Property Type: Condos", 
    caption = sources_med) 

ggsave(
  str_glue("{tc_dir}/m_cn_msp.png"),
  width = 12, height = 8
)


