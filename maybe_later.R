# Doesn't really look that nice
pcode3_shape %>% 
  filter(ADM1_PCODE %in% c("SY02", "SY07")) %>% 
  left_join(eq %>% select(completely_destroyed_houses,
                          idps_in_all_centres,
                          admin4, admin3pcode), 
            by = c("ADM3_PCODE" = "admin3pcode")) %>% ggplot() + 
  geom_sf(size = 0.1, aes(fill = completely_destroyed_houses)) +
  scale_fill_viridis(begin = .5) +
  scale_size_continuous(labels = comma) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "white", colour = NA), 
        plot.caption = element_text(hjust = 0.5)) +
  guides(size = guide_legend(override.aes = list(alpha = 1))) + 
  labs(fill = "IDPs", 
       caption = "Data source: Assistance Coordination Unit, Syria") + 
  theme(plot.caption = element_text(hjust = 0.5))

# sites_beneficiaries_map
admin3 <- read_csv("./data/new_targets_cash.csv") %>% 
  filter(admin3Name_en != "Menbij") %>% 
  pull(admin3Pcode)

pcode3_shape %>% 
  filter(ADM3_PCODE %in% admin3) %>% 
  left_join(sites_locations_fsl, 
            by = c("ADM1_PCODE" = "admin1pcode")) %>% 
  st_as_sf() %>% 
  mutate(type = fct_relevel(type, 
                            c("RC/IDP", "Beneficiaries"))) %>%  
  filter(!is.na(type)) %>% 
  ggplot() + 
  geom_sf(size = 0.1) +
  geom_point(aes(size = persons, 
                 colour = type,
                 x = longitude_x, y = latitude_y), 
             alpha = .7) +
  scale_size_continuous(labels = comma, range = c(0, 10), 
                        breaks = c(0, 100, 300, 1000, 3000, 10000, 30000)) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "white", colour = NA), 
        plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Collective/reception centres and MPC beneficiaries", 
       subtitle = "IDPs in red (only those in centres), beneficiaries in blue, size shows number of persons") + 
  guides(size = guide_legend(override.aes = list(alpha = 1)))  

ggsave("sites_beneficiaries_map.png", dpi = 300, height = 11.7, width = 16.5, units = "in")



# district-beneficiaries-barplot
fsl %>% 
  filter(activity == "Cash Response" & planned_implemented == "Implemented") %>% 
  mutate(governorate = ifelse(governorate == "idleb", "Idleb", governorate)) %>% 
  group_by(governorate, district) %>% 
  summarise(beneficiaries = sum(beneficiaries, na.rm = TRUE)) %>% 
  mutate(district = fct_reorder(district, beneficiaries), 
         governorate = fct_relevel(governorate, 
                                   c("Idlib", "Aleppo"))) %>% 
  ggplot(aes(x = beneficiaries, y = district)) + 
  geom_col(aes(fill = governorate)) + 
  geom_text(aes(label = comma(beneficiaries)), 
            hjust = "inward") + 
  scale_fill_viridis_d(begin = .3) + 
  labs(title = "MPC beneficiaries by district", 
       subtitle = "as of 15 March 2023") + 
  scale_x_continuous(labels = comma)

ggsave("mpc_district.png", dpi = 300, height = 11.7, width = 16.5, units = "in")


site_map <- pcode3_shape %>% 
  filter(ADM1_PCODE %in% c("SY02", "SY07")) %>% 
  left_join(sites_locations_fsl, 
            by = c("ADM1_PCODE" = "admin1pcode")) %>% 
  st_as_sf() %>% 
  mutate(type = fct_relevel(type, 
                            c("RC/IDP", "Beneficiaries"))) %>% 
  ggplot() + 
  geom_sf(size = 0.1) +
  geom_point(aes(size = persons, 
                 colour = type,
                 x = longitude_x, y = latitude_y,
                 text = paste0("site :", site_name, "\n",  
                               "persons: ", persons, "\n", 
                               "type: ", type)), pch = 1
             # Adjusting alpha won't affect a plotly
  ) +
  scale_size_continuous(labels = comma) +
  theme_void() + 
  theme(plot.background = element_rect(fill = "white", colour = NA), 
        plot.caption = element_text(hjust = 0.5), 
        legend.position = "none") +
  labs(title = "Collective/reception centres and MPC beneficiaries", 
       subtitle = "IDPs in red, beneficiaries in blue, size shows number of persons")

ggplotly(site_map, tooltip = c("text")) %>% 
  plotly::style(hoveron = "point") %>% 
  layout(title = list(text = paste0("Collective/Reception centres and cash response beneficiaries", 
                                    "<br>", 
                                    "<sup>", 
                                    "IDPs in red, beneficiaries in blue, size shows number of persons; click and drag to zoom; mouse over for details"))) %>% 
  partial_bundle()


# Text removed from report
# Locations of beneficiaries have been estimated -- the FSL dataset does not call for the input of geocoordinates. Instead, the blue circles are situated at the centroids for communities/villages, as recorded in the OCHA geodamins reference dataset. 

# This means that we do not have an accurate record of which centres have received which activities. This underscores the importance of including of codifying and including each of the temporary sites so that we may track the progress of the response and identify any gaps in coverage that arise. Temporary sites have been included in the revised 4Ws. 