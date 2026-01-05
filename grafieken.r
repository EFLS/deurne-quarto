# Berekening van grafieken edm.

# Grafiek: Aantal ouderen in columns
g_aantal <- d %>%
  ggplot(aes(y = buurt_naam, x = ouderen_aantal)) +
  geom_col(fill = "maroon", alpha = 2/3) +
  geom_text(aes(label = ouderen_aantal), size = 2.5, hjust = -.1) +
  scale_x_continuous(expand = expansion(mult = c(-0, .1))) +
  labs(title = "Aantal ouderen per buurt in Deurne",
       x = "", y = "")


# Kaart: Aantal ouderen
rvt <- read_sf("./data/wzcs.geojson")
rvt$TOTALE_CAP <- parse_number(rvt$TOTALE_CAP)

# Geografische data van buurten inlezen
m <- read_sf("./data/stat_sector_2001.geojson")

# Samenvoegen obv NIS code
dg <- merge(m, d, by.x = "NISCODE", by.y = "buurt_nis")

# Kleurcodes opstellen
pal <- colorBin("Purples", d$ouderen_dichtheid,
                na.color = "#FFFFFF00", alpha = TRUE)
#
m_aantal <- leaflet(dg) %>%
  #  addTiles() %>%
  addPolygons(color = "black", weight = 2,
              fillColor = ~pal(ouderen_dichtheid), fillOpacity = 1,
              label = ~sprintf("%s: %.0f/ha", buurt_naam, ouderen_dichtheid)) %>%
  addLegend(pal = pal, values = ~ouderen_dichtheid,
            title = "Ouderendichtheid", opacity = 1) %>%
  addSymbolsSize(data = st_cast(rvt, "POINT"),
                 values = ~rvt$TOTALE_CAP, shape = "circle", baseSize = 10,
                 color = "red", fillColor = "orange",
                 label = ~sprintf("%s (%s plaatsen)", LABEL, TOTALE_CAP)) %>%
  addLegendSize(values = rvt$TOTALE_CAP, shape = "circle", baseSize = 10,
                color = "red", fillColor = "orange",
                title = "WZCs (cap.)", position = "topright", breaks = 3)
#
m_aantal


# Grafiek bevolkingssamenstelling per wijk
# Data in long format zetten
dl <- d %>%
  select(buurt_naam, ouderen_aantal, bevolking_rest) %>%
  pivot_longer(cols = c(ouderen_aantal, bevolking_rest),
               names_to = "type", values_to = "n")
# Sorteren 
#dl$type <- fct_reorder(dl$type, -dl$n)
#
g_samenstelling <- ggplot(dl, aes(y = buurt_naam)) +
  geom_col(aes(x = n, fill = type)) +
  geom_text(data = d,
            aes(label = bevolking_totaal,
                x = bevolking_totaal, y = buurt_naam),
            size = 2, hjust = -.1) +
  geom_text(data = d,
             aes(x = -.1 * max(bevolking_totaal, na.rm = TRUE),
                 label = sprintf("%.1f%%", ouderen_perc * 100)),
             size = 2.5, hjust = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.03, .075))) +
  scale_fill_discrete(breaks = c("ouderen_aantal", "bevolking_rest"),
                      labels = c("Ouderen", "Rest bevolking")) +
  labs(title = "Samenstelling bevolking",
       subtitle = "Aantal ouderen t.o.v. rest van bevolking",
       x = "", y = "", fill = "") +
  theme_minimal() +
  theme(palette.colour.discrete = "Reds",
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")
#
g_samenstelling

g_dichtheid <- d %>%
  ggplot(aes(y = buurt_naam,
             #y = sprintf("%.1f k/ha", ouderen_dichtheid / 100),
             x = ouderen_dichtheid,
             fill = ouderen_dichtheid)) +
  geom_col(na.rm = TRUE) +
  geom_text(data = d,
             aes(x = -.2 * max(ouderen_dichtheid, na.rm = TRUE),
                 label = sprintf("%2.1f", ouderen_dichtheid)),
             size = 2.5, hjust = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0))) +
  scale_x_reverse() +
  scale_fill_viridis_c(option = "rocket", direction = -1) +
  labs(title = "Ouderendichtheid",
       subtitle = "Oudere inwoners per ha",
       x = "", y = "",
       fill = "Ouderendichtheid") +
  theme_minimal()
#
g_dichtheid

# Combine both plots
g_bevolking_buurt <- 
  # Ouderendichtheid
  g_dichtheid +
  labs(y = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        legend.position = "") +
  # Samenstelling bevolking
  g_samenstelling +
  plot_layout(design = "ABB") +
  labs(caption = "Data: Stad in Cijfers") +
  theme(axis.text.y = element_text(hjust = .5),
        panel.grid.major.y = element_blank(),
        palette.colour.discrete = "Reds",
        legend.position = "bottom",
        plot.title = element_text(hjust = 0))
g_bevolking_buurt

