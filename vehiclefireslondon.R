library(ggplot2)  
library(dplyr)  
library(data.table)  

load_font()  
data <- read.csv(paste0(analysis_path(),"/vehicle fires/Lithium_Fires_Incidents.csv"))

data <- data |> 
  mutate(Type = trimws(Type))

yearlysummary <- data |> 
  group_by(CalendarYear,Type) |> 
  summarise(n=n()) 

summary <- data |> 
  filter(!grepl("Other", Type)) |>
  group_by(Type) |> 
  summarise(n=n()) |> 
  arrange(desc(n)) |> 
  slice(1:5)

yearlysummary <- yearlysummary |> filter(Type %in% summary$Type)


# Find the rightmost point for each line
label_points <- yearlysummary %>%
  group_by(Type) %>%
  filter(CalendarYear == max(CalendarYear))

plot <- yearlysummary |> 
  ggplot(aes(x = CalendarYear, y = n, color = Type)) +
  geom_line(size = 2) +
  geom_text(
    data = label_points,
    aes(label = Type),
    hjust = 0,          # left‑justify so text sits to the right      # tiny push to the right of the endpoint
    show.legend = FALSE,
    color = "black",
    size = 8,
    nudge_x = 0.05,
    family ="myfont"
  ) +
  theme_dj(background = "#e9e4ce",
           grid_color = "grey80") +
  labs(
    title = "E-Bikes Have Fast Become the Number One Source of EV Related\nFire Incidents in London",
    subtitle = "Data: Lithium and Electric Vehicle fires; London Fire Brigade, London Datastore",
    x = "Year",
    y = "Number of Incidents",
    caption = "The Data Journal"
  ) +
  theme(
    axis.title.y.right = element_blank(),
    strip.text = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    legend.position = "none"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.1)),breaks = seq(2017,2025,1)) +
  scale_y_continuous(limits = c(0, 250),breaks = seq(0,250,50)) +
  scale_color_manual(
  values = c(
    "e-bike"      = "#61210f",
    "Car"         = "#ff0000",
    "e-scooter"   = "#ff8d00",
    "Bus/coach"   = "#ffb400",
    "e-cigarette" = "#ffce00"
  )
)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("C:/Users/irffy/Documents/DataJournal/Analysis/vehicle fires/Vehicle_Fires.png",plot = plot,width = 1920, height = 1400, units="px",dpi=150)
