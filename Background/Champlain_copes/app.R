library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dataRetrieval)

theme_matt = function(base_size = 18,
                      dark_text = "grey20"){
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  ggpubr::theme_pubr(base_family="sans") %+replace% 
    theme(
      panel.background  = element_rect(fill="transparent", colour=NA), 
      plot.background = element_rect(fill="transparent", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      text = element_text(colour = mid_text, lineheight = 1.1),
      title = element_text(size = base_size * 1.5,
                           colour = dark_text),
      axis.text = element_text(size = base_size,
                               colour = mid_text),
      axis.title.x = element_text(size = base_size * 1.2,
                                  margin = unit(c(3, 0, 0, 0), "mm")),
      axis.title.y = element_text(size = base_size * 1.2,
                                  margin = unit(c(0, 5, 0, 0), "mm"), 
                                  angle = 90),
      legend.text = element_text(size=base_size * 0.9),
      legend.title = element_text(size = base_size * 0.9, 
                                  face = "bold"),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm")
    )
}

species_cols = c("Leptodiaptomus minutus" = "#ffd029",
                 "Leptodiaptomus minutus juvenile" = "#e3d8af",
                 "Leptodiaptomus minutus male" = "#ffe896",
                 "Leptodiaptomus sicilis" = "#D86F29",
                 "Leptodiaptomus sicilis male" = "#E28C00",
                 "Skistodiaptomus oregonensis" = "#C5C35A",
                 "Skistodiaptomus oregonensis male" = "#e6e6aa", 
                 "Epischura lacustris juvenile" = "plum1", 
                 "Epischura lacustris male" = "plum3", 
                 "Epischura lacustris" = "plum4", 
                 "Limnocalanus macrurus" = "skyblue4", 
                 "Limnocalanus macrurus male" = "skyblue3", 
                 "Limnocalanus macrurus juvenile" = "skyblue", 
                 "Senecella calanoides" = "darkseagreen3",
                 "Leptodora kindti male" = "lightblue3",
                 "Leptodora kindti" = "lightblue4",
                 "Leptodora kindti juvenile" = "lightblue")

#### Data ####

full_data = read.csv("https://raw.githubusercontent.com/ZoopEcoEvo/champlain_seasonality/main/Output/Data/full_data.csv") %>% 
  mutate("sp_name_sub" = str_replace_all(species, pattern = "_", replacement = " "),
         sp_name_sub = str_to_sentence(sp_name_sub), 
         "sp_name" = word(sp_name_sub, start = 1, end = 2),
         "sex" = case_when( # creates a new column called "sex" that is filled with different values when...
           word(sp_name_sub, start = 3, end = 3) == "male" ~ "male", #... the third word in sp_name_sub is 'male'
           word(sp_name_sub, start = 3, end = 3) == "juvenile" ~ "juvenile", #... or the third word in sp_name_sub is 'juvenile'
           TRUE ~ "female")) %>%  # In all other cases, 'female' is used 
  drop_na(size, ctmax)

# Lake Champlain near Burlington, VT
siteNumber = "04294500"
ChamplainInfo = readNWISsite(siteNumber)
parameterCd = "00010"
startDate = "2023-01-01"
endDate = ""
#statCd = c("00001", "00002","00003", "00011") # 1 - max, 2 - min, 3 = mean

# Constructs the URL for the data wanted then downloads the data
url = constructNWISURL(siteNumbers = siteNumber, parameterCd = parameterCd, 
                       startDate = startDate, endDate = endDate, service = "uv")

temp_data = importWaterML1(url, asDateTime = T) %>% 
  mutate("date" = as.Date(dateTime)) %>% 
  select(date, "temp" = X_00010_00000)

## Daily values for the period examined by dataset
collection_conditions = temp_data %>%
  ungroup() %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp),
            med_temp = median(temp),
            var_temp = var(temp), 
            min_temp = min(temp), 
            max_temp = max(temp)) %>% 
  mutate("range_temp" = max_temp - min_temp,
         date = as.Date(date)) %>% 
  ungroup() %>%  
  filter(date >= (min(as.Date(full_data$collection_date)) - 7))

## Mean female thermal limits for each species, grouped by collection
species_summaries = full_data %>%  
  #filter(sex == "female") %>% 
  group_by(sp_name, collection_date, collection_temp) %>%  
  summarise("mean_ctmax" = mean(ctmax),
            "sample_size" = n(),
            "ctmax_st_err" = (sd(ctmax) / sqrt(sample_size)),
            "ctmax_var" = var(ctmax), 
            "mean_size" = mean(size),
            "size_st_err" = (sd(size) / sqrt(sample_size)),
             "size_var" = var(size)) #%>%  
  # ungroup() %>% 
  # complete(sp_name, collection_date)

template_plot= ggplot() + 
  geom_vline(data = unique(select(full_data, collection_date)), 
             aes(xintercept = as.Date(collection_date)),
             colour = "grey90",
             linewidth = 1) + 
  geom_line(data = collection_conditions, 
            aes(x = as.Date(date), y = mean_temp),
            colour = "black", 
            linewidth = 2) + 
  # geom_errorbar(data = species_summaries,
  #               aes(x = as.Date(collection_date), 
  #                   ymin = mean_ctmax - ctmax_st_err, ymax = mean_ctmax + ctmax_st_err,
  #                   colour = sp_name),
  #               position = position_dodge(width = 1),
  #               width = 5, linewidth = 1) + 
  geom_point(data = species_summaries, 
             aes(x = as.Date(collection_date), y = mean_ctmax, colour = sp_name, size = sample_size, group = collection_date)) + 
  scale_colour_manual(values = species_cols) + 
  labs(x = "Date", 
       y = "Temperature (°C)", 
       colour = "Species",
       size = "Sample Size",
       group = "Collection Date") + 
  theme_matt() + 
  theme(legend.position = "right")

#### App ####

ui <- fluidPage(
  titlePanel("Lake Champlain Copepod Ecophysiology"),
  plotlyOutput("plot"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    plotOutput("ctmax_plot"),
    plotOutput("size_plot"),
    verbatimTextOutput("selected_date")
  )
)
# 
# ggplotly(template_plot, 
#          tooltip = c("group"))

server <- function(input, output) {
  # Render the plotly plot
  output$plot <- renderPlotly({
    plot_ly(
      data = species_summaries,
      x = ~collection_date,
      y = ~mean_ctmax,
      size = ~sample_size,
      color = ~sp_name,
      colors = species_cols,
      alpha = 1,
      type = "scatter",
      mode = "markers",
      hoverinfo = "x",
      source = "select_date"
    ) %>%
      layout(title = "Click on a Point to Select Date", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
  })
  
  # React to the selected data point
  observeEvent(event_data("plotly_click", source = "select_date"), {
    event <- event_data("plotly_click", source = "select_date")
    selected_date <- as.Date(event[["x"]])
    ymax = if_else(max(filter(species_summaries, as.Date(collection_date) == selected_date)$sample_size) < 10,
                   10,
                   max(filter(species_summaries, as.Date(collection_date) == selected_date)$sample_size))
    
    output$ctmax_plot <- renderPlot({
      filter(full_data, as.Date(collection_date) == selected_date) %>%  
        ggplot(aes(x = ctmax, fill = sp_name)) + 
        geom_histogram(binwidth = 1) + 
        scale_fill_manual(values = species_cols) + 
        xlim(20,45) + 
        theme_matt() + 
        theme(legend.position = "none")
    })
    
    output$size_plot <- renderPlot({
      filter(full_data, as.Date(collection_date) == selected_date) %>%  
        ggplot(aes(x = size, fill = sp_name)) + 
        geom_histogram(binwidth = 0.1) + 
        scale_fill_manual(values = species_cols) + 
        xlim(0,2) +
        theme_matt() + 
        theme(legend.position = "none")
    })
    
  })
}

shinyApp(ui, server)
