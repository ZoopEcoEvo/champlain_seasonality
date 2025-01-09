Seasonality in Lake Champlain Copepod Thermal Limits
================
2025-01-09

- [Copepod Collection](#copepod-collection)
- [Temperature Variability](#temperature-variability)
- [Trait Variation](#trait-variation)
  - [Variation with temperature](#variation-with-temperature)
  - [Sex and stage variation in thermal
    limits](#sex-and-stage-variation-in-thermal-limits)
  - [Trait Correlations and
    Trade-offs](#trait-correlations-and-trade-offs)
- [Other patterns in variation](#other-patterns-in-variation)
- [Distribution Lag Non-Linear Model (DLNM
  approach)](#distribution-lag-non-linear-model-dlnm-approach)
- [Hindcasting vulnerability with different acclimation
  scenarios](#hindcasting-vulnerability-with-different-acclimation-scenarios)
  - [Scenario 1](#scenario-1)
  - [Scenario 2](#scenario-2)
  - [Scenario 3](#scenario-3)

## Copepod Collection

Copepods were collected at approximately weekly intervals from Lake
Champlain (Burlington Fishing Pier). Plankton was collected from the top
3 meters using a 250 um mesh net.

``` r
# # Lake Champlain near Burlington, VT
# siteNumber = "04294500"
# ChamplainInfo = readNWISsite(siteNumber)
# parameterCd = "00010"
# startDate = "2023-01-01"
# endDate = "2024-5-20"
# #statCd = c("00001", "00002","00003", "00011") # 1 - max, 2 - min, 3 = mean
# 
# # Constructs the URL for the data wanted then downloads the data
# url = constructNWISURL(siteNumbers = siteNumber, parameterCd = parameterCd,
#                        startDate = startDate, endDate = endDate, service = "uv")
# 
# raw_temps = importWaterML1(url, asDateTime = T) %>%
#   mutate("date" = as.Date(dateTime),
#          "hour" = hour(dateTime)) %>%
#   select(dateTime, tz_cd, date, hour, degC = X_00010_00000)
# 
# temp_data =  raw_temps %>%
#   select(date, hour, "temp" = degC)
# 
# write.csv(temp_data, file = "./Output/Data/champlain_temps.csv", row.names = F)
```

Collections began in late May 2023. Several gaps are present, but
collections have continued at roughly weekly intervals since then.
Copepods from 48 collections were used to make a total of 1312 thermal
limit measurements. Over this time period, collection temperatures
ranged from 2.5 to 26.5°C.

There is substantial variation in thermal limits across the species
collected. There is also some degree of variation within the species,
with thermal limits increasing slightly during the summer.

``` r
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
  filter(date >= (min(as.Date(full_data$collection_date)) - 7)) %>% 
  left_join(unique(select(full_data, collection_date, collection_temp)), by = join_by(date == collection_date))

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
            "size_var" = var(size)) %>%  
  ungroup() %>% 
  complete(sp_name, collection_date) %>% 
  arrange(desc(sample_size))

adult_summaries = full_data %>%  
  filter(sex == "female") %>% 
  group_by(sp_name, collection_date, collection_temp) %>%  
  summarise("mean_ctmax" = mean(ctmax),
            "sample_size" = n(),
            "ctmax_st_err" = (sd(ctmax) / sqrt(sample_size)),
            "ctmax_var" = var(ctmax), 
            "mean_size" = mean(size),
            "size_st_err" = (sd(size) / sqrt(sample_size)),
            "size_var" = var(size)) %>%  
  ungroup() %>% 
  complete(sp_name, collection_date) %>% 
  arrange(desc(sample_size))


tseries_data = full_data %>% 
  mutate(sp_name = fct_reorder(sp_name, ctmax, .desc = T))

ggplot() + 
  geom_vline(data = unique(select(tseries_data, collection_date)), 
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
  # geom_point(data = adult_summaries, 
  #            aes(x = as.Date(collection_date), y = mean_ctmax, colour = sp_name, size = sample_size)) + 
  geom_point(data = tseries_data, 
             aes(x = as.Date(collection_date), y = ctmax, colour = sp_name),
             size = 2, position = position_jitter(width = 1, height = 0)) + 
  scale_colour_manual(values = species_cols) + 
  labs(x = "Date", 
       y = "Temperature (°C)", 
       colour = "Species",
       size = "Sample Size") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/main-fig-ctmax-timeseries-1.png" style="display: block; margin: auto;" />

``` r

lake_temps = ggplot() + 
  geom_line(data = collection_conditions, 
            aes(x = as.Date(date), y = mean_temp),
            colour = "black", 
            linewidth = 1) + 
  labs(x = "Date", 
       y = "Temperature (°C)", 
       colour = "Species",
       size = "Sample Size") + 
  theme_matt() + 
  theme(legend.position = "right")
```

Temperatures observed at the time of collection closely resembled the
maximum daily temperature from the temperature sensor data. Maximum
temperature was used as a proxy instead of mean temperature as
collections were usually made during afternoons or early evenings, just
following the warmest part of the day.

``` r
collection_conditions %>% 
  drop_na(collection_temp) %>%  
  ggplot(aes(x = max_temp, y = collection_temp)) + 
  geom_abline(intercept = 0, slope = 1,
              linewidth = 1, colour = "grey") + 
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(5,15,25)) + 
  scale_y_continuous(breaks = c(5,15,25)) + 
  labs(x = "Max. Temp. from Sensor (°C)",
       y = "Collection Temp. (°C)") + 
  theme_matt()
```

<img src="../Figures/markdown/supp-fig-temp-acc-1.png" style="display: block; margin: auto;" />

Size also varied, but primarily between rather than within species.

``` r
ggplot() + 
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
  geom_point(data = adult_summaries, 
             aes(x = as.Date(collection_date), y = mean_size * 40, colour = sp_name, size = sample_size),
             position = position_dodge(width = 1)) + 
  scale_colour_manual(values = species_cols) + 
  scale_y_continuous(
    name = "Temperature", # Features of the first axis
    sec.axis = sec_axis(~./40, name="Prosome Length (mm)"), # Add a second axis and specify its features
    breaks = c(0,5,10,15,20,25,30)
  ) + 
  labs(x = "Date", 
       y = "Temperature (°C)", 
       colour = "Species") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/supp-fig-size-timeseries-1.png" style="display: block; margin: auto;" />

``` r
sample_dates_plot = full_data %>%  
  filter(sp_name != "Osphranticum labronectum") %>% 
  mutate(sp_name = as.factor(sp_name),
         sp_name = fct_reorder(sp_name, ctmax)) %>% 
  ggplot(aes(x = lubridate::as_date(collection_date), 
             y = sp_name, fill = sp_name)) + 
  # geom_vline(xintercept = as_date(
  #   c("2023-05-01",
  #     "2023-09-01",
  #     "2024-01-01",
  #     "2024-05-01")),
  #   colour = "grey",
  #   linewidth = 1) + 
  geom_density_ridges(bandwidth = 30,
                      jittered_points = TRUE, 
                      point_shape = 21,
                      point_size = 1,
                      point_colour = "grey30",
                      point_alpha = 0.8,
                      alpha = 0.8,
                      position = position_points_jitter(
                        height = 0.1, width = 0)) + 
  scale_fill_manual(values = species_cols) + 
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b") + 
  coord_cartesian(xlim = lubridate::as_date(c("2023-06-08", "2024-05-08"))) + 
  labs(x = "Day of Year", 
       y = "Species") + 
  theme_matt() + 
  #theme_ridges(grid = T) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5))
```

The samples captured the broad seasonal changes in calanoid copepod
community composition in the lake. We note, however, that rare species
(e.g. *Senecella* and *Limnocalanus*) were often preferentially sampled,
so are over-represented in the data set.

Throughout the season, the prevalence of various unidentified pathogens
also varied, with very little infection observed during the Winter and
Spring.

``` r
pathogen_cols = c("no" = "grey95", "cloudy" = "honeydew3", "spot" = "antiquewhite3", "other" = "tomato3")

full_data %>% 
  select(collection_date, dev_eggs, pathogen, lipids, sp_name, sex) %>% 
  group_by() %>% 
  filter(sex != "juvenile") %>% 
  group_by(collection_date) %>% 
  count(pathogen) %>% 
  filter(pathogen != "uncertain") %>% 
  pivot_wider(id_cols = "collection_date", 
              names_from = pathogen, 
              values_from = n,
              values_fill = 0) %>% 
  mutate(total = sum(no, cloudy, spot, other)) %>% 
  pivot_longer(cols = c(no, cloudy, spot, other),
               names_to = "pathogen", 
               values_to = "count") %>% 
  mutate(percent = count/total,
         collection_date = lubridate::as_date(collection_date),
         pathogen = fct_relevel(pathogen, "no", "cloudy", "spot", "other")) %>% 
  ggplot(aes(x = collection_date, y = percent, fill = pathogen)) + 
  geom_area() + 
  scale_fill_manual(values = pathogen_cols) + 
  scale_y_continuous(breaks = c(0,1)) + 
  labs(x = "Collection Date", 
       y = "Proportion", 
       fill = "Pathogen") + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line())
```

<img src="../Figures/markdown/supp-fig-pathogen-props-1.png" style="display: block; margin: auto;" />

The transparent bodies of these copepods also allowed us to examine
seasonal patterns in lipid reserves and in the production of eggs.
Maturing oocytes are visible in female copepods before they are
released. There was no strong seasonal cycle in the production of these
eggs in any species, and instead, females were reproductively active
throughout their respective seasons of occurence.

``` r
dev_eggs_cols = c("no" = "grey95", "yes" = "lightblue3")

full_data %>% 
  select(collection_date, dev_eggs, pathogen, lipids, sp_name, sex) %>% 
  group_by(sp_name) %>% 
  filter(sex != "juvenile") %>% 
  group_by(sp_name, collection_date) %>% 
  count(dev_eggs) %>% 
  filter(dev_eggs != "uncertain") %>% 
  pivot_wider(id_cols = c("collection_date", "sp_name"), 
              names_from = dev_eggs, 
              values_from = n,
              values_fill = 0) %>% 
  mutate(total = sum(no, yes)) %>% 
  pivot_longer(cols = c(no, yes),
               names_to = "dev_eggs", 
               values_to = "count") %>% 
  mutate(percent = count/total,
         collection_date = lubridate::as_date(collection_date),
         dev_eggs = fct_relevel(dev_eggs, "no", "yes")) %>% 
  ungroup() %>% 
  complete(collection_date, nesting(sp_name, dev_eggs), fill = list(percent = 1)) %>% 
  mutate(percent = if_else(is.na(total) & dev_eggs == "yes", 0, percent)) %>% 
  ggplot(aes(x = collection_date, y = percent, fill = dev_eggs)) + 
  facet_wrap(sp_name~., ncol = 1) + 
  geom_area() + 
  scale_fill_manual(values = dev_eggs_cols) + 
  scale_y_continuous(breaks = c(0,1)) + 
  labs(x = "Collection Date", 
       y = "Proportion", 
       fill = "Developing \nEggs") + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line())
```

<img src="../Figures/markdown/supp-fig-deveggs-props-1.png" style="display: block; margin: auto;" />

The presence of lipids varied across species, with only *L. minutus*,
*L. sicilis*, and *Limnocalanus* regularly possessing lipid stores.

``` r
lipid_cols = c("no" = "grey95", "yes" = "sienna2")

full_data %>% 
  select(collection_date, dev_eggs, pathogen, lipids, sp_name, sex) %>% 
  group_by(sp_name) %>% 
  filter(sex != "juvenile") %>% 
  group_by(sp_name, collection_date) %>% 
  count(lipids) %>% 
  filter(lipids != "uncertain") %>% 
  pivot_wider(id_cols = c("collection_date", "sp_name"), 
              names_from = lipids, 
              values_from = n,
              values_fill = 0) %>% 
  mutate(total = sum(no, yes)) %>% 
  pivot_longer(cols = c(no, yes),
               names_to = "lipids", 
               values_to = "count") %>% 
  mutate(percent = count/total,
         collection_date = lubridate::as_date(collection_date),
         lipids = fct_relevel(lipids, "no", "yes")) %>% 
  ungroup() %>% 
  complete(collection_date, nesting(sp_name, lipids), fill = list(percent = 1)) %>% 
  mutate(percent = if_else(is.na(total) & lipids == "yes", 0, percent)) %>% 
  ggplot(aes(x = collection_date, y = percent, fill = lipids)) + 
  facet_wrap(sp_name~., ncol = 1) + 
  geom_area() + 
  scale_fill_manual(values = lipid_cols) + 
  scale_y_continuous(breaks = c(0,1)) + 
  labs(x = "Collection Date", 
       y = "Proportion", 
       fill = "Lipids\nPresent") + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_line())
```

<img src="../Figures/markdown/supp-fig-lipids-props-1.png" style="display: block; margin: auto;" />

## Temperature Variability

Lake Champlain is highly seasonal, with both average temperatures and
temperature variability changing throughout the year. These patterns in
the experienced thermal environment may drive the observed variation in
copepod thermal limits. However, the time period affecting copepod
thermal limits is unknown. Depending the on the duration of time
considered, there are large changes in the experienced environment, in
particular regarding the temperature range and variance. Consider for
example three time periods: the day of collection, one week prior to
collection, and four weeks prior to collection. While the overall
pattern is similar, we can see that, unsurprisingly, considering longer
periods of time results in larger ranges and slightly changes the
pattern of variance experienced.

``` r
## Defining the function to get predictor values for periods of different lengths
get_predictors = function(daily_values, raw_temp, n_days){
  prefix = str_replace_all(xfun::numbers_to_words(n_days), pattern = " ", replacement = "-")
  
  mean_values = daily_values %>% 
    ungroup() %>% 
    mutate(mean_max = slide_vec(.x = max_temp, .f = mean, .before = n_days, .complete = T),
           mean_min = slide_vec(.x = min_temp, .f = mean, .before = n_days, .complete = T),
           mean_range = slide_vec(.x = range_temp, .f = mean, .before = n_days, .complete = T)) %>% 
    select(date, mean_max, mean_min, mean_range) %>% 
    rename_with( ~ paste(prefix, "day", .x, sep = "_"), .cols = c(-date))
  
  period_values = raw_temp %>% 
    mutate(mean = slide_index_mean(temp, i = date, before = days(n_days), 
                                   na_rm = T),
           max = slide_index_max(temp, i = date, before = days(n_days), 
                                 na_rm = T),
           min = slide_index_min(temp, i = date, before = days(n_days),
                                 na_rm = T),
           med = slide_index_dbl(temp, .i = date, .before = days(n_days), 
                                 na_rm = T, .f = median),
           var = slide_index_dbl(temp, .i = date, .before = days(n_days), 
                                 .f = var),
           range = max - min) %>%  
    select(-temp) %>%  
    distinct() %>% 
    rename_with( ~ paste(prefix, "day", .x, sep = "_"), .cols = c(-date))%>% 
    inner_join(mean_values, by = c("date")) %>%  
    drop_na()
  
  return(period_values)
}
```

Organisms are unlikely to acclimate instantaneously to changes in
temperature. To explore the potential temporal window these copepods are
responding to, we examined the correlation between thermal limits and
summaries of the thermal environment over different periods of time. For
each species (inclusive of all sexes and stages), we examined the
correlation between CTmax and one of nine representations of the thermal
environment calculated for periods of time from 1 to 60 days before
collection. These parameters include the overall maximum, minimum,
median, and mean temperature for the period of time, the temperature
range and variance during this time, and the mean daily temperature
maximum, minimum, and range. We also examined the correlation between
CTmax and the temperature recorded at the time of collection.

Shown below are the correlation coefficients for these relationships.
Each facet shows the relationship for a different parameter, plotted
against the duration of the time period before collection.

``` r
corr_vals %>% 
  mutate(parameter = fct_relevel(parameter, c("min", "max", "range",
                                              "mean", "med", "var",
                                              "mean_min", "mean_max", "mean_range"))) %>% 
  ggplot(aes(x = duration, y = correlation, colour = sp_name)) + 
  facet_wrap(.~parameter) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 0.9) + 
  geom_line(linewidth = 1.5) + 
  scale_colour_manual(values = species_cols) + 
  labs(x = "Duration (days)",
       y = "Correlation", 
       colour = "Species") + 
  theme_matt_facets()
```

<img src="../Figures/markdown/supp-fig-correlation-durations-1.png" style="display: block; margin: auto;" />

This table contains the top three factors for each species (based on
correlation coefficient).

Shown here is a graphical summary of the duration of the best predictors
for each species. Note that for the two Leptodiaptomids, collection
temperature had the largest correlation coefficient so duration is zero.
This representation highlights that there is variation across the
community not only in the potential driver (e.g. minimum vs. maximum
temperatures) but also in the duration of time. This variation is not
grouped by season (the winter and summer communities both have
representative species apparently responding to short and long
durations).

``` r
corr_vals %>%  
  filter(sig == "Sig.") %>% 
  drop_na(correlation) %>% 
  group_by(sp_name) %>%
  arrange(desc(correlation)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate("num" = row_number(), 
         sp_name = fct_reorder(sp_name, duration, .fun = mean, .desc = T)) %>% 
  arrange(sp_name) %>% 
  select("Species" = sp_name, "Predictor" = parameter, "Duration" = duration, "Correlation" = correlation, num) %>% 
  ggplot(aes(x = Species, y = Duration, fill = Predictor, group = num)) + 
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(width = 0.6),
           colour = "black") + 
  scale_fill_manual(values = c("coll_temp" = "black", "max" = "white", "min" = "grey")) + 
  theme_matt() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/main-fig-acc-durations-1.png" style="display: block; margin: auto;" />

## Trait Variation

Shown below are the clutch size distributions for the three diaptomiid
species, which produce egg sacs that allow for easy quantification of
fecundity.

``` r
full_data %>%  
  drop_na(fecundity) %>%  
  ggplot(aes(x = fecundity, fill = sp_name_sub)) + 
  facet_wrap(.~sp_name_sub, ncol = 1) + 
  geom_histogram(binwidth = 2) + 
  scale_fill_manual(values = species_cols) + 
  labs(x = "Fecundity (# Eggs)") +
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/supp-fig-fecundity-histogram-1.png" style="display: block; margin: auto;" />

One of the main aims of this project is to examine the patterns and
processes driving variation in upper thermal limits across these species
of copepods.

### Variation with temperature

We expect one of the primary drivers of copepod thermal limits to be
temperature, as individuals acclimate to seasonal changes. Shown below
are the seasonal patterns of when copepods were included in CTmax
measurements (a proxy for the season of occurrence), and thermal limits
for each species plotted against the temperature at the time of
collection. We generally see an increase in thermal limits with
increasing collection temperature.

``` r
sp_ctmax_temp = full_data %>% 
  filter(sp_name != "Osphranticum labronectum") %>% 
  mutate(sp_name = as.factor(sp_name),
         sp_name = fct_reorder(sp_name, ctmax, .desc = T)) %>% 
  ggplot(aes(x = collection_temp, y = ctmax, colour = sp_name)) + 
  facet_wrap(sp_name~.) + 
  geom_smooth(method = "lm", se = F, linewidth = 1.5, colour = "grey30") + 
  geom_point(size = 2, alpha = 0.4) + 
  labs(x = "Collection Temp. (°C)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "none")
```

``` r
ggarrange(sample_dates_plot, sp_ctmax_temp, nrow = 1, 
          labels = "AUTO")
```

<img src="../Figures/markdown/main-fig-sp-summaries-1.png" style="display: block; margin: auto;" />

The interaction between seasonal changes in temperature and the
acclimation of thermal limits likely affects vulnerability of each
species to warming. Shown below are warming tolerance values for each
species, calculated as the difference between individual CTmax and the
temperature at the time of collection. All species maintained some
degree of buffer between environmental temperatures and upper thermal
limits, but *L. minutus* appears to approach its upper thermal limit
during the warmest collections during the summer.

Also shown below is the relationship between fecundity (the number of
eggs contained in a clutch) for the three diaptomid species. For the two
Leptodiaptomus species, there is no relationship between clutch size and
temperature, while there appears to be a general increase in clutch size
with temperature in the Skistodiaptomus species.

``` r

wt_temp = full_data %>% 
  filter(sp_name != "Osphranticum labronectum") %>% 
  ggplot(aes(x = collection_temp, y = warming_tol, colour = sp_name)) + 
  geom_point(size = 3,
             alpha = 0.3) + 
  geom_smooth(method = "lm", linewidth = 3) +
  labs(x = "Collection Temperature (°C)", 
       y = "Warming Tolerance (°C)",
       colour = "Species")  + 
  ylim(0,30) + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "none")

eggs_temp = full_data %>% 
  filter(sp_name != "Osphranticum labronectum") %>% 
  ggplot(aes(x = collection_temp, y = fecundity, colour = sp_name)) + 
  geom_point(size = 3,
             alpha = 0.3) + 
  geom_smooth(method = "lm", linewidth = 3) +
  labs(x = "Collection Temperature (°C)", 
       y = "Fecundity (# Eggs)",
       colour = "Species")  + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")

ggarrange(wt_temp, eggs_temp, 
          common.legend = T, legend = "right")
```

<img src="../Figures/markdown/main-fig-trait-coll-temp-plots-1.png" style="display: block; margin: auto;" />

``` r
# Copepods spent several days in lab during experiments. Shown below are the CTmax residuals (taken from a model of CTmax against collection temperature) plotted against the time spent in lab before measurements were made. Individual regressions are shown for the residuals against days in lab for each collection. We can see clearly that thermal limits are fairly stable over time. 


# ggplot(ctmax_resids, aes(x = days_in_lab, y = resids, colour = sp_name, group = collection_date)) + 
#   facet_wrap(sp_name~.) + 
#   geom_point(size = 4, alpha = 0.5) + 
#   geom_smooth(method = "lm", se = F, linewidth = 1) + 
#   #scale_x_continuous(breaks = c(0:5)) + 
#   labs(x = "Days in lab", 
#        y = "CTmax Residuals") + 
#   scale_colour_manual(values = species_cols) + 
#   theme_matt_facets() + 
#   theme(legend.position = "none")
```

``` r

model_data = full_data %>%  
  drop_na(size, ctmax) %>%  
  filter(sp_name != "Osphranticum labronectum") %>% 
  mutate(temp_cent = scale(collection_temp, center = T, scale = F),
         size_cent = scale(size, center = T, scale = F))

minimal.model = lme4::lmer(data = model_data,
                           ctmax ~ sp_name + sex + temp_cent +
                             (1|days_in_lab))

full.model = lme4::lmer(data = filter(model_data, sp_name != "Osphranticum labronectum"),
                        ctmax ~ sp_name*sex*temp_cent +
                          (1|days_in_lab))

drop1(full.model, scope=~.)
## Single term deletions
## 
## Model:
## ctmax ~ sp_name * sex * temp_cent + (1 | days_in_lab)
##                       npar    AIC
## <none>                     5280.1
## sp_name                  0 5280.1
## sex                      0 5280.1
## temp_cent                0 5280.1
## sp_name:sex             10 5271.0
## sp_name:temp_cent        0 5280.1
## sex:temp_cent            0 5280.1
## sp_name:sex:temp_cent   10 5281.0

reduced.model = lme4::lmer(data = filter(model_data, sp_name != "Osphranticum labronectum"),
                           ctmax ~ sp_name + sex + temp_cent +
                             sp_name:temp_cent + sex:temp_cent +
                             sp_name:sex:temp_cent +
                             (1|days_in_lab))

performance::test_performance(minimal.model, reduced.model, full.model)
## Name          |   Model |      BF | df | df_diff |   Chi2 |      p
## ------------------------------------------------------------------
## minimal.model | lmerMod |         | 11 |         |        |       
## reduced.model | lmerMod |  > 1000 | 28 |   17.00 | 169.80 | < .001
## full.model    | lmerMod | < 0.001 | 38 |   10.00 |  10.90 | 0.365 
## Models were detected as nested (in terms of fixed parameters) and are compared in sequential order.
performance::check_model(reduced.model)
```

<img src="../Figures/markdown/supp-fig-model-tests-1.png" style="display: block; margin: auto;" />

``` r

car::Anova(reduced.model, type = "III", test = "F")
## Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
## 
## Response: ctmax
##                               F Df  Df.res    Pr(>F)    
## (Intercept)           5200.6616  1   17.49 < 2.2e-16 ***
## sp_name                 86.8617  5 1277.20 < 2.2e-16 ***
## sex                     32.6821  2 1280.05 1.435e-14 ***
## temp_cent               67.3972  1 1275.90 5.380e-16 ***
## sp_name:temp_cent       10.6290  5 1277.95 5.073e-10 ***
## sex:temp_cent           15.0090  2 1276.10 3.607e-07 ***
## sp_name:sex:temp_cent    6.8593 10 1276.41 1.661e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

sp_ctmax = emmeans::emmeans(reduced.model, specs = "sp_name") %>% 
  data.frame() %>% 
  select(sp_name, "species_ctmax" = emmean)

model_coefs = emmeans::emtrends(reduced.model, var = "temp_cent", specs = "sp_name") %>% 
  data.frame() %>% 
  inner_join(sp_ctmax) 

ctmax_resids = model_data %>% 
  mutate(resids = residuals(reduced.model))
```

``` r
arr_combined = synth_arr %>%
  filter(measure == "upper" & mean_lim > 20) %>% 
  select("group" = genus, arr, mean_lim) %>% 
  mutate("dataset" = "synthesis") %>% 
  bind_rows(
    select(model_coefs, "group" = sp_name, 'arr' = temp_cent.trend, 'mean_lim' = species_ctmax)
  ) %>% 
  mutate(dataset = if_else(is.na(dataset), "new data", "synthesis"),
         group = fct_reorder(group, arr, .desc = T))


ggplot(arr_combined, aes(x = mean_lim, y = arr)) + 
  geom_smooth(method = "lm", se = F, 
              linewidth = 2, colour = "grey30") + 
  geom_point(data = filter(arr_combined, dataset != "new data"), 
             size = 4, colour = "grey") + 
  geom_point(data = filter(arr_combined, dataset == "new data"),
             aes(colour = group), 
             size = 4) + 
  scale_colour_manual(values = species_cols) + 
  labs(x = "Thermal Limit", 
       y = "ARR", 
       colour = "Species") +
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/main-fig-ARR-synth-plot-1.png" style="display: block; margin: auto;" />

### Sex and stage variation in thermal limits

Previous sections have generally lumped juvenile, female, and male
individuals together. There may be important stage- or sex-specific
differences in CTmax though. For all species but Osphranticum, we have
measurements for individuals in different stages and of different sexes.

``` r
sex_sample_sizes = ctmax_resids %>%  
  group_by(sp_name, sex) %>%  
  summarise(num = n()) %>%  
  pivot_wider(id_cols = sp_name,
              names_from = sex, 
              values_from = num,
              values_fill = 0) %>% 
  select("Species" = sp_name, "Juvenile" = juvenile, "Female" = female, "Male" = male)

knitr::kable(sex_sample_sizes, align = "c")
```

|        Species         | Juvenile | Female | Male |
|:----------------------:|:--------:|:------:|:----:|
|  Epischura lacustris   |    37    |   45   |  20  |
| Leptodiaptomus minutus |    12    |  273   |  38  |
| Leptodiaptomus sicilis |    31    |  356   |  95  |
| Limnocalanus macrurus  |    4     |   43   |  39  |
|  Senecella calanoides  |    13    |   21   |  8   |
|   Skistodiaptomus sp   |    15    |  231   |  28  |

Across group comparisons show that there are generally no differences in
thermal limits (represented here as the residuals from a CTmax ~
collection_temp x species linear regression), with the exception of
Senecella males, which may have lower thermal limits (although sample
sizes are very small in this group).

``` r
# ctmax_resids %>% 
#   filter(sp_name != "Osphranticum labronectum") %>% 
#   ggplot(aes(x = sex, y = resids, colour = sp_name)) + 
#   facet_wrap(sp_name~.) + 
#   geom_jitter(width = 0.1, alpha = 0.5) + 
#   geom_boxplot(width = 0.4, fill = NA, colour = "black", 
#                linewidth = 1, outlier.colour = NA) + 
#   scale_colour_manual(values = species_cols) + 
#   theme_matt_facets()
```

### Trait Correlations and Trade-offs

A relationship between size and upper thermal limits has been suggested
in a wide range of other taxa. Shown below are the measured upper
thermal limits plotted against prosome length. The overall relationship
(inclusive of all species) is shown as the black line in the background.
Regressions for each individual species are also shown. Across the
entire assemblage, there is a strong decrease in thermal limits with
increasing size.

``` r
full_data %>% 
  #filter(sex == "female") %>%  
  ggplot( aes(x = size, y = ctmax, colour = sp_name)) + 
  geom_point(size = 2, alpha = 0.3) + 
  geom_smooth(data = full_data, 
              aes(x = size, y = ctmax),
              method = "lm", 
              colour ="black", 
              linewidth = 2.5) + 
  labs(x = "Length (mm)", 
       y = "CTmax (°C)",
       colour = "Species") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/misc-ctmax-size-1.png" style="display: block; margin: auto;" />

Shown here is the relationship for each species individually.

``` r
full_data %>% 
  #filter(sex == "female") %>%  
  group_by(sp_name) %>% filter(n() >2) %>% filter(!str_detect(sp_name, pattern = "kindti")) %>% 
  ggplot( aes(x = size, y = ctmax, colour = sp_name)) + 
  facet_wrap(sp_name~., scales = "free", nrow = 2) + 
  geom_point(size = 2, alpha = 0.8) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  labs(x = "Length (mm)", 
       y = "CTmax (°C)",
       colour = "Species") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/misc-ind-sp-ctmax-size-1.png" style="display: block; margin: auto;" />

Shown below is the relationship between mean size and mean thermal
limits for females of each species. We see that larger species within
the community tend to have a lower thermal limit than smaller species.

``` r
full_data %>% 
  group_by(sp_name, sex) %>% 
  summarize(mean_ctmax = mean(ctmax, na.rm = T),
            mean_size = mean(size, na.rm = T)) %>% 
  #filter(sex == "female") %>% 
  ggplot(aes(x = mean_size, y = mean_ctmax)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2, colour = "black") + 
  geom_point(aes(colour = sp_name, shape = sex),
             size = 5) + 
  labs(x = "Length (mm)", 
       y = "CTmax (°C)",
       colour = "Species") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/main-fig-mean-ctmax-mean-size-plot-1.png" style="display: block; margin: auto;" />

Shown here is the relationship between fecundity and size, showing the
classic pattern of increasing egg production with increasing size.

``` r
size_fecund_plot = ctmax_resids %>%  
  drop_na(fecundity) %>% 
  ggplot(aes(x = size, y = fecundity, colour = sp_name)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  geom_point(size = 2, alpha = 0.5) + 
  labs(x = "Prosome length (mm)", 
       y = "Fecundity (# Eggs)",
       colour = "Species") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

Individuals may also allocate energy to different fitness related
traits, prioritizing reproductive output over environmental tolerance,
for example. Shown below is the relationship between CTmax residuals
(again, controlling for the effects of collection temperature) against
fecundity. We can see clearly that individuals with increased fecundity
are not decreasing thermal limits, suggesting that there is no energetic
trade-off between these traits.

``` r
ctmax_fecund_plot = ctmax_resids %>%  
  drop_na(fecundity) %>% 
  ggplot(aes(x = resids, y = fecundity, colour = sp_name)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  geom_point(size = 2, alpha = 0.5) + 
  labs(x = "CTmax Residuals (°C)", 
       y = "Fecundity (# Eggs)",
       colour = "Species") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")

ggarrange(size_fecund_plot, ctmax_fecund_plot, ncol = 1, common.legend = T, legend = "right")
```

<img src="../Figures/markdown/main-fig-fecundity-plots-1.png" style="display: block; margin: auto;" />

Previous studies have shown that the magnitude of the size-fecundity
correlation may be environmentally-dependent. This is not visible is the
data from these collections.

``` r
corr_data = full_data %>% 
  drop_na(fecundity) %>% 
  filter(sp_name %in% c("Leptodiaptomus sicilis",
                        "Leptodiaptomus minutus", 
                        "Skistodiaptomus sp")) %>%  
  group_by(collection_date, collection_temp, sp_name) %>% 
  summarise(size_fec_corr = cor(size, fecundity),
            n = n(),
            mean_fecundity = mean(fecundity)) %>% 
  filter(n >= 3) %>% 
  ungroup() %>%  
  group_by(sp_name) %>% 
  mutate(temp_cent = scale(collection_temp, scale = F))

ggplot(corr_data, aes(x = temp_cent, y = size_fec_corr, colour = sp_name)) + 
  facet_wrap(sp_name~., nrow = 3) + 
  geom_hline(yintercept = 0) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm", linewidth = 2) + 
  scale_color_manual(values = species_cols) + 
  labs(x = "Temperature (centered)", 
       y = "Correlation Coefficient") + 
  coord_cartesian(ylim = c(-1, 1)) +
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/supp-fig-fec-size-corr-vs-temp-1.png" style="display: block; margin: auto;" />

``` r

# ggplot(corr_data, aes(x = size_fec_corr)) +
#     facet_wrap(sp_name~., nrow = 3) +
#   geom_histogram(binwidth = 0.2)
```

## Other patterns in variation

*Leptodiaptomus sicilis* is the most abundant species during the winter.
There was a large shift in the size of mature females towards the end of
December. These large and small individuals are the same species
(confirmed via COI sequencing), suggesting this shift may instead
reflect a transition from one generation to another. This size
difference may be caused by differences in the developmental
environments. For example, individuals developing in January grow up at
very low temperatures, and therefore may reach larger sizes. These
individuals over-summer in deep waters, then re-emerge in October and
produce a new generation. Water temperatures are still fairly high
through November, which results in a generation of smaller individuals.
These individuals mature in time to produce a new generation in January.

``` r
full_data %>%  
  filter(sp_name == "Leptodiaptomus sicilis") %>% 
  filter(sex != "juvenile") %>% 
  group_by(collection_date) %>% 
  mutate(size_center = scale(size, center = T, scale = F)) %>% 
  ggplot(aes(y = factor(collection_date), x = size, fill = collection_temp)) + 
  facet_wrap(sex~.) + 
  geom_density_ridges(bandwidth = 0.04) + 
  geom_vline(xintercept = 0.89) + 
  labs(x = "Size (mm)",
       y = "Date", 
       fill = "Coll. Temp. (°C)") + 
  theme_matt() + 
  theme(legend.position = "right",
        axis.text.y = element_text(size = 12))
```

<img src="../Figures/markdown/supp-fig-lsic-morph-size-1.png" style="display: block; margin: auto;" />

A similar, but less distinct pattern can be observed in L. minutus
individuals as well.

``` r
full_data %>%  
  filter(sp_name == "Leptodiaptomus minutus") %>% 
  filter(sex != "juvenile") %>% 
  ggplot(aes(y = factor(collection_date), x = size, fill = collection_temp)) + 
  facet_wrap(sex~.) + 
  geom_density_ridges(bandwidth = 0.04) + 
  geom_vline(xintercept = 0.69) + 
  labs(x = "Size (mm)",
       y = "Date", 
       fill = "Coll. Temp. (°C)") + 
  coord_cartesian(xlim = c(0.5,0.9)) + 
  theme_matt() + 
  theme(legend.position = "right",
        axis.text.y = element_text(size = 12))
```

<img src="../Figures/markdown/supp-fig-lmin-morph-size-1.png" style="display: block; margin: auto;" />

## Distribution Lag Non-Linear Model (DLNM approach)

Distributed lag models examine a response variable, measured at multiple
time points, as a function of the lagged occurrence of some predictor
variable (response y at time t as a function of predictor x(t-lag). This
method utilizes a bi-dimensional dose-lag-response function, which
essentially examines not only the dose effect, but the effect of the
timing of the dose.

``` r
# Run this code, save the product, and then just read in the temp lag data object. Takes too long to run each time this document is knit. 

# lag_temps = temp_data %>%
#   group_by(date, hour) %>%
#   summarize("mean_temp" = mean(temp, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(point_num = row_number())
# 
# uniq_days = length(unique(lag_temps$date))
# 
# g = gam(mean_temp ~ s(point_num, bs="cr", k=uniq_days + 10),
#     method = "REML",
#     data = lag_temps)
# 
# points = seq(1, nrow(lag_temps), length.out = length(lag_temps$hour))
# 
# df.res = df.residual(g)
# 
# pred_temps = predict(g, newdata = lag_temps, type = "response", se.fit = TRUE)
# 
# lag_temps = lag_temps %>%
#   mutate(trend_T = pred_temps$fit,
#          trend_se = pred_temps$se.fit,
#          temp_diff = mean_temp - trend_T)
# 
# write.csv(lag_temps, file = "./Output/Data/lag_temps.csv", row.names = F)
```

``` r

dlnm_data = full_data %>%  
  filter(sex == "female") %>% 
  filter(sp_name %in% c(
    "Leptodiaptomus sicilis",
    "Leptodiaptomus minutus",
    "Skistodiaptomus sp"
  )) %>% 
  select(collection_date, collection_temp, sp_name, ctmax) %>% 
  group_by(collection_date, collection_temp, sp_name) %>%  
  summarise(mean_ctmax = mean(ctmax, na.rm = T),
            sample = n())

temp_data %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp),
            max_temp = max(temp, na.rm = T)) %>% 
  right_join(dlnm_data, by = join_by("date" == "collection_date")) %>% 
  ggplot(aes(x = max_temp, y = mean_ctmax)) + 
  facet_wrap(.~sp_name) + 
  geom_smooth(method = "gam") + 
  geom_point() + 
  labs(x = "Max Daily Temp. (°C)",
       y = "Mean CTmax (°C)") + 
  theme_matt_facets() + 
  theme(strip.text.x = element_text(size = 12))
```

<img src="../Figures/markdown/supp-fig-daily-max-ctmax-1.png" style="display: block; margin: auto;" />

``` r

sp_list = unique(dlnm_data$sp_name)

for(lag_species in sp_list){
  
  dlnm_data_sp = dlnm_data %>% 
    filter(sp_name == lag_species)
  
  # We need to estimate a matrix of exposure histories for each observation. This contains the series of exposures at each lag (l) for each of the n observations, constrained between l0 (minimum lag) and L (max lag). 
  
  dates = dlnm_data_sp$collection_date # For each of these dates, make a vector of the past 30 days (including the day of collection). NOTE: Don't use 'unique' dates here since some collections had multiple species
  
  exp_hist_z = data.frame()
  exp_hist_trend = data.frame()
  
  for(d in dates){
    
    history = lag_temps %>% 
      filter(date <= d & date > d - 10) %>% 
      arrange(desc(date), desc(hour)) %>% 
      mutate(lag = row_number() - 1) %>% 
      select(lag, mean_temp, temp_diff)
    
    z_vec = scale(history$mean_temp)[,1]
    names(z_vec) = history$lag
    
    trend_vec = history$temp_diff
    names(trend_vec) = history$lag
    
    exp_hist_z = bind_rows(exp_hist_z, z_vec)
    exp_hist_trend = bind_rows(exp_hist_trend, trend_vec)
    
  }
  
  #print(max(exp_hist_trend, na.rm = T))
  
  # The cross-basis function from dlnm will use the class of the x parameter to determine what to do. In our case, we need to provide it with the matrix of exposure histories for reach observation (row) and lag (column). 
  
  cb_temps = crossbasis(exp_hist_trend, lag = c(0,dim(exp_hist_trend)[2]-1), 
                        argvar =list(fun="cr",df=3), 
                        arglag=list(fun="cr",df=3,intercept=T))
  
  #summary(cb_temps)
  
  penalized_mat <- cbPen(cb_temps)
  
  #fitting GAM
  lag.gam = gam(data = dlnm_data_sp, 
                mean_ctmax ~ collection_temp + cb_temps, 
                method = "GCV.Cp",
                paraPen=list(cb_temps=penalized_mat))
  
  # summary(lag.gam)
  # AIC(lag.gam)
  
  #estimation of exposures effects
  
  #default plots
  pred_gam_Zs<-crosspred(cb_temps, lag.gam, 
                         cumul=F, cen=0, ci.level = 0.95,
                         at=seq(-4,4, 0.1))
  
  plot(pred_gam_Zs, "contour", main = lag_species)
  # 
  # plot(pred_gam_Zs, border = 2, cumul=F,
  #       theta=110,phi=20,ltheta=-80)
  
  plot(pred_gam_Zs, "slices",
       var = c(3,-3),
       lag = c(1,200),
       col = 2)
  
}
```

<img src="../Figures/markdown/supp-fig-dlnm-plot-1.png" style="display: block; margin: auto;" /><img src="../Figures/markdown/supp-fig-dlnm-plot-2.png" style="display: block; margin: auto;" /><img src="../Figures/markdown/supp-fig-dlnm-plot-3.png" style="display: block; margin: auto;" /><img src="../Figures/markdown/supp-fig-dlnm-plot-4.png" style="display: block; margin: auto;" /><img src="../Figures/markdown/supp-fig-dlnm-plot-5.png" style="display: block; margin: auto;" /><img src="../Figures/markdown/supp-fig-dlnm-plot-6.png" style="display: block; margin: auto;" />

## Hindcasting vulnerability with different acclimation scenarios

Using the observed thermal limit data, we can produce a hindcast of
thermal stress for Lake Champlain copepods. For these initial assays, we
will define thermal stress as any time when maximum daily water
temperature is within 5°C of copepod CTmax or higher (i.e. warming
tolerance is less than 5°C). We will use three different scenarios: 1)
each species has a unique but fixed thermal limit (average measured
CTmax), 2) species have unique thermal limits and are able to acclimate
(constant ARR across all species used to predict CTmax based on average
daily temperatures), and 3) species have unique thermal limits and
species-specific acclimation (CTmax predicted using whichever
environmental factor and duration is the strongest candidate for driving
acclimation - from the correlation analysis). In all cases, data is
filtered to just thermal limits of adult females.

### Scenario 1

``` r
mean_ctmax = full_data %>% 
  filter(sex == "female") %>%  
  group_by(sp_name) %>% 
  summarize("mean_ctmax" = mean(ctmax)) %>% 
  arrange(mean_ctmax)

knitr::kable(mean_ctmax)
```

| sp_name                  | mean_ctmax |
|:-------------------------|-----------:|
| Senecella calanoides     |   22.04400 |
| Limnocalanus macrurus    |   22.88927 |
| Leptodiaptomus sicilis   |   29.15785 |
| Leptodiaptomus minutus   |   31.53916 |
| Epischura lacustris      |   31.85441 |
| Skistodiaptomus sp       |   35.67407 |
| Osphranticum labronectum |   36.31250 |

``` r
# # Constructs the URL for the full temperature data set; RUN THIS ONCE
# hind_url = constructNWISURL(siteNumbers = siteNumber, parameterCd = parameterCd, service = "uv")
# 
# hind_temp_data = importWaterML1(hind_url, asDateTime = T) %>%
#   mutate("date" = as.Date(dateTime)) %>%
#   select(date, "temp" = X_00010_00000)
# 
# write.table(x = hind_temp_data, file = "hindcast_temps.csv", row.names = F, sep = ",")
```

``` r
# ggplot(hind_temp_data, aes(x = date, y = temp)) + 
#   geom_line(linewidth = 0.1) + 
#   labs(x = "Date", 
#        y = "Water Temperature (°C)") +
#   theme_matt()
```

In the simplest scenario, species thermal limits are static through
time, represented by the average CTmax of adult female copepods. In this
scenario, only three of the seven observed species are exposed to
thermal stress (temperatures within 5°C of CTmax). Temperatures
approached the thermal limit of *Leptodiaptomus sicilis* on a handful of
days. By contrast, *Senecella calanoides* and *Limnocalanus macrurus*
were both exposed to substantial thermal stress throughout a large
portion of the year, likely explaining why these species are absent from
the community for the summer and fall periods.

``` r
hind1_data = hind_temp_data %>% 
  group_by(date) %>% 
  summarize("daily_max" = max(temp),
            "daily_mean" = mean(temp),) %>% 
  bind_cols(pivot_wider(mean_ctmax, names_from = sp_name, values_from = mean_ctmax)) %>%  
  pivot_longer(cols = c(-date, -daily_max, -daily_mean),
               names_to = "species", 
               values_to = "mean_ctmax") %>%  
  mutate(lim_diff = mean_ctmax - daily_max) %>%  
  mutate(doy = yday(date),
         "method" = "No_acclimation")

hind_daily_temp_data = hind_temp_data %>%
  ungroup() %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp),
            med_temp = median(temp),
            var_temp = var(temp), 
            min_temp = min(temp), 
            max_temp = max(temp)) %>% 
  mutate("range_temp" = max_temp - min_temp)

#table(hind1_data$species)

hind1_data %>% 
  filter(lim_diff <= 5) %>%  
  ggplot(aes(x = doy, y = lim_diff, colour = species)) +
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, 
             colour = "grey") + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = F) + 
  labs(x = "Day of Year", 
       y = "Predicted Warming Tolerance \n(°C Above Daily Max)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/supp-fig-hind1_scenario-1.png" style="display: block; margin: auto;" />

### Scenario 2

In the second scenario, thermal limits vary within and between species.
A simple model is used to predict species thermal limits based on mean
daily temperature (CTmax as a function of species and collection
temperature, but without the interaction between these two factors).
These predicted thermal limits are then compared against the maximum
daily temperature to estimate thermal stress, as in Scenario 1.
Including this simple form of acclimation in the model reduced the
degree of thermal stress for each species, eliminating it entirely for
*Leptodiaptomus sicilis*. Note that the magnitude of the predicted
stress is low enough that removing the 5°C buffer around the predicted
thermal limits would actually limit predicted thermal stress to just a
few days for *Senecella calanoides*.

``` r
hindcast_model1 = lm(data = filter(full_data, sex == "female"),
                     ctmax ~ collection_temp + sp_name)

hind2_data = hind_temp_data %>% 
  group_by(date) %>% 
  summarize("collection_temp" = mean(temp),
            "daily_max" = max(temp)) %>% 
  bind_cols(
    pivot_wider(mean_ctmax, 
                names_from = sp_name, 
                values_from = mean_ctmax)) %>% 
  pivot_longer(cols = c(-date, -daily_max, -collection_temp),
               names_to = "sp_name", 
               values_to = "mean_ctmax") %>% 
  select(-mean_ctmax) %>% 
  mutate("pred_ctmax" = predict.lm (hindcast_model1, newdata = .)) %>% 
  select(date, "daily_mean" = collection_temp, daily_max, "species" = sp_name, pred_ctmax) %>% 
  mutate(lim_diff = pred_ctmax - daily_max) %>% 
  #filter(lim_diff <= 0) %>%  
  mutate(doy = yday(date),
         "method" = "Constant_acclimation")

# ggplot(hind2_data, aes(x = daily_mean, y = pred_ctmax, colour = species)) +
#   geom_smooth(method = "lm") 

# table(hind2_data$species)
hind2_data %>%  
  filter(lim_diff <= 5) %>%  
  ggplot(aes(x = doy, y = lim_diff, colour = species)) +
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, 
             colour = "grey") + 
  geom_point(alpha = 0.5) +
  geom_smooth() + 
  labs(x = "Day of Year", 
       y = "Predicted Warming Tolerance \n(°C Above Daily Max)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/supp-fig-hind2_scenario-1.png" style="display: block; margin: auto;" />

### Scenario 3

The final scenario allows the environmental variable used to predict
CTmax to vary between species. For species observed in fewer than 5
collections, we use the same approach as in Scenario 2. For species
observed in more than 5 collections, however, the factor with the
strongest correlation with CTmax is used to predict thermal limits.
These factors are included below.

``` r
hind_preds = corr_vals %>%  
  filter(sig == "Sig.") %>% 
  drop_na(correlation) %>% 
  group_by(sp_name) %>%
  arrange(desc(correlation)) %>% 
  slice_head(n = 1) %>% 
  select("Species" = sp_name, "Predictor" = parameter, "Duration" = duration, "Correlation" = correlation, "P-Value" = p.value)

knitr::kable(hind_preds, align = "c")
```

|        Species         | Predictor | Duration | Correlation |  P-Value  |
|:----------------------:|:---------:|:--------:|:-----------:|:---------:|
|  Epischura lacustris   |    max    |    29    |  0.9147764  | 0.0000000 |
| Leptodiaptomus minutus | coll_temp |    0     |  0.7732711  | 0.0000000 |
| Leptodiaptomus sicilis | coll_temp |    0     |  0.2833554  | 0.0000001 |
| Limnocalanus macrurus  |    min    |    8     |  0.5496742  | 0.0000000 |
|  Senecella calanoides  |    max    |    19    |  0.5038896  | 0.0006688 |
|   Skistodiaptomus sp   |    max    |    2     |  0.7972819  | 0.0000000 |

``` r
hind3_data = hind2_data %>% # Contains data for species that won't change from scenario 2
  filter(!(species %in% corr_vals$sp_name))

preds_to_pull = hind_preds %>%  
  select(Species, Predictor, Duration) 

for(i in 1:length(preds_to_pull$Species)){
  
  duration = preds_to_pull$Duration[i]
  
  if(duration == 0){ #The prior day temperature metrics should be used
    
    predictors = hind_daily_temp_data %>% 
      mutate(date = date) 
    
    parameter = "mean_temp" #using mean temperature as a proxy for collection temp
    
    model_data = full_data %>%
      filter(sp_name %in% preds_to_pull$Species[i]) %>% 
      filter(sex == "female") %>% 
      mutate(collection_date = as_date(collection_date)) %>% 
      inner_join(predictors, join_by(collection_date == date)) %>%  
      select(ctmax, contains(parameter))
    
    if(dim(model_data)[2] == 2){
      hind.model = lm(data = model_data, 
                      ctmax ~ .)
      
      sp_data = predictors %>% 
        select(date, contains(parameter)) %>% 
        mutate(pred_ctmax = predict(hind.model, newdata = .)) %>%  
        select(date, pred_ctmax) %>% 
        inner_join(hind_daily_temp_data, by = c("date")) %>% 
        mutate("species" = preds_to_pull$Species[i],
               "doy" = yday(date),
               lim_diff = pred_ctmax - max_temp) %>% 
        select(date, daily_mean = mean_temp, daily_max = max_temp, species, pred_ctmax, lim_diff, doy)
      
      hind3_data = bind_rows(hind3_data, sp_data)
    }else{
      print(c(unique(sp_data$species), "Too many columns selected"))
    }
    
  }
  
  if(duration > 0){
    #Neither the prior day nor day of metrics should be used; use duration as n_days
    
    predictors = get_predictors(daily_values = hind_daily_temp_data, 
                                raw_temp = hind_temp_data, 
                                n_days = duration)
    
    parameter = preds_to_pull$Predictor[i]
    
    model_data = full_data %>%
      filter(sp_name %in% preds_to_pull$Species[i]) %>% 
      filter(sex == "female") %>% 
      mutate(collection_date = as_date(collection_date)) %>% 
      left_join(predictors, join_by(collection_date == date)) %>%  
      select(ctmax, contains(paste("day_", parameter, sep = "")))
    
    if(dim(model_data)[2] == 2){
      hind.model = lm(data = model_data, 
                      ctmax ~ .)
      
      sp_data = predictors %>% 
        select(date, contains(parameter)) %>% 
        mutate(pred_ctmax = predict(hind.model, newdata = .)) %>%  
        select(date, pred_ctmax) %>% 
        inner_join(hind_daily_temp_data, by = c("date")) %>% 
        mutate("species" = preds_to_pull$Species[i],
               "doy" = yday(date),
               lim_diff = pred_ctmax - max_temp) %>% 
        select(date, daily_mean = mean_temp, daily_max = max_temp, species, pred_ctmax, lim_diff, doy)
      
      hind3_data = bind_rows(hind3_data, sp_data)
      
    }else{
      print(c(unique(sp_data$species), "Too many columns selected"))
    }
    
  }
}


hind3_data = hind3_data %>% 
  mutate("method" = "Variable_acclimation")
```

This third approach did not affect the predicted patterns in
*Limnocalanus* or *Senecella* (neither species has been observed in
enough collections to estimate the effects of different environmental
factors). Changing the acclimation approach did affect patterns in
thermal limits in the other species though. The figure below shows how
predicted warming tolerance varies over the year in the seven species,
based on the three different prediction methods. In general, constant
thermal limits (the ‘no acclimation’ method) resulted in larger warming
tolerance during the winter and lower warming tolerance during the
summer, although this effect was small in most species.

``` r
synthesis = bind_rows(
  select(hind1_data, date, doy, daily_mean, daily_max, species, "pred_ctmax" = mean_ctmax, lim_diff, method),
  select(hind2_data, date, doy, daily_mean, daily_max,  species, pred_ctmax, lim_diff, method),
  select(hind3_data, date, doy, daily_mean, daily_max,  species, pred_ctmax, lim_diff, method)) %>% 
  mutate(method = fct_relevel(method, "No_acclimation", "Constant_acclimation", "Variable_acclimation")) %>% 
  filter(!(species == "Osphranticum labronectum" & method == "Variable_acclimation"))

climatology = synthesis %>% 
  group_by(species, doy, method) %>%  
  summarise("mean_diff" = mean(lim_diff),
            "min_diff" = min(lim_diff),
            "max_diff" = max(lim_diff)) %>% 
  mutate(method = fct_relevel(method, "No_acclimation", "Constant_acclimation", "Variable_acclimation"))

acc_effects = synthesis %>% 
  pivot_wider(id_cols = c(date, species, doy), 
              names_from = method, 
              values_from = lim_diff) %>%  
  mutate("const_acc_effect" = Constant_acclimation - No_acclimation,
         "var_acc_effect" = Variable_acclimation - No_acclimation)

ggplot(synthesis, aes(x = doy, y = lim_diff, colour = method)) + 
  facet_wrap(species~.) + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5, colour = "grey") + 
  geom_point(alpha = 0.1) + 
  labs(x = "Day of Year", 
       y = "Predicted Warming Tolerance (°C Above Daily Max)") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_matt_facets(base_size = 18) + 
  theme(strip.text.x.top = element_text(size = 10))
```

<img src="../Figures/markdown/supp-fig-hind_cast_summary-1.png" style="display: block; margin: auto;" />

``` r
wt_hindcast_summary = synthesis %>%  
  mutate("year" = year(date)) %>% 
  group_by(species, year, method) %>% 
  summarise("min_wt" = min(lim_diff),
            "max_wt" = max(lim_diff)) %>% 
  pivot_longer(cols = c(min_wt, max_wt), 
               names_to = "metric", 
               values_to = "wt") %>% 
  group_by(species, method, metric) %>% 
  summarise("mean_wt" = mean(wt))

wt_hindcast_summary %>% 
  filter(metric == "min_wt") %>% 
ggplot(aes(x = method, y = mean_wt, group = species, colour = species)) + 
  #facet_wrap(.~metric, scales = "free_y") + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = 5) + 
  geom_line(linewidth = 2) + 
  geom_point(size = 3) + 
  labs(x = "Scenario", 
       y = "Mean Yearly Minimum WT (°C)") + 
  scale_color_manual(values = species_cols) + 
  theme_matt_facets() + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/supp-fig-acc_scenario_effects-1.png" style="display: block; margin: auto;" />
