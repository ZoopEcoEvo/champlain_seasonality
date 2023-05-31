Seasonality in Lake Champlain Copepod Thermal Limits
================
2023-05-30

- <a href="#temperature-variation"
  id="toc-temperature-variation">Temperature Variation</a>
- <a href="#trait-variation" id="toc-trait-variation">Trait Variation</a>
  - <a href="#variation-with-temperature"
    id="toc-variation-with-temperature">Variation with temperature</a>
- <a href="#trait-correlations" id="toc-trait-correlations">Trait
  Correlations</a>

``` r
### To Do 

# Pull residuals from CTmax ~ temperature model, and examine the relationship with fecundity
```

## Temperature Variation

## Trait Variation

``` r
ggplot(full_data, aes(x = size, fill = sp_name)) + 
  facet_wrap(.~sp_name, ncol = 1) + 
  geom_histogram(binwidth = 0.05) + 
  scale_fill_manual(values = species_cols) + 
  labs(x = "Prosome length (mm)") + 
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r

ggplot(full_data, aes(x = ctmax, fill = sp_name)) + 
  facet_wrap(.~sp_name, ncol = 1) + 
  geom_histogram(binwidth = 1) + 
  scale_fill_manual(values = species_cols) + 
  labs(x = "CTmax (°C)") + 
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-3-2.png" style="display: block; margin: auto;" />

``` r

full_data %>%  
  drop_na(fecundity) %>%  
ggplot(aes(x = fecundity, fill = sp_name)) + 
  facet_wrap(.~sp_name, ncol = 1) + 
  geom_histogram(binwidth = 2) + 
  scale_fill_manual(values = species_cols) + 
  labs(x = "Fecundity (# Eggs)") +
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-3-3.png" style="display: block; margin: auto;" />

### Variation with temperature

``` r
ggplot(full_data, aes(x = collection_temp, y = ctmax, colour = sp_name)) + 
  geom_point(size = 3) + 
  labs(x = "Collection Temperature (°C)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r

ggplot(full_data, aes(x = collection_temp, y = size, colour = sp_name)) + 
  geom_point(size = 3) + 
  labs(x = "Collection Temperature (°C)", 
       y = "Length (mm)")  + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-4-2.png" style="display: block; margin: auto;" />

``` r

ggplot(full_data, aes(x = collection_temp, y = warming_tol, colour = sp_name)) + 
  geom_point(size = 3) + 
  labs(x = "Collection Temperature (°C)", 
       y = "Warming Tolerance (°C)")  + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-4-3.png" style="display: block; margin: auto;" />

``` r

ggplot(full_data, aes(x = collection_temp, y = fecundity, colour = sp_name)) + 
  geom_point(size = 3) + 
  labs(x = "Collection Temperature (°C)", 
       y = "Fecundity (# Eggs)")  + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-4-4.png" style="display: block; margin: auto;" />

``` r
ggplot(full_data, aes(x = days_in_lab, y = ctmax, colour = sp_name)) + 
  facet_wrap(sp_name~.) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  scale_x_continuous(breaks = c(0:3)) + 
  labs(x = "Days in lab", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt_facets() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## Trait Correlations

``` r
ggplot(full_data, aes(x = size, y = ctmax, colour = sp_name)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  geom_point(size = 4) + 
  labs(x = "Length (mm)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
ggplot(full_data, aes(x = size, y = fecundity, colour = sp_name)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  geom_point(size = 4) + 
  labs(x = "Prosome length (mm)", 
       y = "Fecundity (# Eggs)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
ggplot(full_data, aes(x = ctmax, y = fecundity, colour = sp_name)) + 
  geom_smooth(method = "lm", se = F, linewidth = 2) + 
  geom_point(size = 4) + 
  labs(x = "CTmax (°C)", 
       y = "Fecundity (# Eggs)") + 
  scale_colour_manual(values = species_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />
