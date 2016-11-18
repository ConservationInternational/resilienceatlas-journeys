library(ggplot2)
library(dplyr)
library(cdbr)
library(foreach)
library(plotly)
library(raster)
library(RColorBrewer)
library(rgdal)
library(tidyr)

# Extract minimum temperatures areas where more than xx ha of Arabica is grown, 
# and summarize trends by country

# Calculate mean change in minimum temperatures in 2050 in areas where coffee Arabica is grown, by country

# Show total value of this production
ISOs <- c("ETH", "UGA", "CIV", "TZA", "KEN", "BRD", "RWA")
ISOs_string <- paste0("'", paste(ISOs, collapse="', '"), "'")

# Calculate historical temperature trends in each of these countries from this data
data_prefix <- 'global_cru_ts3_23_'
data_suffix <- '_19850101_20141231_trend_decadal'
datasets <- c('tmn', 'tmp', 'tmx')
hist_temps <- foreach (dataset=datasets, .combine=rbind) %do% {
    hist_q <- paste0('SELECT name_engli, min(val), avg(val), max(val)
    FROM (select * from gadm28_adm0 WHERE iso IN (', ISOs_string, ')) AS countries
    , (
      SELECT val, geom AS the_geom_webmercator
        FROM (
          SELECT
          (ST_DumpAsPolygons(the_raster_webmercator)).*
          FROM ', data_prefix, dataset, data_suffix, ') AS temps
    ) AS temps
    WHERE ST_Intersects(countries.the_geom_webmercator, temps.the_geom_webmercator) AND val != -9999
    GROUP BY name_engli, countries.the_geom_webmercator
    ORDER BY name_engli, countries.the_geom_webmercator')
    stats <- cdb_sql_call(hist_q)
    stats <- stats$rows
    stats$var <- dataset
    #gather(stats, var, stat, min:max)
    stats
}
hist_temps$period <- '1985-2015'

# Calculate future temperature trends in each of these countries from this data
fut_temps_allmodels <- brick('diff_tasmin_rcp85_2040-2059.tif')
proj4string(fut_temps_allmodels) <- '+init=epsg:4326'

countries <- readOGR('O:/Data/Global/GADM', 'gadm28_adm0')
countries <- countries[countries$ISO %in% ISOs, ]
countries$NAME_ENGLI <- as.character(countries$NAME_ENGLI)

fut_temps <- foreach (n=1:nrow(countries), .combine=rbind) %do% {
    this_country <- countries[n, ]
    temp <- mask(crop(fut_temps_allmodels, this_country), this_country)

    # Calculate multimodel mean
    mean_chgs <- apply(getValues(temp), 2, mean, na.rm=TRUE)

    # Calculate min and max projections
    data.frame(name_engli=this_country$NAME_ENGLI,
               avg=mean(mean_chgs),
               min=min(mean_chgs),
               max=max(mean_chgs))
}

# Convert from change over next 35 years to change per decade
fut_temps$min <- fut_temps$min / 3.5
fut_temps$avg <- fut_temps$avg / 3.5
fut_temps$max <- fut_temps$max / 3.5
fut_temps$var <- 'tmn'
fut_temps$period <- '2040-2060'
# Fix encoding so Cote d'Ivoire appears correctly
fut_temps$name_engli <- iconv(fut_temps$name_engli, "UTF-8")
fut_temps$name_engli <- enc2utf8(as.character(fut_temps$name_engli))

temps <- full_join(hist_temps, fut_temps)

# Don't plot the errorbars on the historical estimates as they are the observed 
# data, without model uncertainty (though of course with other errors 
# sources...)
temps$min[temps$period != '2040-2060'] <- NA
temps$max[temps$period != '2040-2060'] <- NA

temps$period[temps$period != '2040-2060'] <- "Historical"
temps$period[temps$period == '2040-2060'] <- "Future"

temps <- filter(temps, var == 'tmn')

p <- plot_ly(x = ~name_engli, y = ~avg, color = ~period, colors = 'Accent') %>%
    add_markers(data = filter(temps, period == 'Historical')) %>%
    add_markers(error_y  =  ~list(type = "data", symmetric = FALSE, arrayminus = avg-min, array = max-avg), data = filter(temps, period == 'Future')) %>%
    config(showLink = FALSE) %>%
    config(displayModeBar = FALSE)
p <- layout(p, xaxis=list(title=""), yaxis = list(title = "Rate of change in minimum temperature (C)"))
plotly_POST(p, filename = "african-coffee-mintemp-trends")
