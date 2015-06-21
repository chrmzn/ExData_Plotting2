# Load the libraries we will be using for the task 
library(data.table)
library(dplyr)
library(ggplot2)

###
#   Core Functions - These will be shared between all plots
###

# Read the rds files in and convert them to a data.table
dataLoad <- function (filename) {
    data.table(readRDS(filename))
}

# Takes the function to plot a graph and a plot name to save the data
# as a PNG
saveGGPlot2 <- function( plotName, func, ... ){
    # Execute the function returning our plot object
    ggplot_obj <- func(...)
    # Using the filename save the plot
    ggsave(plotName, ggplot_obj)
}

###
#   Plot6 Functions
###

summarise_data <- function(dfNEI, dfSCC){
    # Start by merging the data
    merged_data <- merge(dfNEI, dfSCC, by = 'SCC')
    # Select out our sources of Mobile On-Road vehicles
    total_summary <- merged_data %>% 
        filter(fips %in% c("24510", "06037")) %>% 
        filter(grep('^Mobile - On-Road', EI.Sector)) %>% 
        group_by(year, fips) %>% 
        summarise(total_emissions=sum(Emissions)) %>% 
        arrange(year)
    # As we are trying to determine which has has seen the greater changes
    # over time we use 1999 as our base year and plot each years 
    base_value <- total_summary %>% 
        mutate(base_emissions=total_emissions) %>% 
        filter(year=='1999') %>% select(fips, base_emissions)
    # merge the base year back into the plot
    total_summary <- merge(total_summary, base_value, by='fips')
    # then taking base_emission year plot each subsequent year as a
    # percentage of the base year and minus 1 from the value to highlight 
    # the difference between a decrease and increase
    total_summary <- total_summary %>%
        mutate(percent_of_base=total_emissions/base_emissions) %>%
        mutate(percent_of_base=percent_of_base-1) %>%
        mutate(year=factor(year), fips=factor(fips))
    # Finally correct the names for fips. Makes plotting the graph more clear
    levels(total_summary$fips)[levels(total_summary$fips)=="06037"] <- "Los Angeles County"
    levels(total_summary$fips)[levels(total_summary$fips)=="24510"] <- "Baltimore City"
    total_summary
}

plot6 <- function( summary_data ){
    # Setup our plot region
    ggplot(summary_data, aes(x=year, y=percent_of_base, fill=fips)) + 
        geom_bar(stat='identity') + facet_wrap(~fips) + 
        theme(legend.position='none') + 
        labs(x='Year', y=expression(PM[2.5] * ' Emissions (% of 1999)')) + 
        ggtitle(expression('Changes in Motor Vehicle ' * PM[2.5] * ' Emissions since 1999'))
}

dtNEI <- dataLoad('summarySCC_PM25.rds')
dtSCC <- dataLoad('Source_Classification_Code.rds')

data_summary <- summarise_data(dtNEI, dtSCC)
saveGGPlot2('plot6.png', plot6, data_summary)