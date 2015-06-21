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
#   Plot4 Functions
###

summarise_data <- function(dfNEI, dfSCC){
    # Start by merging the data
    merged_data <- merge(dfNEI, dfSCC, by = 'SCC')
    # Select out our sources of Coal Fuel Combustion
    total_summary <- merged_data %>% 
        filter(grep('^Fuel Comb.*Coal', EI.Sector)) %>% 
        group_by(year, EI.Sector) %>% 
        # Group by the year and EI.Sector taking a sum of the total emissions
        # and log the value to make plotting their all the sectors together 
        # look better
        summarise(total_emissions=log(sum(Emissions))) %>%
        # And tidy up the names
        mutate(Sector=gsub('Fuel Comb - ', '', EI.Sector)) %>%
        mutate(Sector=gsub('- Coal', '', Sector))
}

plot4 <- function( summary_data ){
    # Setup our plot region
    ggplot(summary_data, aes(year, total_emissions, color=Sector)) + 
        # Add the lines and update the legend title
        geom_line() + scale_colour_discrete(name='Source') +
        # Set labels and title
        labs(x='Year', y=expression("Log " * PM[2.5] * " Emissions (Tons)")) +
        theme(plot.title=element_text(size = 10)) + 
        ggtitle(expression("United States " * PM[2.5] * " Emissions from Coal Fuel Combustion"))
}

dtNEI <- dataLoad('summarySCC_PM25.rds')
dtSCC <- dataLoad('Source_Classification_Code.rds')

data_summary <- summarise_data(dtNEI, dtSCC)
saveGGPlot2('plot4.png', plot4, data_summary)

