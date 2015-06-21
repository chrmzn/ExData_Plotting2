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
#   Plot5 Functions
###

summarise_data <- function(dfNEI, dfSCC){
    # Start by merging the two data sets on the SCC variable
    merged_data <- merge(dfNEI, dfSCC, by = 'SCC')
    # Start by selecting out the Baltimore City values
    total_summary <- merged_data %>% 
        filter(fips == "24510") %>% 
        # Then grep for Mobile - On-Road from EI.Sector
        filter(grep('^Mobile - On-Road', EI.Sector)) %>% 
        # Group by year and Vehicle type
        group_by(year, EI.Sector) %>% 
        # Take a sum of the emissions and it's log (to account for)
        # skew between the types
        summarise(total_emissions=log(sum(Emissions))) %>% 
        # Finally clean the sector names
        mutate(Sector=gsub('Mobile - On-Road ', '', EI.Sector)) 
    total_summary
}

plot5 <- function( summary_data ){
    # Setup our plot region
    ggplot(summary_data, aes(year, total_emissions, color=Sector)) +
        # Add the lines and update the legend title
        geom_line() + scale_colour_discrete(name='Vehicle Type') +
        # Set labels and title
        labs(x='Year', y=expression("Log " * PM[2.5] * " Emissions (Tons)")) +
        theme(plot.title=element_text(size = 10)) + 
        ggtitle(expression("Baltimore City " * PM[2.5] * " Emissions from Vehicles"))
}

dtNEI <- dataLoad('summarySCC_PM25.rds')
dtSCC <- dataLoad('Source_Classification_Code.rds')

data_summary <- summarise_data(dtNEI, dtSCC)
saveGGPlot2('plot5.png', plot5, data_summary)