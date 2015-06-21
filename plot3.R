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
#   Plot3 Functions
###

summarise_data <- function(df){
    df %>% 
        # filter for Baltimore City data
        filter(fips=="24510") %>%
        # group the data by year
        group_by(year, type) %>% 
        # sum their respective emission values
        summarise(total_emissions=sum(Emissions)) %>%
        # as the source types operate on different scales
        # mutate to take the log of the emissions makes it easier
        # to see the difference between each of the factors
        mutate(total_emissions=log(total_emissions))
}

plot3 <- function( summary_data ){
    # Setup our plot region
    ggplot(summary_data, aes(year, total_emissions, group=type, color=type)) +
        # Add the lines and update the legend title
        geom_line() + scale_colour_discrete(name='Source') +
        # Set labels and title
        labs(x='Year', y=expression("Log " * PM[2.5] * " Emissions (Tons)")) +
        ggtitle(expression("Total Baltimore City " * PM[2.5] * " Emissions by Source"))
}

dtNEI <- dataLoad('summarySCC_PM25.rds')
dtSCC <- dataLoad('Source_Classification_Code.rds')

data_summary <- summarise_data(dtNEI)
saveGGPlot2('plot3.png', plot3, data_summary)
