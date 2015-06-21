# Load the libraries we will be using for the task 
library(data.table)
library(dplyr)

###
#   Core Functions - These will be shared between all plots
###

# Read the rds files in and convert them to a data.table
dataLoad <- function (filename) {
    data.table(readRDS(filename))
}


# Takes the function to plot a graph and a plot name to save the data
# as a PNG
plotGraph <- function( plotName, func, ... ){
    # Create the png device
    png(filename = plotName )
    # Plot the graph using the funciton that we pass to plotGraph
    # along with any additional arguments
    func(...)
    # Close the device!
    dev.off()
}


###
#   Plot1 Functions
###

summarise_data <- function(df){
    df %>% 
        # group the data by year
        group_by(year) %>% 
        # sum their respective emission values
        summarise(total_emissions=sum(Emissions)) %>%
        # Scale the total_emissions by 1,000 to make the graph easier to read
        mutate(total_emissions=total_emissions/1000)
}

plot1 <- function( summary_data ){
    # Create our expression label and total
    plotYLabel <- expression(PM[2.5] * " Emissions (1,000's of Tons)" )
    plotTitle <- expression("Total United States " * PM[2.5] * " Emissions")
    # Setup the plot
    plot(summary_data$year, summary_data$total_emissions, type='n', xlab='Year',
         ylab = plotYLabel)
    title(plotTitle)
    # Plot as a line
    lines(summary_data$year, summary_data$total_emissions)
}

dtNEI <- dataLoad('summarySCC_PM25.rds')
dtSCC <- dataLoad('Source_Classification_Code.rds')

data_summary <- summarise_data(dtNEI)
plotGraph('plot1.png', plot1, data_summary)
