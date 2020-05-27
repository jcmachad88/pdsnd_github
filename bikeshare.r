# Analyst Name: Juan Machado
# Date Reviewed: May 1, 2020

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(ggplot2)

head(ny)

head(wash)

head(chi)

# What is the most common trip from start to end for each city?

common_trip = function(data) {
    
    #Creates new Trip.Route variable by concatenating trip start and end locations 
    data$Trip.Route = paste(data$Start.Station, data$End.Station, sep = " + ")
    
    #Creates table for trips sorted in decreasing order of frequency
    trips = data.frame(sort(table(data$Trip.Route), decreasing = T))
    
    #Rename columns from data frame
    names(trips)[names(trips) == "Var1"] = "Trip.Route"
    
    #Display the 5 most common routes
    return (head(trips, 5))
    
    }

common_trip(ny)
common_trip(wash)
common_trip(chi)

# The most common trip route in New York is starting in E 7 St & Ave A and ending at Cooper Square & E 7 St
# The most common trip route in DC is starting at Jefferson & 14th and ending at the same station
# The most common trip routes in Chicago are starting at Lake Shore & Monroe and ending at Streeter & Grand
# and starting at Streeter & Grand and ending at the same station

# What is the average travel time for users in different cities?

mean_travel = function (data) {  
    
    # Print median and mean values for trip duration in minutes
    
    print(summary(data$Trip.Duration/60)[3:4])
    
    # Create histogram of trip duration
    
    qplot(x = Trip.Duration/60, binwidth = 2.5, data = subset(data, !is.na(Trip.Duration))) +
            xlab('Number of minutes for trip') +
            ylab('Number of users') +
            scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 5))
}

mean_travel(ny)
mean_travel(wash)
mean_travel(chi)

# The average travel time in New York is 15.06 minutes
# The average travel time in D.C. is 20.57 minutes
# The average travel time in Chicago is 15.62 minutes

# What is the average travel time for users in different cities by gender?

mean_travel_gender = function (data) {  
    
    # Print summary statistics for trip duration by gender
    
    summary_gender = by(data$Trip.Duration/60, data$Gender, summary)
    print(summary_gender)
    
    # Create separate box plots of trip duration by gender
    
    qplot(x = Gender, y = Trip.Duration/60, 
                 data = subset(data, Gender == 'Male' | Gender == 'Female'), geom = 'boxplot') +
                 coord_cartesian(ylim = c(0, 25))
}

mean_travel_gender(ny)
mean_travel_gender(chi)

#In both New York and Chicago, women on average take longer trips than men

system('python -m nbconvert Explore_bikeshare_data.ipynb')
