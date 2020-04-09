# COVID-19-and-Me

An R Shiny Dashboard showing the rate of growth of COVID-19 near the user.

## Background

There are already many dashboards and charts about the spread of COVID-19.  This project's purpose is two-fold:

1.  To focus on the communities near where the user lives.  Hence "COVID-19 **and Me**".  By personalizing the data, we hope the user will feel the connection between virus spread and their and their neighbours' behaviour.
1.  To focus on the doubling time.  When the number of cases is plotted with a log scale, the slope gives the rate of growth.  But since we care about the rate of growth, we should simply plot it directly.

As government and society change behaviour, we should see a change in the doubling time of the virus cases.

## Data

Our raw data comes from [Johns Hopkins University Center for Systems Science and Engineering](https://github.com/CSSEGISandData/COVID-19).

## Basic App Operation

The app is hosted at [my ShinyApps.io page](https://covid19andme.shinyapps.io/COVID-19andMe/).  The user chooses their country and, if applicable Province/State and County.  Our data source only has Province/State level data for Australia, Canada, China, US.  For the US, there is also County level data.  The app then displays the current doubling time for their nearby regions.  The user's region is highlighted.

We have a second tab that explains how and why we do this.

## Current Issues

Our handling of geographic data is rough.  Our computation of nearby regions (distance) is hacky.

Our handling of the axes for charts with missing, negative, or very large values is poor.

We need a better way to update the data on shinyapps.io.

There is no way to allow feedback from the user.

## Future Ideas

Put a choropleth map of the user's neighbourhood.

Allow the user to choose their own regions (not necessarily nearby) using `selectInput(multiple = TRUE)` and mix countries and provinces/state.  Plot time with `ggplot(aes(color = Place))` so that color represents the regions.