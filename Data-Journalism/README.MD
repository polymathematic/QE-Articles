# An Introduction to Data Journalism

Data visualization is a useful tool for journalists. The use of data within journalism can tell stories in interesting ways, and adds a type of credibility that anecdote alone cannot. This guide explains the basics and best practices of creating effective visualizations for posts on GreaterGreaterWashington.

## When to Create a Visualization

Visualizations should support and enhance the articles they’re in. For example, Section 1 of GGW’s post template contains the “Main thing” that the post will convey. If a visualization will be used as the post’s main image, the first visualization should illustrate and support this main thing.

Data visualizations don’t always have to be created from scratch. If a suitable visualization already exists from another source, that visual should be used as-is with attribution (a link to the original). If the visual is unsuitable (for example, if it is unreadable at the required width) it should be recreated with attribution to the original visual’s author.

## Step 1: Determine the right visualizations for your article

The subject of the article should drive the visualization, not the other way around. Authors should consider where examining available data could improve or substantiate the points made in the article. Data should add to the subject without disrupting the logical flow of the article. Authors should avoid visuals and data that is irrelevant or likely to confuse.

In this step, quickly sketching the final visualization is a good exercise to test if the the data makes sense in context, and focus the goal of subsequent steps. This is particularly important for more complex visualizations.

Picking the right visual for the article is an extensive topic. A huge variety of visuals and combinations of visuals exist which may or may not be appropriate for a given situation.  In general, understanding the proper application of a few simple visuals is adequate for most purposes.

* Bar charts compare aggregate values across simple groupings (e.g., 2016 average rents by neighborhood)
* Line charts show how aggregate values have changed over time with a line for each grouping of interest. (e.g., Average rents from 2000 to 2016 by neighborhood)
* Scatter plots illustrate how two numeric variables interact with each other. (e.g., Average rents compared to median income in 2016 by neighborhood)
* Maps can use a variety of graphic elements such as shaded regions (“Choropleth maps”) or points (“Point maps”)

Most data can be conveyed using one or more of these four simple visuals. More advanced visuals are welcome, but are usually not strictly necessary.

Finally, it is worth noting that pie charts are never appropriate. Consider instead: point graphs or Pareto charts.

For more on picking the right visual, see… by...

## Step 2: Finding the right data

Data on many aspects of the greater DC area are available at several online repositories. This list is a good place to start:

* http://data.codefordc.org/
* http://opendata.dc.gov/
* http://factfinder.census.gov/
* http://data.octo.dc.gov/

Data can also be obtained through Freedom of Information Act requests directly to the agency of interests. FOIA requests can be  time consuming and often involve repeated follow-ups. For more, see this guide on FOIA requests from the National Security Archive.

If data is unavailable, the author should reconsider step 1, how to tell the story with data.

For more on obtaining good data, see… by...

## Step 3: Transforming data

Raw data is not usually right for the specific visualization the author intends. The process of converting data into the desired form is a non standard task that can be either easy, hard, or anything in between. 

Spreadsheet applications such as Google sheets are most commonly used for data transformation. More advanced tools include R, Python, SQL or QGIS for geospatial applications.  

Regardless of the tool, the process of data transformation is the same. At a minimum, the author should ensure that the data meets the following criteria:

* Cleaned (Fix obvious errors in the data. i.e., spelling errors)
* Filtered (Subset a city-wide database to a specific neighborhood)
* Aggregated (Turn a dataset with individual 911 calls into a dataset with total calls per hour)
* Formatted (Alter variables so they match the desired format  “1/1/2017” to “January 1, 2017”)

Transforming data tends to be the most time consuming in the process. It is also the point where mistakes are most likely to occur. [[assistance?]]

For more on transforming data, see… by...

## Step 4: Visualizing the Data

Using the sketch from Step 1 and data from Step 3, the author should use an appropriate tool to create the data visualization. 

Greater Greater Washington does not have rigid style standards for visualizations. Authors should instead adhere to the following set of guidelines and best-practices: 

* The visual should contain a title and may contain a subtitle to explain the purpose of the visual.
* The visual should contain some attribution to the data source.
* The visual’s width should be exactly 1200 pixels.
* The font should be large enough to be legible on mobile devices. 
* No more than five colors should be used.
* Color palettes should be viewable for individuals with red-green colorblindness  (see here for examples)
* No color gradients
* Sans Serif fonts, only. [Open Sans Condensed](https://fonts.google.com/specimen/Open+Sans+Condensed) is a good option.
* Units should always be specified clearly ($1.3M/year vs. 1,300,000)
* Maps of Washington, DC should use the Maryland State Plane Projection (EPSG: 26985) to minimize distortion.

Basic data visualization tools are readily available within Google sheets (see guide). More advanced tools are available in Python ([matlibplot](http://matplotlib.org/)), R ([ggplot2](http://ggplot2.org/)), [Tableau Public](https://public.tableau.com/s/), [QGIS](http://www.qgis.org/en/site/), and [D3.js](https://d3js.org/).

Certain visualizations can also be created using GreaterGreaterWashington’s own data visualization tool (a fork of [RAW](http://raw.densitydesign.org/) by DensityDesign). The tool provides authors with the ability to create specific visualizations pre-formatted to adhere to the above best practices and Greater Greater Washington’s website design.

When submitting the final visual the author should submit the visual as both a vector graphic (SVG) and as a raster graphic (PNG, GIF, JPEG). 

## Step 5: Creating a repository

Savvy readers will rightly expect to view source data and any intermediate transformations used to create a visualization. As a best-practice, authors are urged to provide a link at the end of their article to the data and other intermediate products such as code. 

Github is the gold standard, for these repositories. For example, [here is 538’s extensive set of repositories](https://github.com/fivethirtyeight/data). For many advanced visuals, such as those built in R, Python, or D3.js, Github is strongly encouraged. 

For simpler visuals, a public link to Google sheets is a convenient alternative. An embedded Tableau workbook is also an attractive option as it provides the underlying data by default.

For an example of well-documented repositories, see… by...

## Limitations of Data Visualization

It is possible for data to mislead, so care must be taken to ensure that the story told by the visual is accurate. The author should verify the data by consulting subject matter experts or even direct confirmation of data points when appropriate. 

Visualizations also tend to dehumanize subject matter. Data visualizations excel at simplifying stories at a grand scale, but this abstraction comes at a cost: the subject matter is less relatable at an individual level. This detachment can be useful as when human error introduces bias (the anti-vaccination movement, for example). In other cases, though, this detachment can weaken a story or even be outright offensive. A best practice is to pair visualization with representative anecdotes. In this way, the reader can understand both macro trends and how they play out at an individual level.

## Additional Resources 
* [Ezra Klein on visualization attribution](http://www.vox.com/2015/4/13/8405999/how-vox-aggregates)
* [Nate Silver’s manifesto](http://fivethirtyeight.com/features/what-the-fox-knows/)
* [Data journalism handbook (Beta)](http://datajournalismhandbook.org/1.0/en/)
