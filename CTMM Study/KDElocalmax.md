The [Crosstown Multimodal transport study](http://www.dccrosstownstudy.com/) is part of the effort to improve transportation options in the District of Columbia. DC's Department of Transportation is weighing the costs and benefits of numerous improvements for people who walk, cycle, or use transit between the Columbia Heights and Brookland neighborhoods. The study actively solicits input from the public, and to date has held three public meetings (with a fourth scheduled) and created a [website](http://www.dccrosstownstudy.com/interactivemap/) where the public can provide commentary on specific locations within the study area.

The comment map interested me. Individual comments are insightful and reading through them gives a good impression about how the public perceives a given area. I wanted the big picture though: What did the comments indicate were the most important issues in the study area?

I reached out to the study's leadership and they graciously provided me with the raw data from the comment map. My plan was to identify areas with a high-density of comments and then use text analysis on the comments to determine overarching themes.

The results were not as satisfying as I had hoped. As it turns out, 600 comments across the entire study area proved to be too few. Even the densest areas only had a small number of comments each. Nonetheless, I want to document the process for identifying the areas of interest. It may have a use elsewhere. 

##The results

Here's the bottom line up front: the five areas with the highest (relative) density of comments are... 

1. Columbia Heights Metro Station ([link](https://www.google.com/maps/@38.9288193,-77.0326456,3a,75y,242.45h,85.26t/data=!3m7!1e1!3m5!1sFZeYzreWbwfTsYp3vr_qtA!2e0!6s%2F%2Fgeo3.ggpht.com%2Fcbk%3Fpanoid%3DFZeYzreWbwfTsYp3vr_qtA%26output%3Dthumbnail%26cb_client%3Dmaps_sv.tactile.gps%26thumb%3D2%26w%3D203%26h%3D100%26yaw%3D146.93626%26pitch%3D0!7i13312!8i6656))
2. The street layout west of the hospital center ([link](https://www.google.com/maps/@38.9297734,-77.0171353,3a,75y,238.57h,79.03t/data=!3m6!1e1!3m4!1sxODjNAJP4Qk_zPrZfrnijQ!2e0!7i13312!8i6656))
3. Michigan Avenue along Catholic University's southern edge ([link](https://www.google.com/maps/@38.9320797,-76.9988472,3a,75y,65.83h,83.74t/data=!3m6!1e1!3m4!1s0o1xceYddYc718oJGoRT_A!2e0!7i13312!8i6656))
4. The Metropolitan Branch Trail crossing at Monroe Street ([link](https://www.google.com/maps/@38.93223,-76.9949797,3a,75y,18.99h,83.31t/data=!3m6!1e1!3m4!1sx3oMSNP5mNFpKaIuivkuLg!2e0!7i13312!8i6656))
5. The intersection of Michigan Avenue and 10th Street NE ([link](https://www.google.com/maps/@38.935459,-76.993003,3a,75y,181.65h,80.95t/data=!3m7!1e1!3m5!1sZam189MqE2OwAHPk-kiBwg!2e0!6s%2F%2Fgeo3.ggpht.com%2Fcbk%3Fpanoid%3DZam189MqE2OwAHPk-kiBwg%26output%3Dthumbnail%26cb_client%3Dmaps_sv.tactile.gps%26thumb%3D2%26w%3D203%26h%3D100%26yaw%3D143.30801%26pitch%3D0!7i13312!8i6656))

![](/content/images/2016/09/maxkde-1.svg)

This is an intuitive finding. As a cyclist and pedestrian in the area, I have a lot of thoughts on 2, 4, and 5. Trying to bike across Monroe, in the mornings can be a little dicey.

##How it was done

At a high level, the process for locating the most commented-on areas is simple: use comment latitude and longitude to estimate the underlying geographic distribution, find the points that correspond to peaks in that distribution, and then keep the top X points by estimated density. 

For the first part, I used [kernel density estimation](https://en.wikipedia.org/wiki/Kernel_density_estimation) of the comment geographic distribution. This gives us the smooth heat map you see above. 

Bandwidth selection is usually the dilemma with KDE. Here I cheated a bit by assuming that the underlying distribution will be strongly dictated by two factors: First, how precisely users of the map placed their comments. Second, people's notion of what constitutes a place (e.g., how far can I get from the Columbia Heights Metro before I would no longer describe myself as "near the intersection"). In my estimate, the answer to both is "about 300 meters." 


    #Determine Bandwidth and KDE output matrix side
    #1 mile = 1609.34 meters
    #1 degree lat = 54.6 miles
    #1 degree long = 69 miles)

    meters <- 300
    areadim <- c()

    areadim['EW'] <- (max(dcmm_df$coords.x1) -
                      min(dcmm_df$coords.x1)) * 
                      (54.6 * 1609.34)

    areadim['NS'] <- (max(dcmm_df$coords.x2) - 
                      min(dcmm_df$coords.x2)) *
                      (69 * 1609.34)

    areadim <- floor(areadim/40)
    bandwidth <- (((meters)/1609.34)/((54.6+69)/2))

    #Create density estimate
    dcmm_kde <- kde2d(dcmm_df$coords.x1,
                      dcmm_df$coords.x2,
                      n = areadim,
                      h = bandwidth)


Once we have the density estimate, we need to check second order conditions across the matrix to find the local maxima. Since the output of KDE is a matrix, this is as simple as differencing twice across latitude and longitude and then taking those points that meet second order conditions on both latitude and longitude.

    #Find local maxima
    maxima <- function(x){
      temp <- rep(FALSE, length(x))
      temp[which(diff(sign(diff(x)))==-2)+1] <- TRUE
      return(temp)
    }

    maxima_long <- apply(dcmm_kde$z,1,maxima)
    maxima_lat <- apply(dcmm_kde$z,2,maxima)
    maxima_comb <- t(maxima_long) & maxima_lat

This will give you all local maxima, so as a final step check the estimated density at each location and choose which are the most important.

That's it, a very simple but direct means of extracting structure from a geographic distribution. Once you have the points of interest, it is possible to subset the original comments based on proximity, or compare against other geospatial datasets.

You can find the full source code and the comment data on [my Github site](https://github.com/DanHenebery/dc-multimodal-comment-maps).