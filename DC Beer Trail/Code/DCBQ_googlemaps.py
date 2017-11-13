# -*- coding: utf-8 -*-
"""
        *  *  *
        =======
        =======

The District Beer Quest

          by

       Daniel H

Based on the work of Randy Olson:
http://www.randalolson.com/2016/06/05/computing-optimal-road-trips-on-a-limited-budget/
"""

#Load libraries
import math
import pandas as pd
import numpy as np
from itertools import permutations
import googlemaps

#Load Data
df = pd.read_csv("Data/DCBQ.csv")

#Set up Google Maps API
gmaps = googlemaps.Client(key="AIzaSyDwjotEc82BIxxN74hLIcDEodZQGswuGZE")

#Verify lat long
for index, row in df.iterrows():
    try:
        loc = gmaps.geocode(row['address'])[0]['geometry']['location']
        df.lat[index] = loc['lat']
        df.lng[index] = loc['lng']
        print("Found %s!" % (row['address']))
    except Exception as e:
        print("Can't find %s." % (row['address']))
          
#write to csv
df.to_csv('Data/DCBQ.csv', index=False)  

#Build Distance matrix
waypoint_distances = {}
waypoint_durations = {}

for (waypoint1, waypoint2) in permutations(df.address, 2):
    try:
        route = gmaps.distance_matrix(origins=[waypoint1],
                                      destinations=[waypoint2],
                                      mode="driving",
                                      language="English",
                                      units="metric")

        # "distance" is in meters
        distance = route["rows"][0]["elements"][0]["distance"]["value"]

        # "duration" is in seconds
        duration = route["rows"][0]["elements"][0]["duration"]["value"]

        print("From %s to %s is %s and takes %s" % (waypoint1, waypoint2, distance, duration))

        waypoint_distances[frozenset([waypoint1, waypoint2])] = distance
        waypoint_durations[frozenset([waypoint1, waypoint2])] = duration
    
    except Exception as e:
        print("Error with finding the route between %s and %s." % (waypoint1, waypoint2))

with open("Data/DCBQ_dist.csv", "w") as out_file:
    out_file.write(",".join(["waypoint1",
                              "waypoint2",
                              "distance_m",
                              "duration_s"]))
    
    for (waypoint1, waypoint2) in waypoint_distances.keys():
        out_file.write("\n" + ",".join([str('"' + waypoint1 + '"'),
                                  str('"' + waypoint2 + '"'),
                                  str(waypoint_distances[frozenset([waypoint1, waypoint2])]),
                                  str(waypoint_durations[frozenset([waypoint1, waypoint2])])]))
