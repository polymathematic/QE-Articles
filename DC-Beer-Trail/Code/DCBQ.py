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

import pandas as pd
import numpy as np
import copy
import random
from tqdm import tqdm
from deap import algorithms
from deap import base
from deap import creator
from deap import tools
import webbrowser

route_distances = {}
route_durations = {}
brewery_opentimes = {}
brewery_closetimes = {}
 
all_breweries = set()

route_data = pd.read_csv('Data/DCBQ_dist.csv')
brewery_data = pd.read_csv('Data/DCBQ.csv')

for i, row in route_data.iterrows():
    # Distance = meters
    route_distances[frozenset([row.waypoint1, row.waypoint2])] = row.distance_m

    # Duration = minutes
    route_durations[frozenset([row.waypoint1, row.waypoint2])] = row.duration_s / 60.0
    all_breweries.update([row.waypoint1, row.waypoint2])

creator.create("FitnessMulti", base.Fitness, weights=(-1.0,1.0))
creator.create("Individual", list, fitness=creator.FitnessMulti)

def create_route(locs):
    rte = random.sample(locs, len(locs))
    return rte

toolbox = base.Toolbox()
toolbox.register('waypoints', create_route, all_breweries)
toolbox.register('individual', tools.initIterate, creator.Individual, toolbox.waypoints)
toolbox.register('population', tools.initRepeat, list, toolbox.individual)

def eval_beer_quest(individual):

    trip_length = 0.
    individual = list(individual)

    for index in range(1, len(individual)):
        waypoint1 = individual[index - 1]
        waypoint2 = individual[index]
        trip_length += route_distances[frozenset([waypoint1, waypoint2])]

        return trip_length, 0 #Second value is placeholder for an additional constraint based on business hours 

def mutation_swap(individual):
    index1 = random.randint(0, len(individual) - 1)
    index2 = index1
    while index2 == index1:
        index2 = random.randint(0, len(individual) - 1)      
    individual[index1], individual[index2] = individual[index2], individual[index1]
    
    return individual,

toolbox.register('evaluate', eval_beer_quest)
toolbox.register('mutate', mutation_swap)
toolbox.register("select", tools.selTournament, tournsize=3)
pop = toolbox.population(n=10)

# This is a hack to make the tqdm progress bar work
stats = tools.Statistics(lambda ind: (int(ind.fitness.values[0]), round(ind.fitness.values[1], 2)))
stats.register('Minimum', np.min, axis=0)
stats.register('Maximum', np.max, axis=0)
stats.register('Progress', lambda x: pbar.update())

hof = tools.HallOfFame(1)

#run it
total_gens = 600000
pbar = tqdm(total=total_gens)
pop, log = algorithms.eaSimple(pop,
                               toolbox,
                               cxpb=0.,
                               mutpb=1.0,
                               ngen=total_gens,
                               stats = stats,
                               halloffame = hof,
                               verbose=False)
pbar.close()

x = list(map(toolbox.evaluate, hof))

output_file = 'Output.html'
CreateOptimalRouteHtmlFile(hof[0], 1, display=True)
