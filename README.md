Traveling Salesman Problem via Genetic Algorithm

![](TSP_PMX.gif?raw=true "")

Implementation is in Lua 5.3 and is based on David E. Goldberg's book "Genetic Algorithms: In Search, Optimization and Machine Learning".

Three different crossover operators are tested and their performance is compared - Partially Matched Crossver, Order Crossover and Cycling Crossover altogether with a version without crossover but just inversion as mutation. PMX, OX, CX and Inversion versions are tested via different parameter values for crossover exchange section length and inversion length(for CX the former does not matter). Each takes 10 values as 10% increse steps over the chromosome length and different graphs are plotted to find the optimal values for each crossover operator. Here are results on the Berlin52 data set taken from TSPLIB(https://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/):

PMX:
![](TSP/Berlin52_Multi_PMX.bmp?raw=true "")

OX:
![](TSP/Berlin52_Multi_OX.bmp?raw=true "")

For the CX version the green graph is 0 crossover length meaning no crossover is performed and only different inversion lengths are tried as mutations.

CX(with inversion 5-50%:)
![](TSP/Berlin52_5_50_Multi_CX.bmp?raw=true "")

CX(with inversion 55-100%:)
![](TSP/Berlin52_55_100_Multi_CX.bmp?raw=true "")


No improvements are made in the algorithm like fitness scaling, ranking, overlapping population, crowd factoring and so on. Only the simple  implementation is tested. The results are saved in a bitmap file which is implemented in pure Lua and is quite slow though.

