# Introduction #

We'd like to create a graphical simulator for the vanilla Create robot. We want it to be extremely simple to use so that beginning programmers could use it without having to go through an arduous install process.

The plan is to write the simulator in Python and to use PyGame for the graphics. It appears at the moment that PyGame 1.8 using Python 2 is the most mature/stable option.

The simulator should be able to support multiple robots running at once. To begin, they can be represented graphically as colored circles, and with a segment of the circle colored differently to indicate the front. Labels could be put on the robots to further distinguish them.

The simulation is in 2D, and should simulate rooms with common (rectangular) obstacles such as table or chairs. Of course, if there is more than one robot in the simulator, the Create's themselves are also obstacles.