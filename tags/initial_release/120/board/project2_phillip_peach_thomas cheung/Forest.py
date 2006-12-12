#################################
# Forest Classes and Methods    #
#                               #
# By Phillip Peach              #
# some methods by Thomas Cheung #
#################################

import random
from Helicopter import *

class Forest:

    def __init__(self, config):
        self.lightning, self.lightning_rect = load_image('lightning.bmp',WHITE)
        self.set_up_config(config)
        self.forest = self.read_in_forest(self.init_file)
        self.pick_wind_dir()

    def set_up_config(self,config):
        """Converts appropriate parts of the conifg
        into local class instance variables. Missing or
        incorrect variables will go to default."""
        #This is not a good way to do this,
        #tried a way with dictionaries but
        #that didn't work right so, fugly.
        
        default = {'simulation_speed': '2500',
                   'simulation_duration': '500',
                   'storm_duration': '10',
                   'initilization_file': 'forest.txt',
                   'logging_enabled': 'False',
                   'log_file': 'log.txt',
                   'number_of_firefighter_helicopters': '0',
                   'end_of_simulation_dump_file': 'None',
                   'normal_burning_chance': '0.20',
                   'wind_burning_chance': '0.40',
                   'lee_burning_chance': '0.05',
                   'burn_duration': '5',
                   'water_duration': '8',
                   'water_surpression': '0.03'}
        
        try:
            self.sim_speed = int(config['simulation_speed'])
        except KeyError:
            self.sim_speed = int(default['simulation_speed'])
        except ValueError:
            self.sim_speed = int(default['simulation_speed'])

        try:
            self.sim_dur = int(config['simulation_duration'])
        except KeyError:
            self.sim_dur = int(default['simulation_duration'])
        except ValueError:
            self.sim_dur = int(default['simulation_duration'])

        try:
            self.storm_dur = int(config['storm_duration'])
        except KeyError:
            self.sim_dur = int(default['storm_duration'])
        except ValueError:
            self.sim_dur = int(default['storm_duration'])

        try:
            self.init_file = config['initilization_file']
        except KeyError:
            self.sim_dur = default['initilization_file']

        try:
            if config['logging_enabled'] in ['True', 'T', '1']:
                self.logging = True
            else:
                self.logging = False
        except KeyError:
            self.logging = False

        try:
            self.log_file = config['log_file']
        except KeyError:
            self.log_file = default['log_file']

        try:
            self.nmbr_helis = int(config['number_of_firefighter_helicopters'])
        except KeyError:
            self.nmbr_helis = int(default['number_of_firefighter_helicopters'])
        except ValueError:
            self.nmbr_helis = int(default['number_of_firefighter_helicopters'])

        try:
            self.dump_file = config['end_of_simulation_dump_file']
        except KeyError:
            self.dump_file = int(default['end_of_simulation_dump_file'])

        try:
            self.norm_chance = float(config['normal_burning_chance'])
        except KeyError:
            self.norm_chance = float(default['normal_burning_chance'])
        except ValueError:
            self.norm_chance = float(default['normal_burning_chance'])

        try:
            self.wind_chance = float(config['wind_burning_chance'])
        except KeyError:
            self.wind_chance = float(default['wind_burning_chance'])
        except ValueError:
            self.wind_chance = float(default['wind_burning_chance'])

        try:
            self.lee_chance = float(config['lee_burning_chance'])
        except KeyError:
            self.lee_chance = float(default['lee_burning_chance'])
        except ValueError:
            self.lee_chance = float(default['lee_burning_chance'])

        try:
            self.burn_dur = int(config['burn_duration'])
        except KeyError:
            self.burn_dur = int(default['burn_duration'])
        except ValueError:
            self.burn_dur = int(default['burn_duration'])

        try:
            self.water_dur = int(config['water_duration'])
        except KeyError:
            self.water_dur = int(default['water_duration'])
        except ValueError:
            self.water_dur = int(default['water_duration'])

        try:
            self.water_surp = float(config['water_surpression'])
        except KeyError:
            self.water_surp = float(default['water_surpression'])
        except ValueError:
            self.water_surp = float(default['water_surpression'])

    def read_in_forest(self, fname = 'forest.txt'):
        """Reads in a text file and formats as
        as forest list."""
        infile = open(fname, 'r')
        forest = []
        for line in infile:
            current_row = list(line.strip())
            i = 0
            while i < len(current_row):
                current_row[i] = [current_row[i], 0]
                i+= 1
            forest.append(current_row)
        self.forest = forest
        return forest

    def write_out_forest(self, forest, fname, append=False):
        """Writes or appends the forest to a file, removing
        all the list qualities leaving just T's, B's,
        and E's."""
        if append:
            outfile = open(fname, 'a')
        else:
            outfile = open(fname, 'w')
        for row in forest:
            row_string = ''
            for tree in row:
                row_string += (tree[0])
            outfile.write(row_string + '\n')
        outfile.close()

    def copy_forest(self,forest):
        """Having problems simply copying nested lists
        instead of pointing to the old list. The only way
        I can seem to force a full copy is to go through
        element by element, so I'm making it into a function."""
        new_forest = []
        for r in range(len(forest)):
            current_row = forest[r]
            new_row = []
            for element in current_row:
                new_row.append(element[:])
            new_forest.append(new_row)
        return new_forest

    def print_forest(self):
        """Prints out the forest to the display while removing
        all the list attributes."""
        forest_string = ''
        for row in self.forest:
            row_string = ''
            for tree in row:
                row_string += (tree[0])
            forest_string += row_string + '\n'
        print forest_string
        print '\n\n'

    def pick_wind_dir(self):
        """Picks a random cardinal direction
        or none for the wind."""
        wind_dirs = { 0:'north',
                      1:'south',
                      2:'east',
                      3:'west',
                      4:'none'}
        wind = random.randint(0,4)
        self.wind = wind_dirs[wind]

    def lightning_strike(self):
        """Picks a random plot, if a tree,
        sets it on fire. Returns the strike location for other
        functions to use."""
        forest = self.copy_forest(self.forest)
        r, c = self.pick_random_tree()
        if forest[r][c][0] == 'T':
            forest[r][c] = ['B', 0]
        self.forest = forest
        return [r, c]

    def pick_random_tree(self):
        """Returns a random plot in
        a forest."""
        forest = self.forest
        nmbr_rows = len(forest)
        nmbr_columns = len(forest[0])
        r = random.randint(0, nmbr_rows - 1)
        c = random.randint(0, nmbr_columns - 1)
        return r,c

    def perform_next_tick(self):
        """Returns a new forest and sets the class
        forest to the same with any new fires
        set by spreading, any damp trees evaporated to
        normal and any burned out fires reduced to empty land."""
        wind = self.wind
        current_forest = self.copy_forest(self.forest)
        new_forest = []
        for row in range(len(current_forest)):
            current_row = current_forest[row]
            for col in range(len(current_forest[0])):
                #Increment the how long have I been what I've been variable.
                current_row[col][1] += 1
                #Forest plot transformation processing is here.
                if current_row[col][0] == 'B':
                    if self.tree_stopped_burning(current_forest, row, col):
                        current_row[col] = ['E', 0]
                elif current_row[col][0] == 'W':
                    if self.has_water_evaporated(current_forest, row, col):
                        current_row[col]=['T', 0]
                elif current_row[col][0] in ['T', 'W']:
                    if self.will_tree_burn(current_forest, row, col, wind):
                        current_row[col]=['B',0]
            new_forest.append(current_row)
            self.forest = new_forest
        return new_forest

    def tree_stopped_burning(self, current_forest, r, c):
        """Returns True or False depending on whether a tree
        hass burned out or not."""
        ticks_burning=current_forest[r][c][1]
        if ticks_burning > self.burn_dur:
            return True
        else:
            return False

    def has_water_evaporated(self,current_forest, r, c):
        """Finds out if water from a damp tree
        has evaporated. Returns True or False."""
        ticks_burning=current_forest[r][c][1]
        if ticks_burning < self.water_dur:
            return False
        else:
            return True

    def will_tree_burn(self,current_forest,r,c,wind):
        """Calculates a tree's chance to burn
        taking into account the wind and
        returns True or False based on that."""
        opposites = { 'north':'south',
                      'south':'north',
                      'west':'east',
                      'east':'west',
                      'none':'none'
                    }
        chance_burn = 0
        neighbors = self.get_neighbors(current_forest, r, c)
        for tree in neighbors:
            windward=wind == tree[2]
            leeward=opposites[wind] == tree[2]
            
            #Element [2] of each tree being the
            #cardinal direction with respect to
            #the tree whose burning chance is
            #being calcualted.

            if (current_forest[tree[0]][tree[1]][0] == 'B') and windward:
                chance_burn += self.wind_chance
            elif (current_forest[tree[0]][tree[1]][0] == 'B') and leeward:
                chance_burn += self.lee_chance
            elif (current_forest[tree[0]][tree[1]][0] == 'B'):
                chance_burn += self.norm_chance
            elif (current_forest[tree[0]][tree[1]][0] == 'W'):
                chance_burn -= self.water_surp *\
                               (self.water_dur - current_forest[tree[0]][tree[1]][1])

        if chance_burn > random.random():
            return True
        else:
            return False

    def get_neighbors(self,forest, r, c):
        """Out of all the possible neighbors a tree
        could have, returns the ones that actually
        exist. Additionally adds the cardinal
        direction to allow for wind calculations."""
        possible_neighbors = [    [r,c-1,'west'],
                                  [r,c+1,'east'],
                                  [r-1,c,'north'],
                                  [r+1,c,'south'],
                                  [r,c,'center']
                             ]
        neighbors = []
        for neighbor in possible_neighbors:
            #Is neighbor valid (i.e. not off the edge): keep it.
            if (0 <= neighbor[0] <= (len(forest) - 1)) and \
               (0 <= neighbor[1] <= (len(forest[0]) - 1)):
                neighbors.append(neighbor)
        return neighbors

    def rotate(self):
        """Returns a new forest rotated 90 degrees from its
        previous position."""
        forest = self.copy_forest(self.forest)    
        new_forest = []
        for col in range(len(forest[0])):
            new_row = []
            for row in range(len(forest)-1, -1, -1):
                new_row.append(forest[row][col])
            new_forest.append(new_row)
        self.forest=new_forest
        return new_forest

        def instance_n_fire_helis(self,n):
            """Instantiate n number members of the Helicopter class
            stow them all in a dictionary and return that dictionary."""
            fire_helis = {}
            for heli_nmbr in range(n):
                key = 'fire_heli%s' % heli_nmbr
                fire_helis[key] = Helicopter(self.forest)
            return fire_helis
                    
                
    def dump_water(self,fire_heli):
        """Takes a fire heli, queries it's target
        and turns the appropriate squares to W."""
        #Really too tightly coupled to the helicopters.
        #Not the best place for this.
        forest = self.copy_forest(self.forest)
        if fire_heli.target == fire_heli.home_base:
            return None
        c = fire_heli.target[1]
        r = fire_heli.target[0]
        neighbors = fire_heli.get_neighbors(forest, r, c, 1)
        for neighbor in neighbors:
            if forest[neighbor[0]] [neighbor[1]][0] != 'E':        
                forest[neighbor[0]] [neighbor[1]] = ['W',0]
        self.forest = forest

class Basic_Visual_Forest(Forest):
    #Difference in this is fire_helis are fed a different bitmap.
    def instance_n_fire_helis(self,n,square_size):
        """Instantiate n number members of the Helicopter class
        stow them all in a dictionary and return that dictionary."""
        fire_helis = {}
        for heli_nmbr in range(n):
            key = 'fire_heli'+str(heli_nmbr)
            fire_helis[key] = Helicopter(self.forest,'helicopter1.bmp',square_size)
        return fire_helis


    def instance_extra_fire_heli(self,fire_helis,nmbr,square_size):
        """Takes the fire heli dictionary and other pertinent info, and adds another
        helicopter to the dictionary before returning it."""
        key = 'fire_heli%s' % nmbr
        fire_helis[key]=Helicopter(self.forest, 'helicopter1.bmp', square_size)
        return fire_helis

    def remove_fire_heli(self, fire_helis,nmbr):
        """Removes a fire heli from the dictionary."""
        key = 'fire_heli%s' % nmbr
        del fire_helis[key]
        return fire_helis

    def update_forest(self,board):
        """Takes the forest and board and
        visually updates it."""
        for row in range(board.num_rows):
            for col in range(board.num_columns):
                color = self.letter_to_color(self.forest[row][col][0])
                board.set_color(row, col, color)
        board.update()
       # return board

    def set_square_size(self, cols, rows, screen_size =(1000, 700)):
        square_size = 20.0
        while cols * square_size > screen_size[0]or rows * square_size > screen_size[1]:
            square_size -= 1
        if square_size < 1:
            return 1
        self.square_size = square_size
        return int(square_size)

    def letter_to_color(self,letter):
        """ Converts a letter to a color.
        """
        if letter == 'E':
            return BROWN
        elif letter == 'T':
            return GREEN
        elif letter == 'B':
            return RED
        elif letter == 'L':
            return BLUE
        elif letter == 'W':
            return PURPLE
        else:
            return BLACK  # unknown letter

class Advanced_Visual_Forest(Basic_Visual_Forest):
    
    def update_forest(self, board):
        """Takes the forest and board and
        visually updates it."""
        board.update(self.forest)

    def instance_n_fire_helis(self,n,square_size):
        """Instantiate n number members of the Helicopter class
        stow them all in a dictionary and return that dictionary."""
        fire_helis = {}
        for heli_nmbr in range(n):
            key = 'fire_heli%s' % heli_nmbr
            fire_helis[key] = Helicopter(self.forest, 'helicopter.bmp', square_size)
        return fire_helis

    def instance_extra_fire_heli(self, fire_helis, nmbr, square_size):
        """Takes the fire heli dictionary and other pertinent info, and adds another
        helicopter to the dictionary before returning it."""
        key = 'fire_heli%s' % nmbr
        fire_helis[key]= Helicopter(self.forest, 'helicopter.bmp', square_size)
        return fire_helis

