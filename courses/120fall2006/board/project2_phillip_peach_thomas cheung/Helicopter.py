#############################
# Water Bombing Helicopters #
#                           #
# By Phillip Peach          #
#                           #
#############################
from Board import *
import math

class Helicopter:
    """Helicopters that seek out targets and
    then return home, should be generic except
    now it's not, could be a base for a
    genric chopper, override the forest
    specific targeting."""
    #List of targets for all heli's to
    #allow target duplication avoidance
    _targets=[]
    def __init__(self,forest, image = 'helicopter.bmp',\
                 square_size = 20, home_base = [-1.0, -1.0]):
        #There's some kind of bug that occurs
        #if home_base[0]!=home_base[1]
        #may have been fixed have not tested in while
        
        #Because of the great fun of going between
        #the normal (x,y) and (r,c) i am declaring
        #by fiat that all cooridnates will be in a
        # list of [y,x] unless otherwise noted
        #inside this class.
        self.top_speed = 0.5
        self.forest_width = len(forest[0])
        self.forest_height = len(forest)
        self.home_base = home_base
        self.position = self.home_base[:]
        self.speed = 0.0
        self.angle = 0.0
        self.outbound = False
        self.target = self.home_base[:]
        self.image, self.rect = load_image(image, WHITE)
        self.square_size = square_size
        self.rect.x = home_base[1] * self.square_size
        self.rect.y = home_base[0] * self.square_size
        self.time_to_retarg = 0
        
        
    def out_of_bounds(self):
        """Bounds checking for if any of the helicopters goes
        haywire."""
        if self.position[0] <= -2 or \
           self.position[0] >= self.forest_height*self.square_size+2 or \
           self.position[1] <= -2 or \
           self.position[1] >= self.forest_width*self.square_size+2:
            return True
        else:
            return False
    
    def update_position(self,forest):
        if self.out_of_bounds():
            self.__init__(forest,self.home_base)
        if (self.target != self.home_base) and self.time_to_retarg == 0:
            self.del_target()
            self.choose_target(forest)
            self.time_to_retarg=10
        else:
            self.time_to_retarg -= 1
            if self.at_target():
                self.speed = 0.0
                self.position = self.home_base[:]
        if self.target==self.position:
            self.speed=0.0
        else:
            self.speed=self.top_speed
        
        x_mov = math.cos(self.angle) * self.speed
        y_mov = math.sin(self.angle) * self.speed
        self.position[1] += x_mov
        self.position[0] += y_mov
        self.rect.y = self.position[0] * self.square_size
        self.rect.x = self.position[1] * self.square_size
        
        if self.at_target():
            return True
        else:
            return False

    def change_dir(self, forest):
        if self.outbound:
            self.del_target()
            self.target = self.home_base
            self.debug = self.position
            self.figure_angle(self.position, self.target)
            self.outbound = False
        else:
            self.time_to_retarg = 10
            self.choose_target(forest)
            self.outbound = True

    def grid_position(self):
        """Return the closest forest grid to the heli's position.
        Returns normal x,y"""
        x = self.round_float(self.position[1])/self.square_size
        y = self.round_float(self.position[0])/self.square_size
        return [x,y]
    
    def round_float(self,flt):
        """Returns a normal roundings of a float. I.e. if the tenths value is
        >= 5 rounds up else down."""
        if math.modf(flt)[0] < 0.5:
            return int(flt)
        else:
            return int(flt) + 1
    def at_target(self):
        x_diff = abs(self.target[1] - self.position[1])
        y_diff = abs(self.target[0] - self.position[0])
        if x_diff<0.5 and y_diff <0.5:
            return True
        else:
            return False
        
    def choose_target(self, forest):
        #The target and a second number for desirbilty
        best_target = [self.home_base[:], 0]

        #Exclusion zones keep helicopters from picking adjacent targets
        #but if there's nothing to choose then reduce the zone size so
        #the helicopter doesn't do nothing.
        for exclusion_zone in range(3,-1,-1):

            for r in range(len(forest)):
                for c in range(len(forest[0])):
                    if forest[r][c][0] == 'B' and \
                       self.target_avalible(forest, r, c, exclusion_zone):
                        desirbility = self.targ_desirbility([r, c], forest)
                        
                        if best_target[1] < desirbility:
                            best_target=[[r, c], desirbility]
                            
            if best_target != [self.home_base, 0]:
                self.target = best_target[0]
                Helicopter._targets.append(self.target)
                self.figure_angle(self.position, self.target)
                return
            else:
                self.target = self.home_base
                self.figure_angle(self.position, self.target)
                self.out_bound = False

    def targ_desirbility(self,potential_targ,forest):
        """Computes a desirbilty index for potential
        targets. Tweak this if you want to modify
        helicopter behavior."""
        dist = self.distance(potential_targ, self.position)

        neighbors = self.get_neighbors(forest, potential_targ[0], potential_targ[1], 3)
        nearby_fires = 0
        for neighbor in neighbors:
            if forest[neighbor[0]][neighbor[1]][0]=='B':
                nearby_fires+=1
                
        #At close distance we start ignoring distance and just
        #return based on the number of fires. An attempt to reduce
        #the number of helicopters targeting the closer but stupider
        #objective.
        if dist<6.0 and nearby_fires>7:
            return 1000+nearby_fires
        
        try:
            #Increase the constant in this to increase
            #the importance of burning neighbors
            #or decrease to increase the importance
            #of nearness.
            desirbility=nearby_fires*(7.9/dist)
        except ZeroDivisionError:
            #If we had a div error it means that
            #the distance went to zero so this
            #is really desirable / we're there anyway.
            desirbility=999
            
        return desirbility
        
    def distance(self, coorid1, coorid2):
        """Returns the distance between two cooridnates."""
        x_dist = coorid2[1] - coorid1[1]
        y_dist = coorid2[0] - coorid1[0]
        distance = math.sqrt(x_dist**2 + y_dist**2)
        return distance

    def forest_dimensions(self,forest):
        rows=len(forest)
        cols=len(fores[0])
        return rows,cols
    
    #Remember, the forest and board y axis is reversed
    #from the mathmatical normal, 0 at the top and pos numbers go down,
    #but x axis is normal. Also remember list is
    #[y,x] instead of the normally expected x,y
    def figure_angle(self,coorid1,coorid2):
        """Gets the angle between two points."""
        x_dist=coorid2[1]-coorid1[1]
        y_dist=coorid2[0]-coorid1[0]
        try:
            ref_angle=abs(math.atan(y_dist/x_dist))
        except ZeroDivisionError:
            ref_angle=0.0
        if y_dist>0 and x_dist>0:
            self.angle=ref_angle
        elif y_dist>0 and x_dist<0:
            self.angle=math.pi-ref_angle
        elif y_dist<0 and x_dist<0:
            self.angle=ref_angle+math.pi
        else:
            self.angle=(math.pi*2.0)-ref_angle
        
        
    def target_avalible(self,forest,r,c,exclusion_zone=3):
        """Returns True if no other helicopter has that target or one within
        the exclusion zone and False otherwise."""
        neighbors=self.get_neighbors(forest,r,c,exclusion_zone)
        for neighbor in neighbors:
            if neighbor in Helicopter._targets:
                return False
        return True

            

    def get_neighbors(self,forest,r,c,d):
        """Returns all the neighbors in existance, d distance away
        from the given point."""
        possible_neighbors=[]
        start_row=r-d
        final_row=r+d
        start_col=c-d
        final_col=c+d
        
        cur_row=start_row
        while cur_row<=final_row:
            cur_col=start_col
            while cur_col<=final_col:
                possible_neighbors.append([cur_row,cur_col])
                cur_col+=1
            cur_row+=1

        neighbors=[]
        for neighbor in possible_neighbors:
            if (0<=neighbor[0]<=(len(forest)-1)) and \
               (0<=neighbor[1]<=(len(forest[0])-1)):
                neighbors.append(neighbor)
        return neighbors
    
    def del_target(self):
        """Delete the current target from the master target list."""
        try:
            Helicopter._targets.remove(self.target)
        except ValueError:
            pass

    def __del__(self):
        self.del_target()
