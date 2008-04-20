 ######################################
# Forest Simulators                  #
#                                    #
# by Thomas Cheung and Phillip Peach #
#                                    #
######################################


from Forest import *
from Board import *
import time

class Basic_Graphical_Simulation:
    def __init__(self, Basic_Visual_Forest, screen_size):
        self.forest = Basic_Visual_Forest
        
        #Set Control Variables
        self.max_tick = self.forest.sim_dur
        self.storm_l = self.forest.storm_dur
        self.sim_speed = self.forest.sim_speed
        self.tick = 0

        #Set up the Board, which sets up the display for us.
        
        rows = len(self.forest.forest)
        cols = len(self.forest.forest[0])
        self.square_size = self.forest.set_square_size(cols, rows)
        self.board  = Board(rows, cols, square_color = GREEN, background_color = WHITE,
              square_width = self.square_size, square_height = self.square_size)
        #Board surface is a copy of the unadorned board, so helicopter movement can be covered up.
        self.board_surface = pygame.Surface(self.board.screen.get_size())
        #For making lightning strikes distinct.
        self.blue_square = pygame.Surface((self.square_size-2,self.square_size-2))
        self.blue_square.fill(BLUE)

        #Set Up any Helicopters
        self.nmbr_helis = self.forest.nmbr_helis
        self.fire_helis = self.forest.instance_n_fire_helis(self.nmbr_helis, self.square_size)

        #Set up custom event IDs.
        self.FOREST_UPDATE_EVENT = 25
        self.STORM_EVENT = 26
        self.HELICOPTER_MOVE_EVENT = 27

        #Define custom helicopter event so it can be popped into the event queue.
        self.helievent = pygame.event.Event(self.HELICOPTER_MOVE_EVENT)

        #Ignore some events to reduce risk of queue flood.
        pygame.event.set_blocked((MOUSEMOTION, ACTIVEEVENT))

        #Set up the clock and the event timers, be careful, the helicopter occurs
        #often enough to flood the queue if something takes a significant amount of
        #time to process. It is a very good idea to disable the helicopter timer if
        #you are doing ANY significant processing.

        self.clock = pygame.time.Clock()
        pygame.time.set_timer(self.FOREST_UPDATE_EVENT, self.sim_speed)
        pygame.time.set_timer(self.STORM_EVENT, self.sim_speed/2)

    def main(self):
        """Main simulator function. Call to get things started."""
        
        self.forest.update_forest(self.board)
        self.board_surface.blit(self.board.screen,(0,0))
        self.set_caption()
        self.print_instructions()
        self.log('main')

        self.pause = False

        while True:
            self.clock.tick(50)

            if self.handle_queue() == 'quit':
                return

    def print_instructions(self):
        for line in range(60):
            print
        print "****"
        print "Forestfire Simulator Started for", self.max_tick, "Ticks."
        print "****"
        print "Click on a plot to hit it with lightning."
        print
        print "(P)ause the simulation"
        print "(R)otate the board 90 degrees clockwise."
        print " Add a Firefighter (+)."
        print " Remove a Firefighter (-)."
        print "(Q)uit the simulator and return to the main menu."

    def handle_queue(self):
        """Call once each loop through the event cycle.
        Calls specific handler functions to actually do things."""

        events=pygame.event.get()

        if len(events)>50:
            #50 events is dangerously high for this and probably
            #indicates near impossible to clear backlog and something
            #leaving too many events in the queue. But if
            #we're lucky dumping the whole queue might let the
            #program recover.
            
            events=[]
            
        for event in events:

            if event.type == self.HELICOPTER_MOVE_EVENT and self.still_running():
                self.helicopter_move()

            elif event.type == self.FOREST_UPDATE_EVENT and self.still_running():
                self.forest_update()

            elif event.type == self.STORM_EVENT and self.still_running():
                self.storm()

            elif event.type == MOUSEBUTTONDOWN and self.still_running():
                self.hand_of_god()
            
            elif event.type == pygame.QUIT:
                pygame.display.quit()
                return 'quit'

            elif event.type == pygame.KEYDOWN:
                
                if event.key == pygame.K_q: # quit
                    pygame.display.quit()
                    return 'quit'

                elif event.key == pygame.K_r: #rotate the board 90 deg.
                    self.rotate()

                elif event.key == pygame.K_p: #pause the sim
                    self.pause = self.do_pause(self.pause)

                elif event.key == pygame.K_UNDERSCORE: #remove heli
                    self.decrease_helis()

                elif event.key == pygame.K_MINUS: #remove heli
                    self.decrease_helis()

                elif event.key == pygame.K_EQUALS: #add heli
                    self.increase_helis()

                elif event.key == pygame.K_PLUS: #add heli
                    self.increase_helis()

    def still_running(self):
        pause = self.pause
        if not pause and self.tick != self.max_tick :
            return True
        else:
            return False

    def do_pause(self, pause):
        if pause:
            self.set_caption()
##            pygame.time.set_timer(self.FOREST_UPDATE_EVENT, self.sim_speed)
##            pygame.time.set_timer(self.STORM_EVENT, self.sim_speed/2)
##            pygame.time.set_timer(self.STORM_EVENT, self.sim_speed/30)
        else:
            caption =pygame.display.get_caption()[0]
            caption = "*PAUSED* " + caption
            pygame.display.set_caption(caption)
##            pygame.time.set_timer(self.FOREST_UPDATE_EVENT,0)
##            pygame.time.set_timer(self.STORM_EVENT,0)
##            pygame.time.set_timer(self.STORM_EVENT,0)
        return not pause

    def set_caption(self):
        """Sets the caption based on a number of attributes."""

        caption = ""
        caption += "Tick: %s" % self.tick
        if self.storm_l > 1:
            caption += ", %s ticks of Storm remaining" % self.storm_l
        if self.forest.wind != "none":
            caption += ", Winds are coming from the %s" % self.forest.wind
        else:
            caption += ", there is no Wind"
        if self.nmbr_helis > 0:
            caption += ", there are %s Firefighters" % self.nmbr_helis
        caption += "."
        pygame.display.set_caption(caption)

    def helicopter_move(self):
        """Updates the visual position of every helicopter and dumps water
        if appropriate."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, 0)
        
        #For every helicopter, cover it over with background.
        for f in self.fire_helis:
            fire_heli = self.fire_helis[f]
            self.board.screen.blit(self.board_surface, fire_heli.rect, fire_heli.rect)
                
        #And now move, blit, do any water dumps etc.
        for f in self.fire_helis:
            fire_heli = self.fire_helis[f]
            at_targ = fire_heli.update_position(self.forest.forest)

            self.board.screen.blit(fire_heli.image, fire_heli.rect)
            
            if at_targ:
                self.forest.dump_water(fire_heli)
                fire_heli.change_dir(self.forest.forest)
                
                self.forest.update_forest(self.board)
                self.board_surface.blit(self.board.screen,(0,0))
                
        pygame.display.update()
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, self.sim_speed/30)

    def forest_update(self):
        """Updates the forest and displays the new forest."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, 0)

        self.forest.perform_next_tick()
        self.forest.update_forest(self.board)
        self.board_surface.blit(self.board.screen,(0, 0))
        self.tick += 1
        self.set_caption()
        self.log('forest_update')
        
        #Place a single helicopter event on the queue so that it
        #will be processed on the next pass, should help the
        #disappearing helicopter problem. 
        pygame.event.post(self.helievent)
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, self.sim_speed/30)

    def storm(self):
        """Causes a random lightning strike and displays the new forest."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,0)
        
        if self.storm_l > 0:
            #Do the lightning strike and retrieve it's position.
            r, c = self.forest.lightning_strike()
            self.strike(r, c)

            self.storm_l -= 1
            self.set_caption()
            
            pygame.time.set_timer(self.STORM_EVENT, self.sim_speed)
        #Shut down the storm event timer once storm_l is 0.
        else:
            pygame.time.set_timer(self.STORM_EVENT, 0)
            
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, self.sim_speed/30)

    def hand_of_god(self):
        """Causes a lightning strike to the screw under the mouse cursor."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, 0)
        
        x, y = pygame.mouse.get_pos()

        c = int(x/self.square_size)
        r = int(y/self.square_size)

        self.strike(r, c)

        if self.forest.forest[r][c][0] in ['T', 'W']:
            self.forest.forest[r][c] = ['B', 0]

        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, self.sim_speed/30)

    def strike(self, r, c):
        """Recolours a square of the board blue in response to any lightning."""
        #Colour over the square with blue.
        x = c * self.square_size
        y = r * self.square_size
        self.board.screen.blit(self.blue_square, (x, y))
        self.log('strike', (c, r))

    def rotate(self):
        """Rotates the board 90 degrees and updates the display."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,0)
        del self.board
        self.forest.rotate()
        rows=len(self.forest.forest)
        cols=len(self.forest.forest[0])
        self.board  = Board(rows, cols, square_color = GREEN, background_color = WHITE,
                            square_width = self.square_size, square_height = self.square_size)
        self.forest.update_forest(self.board)
        self.set_caption()
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,83)

    def increase_helis(self):
        """Adds another helicopter to the simulation."""
        self.forest.instance_extra_fire_heli(\
            self.fire_helis, self.nmbr_helis, self.square_size)
        self.nmbr_helis += 1
        print "\nAdded a Firefighter Helicopter. Now :", self.nmbr_helis
        self.set_caption()

    def decrease_helis(self):
        """Removes a helicopter from the simulation."""
        try:
            self.forest.remove_fire_heli(self.fire_helis, (self.nmbr_helis - 1))
            self.nmbr_helis -= 1
            print "\nRemoved a Firefighter Helicopter. Now :",self.nmbr_helis
            self.set_caption()
        except KeyError:
            print "\nNo Firefighters to remove."

    def log(self, l_type, param = None):
        """If logging is enabled logs an event in the
        log. Callers can pass in a parameter to include in the log."""
        
        if not self.forest.logging:
            return
    
        if l_type == "main":
            self.log_start()
        elif l_type == "forest_update":
            self.log_tick()
        elif l_type == "strike" and param != None:
            self.log_strike(param)
        elif l_type == "rotate":
            self.log_rotate()

    def log_start(self):
        """Add beginning of log info to the log."""
        #Get some info to put in the log.
        year, month, day, hour, minute = time.localtime()[:5]
        nmbr_t, nmbr_b, nmbr_e, nmbr_w = self.count_forest()
        
        log_file_name = self.forest.log_file
        log_file = open(log_file_name,'a')
        s1 =\
"""
######################################################
Forest Fire Simulator initialized for %s steps.
On %s/%s/%s at %s:%s.

""" % (self.max_tick, day, month, year, hour, minute)
        if self.forest.wind == 'none':
            s2 = "There is no wind.\n"
        else:
            s2 = "Winds are coming from the %s.\n\n" %self.forest.wind
        s3 =\
"""##########
Initial Forest
##########

There are %s Trees, %s plots of Empty land, %s Burning Trees and %s Water Logged Trees.

""" % (nmbr_t, nmbr_e, nmbr_b, nmbr_w)
        
        log_file.write(s1)
        log_file.write(s2)
        log_file.write(s3)
        log_file.close()
        self.forest.write_out_forest(self.forest.forest,log_file_name,True)
        



    def log_tick(self):
        """Add the forest as it stands now and some info to the log."""
        nmbr_t, nmbr_b, nmbr_e, nmbr_w = self.count_forest()
        log_file_name = self.forest.log_file
        s1=\
"""

##########
Tick: %s
##########

There are %s Trees, %s plots of Empty land, %s Burning Trees and %s Water Logged Trees.

""" % (self.tick, nmbr_t, nmbr_e, nmbr_b, nmbr_w)
        log_file = open(log_file_name,'a')
        log_file.write(s1)
        log_file.close()
        self.forest.write_out_forest(self.forest.forest,log_file_name,True)



    def log_strike(self,pos):
        """Add lighting strike info to the log."""
        log_file_name = self.forest.log_file
        log_file = open(log_file_name, 'a')
        coorid = str(pos)
        s1 = "\nLightning strike at %s.\n" % coorid
        log_file.write(s1)

    def count_forest(self):
        """Takes the forest and returns some statistics."""
        forest = self.forest.forest
        rows = len(forest)
        cols = len(forest[0])

        nmbr_t = 0
        nmbr_b = 0
        nmbr_e = 0
        nmbr_w = 0

        for row in range(rows):
            for col in range(cols):
                if forest[row][col][0] == "T":
                    nmbr_t += 1
                if forest[row][col][0] == "B":
                    nmbr_b += 1
                if forest[row][col][0] == "E":
                    nmbr_e += 1
                if forest[row][col][0] == "W":
                    nmbr_w += 1

        return nmbr_t, nmbr_b, nmbr_e, nmbr_w




class Advanced_Graphical_Simulation(Basic_Graphical_Simulation):
    #This shouldn't have to override so many methods.
    #But there's just enough differences to require overriding
    #pretty much everything. Rewriting Board (or Advanced Board)
    #to be more alike.
    
    def __init__(self, Advanced_Visual_Forest):
        self.forest = Advanced_Visual_Forest
        
        #Set Control Variables
        self.max_tick = self.forest.sim_dur
        self.storm_l = self.forest.storm_dur
        self.sim_speed = self.forest.sim_speed
        self.tick = 0

        #Set up the Board, and the display.
        
        rows=len(self.forest.forest)
        cols=len(self.forest.forest[0])
        self.square_size = self.forest.set_square_size(cols, rows)
        self.board = Forest_Board((cols, rows))

        self.screen = pygame.display.set_mode((cols * self.square_size, rows * self.square_size))


        #Set Up any Helicopters
        self.nmbr_helis = self.forest.nmbr_helis
        self.fire_helis = self.forest.instance_n_fire_helis(self.nmbr_helis, self.square_size)

        #Set up custom event IDs.
        self.FOREST_UPDATE_EVENT = 25
        self.STORM_EVENT = 26
        self.HELICOPTER_MOVE_EVENT = 27

        #Define custom helicopter event so it can be popped into the event queue.
        self.helievent = pygame.event.Event(self.HELICOPTER_MOVE_EVENT)

        #Ignore some events to reduce risk of queue flood.
        pygame.event.set_blocked((MOUSEMOTION,ACTIVEEVENT))

        #Set up the clock and the event timers, be careful, the helicopter occurs
        #often enough to flood the queue if something takes a significant amount of
        #time to process. It is a very good idea to disable the helicopter timer if
        #you are doing ANY significant processing.

        self.clock = pygame.time.Clock()
        pygame.time.set_timer(self.FOREST_UPDATE_EVENT, self.sim_speed)
        pygame.time.set_timer(self.STORM_EVENT, self.sim_speed/2)

    def main(self):
        """Main simulator function. Call to get things started."""
        self.forest.update_forest(self.board)
        self.screen.blit(self.board.bg,(0,0))
        self.set_caption()
        self.print_instructions()
        self.log('main')

        self.pause = False

        while True:
            self.clock.tick(50)
            pygame.display.update()

            if self.handle_queue() == 'quit':
                return

    def helicopter_move(self):
        """Updates the visual position of every helicopter and dumps water
        if appropriate."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT, 0)
        
        #For every helicopter, cover it over with background.
        for f in self.fire_helis:
            fire_heli=self.fire_helis[f]
            self.screen.blit(self.board.bg, fire_heli.rect, fire_heli.rect)
                
        #And now move, blit, do any water dumps etc.
        for f in self.fire_helis:
            fire_heli=self.fire_helis[f]
            at_targ = fire_heli.update_position(self.forest.forest)

            self.screen.blit(fire_heli.image,fire_heli.rect)
            
            if at_targ:
                self.forest.dump_water(fire_heli)
                fire_heli.change_dir(self.forest.forest)
                
                self.forest.update_forest(self.board)
    
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,self.sim_speed/30)

    def forest_update(self):
        """Updates the forest and displays the new forest."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,0)

        self.forest.perform_next_tick()
        self.forest.update_forest(self.board)
        self.tick+=1
        self.set_caption()
        self.log('forest_update')

        self.screen.blit(self.board.bg, (0,0))
        #Place a single helicopter event on the queue so that it
        #will be processed on the next pass, should help the
        #disappearing helicopter problem. 
        pygame.event.post(self.helievent)
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,self.sim_speed/30)

    def strike(self, r, c):
        """Blit a lightning bolt onto the appropriate plot."""
        
        square_size = self.square_size

        #Manually blit the plot to burning if it's not empty ground.
        #Because it won't be noticed/updated if it doesn't go through
        #the update forest procedure. Yea, that sucks. Really a light
        #bug in forest board.
        if self.forest.forest[r][c][0]!="E":
            x = c * square_size
            y = r * square_size
            self.screen.blit(self.board.burn, (x,y))
        
        x = (c)*self.square_size-1.0
        y = (r)*self.square_size-3.3
        self.log('strike',(c,r))

        #Blit the lightning bolt, which will get cleared on the next forest update.
        self.screen.blit(self.board.lightning,(x,y))

        pygame.display.update()

    def rotate(self):
        """Rotates the board 90 degrees and updates the display."""
        #Shutting down the heli timer to prevent queue flood.
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,0)
        del self.board
        
        self.forest.rotate()
        rows=len(self.forest.forest)
        cols=len(self.forest.forest[0])
        self.screen = pygame.display.set_mode((cols * self.square_size, rows * self.square_size))
        
        self.board = Forest_Board((cols,rows))
        self.forest.update_forest(self.board)
        self.screen.blit(self.board.bg, (0,0))
        print "\n Rotated the board 90 degrees."
        pygame.time.set_timer(self.HELICOPTER_MOVE_EVENT,83)

