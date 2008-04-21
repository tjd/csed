##############################################
# CMPT 120 Project 2: Forest Fire Simulator. #
#                                            #
#By: Thomas Cheung and Phillip Peach         #
#                                            #
#Using a very slightly modified Board.py     #
#and a more heavily modified starter.py now  #
#forest_maker.py.                            #
##############################################



import pygame
import os
from Forest_Simulators import *
from forest_maker import *

QUIT=['q','quit','exit','done']

def main():
    #Need to do this because pygame delays are used throughout.
    pygame.init()
    try:
        config = read_in_config()
    except IOError:
        config = {}

    print 
    print "Welcome to the Forest Fire Simulator."

    #Loop until explicitly quit.
    answer = -1
    while answer not in QUIT:
        print_instructions()
        print
        answer = raw_input("What would you like to do? ").strip().lower()
        while answer not in ['r','m','c','h','e'] and answer not in QUIT:
            print
            answer = raw_input("What would you like to do? ").strip().lower()
        
        if answer in 'c':
            try:
                config=change_config(config)
            #Caused by a down stream function loading from an incorrectly config
            #formatted and getting an unexpected value for something.
            except ValueError:
                print 
                print "Error: There is a problem with your configuration If you not\
have not changed any settings this session your configuration file is probably damaged."
                print "If you cannot repair the problem yourself delete\
the file and restart the program."
                pygame.time.wait(10000)
                
        elif answer in 'm':
            run_forest=make_forest()
            if run_forest != None:
                run_sim(config,run_forest)
                
        elif answer in 'r':
            run_sim(config)
            
        elif answer in 'e':
            print_credits()

def change_config(config):
    """Changes the configuration restricting changes to allowed values."""
    #Works by calling child functions.
    default={'simulation_speed': '2500',
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

    saved=True
    while True:
        print
        print "****"
        print "1. Change Basic Simulation Settings."
        print "2. Change Advanced Simulation Settings."
        print "3. Change Other Settings."
        print "4. Save Configuration."
        print "5. Back to Main Menu."
        print

        try:
            print
            choice = int(raw_input("Which type of option would you like to modify? "))
        except ValueError:
            choice = -1
            
        while not (0 < choice <=5):
            try:
                print
                choice = int(raw_input("Which type of option would you like to modify? "))
            except ValueError:
                choice = -1

        if choice == 1:
            saved = False
            config = change_basic(config,default)

        elif choice == 2:
            saved = False
            config = change_advanced(config,default)

        elif choice == 3:
            saved = False
            config = change_other(config,default)

        elif choice == 4:
            saved = True
            save_config(config)

        elif choice == 5 and saved:
            return config

        elif choice == 5 and not saved:
            print
            print "You have not saved your most recent configuration changes."
            print "If you don't save these settings will only be used until the end of this session."
            print
            save = raw_input("Would you like to save now? ").strip().lower()
            while save not in ['yes','y','no','n']:
                save = raw_input("Would you like to save now? ")
            if save in ['yes','y']:
                saved = True
                save_config(config)
            return config


def change_basic(config,default):
    """Change all basic configuration settings."""
    #Set all the local variables.
    sim_speed, sim_dur, storm_dur, heli_nmbr = basic_config_variables(config,default)

    while True:
        print
        print
        print "1. Simulation Speed (%s)." % int(sim_speed)
        print "2. Simulation Duration (%s)." % int(sim_dur)
        print "3. Storm Duration (%s)." % int(storm_dur)
        print "4. Number of Helicopters (%s)." % int(heli_nmbr)
        print "5. Return to main configuration menu."

        try:
            print
            choice = int(raw_input("Change: "))
        except ValueError:
            choice = -1
            
        while not (0 < choice <=5):
            try:
                print 
                choice = int(raw_input("Change: "))
            except ValueError:
                choice = -1

        if choice == 1:
            sim_speed = raw_input("Simulation Speed (%s): " % int(sim_speed))
            if not string_is_int(sim_speed):
                print "That value is not valid value for this setting."
                pygame.time.wait(1000)
                sim_speed = basic_config_variables(config,default)[0]
            config['simulation_speed'] = sim_speed

        if choice == 2:
            sim_dur = raw_input("Simulation Duration (%s): " % int(sim_dur))
            if not string_is_int(sim_dur):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                sim_dur = basic_config_variables(config,default)[1]
            config['simulation_duration'] = sim_dur

        if choice == 3:
            storm_dur = raw_input("Storm Duration (%s): " % int(storm_dur))
            if not string_is_int(storm_dur):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                storm_dur = basic_config_variables(config,default)[2]
            config['storm_duration'] = storm_dur

        if choice == 4:
            heli_nmbr = raw_input("Number of Helicopters (%s): " % int(heli_nmbr))
            if not string_is_int(heli_nmbr):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                heli_nmbr = basic_config_variables(config,default)[3]
            config['number_of_firefighter_helicopters'] = heli_nmbr

        if choice == 5:
            return config
        
def basic_config_variables(config,default):
    """Returns all the basic configuration variables, substituting
    defualts if the values are not present."""
    #Must find the better to do this soon.
    
    try:
        sim_speed = config['simulation_speed']
    except KeyError:
        sim_speed = default['simulation_speed']

    try:
        sim_dur = config['simulation_duration']
    except KeyError:
        sim_dur = default['simulation_duration']

    try:
        storm_dur = config['storm_duration']
    except KeyError:
        storm_dur = default['storm_duration']

    try:
        heli_nmbr = config['number_of_firefighter_helicopters']
    except KeyError:
        heli_nmbr = default['number_of_firefighter_helicopters']

    return sim_speed, sim_dur, storm_dur, heli_nmbr

def change_advanced(config, default):
    """Change all advanced configuration settings."""
    
    chance_burn, wind_chance, lee_chance, burn_dur, water_dur, water_surp = adv_config_variables(config, default)
    
    while True:
        print
        print
        print "1. Normal Chance to Burn (%s)." % float(chance_burn)
        print "2. Chance to Burn from the Windward (%s)." % float(wind_chance)
        print "3. Chance to Burn from the Leeward (%s)." % float(lee_chance)
        print "4. Burning Tree Duration (%s)." % int(burn_dur)
        print "5. Damp Tree Duration (%s)." % int(water_dur)
        print "6. Burn Chance Reduction from Water (%s)." % float(water_surp)
        print "7. Return to main configuration menu."

        try:
            print
            choice = int(raw_input("Change: "))
        except ValueError:
            choice = -1
            
        while not (0 < choice <=7):
            try:
                print 
                choice = int(raw_input("Change: "))
            except ValueError:
                choice = -1
                
        if choice == 1:
            chance_burn = raw_input("Normal Chance to Burn (%s): " % float(chance_burn))
            if not string_is_float(chance_burn):
                print "That value is not valid value for this setting."
                pygame.time.wait(1000)
                chance_burn = adv_config_variables(config,default)[0]
            config['normal_burning_chance'] = chance_burn

        if choice == 2:
            wind_chance = raw_input("Chance to Burn from the Windward (%s): " % float(wind_chance))
            if not string_is_float(wind_chance):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                wind_chance = adv_config_variables(config,default)[1]
            config['wind_burning_chance'] = wind_chance

        if choice == 3:
            lee_chance = raw_input("Chance to Burn from the Leeward (%s): " % float(lee_chance))
            if not string_is_float(lee_chance):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                lee_chance = adv_config_variables(config,default)[2]
            config['lee_burning_chance'] = lee_chance

        if choice == 4:
            burn_dur = raw_input("Burning Tree Duration (%s): " % int(burn_dur))
            if not string_is_int(burn_dur):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                burn_dur = adv_config_variables(config,default)[3]
            config['burn_duration'] = burn_dur

        if choice == 5:
            water_dur = raw_input("Damp Tree Duration (%s): " % int(water_dur))
            if not string_is_int(water_dur):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                water_dur = adv_config_variables(config,default)[4]
            config['water_duration'] = water_dur

        if choice == 6:
            water_surp = raw_input("Burn Chance Reduction from Water (%s): " % float(water_surp))
            if not string_is_float(water_surp):
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                water_surp = adv_config_variables(config,default)[5]
            config['water_surpression'] = water_surp

        if choice == 7:
            return config

def adv_config_variables(config,default):
    """Returns all the advanced configuration variables, substituting
    defualts if the values are not present."""
    #And it's just as ugly over here.
    try:
        chance_burn = config['normal_burning_chance']
    except KeyError:
        chance_burn = default['normal_burning_chance']

    try:
        wind_chance = config['wind_burning_chance']
    except KeyError:
        wind_chance = default['wind_burning_chance']

    try:
        lee_chance = config['lee_burning_chance']
    except KeyError:
        lee_chance = default['lee_burning_chance']

    try:
        burn_dur = config['burn_duration']
    except KeyError:
        burn_dur = default['burn_duration']

    try:
        water_dur = config['water_duration']
    except KeyError:
        water_dur = default['water_duration']

    try:
        water_surp = config['water_surpression']
    except KeyError:
        water_surp = default['water_surpression']

    return chance_burn, wind_chance, lee_chance, burn_dur, water_dur, water_surp

def change_other(config,default):
    """Change all the other configuration settings."""
    
    init_file, logging, log_file, dump_file = other_config_variables(config,default)

    while True:
        print
        print
        print "1. Initilization File (%s)." % init_file
        print "2. Logging Enabled (%s)." % logging
        print "3. Log File (%s)." % log_file
        print "4. End of Simulation Dump File (%s)." % dump_file
        print "5. Return to main configuration menu."

        try:
            print
            choice = int(raw_input("Change: "))
        except ValueError:
            choice = -1
            
        while not (0 < choice <=5):
            try:
                print 
                choice = int(raw_input("Change: "))
            except ValueError:
                choice = -1

        if choice == 1:
            init_file = (raw_input("Initilization File (%s): " % init_file)).strip()
            if init_file not in os.listdir(os.getcwd()):
                print "%s does not exist. Can't use it as an initilization file." % init_file
                pygame.time.wait(1000)
                init_file = other_config_variables(config,default)[0]
            config['initilization_file'] = init_file

        if choice == 2:
            logging = (raw_input("Logging Enabled (%s): " % logging)).strip()
            if logging not in ['True', 'False', 'T', 'F', '1', '0']: 
                print "That value is not valid for this setting."
                pygame.time.wait(1000)
                logging = other_config_variables(config,default)[1]
            config['logging_enabled'] = logging

        if choice == 3:
            log_file = (raw_input("Log File (%s): " % log_file)).strip()
            if log_file in os.listdir(os.getcwd()):
                cont = (raw_input("%s already exists, continue (y/n):" % log_file)).strip().lower()
                if cont in ['y','yes']:
                    pass
                else:
                    log_file = other_config_variables(config,default)[2]
            config['log_file'] = log_file

        if choice == 4:
            dump_file = raw_input("End of Simulation Dump File (%s): " % dump_file)
            if dump_file in os.listdir(os.getcwd()):
                cont = (raw_input("%s already exists, continue (y/n):" % dump_file)).strip().lower()
                if cont in ['y','yes']:
                    pass
                else:
                    dump_file = other_config_variables(config,default)[3]
            config['end_of_simulation_dump_file'] = dump_file

        if choice == 5:
            return config
        
def other_config_variables(config,default):
    """Returns all the other configuration variables, substituting
    defualts if the values are not present."""
    #And it remains ugly as sin here as well.
    
    try:
        init_file = config['initilization_file']
    except KeyError:
        init_file = default['initilization_file']

    try:
        logging = config['logging_enabled']
    except KeyError:
        logging = default['logging_enabled']

    try:
        log_file = config['log_file']
    except KeyError:
        log_file = default['log_file']

    try:
        dump_file = config['end_of_simulation_dump_file']
    except KeyError:
        dump_file = default['end_of_simulation_dump_file']

    return init_file, logging, log_file, dump_file


def save_config(config, fname = 'forest.conf'):
    """Saves the config ot the specified file. If it can find the setting
    already present it simply modifies it in the same spot, otherwise settings
    are appended to the and."""

    
    #Create a copy of the file, one list element to a line.
    contents = []
    #If no existing file we'll end up making a new one.
    try:
        infile = open(fname, 'r')
        for line in infile:
            contents.append(line)
        infile.close()
    except IOError:
        pass

    #Make a list of all the keys in config.
    keys = []
    for key in config:
        keys.append(key)

    #Go through the internal copy of the file, if a setting matches
    #something in our keys change the value.
    for line_nmbr in range(len(contents)):
        line = contents[line_nmbr]
        #Ignore comment or blank lines.
        if contents[line_nmbr][0] in ['#','\n','\r']:
            pass
        else:
            #Convert key vaule in the file for comparison.
            key = (line.split(':')[0]).strip().lower().replace(' ','_')
            if key in keys:
                line_key = key.replace('_',' ').title()
                line_value = config[key]
                line = line_key + " : " +  line_value
                contents[line_nmbr] = line
                keys.remove(key)

    #Add any keys that didn't get added before to the bottom of the file.
    for key in keys:
        line_key = key.replace('_',' ').title()
        line_value = config[key]
        line = "\n\n" + line_key + " : " + line_value
        contents.append(line)

    #And now write out the internal version into the real file.      
    outfile = open(fname, 'w')
    for line in contents:
        outfile.write(line)
    outfile.close()
    print "\nConfiguration saved to %s." % fname
    pygame.time.wait(1000)
    

def read_in_config(fname='forest.conf'):
    """Reads in a configuration file and converts it into a dictionary."""
    config={}
    infile = open(fname,'r')
    for line in infile:
        #Ignore comment or blank lines.
        if line[0] not in ['#','\n','\r']:
            setting = line.split(':')
            key = setting[0].strip().lower().replace(' ','_')
            value = setting[1].strip()
            config[key] = value
    return config

def string_is_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def string_is_float(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def run_sim(config,init_forest=None):
    """Sets up the approprite simulator and runs. Can take an optional
    forest and use that as the initial instead of the default forest init file."""
    
    pygame.init()
    forest = Forest(config)
    if init_forest != None:
        forest.forest = init_forest
    rows = len(forest.forest)
    cols = len(forest.forest[0])
    del forest
    
    #This line is assuming that the user is running their heighest possible resolution.
    #If a mantainer knows a way in pygame, SDL or python in general to know what resolution
    #is being CURRENTLY USED then that should be used instead.
    screen_size = pygame.display.list_modes()[0]
    max_screen_size = (screen_size[0]-50,screen_size[1]-50)
    square_size = set_square_size(rows,cols,screen_size)
    
    #If the square size would have to be smaller than 20 pixels basic sim is run because
    #image scaling has not been implemented in advanced sim.
    if square_size < 20 :
        run_basic_sim(config,init_forest,max_screen_size)
    else:
        run_adv_sim(config,init_forest,max_screen_size)

def run_basic_sim(config, init_forest, screen_size):
    forest = Basic_Visual_Forest(config)
    if init_forest != None:
        forest.forest = init_forest
    sim = Basic_Graphical_Simulation(forest, screen_size)
    sim.main()

def run_adv_sim(config, init_forest, screen_size):
    forest = Advanced_Visual_Forest(config)
    if init_forest != None:
        forest.forest = init_forest
    sim = Advanced_Graphical_Simulation(forest)
    sim.main()
    

def print_credits():
    for line in range(60):
        print
    print "****"
    print "The second CMPT 120 Project."
    print "by:\tThomas Cheung: 'I am ZEUS! <evil laugh>'"
    print "and\tPhillip Peach: 'Yes, I really am that geeky.'"
    print
    print "Thanks to Susan Villecroze for the Board class and Toby Donaldson for the starter application."
    print
    print "Thanks for playing :)."
    pygame.time.wait(3000)

def print_instructions():
    print
    print
    print
    print "****"
    print "Options:"
    print 
    print "(R)un simulation."
    print "(M)ake a new forest."
    print "(C)hange configuration."
    print " Cr(e)dits."
    print "(Q)uit."

# automatically calls main() when loaded
if __name__ == '__main__':
    try :
        main()
    #Close the display whatever happens.
    #May want to disable for debug but leave this in for any distribution
    #to users.
    finally:
        pygame.display.quit()
