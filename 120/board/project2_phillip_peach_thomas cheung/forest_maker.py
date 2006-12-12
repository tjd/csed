####################################################
# Forest Maker                                     #
#                                                  #
# Derived from starter.py with modifications       #
# to interface with Phillip and Thomas's Project 2 #
#                                                  #
# Original starter by Toby Donaldson.              #
#                                                  #
####################################################

from Board import *
import os

#my funcn
def set_square_size(cols,rows,screen_size =(1000,700)):
    square_size=20.0
    while cols*square_size > screen_size[0]or rows*square_size >screen_size[1]:
        square_size-=1
    if square_size < 1:
        return 1
    return int(square_size)

#modded
def color_to_letter(color):
    """ Converts a color to a letter.
    """
    if color == BROWN:
        return 'E'   # empty cell
    elif color == GREEN:
        return 'T'   # tree
    elif color == RED:
        return 'B'   # burning tree
    elif color == BLUE:
        return 'L'   # lightning strike
    elif color == PURPLE:
        return 'W'   # water splashed tree
    else:
        return '?'   # color with no code

#modded
def letter_to_color(letter):
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

def next_color(color):
    """ Cycles through the colors.
    """
    if color == BROWN:
        return GREEN
    elif color == GREEN:
        return RED
    elif color == RED:
        return BROWN

def save_to_file(board, fname = 'forest.txt'):
    """ Writes the current contents of the board to a file.
    """
    outfile = open(fname, 'w')
    for row in range(board.num_rows):
        for col in range(board.num_columns):
            c = board.get_color(row, col)
            outfile.write(color_to_letter(c))
        outfile.write('\n')
    print 'Current board written to "%s"' % fname

def board_to_forest(board):
    forest=[]
    for row in range(board.num_rows):
        row_lst=[]
        for col in range(board.num_columns):
            c = board.get_color(row, col)
            l = color_to_letter(c)
            row_lst.append([l,0])
        forest.append(row_lst)
    return forest

#modded
def read_from_file(fname = 'forest.txt'):
    infile = open(fname, 'r')
    forest = []
    for line in infile:
        forest.append(list(line.strip()))
    num_rows = len(forest)
    num_cols = len(forest[0])
    square_size=set_square_size(num_cols,num_rows)
    # note: the square width and height are always 20; it would be better
    #       if their size changed depending on the number of rows and columns
    board = Board(num_rows, num_cols,
                  square_color = BROWN, background_color = WHITE,
                  square_width = square_size, square_height = square_size,
                  caption = 'Forest')
    for row in range(board.num_rows):
        for col in range(board.num_columns):
            color = letter_to_color(forest[row][col])
            board.set_color(row, col, color)
            
    return board


#modded for error checking crashed on bad input before.
def get_int_with_default(prompt, default):
    s = raw_input(prompt).strip()
    if s == '':
        return default
    else:
        try:
            return int(s)
        except ValueError:
            return get_int_with_default(prompt, default)

def print_maker_instructions():
    print 
    print "****"
    print "Click on a square to change its color"
    print
    print "(S)ave the current forest to 'forest.txt'"
    print " S(A)ve a forest under chosen filename."
    print "(R)ead in a forest from 'forest.txt'"
    print " Read in a (F)orest from a chosen filename."
    print
    print " R(U)n the simulation on this forest."
    print "(Q)uit the Forest Maker and return to the main menu."


#modded
def make_forest():
    rows = get_int_with_default('How many rows (default 25)? ', 25)
    cols = get_int_with_default('How many columns (default 25)? ', 25)
    square_size=set_square_size(cols,rows)
    print_maker_instructions()
    board = Board(rows, cols, square_color = GREEN, background_color = WHITE,
                  square_width = square_size, square_height = square_size, caption = 'Forest Maker')

    while True:  # main event loop goes forever
        board.update()
        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.display.quit()
                return
            elif event.type == MOUSEBUTTONDOWN:
                row, col = board.get_mouse_square()
                color = board.get_color(row, col)
                board.set_color(row, col, next_color(color))
            elif event.type == KEYDOWN:
                if event.key == K_q:  # quit if the user presses 'q'
                    pygame.display.quit()
                    return
                
                elif event.key == K_s:  # save current configuration to file
                    save_to_file(board)

                elif event.key == K_a:   #ask for fname, save there.
                    fname = raw_input('Filename: ').strip()
                    #clear the queue when we typed.
                    pygame.event.get()
                    if fname in os.listdir(os.getcwd()):
                        owrite = raw_input('%s already exists. Overwrite: ' % fname)\
                                 .strip().lower()
                        #clear the queue when we typed.
                        pygame.event.get()
                        if owrite in ['y','yes']:
                            save_to_file(board,fname)
                    else:
                        save_to_file(board,fname)
                        
                    
                elif event.key == K_r:  #read in new forest
                    board = read_from_file()
                    
                elif event.key == K_f:   #ask for a fname and read that in
                    fname = raw_input('Filename: ').strip()
                    #clear the queue when we typed.
                    pygame.event.get()
                    try :
                        board = read_from_file(fname)
                    except IOError:
                        print "No such file!"

                elif event.key == K_u:
                    forest=board_to_forest(board)
                    return forest
