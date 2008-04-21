from Board import *

def color_to_letter(color):
    """ Converts a color to a letter.
    """
    if color == BROWN:
        return 'E'   # empty cell
    elif color == GREEN:
        return 'T'   # tree
    elif color == RED:
        return 'F'   # burning tree
    else:
        return '?'   # color with no code

def letter_to_color(letter):
    """ Converts a letter to a color.
    """
    if letter == 'E':
        return BROWN
    elif letter == 'T':
        return GREEN
    elif letter == 'F':
        return RED
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
        
def read_from_file(fname = 'forest.txt'):
    infile = open(fname, 'r')
    forest = []
    for line in infile:
        forest.append(list(line.strip()))
    
    num_rows = len(forest)
    num_cols = len(forest[0])
    # note: the square width and height are always 25; it would be better
    #       if their size changed depending on the number of rows and columns
    board = Board(num_rows, num_cols,
                  square_color = BROWN, background_color = WHITE,
                  square_width = 25, square_height = 25,
                  caption = 'Forest')
    for row in range(board.num_rows):
        for col in range(board.num_columns):
            color = letter_to_color(forest[row][col])
            board.set_color(row, col, color)
            
    return board

def get_int_with_default(prompt, default):
    s = raw_input(prompt).strip()
    if s == '':
        return default
    else:
        return int(s)

def print_instructions():
    print
    print "   click a square to change its color"
    print
    print "   s - save the current board to the file 'forest.txt'"
    print "   r - save the current board from the file 'forest.txt'"
    print "   q - quit the program"

def main():
    rows = get_int_with_default('How many rows (default 25)? ', 25)
    cols = get_int_with_default('How many columns (default 25)? ', 25)
    
    print_instructions()
    board = Board(rows, cols, square_color = BROWN, background_color = WHITE,
                  square_width = 20, square_height = 20, caption = 'Forest')

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
                elif event.key == K_r:
                    board = read_from_file()
                    
# automatically calls main() when loaded
if __name__ == '__main__': main()
