#!/usr/bin/env python

# Developed by Susan Villecroze, with help from Toby Donaldson.

import os, pygame
from pygame.locals import *

####################################################################################
#
# Global variables

RED = (250, 0, 0)
GREEN = (0, 250, 0)
BLUE = (0, 0, 250)
WHITE = (250, 250, 250)
BLACK = (0, 0, 0)
BROWN = (222, 184, 135)   # "burlywood";
                          # for more colors see:
                          #   http://web.njit.edu/~kevin/rgb.txt.html


####################################################################################
#
# Helper functions

def make_zero_matrix(m, n):
    return [n*[0] for row in xrange(m)]

def load_image(name, colorkey=None):
    fullname = os.path.join(os.getcwd(), name)
    try:
        image = pygame.image.load(fullname)
    except  pygame.error, message:
        print 'Cannot load image:', fullname
        raise SystemExit, message
    image = image.convert()
    if colorkey is not None:
        if colorkey is -1:
            colorkey = image.get_at((0,0))
        image.set_colorkey(colorkey, RLEACCEL)
    return image, image.get_rect()


####################################################################################
#
# Main classes

class Board(object):
    """Creates a board object with the given number of rows and columns,
    and each square with given dimensions and color.
    """
    def __init__(self, num_rows = 8, num_columns = 8,
                 square_width = 50, square_height = 50,
                 square_color = WHITE, background_color = GREEN,
                 caption = 'Board'):
        
        #Initialize Everything
        pygame.init()
        pygame.display.set_caption(caption)

        # remember number of rows and columns, etc.
        self.num_rows = num_rows
        self.num_columns = num_columns
        self.square_width = square_width
        self.square_height = square_height
        
        # create the screen
        self.screen_width = num_columns * square_width
        self.screen_height = num_rows * square_height
        self.screen = pygame.display.set_mode((self.screen_width, self.screen_height))

        # create the board surface
        self.boardSurface = pygame.Surface(self.screen.get_size())
        self.boardSurface = self.boardSurface.convert()  # good to do to speed up
        self.boardSurface.fill(background_color) # color it green

        # store squares in matrix
        x = 0
        y = 0
        self.matrix = make_zero_matrix(num_rows, num_columns)
        for row in xrange(num_rows):
            for col in xrange(num_columns):
                self.matrix[row][col] = pygame.Rect(x, y,
                                                    square_width - 2,
                                                    square_height - 2)
                x += square_width
            y += square_height
            x = 0

        # draw squares on board surface, default color is white
        for row in xrange(num_rows):
            for col in xrange(num_columns):
                pygame.draw.rect(self.boardSurface, square_color,
                                 self.matrix[row][col], 0)
                #self.boardSurface.fill((250,250,250),matrix[row][col])

        # create pieces
        self.pieces = make_zero_matrix(num_rows, num_columns)
        
    def update(self):
        """blit or paste everything to screen and update it"""
        self.screen.blit(self.boardSurface,(0, 0))
        for row in xrange(self.num_rows):
            for col in xrange(self.num_columns):
                if type(self.pieces[row][col]) != int: # there is a piece here
                    piece = self.pieces[row][col]
                    pt = self.matrix[row][col]
                    # center the image
                    left = pt.left + pt.width/2 - piece.image.get_width()/2
                    top = pt.top + pt.height/2 - piece.image.get_height()/2
                    self.screen.blit(piece.image,(left,top))

        pygame.display.flip()
        
    def set_color(self, x, y, c):
        """Sets the square at location (x, y) to be color c.
        The color of the square should change on-screen."""
        try:
            self.boardSurface.fill(c, self.matrix[x][y])
        except:
            print c, "is not a valid RGB color"
    
    def get_color(self, x, y):
        """Returns the RGB color of the square at matrix location (x, y).
        Alpha values are removed, for simplicity."""
        pt = self.matrix[x][y]
        return self.boardSurface.get_at((pt.centerx, pt.centery))[:3]

    def get_mouse_square(self):
        """ Returns the row, col of the current mouse click.
        Meant to be called after receiving a MOUSEBUTTONDOWN event type.
        """
        x,y = pygame.mouse.get_pos()
        row = y / self.square_height
        col = x / self.square_width
        return row, col

