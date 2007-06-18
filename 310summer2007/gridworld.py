# gridworld.py

"""
A simple gridworld display. Lets you specify a maze, and the location
of a single robot and goal. Then you can send the robot commands to
move up, down, left, or right, and the on-screen robot will move as
commanded. The robot is a turtle graphics turtle, so moves smoothly
from square to square, and leaves a trail.

See the test1 function at the bottom for example usage.
"""

import turtle

class Gridworld(object):
    def __init__(self, grid):
        """
        grid - a list of lists (or a list of strings), using 'w' to
               represent a wall, 'r' the robot, 'g' the goal, and
               anything else a blank
        """
        self.grid = grid
        # number of horizontal and vertical squares
        self.num_col = len(grid[0])
        self.num_row = len(grid)
        # length and width of each square
        self.square_width = turtle.window_width() / self.num_col
        self.square_height = turtle.window_height() / self.num_row
        # find the robot and goal
        for r, row in enumerate(self.grid):
            for c, square in enumerate(row):
                ch = square.lower()
                if ch == 'r':
                    self.robot_row, self.robot_col = r, c
                elif ch == 'g':
                    self.goal_row, self.goal_col = r, c
        self.wall_color = 'black'
        self.blank_color = 'white'
        self.path_color = 'red'
        self.goal_color = 'gold'
        self.move_speed = 'fast'

    def info(self):
        print self.grid
        print '(width, height) = (%s, %s)' % (self.num_col, self.num_row)
        print '(square_width, square_height) = (%s, %s)' % (self.square_width,
                                                            self.square_height)
        print 'robot at (%s, %s)' % (self.robot_row, self.robot_col)
        print 'goal at (%s, %s)' % (self.goal_row, self.goal_col)
        print 'wall color is %s' % self.wall_color
        print 'blank color is %s' % self.blank_color
        print 'path color is %s' % self.path_color
        print 'goal color is %s' % self.goal_color
        print 'move speed is "%s"' % self.move_speed

    def draw_robot(self):
        turtle.up()
        x = self.robot_col * self.square_width - turtle.window_width() / 2 + self.square_width / 2
        y = -self.robot_row * self.square_height + turtle.window_height() / 2 - self.square_height / 2
        turtle.tracer(True)
        turtle.goto(x, y)
        turtle.down()
    
    def draw(self):
        turtle.tracer(False)
        turtle.speed('fastest')
        turtle.up()
        turtle.goto(0, 0)
        for r, row in enumerate(self.grid):
            for c, square in enumerate(row):
                x = c * self.square_width - turtle.window_width() / 2
                y = -r * self.square_height + turtle.window_height() / 2
                ch = square.lower()
                if ch == 'w':
                    turtle.color(self.wall_color)
                    rectangle_at(x, y, self.square_width, self.square_height, True)
                elif ch == 'g':
                    turtle.color(self.goal_color)
                    triangle_at(x, y, min(self.square_width, self.square_height) / 2, True)
                else:
                    turtle.color(self.blank_color)
                    rectangle_at(x, y, self.square_width, self.square_height, True)
        self.draw_robot()
                
    def go_right(self):
        if self.robot_col < self.num_col - 1:
            turtle.speed(self.move_speed)
            target = self.grid[self.robot_row][self.robot_col + 1]
            if target != 'w':
                self.robot_col += 1
                turtle.setheading(0)  # face east
                turtle.down()
                turtle.color(self.path_color)
                turtle.forward(self.square_width)

    def go_left(self):
        if self.robot_col > 0:
            turtle.speed(self.move_speed)
            target = self.grid[self.robot_row][self.robot_col - 1]
            if target != 'w':
                self.robot_col -= 1
                turtle.setheading(180)  # face west
                turtle.down()
                turtle.color(self.path_color)
                turtle.forward(self.square_width)

    def go_up(self):
        if self.robot_row > 0:
            turtle.speed(self.move_speed)
            target = self.grid[self.robot_row - 1][self.robot_col]
            if target != 'w':
                self.robot_row -= 1
                turtle.setheading(90)  # face north
                turtle.down()
                turtle.color(self.path_color)
                turtle.forward(self.square_height)

    def go_down(self):
        if self.robot_row < self.num_row - 1:
            turtle.speed(self.move_speed)
            target = self.grid[self.robot_row + 1][self.robot_col]
            if target != 'w':
                self.robot_row += 1
                turtle.setheading(-90)  # face south
                turtle.down()
                turtle.color(self.path_color)
                turtle.forward(self.square_height)

    def make_moves(self, s):
        s = s.lower()
        for move in s:
            if move == 'u': self.go_up()
            elif move == 'd': self.go_down()
            elif move == 'r': self.go_right()
            elif move == 'l': self.go_left()


def rectangle_at(x, y, width, height, filled = True):
    """ Put a width-by-height rectangle with upper-left corner at (x, y).
    """
    turtle.up()
    turtle.goto(x, y)
    turtle.down()
    turtle.setheading(0)  # face east
    rectangle(width, height, filled)

def rectangle(width, height, filled = False):
    """ Draw a width-by-height rectangle.
    """
    if filled:
        turtle.begin_fill()
    for i in xrange(2):
        turtle.forward(width)
        turtle.right(90)
        turtle.forward(height)
        turtle.right(90)
    if filled:
        turtle.end_fill()

def triangle_at(x, y, side, filled = True):
    turtle.up()
    turtle.goto(x, y)
    turtle.down()
    turtle.setheading(0)  # face east
    triangle(side, filled)

def triangle(side, filled = False):
    if filled:
        turtle.begin_fill()
    for i in xrange(3):
        turtle.forward(side)
        turtle.right(120)
    if filled:
        turtle.end_fill()

############################################################################
#
# Sample code.
#
############################################################################

grid5 = [['w', 'w' , 'w', 'w', 'w'],
         ['w', 'r' , '.', '.', 'w'],
         ['w', '.' , 'w', '.', 'w'],
         ['w', '.' , '.', 'g', 'w'],
         ['w', 'w' , 'w', 'w', 'w']]

grid10 = ['wwwwwwwwww',
          'wr..wwwwww',
          'www.w...ww',
          'www.w.w.ww',
          'www...w..w',
          'wwww.www.w',
          'w........w',
          'w..w..ww.w',
          'wwww...g.w',
          'wwwwwwwwww']

def test1():
    gw = Gridworld(grid10)
    gw.draw()
    raw_input('return to continue ... ')
    gw.make_moves('RRDDDRDDDDRRR')

if __name__ == '__main__':
    test1()
    raw_input('return to end ... ')
