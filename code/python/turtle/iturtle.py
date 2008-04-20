# iturtle.py

"""
An "improved" turtle that extends turtle.Turtle and adds various helpful
features.
"""

from turtle import Turtle

class iTurtle(Turtle):
    """
    >>> from iturtle import *
    >>> t = iTurtle()
    >>> t.forward(100)     # draw a square
    >>> t.right(90)
    >>> t.forward(100)
    >>> t.right(90)
    >>> t.forward(100)
    >>> t.right(90)
    >>> t.forward(100)
    >>> t.right(90)
    >>> t.total_turning)
    360
    >>> t.total_distance()
    400
    """
    def __init__(self):
        Turtle.__init__(self)
        self.reset_counters()

    def total_distance(self):
        """ Returns the total distance travelled by this turtle.

        This only takes forward(dist) and backward(dist) calls into
        account. Moving backwards is negative, so, for example, if the
        turtle goes forward(10) and then backward(10),
        total_distance() returns 0.
        """
        return self.famount + self.bamount

    def total_turning(self):
        """ Returns the total amount the turtle has turned, left and right.
        
        This only takes left(angle) and right(angle) calls into
        account. Turnign left is negative, so, for example, left(45)
        followed by right(45) makes total_turning() return 0.
        """
        return self.rturn + self.lturn

    def reset_turn_counter(self):
        """ Resets the total turning to 0.
        """
        self.lturn, self.rturn = 0, 0
        
    def reset_distance_counter(self):
        """ Resets the total distance to 0.
        """
        self.famount, self.bamount = 0, 0

    def reset_counters(self):
        """ Resets thet total turning and total distance to 0.
        """
        self.reset_turn_counter()
        self.reset_distance_counter()

    def forward(self, dist):
        Turtle.forward(self, dist)
        if dist < 0:
            self.bamount += dist
        else:
            self.famount += dist

    def backward(self, dist):
        self.forward(-dist)

    def left(self, deg):
        Turtle.left(self, deg)
        if deg > 0:
            self.lturn += deg
        else:
            self.rturn += deg

    def right(self, deg):
        self.left(-deg)

    def jump_to(self, *args):
        """ Go to position without leaving a trail.
        args is either an (x, y) tuple, or two parameters, x and y.
        >>> turtle.jump_to(10, 10)
        >>> turtle.jump_to((14, -14))
        """
        if len(args) == 1:
            x, y = args[0]
        else:
            x, y = args
        
        if self.is_pen_down():
            self.up()
            self.goto(x, y)
            self.down()
        else:
            self.goto(x, y)
        
    def bearing(self, p):
        """ Returns the angle the turtle must turn LEFT to face p.
        """
        return self.towards(p) - self.heading()

    def face(self, p):
        """ Makes the turtle face towards point p.
        """
        self.setheading(self.towards(p))
        
    def get_color(self):
        """ Returns the current pen color.
        """
	return self._color

    def get_delay(self):
        """ Returns the current delay.
        """
	return self._delay

    def get_state(self):
        """ Returns the current state; used with set_state.
        """
	return (self.position(), self.heading(), self.get_color(),
		self.is_pen_down(), self.has_tracer(), self.get_delay())

    def set_state(self, state):
        """ Sets the state as given; used with get_state.
        """
	self.goto(state[0])
	self.setheading(state[1])
	self.color(state[2])
	if state[3]:
	    self.down()
	else:
	    self.up()
	self.tracer(state[4])
        self.delay(state[5])

    def is_pen_up(self):
        """ Retruns true if the pen is up; false otherwise.
        """
	return self._drawing == 0

    def is_pen_down(self):
        """ Retruns true if the pen is down; false otherwise.
        """
	return not self.is_pen_up()

    def has_tracer(self):
        """ Retruns true if the tracer is on; false otherwise.
        """
	return self._tracing

    def circle(self, radius, extent = None):
        """ Draw a circle with given radius.
        The center is the turtle's position; extent
        determines which part of the circle is drawn. If not given,
        the entire circle is drawn.

        If extent is not a full circle, one endpoint of the arc is the
        current pen position. The arc is drawn in a counter clockwise
        direction if radius is positive, otherwise in a clockwise
        direction. In the process, the direction of the turtle is
        changed by the amount of the extent.

        >>> turtle.circle(50)
        >>> turtle.circle(120, 180)  # half a circle
        """
        state = self.get_state()
        self.up()
        self.tracer(False)
        
        self.setheading(0)         # circles are drawn radius units to the
        x, y = self.position()     # left of the turtle center, so to center
        self.goto(x, y - radius)   # the circle over the turtle we subtract
                                   # radius from y
        Turtle.circle(self, radius, extent)
        self.set_state(state)        

    def dot_at(self, p, color = 'red', radius = 4):
        """ Draws a filled circle at point p.
        Returns the turtle to its original state after drawing the dot.
        """
        state = self.get_state() # remember turtle state

        self.up()                
        self.tracer(False)
        
        self.goto(self.position()) 
        
        self.color(color)
        self.begin_fill()
        self.circle(radius)
        self.end_fill()
        
        self.set_state(state)    # restore original turtle state

    def dot_here(self, color = 'red', radius = 4):
        """ Put a dot at the turtle's current position.
        """
        self.dot_at(self.position(), color, radius)

    def stripe_forward(self, dist, c1 = 'blue', c2 = 'red', width = 5):
        step = dist / width
        leftover = dist % width
        use_c1 = True
        for i in xrange(step):
            self.color(c1 if use_c1 else c2)
            self.forward(width)
            use_c1 = not use_c1
        self.color(c1 if use_c1 else c2)
        self.forward(leftover)

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    _test()
