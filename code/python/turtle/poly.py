# poly.py

# To use a regular turtle, uncomment the line below, then comment out the
# following two lines; vice-versa if you want to use an iTurtle (which
# you normally will want to)

#import turtle
from iturtle import iTurtle # iturtle tracks total turning, and total
turtle = iTurtle()          # turning and total distance

def poly(side, angle):
    """ Draws a polygon with given side length and exterior angle (in degrees).
    
    For example:
       poly(100, 45) draws an octagon
       poly(100, 60) draws a hexagon
       poly(100, 72) draws a pentagon
       poly(100, 90) draws a square

    You can simulate circles with commands like these:
       poly(1, 1) a basic circle
       poly(1, 2) a smaller circle

    Other interesting figures can be drawn with angles over 90, e.g.
       poly(100, 108) is a ten-pointed star
       poly(100, 144) is the classic five-pointed star
       poly(100, 145) is a many-pointed star

    Non-integer parameters can cause poly to run forever, or at least
    for an extremely long time, e.g.

       poly(100, 100.1) runs for a long time

    """
    turtle.forward(side)
    turtle.right(angle)
    turn = angle
    while turn % 360 != 0:
        turtle.forward(side)
        turtle.right(angle)
        turn += angle

def dot_poly(side, angle):
    turtle.forward(side)
    turtle.dot_here()
    turtle.right(angle)
    turn = angle
    while turn % 360 != 0:
        turtle.forward(side)
        turtle.dot_here()
        turtle.right(angle)
        turn += angle

def color_poly1(side, angle):
    turtle.color('red')
    turtle.forward(side)
    turtle.right(angle)
    turn = angle
    count = 0
    while turn % 360 != 0:
        if count % 2 == 0:
            turtle.color('blue')
        else:
            turtle.color('red')
        count += 1
        turtle.forward(side)
        turtle.right(angle)
        turn += angle

def color_poly2(side, angle):
    color_forward(side, c2 = 'yellow', width = 10)
    turtle.right(angle)
    turn = angle
    while turn % 360 != 0:
        color_forward(side, c2 = 'yellow', width = turn % 360)
        turtle.right(angle)
        turn += angle

def gen_poly(side, angle):
    yield side, angle
    turn = angle
    while turn % 360 != 0:
        yield side, angle
        turn += angle

def poly1(side, angle):
    for s, a in gen_poly(side, angle):
        turtle.forward(s)
        turtle.right(a)

def vector(angle, length):
    turtle.setheading(angle)
    turtle.forward(length)

def duopoly(side1, angle1, side2, angle2):
    """ Interleaves two "poly" calls with the given parameters.

    Many interesting diagrams can be made, e.g.

      duopoly(50, 90, 50, 320)
      duopoly(40, 10, 40, -15)
      duopoly(1, 1, 50, 90)
      duopoly(50, 45, 20, 9)
    """
    n1 = lcm(angle1, 360) / angle1
    n2 = lcm(angle2, 360) / angle2
    for c in xrange(lcm(n1, n2)):
        vector(c * angle1, side1)
        vector(c * angle2, side2)

def multipoly(side_angle_list):
    """
    Interleaves multiple poly calls.
    side_angle_list is a list of (side, angle) pairs
    
    Some examples:
         multipoly([(1, 2), (2, 3), (3, 4), (4, 5)])
         multipoly([(1, 2), (2, -3), (3, 4), (4, 5)])
         multipoly([(20, 45), (20, 90), (20, -45)])
         multipoly([(20, 12), (20, -12)])

         # eight small circles
         multipoly([(55, 90 + 12), (55, 45), (50, -45), (50, -90)])         
    """
    n = lcm_multi(lcm(angle, 360) / angle for side, angle in side_angle_list)
    for c in xrange(n):
        for side, angle in side_angle_list:
            vector(c * angle, side)        

def rand_multipoly(num_poly = 5, tracer = False):
    """
    >>> rand_multipoly()
    [(24, -320), (11, 16), (12, -228), (3, -137)]
    >>> rand_multipoly()
    [(11, -10), (16, 304)]
    >>> rand_multipoly()
    [(7, -15), (29, 353)]
    >>> rand_multipoly()
    [(11, -225), (13, 40), (17, -344)]
    """
    import random
    lst = [(random.randint(1, 30), random.randint(-360, 360))
           for i in xrange(random.randint(1, num_poly))]
    print lst
    turtle.reset()
    turtle.tracer(tracer)
    multipoly(lst)
    turtle.tracer(True)

def gcd(a, b):                                       
    """
    Returns the greatest common divisor of a and b.
    Input:
        a -- an integer
        b -- an integer
    Output:
        an integer, the gcd of a and b
    Examples:
    >>> gcd(97,100)
    1
    >>> gcd(97 * 10**15, 19**20 * 97**2)
    97L
    """
    a, b = abs(a), abs(b)
    if a == 0: return b
    if b == 0: return a
    while b != 0: 
        a, b = b, a % b
    return a

def lcm(a, b):
    """ Returns the lowest common multiple of a and b.
    """
    return (a * b) / gcd(a, b)

def lcm_multi(nums):
    """ Returns the lowest common multiple of the numbers on list nums.
    Relies on the fact that lcm(a, b, c) = lcm(lcm(a,b), c), and
    lcm(a, 1) = a.
    """
    return reduce(lcm, nums, 1)
    
def arc_right(size, angle):
    for step in xrange(angle):
        turtle.forward(size)
        turtle.right(1)

def arc_left(size, angle):
    for step in xrange(angle):
        turtle.forward(size)
        turtle.left(1)

def ray(size):
    for step in xrange(2):
        arc_left(size, 90)
        arc_right(size, 90)

def sun(size):
    """ Try running sun(1) to see a diagram of the sun.
    """
    for step in xrange(9):
        ray(size)
        turtle.right(160)

def color_forward(dist, c1 = 'blue', c2 = 'red', width = 5):
    step = dist / width
    leftover = dist % width
    use_c1 = True
    for i in xrange(step):
        turtle.color(c1 if use_c1 else c2)
        turtle.forward(width)
        use_c1 = not use_c1
    turtle.color(c1 if use_c1 else c2)
    turtle.forward(leftover)
        
if __name__ == '__main__':
    turtle.speed('fastest')
    turtle.tracer(False)
    dot_poly(200, 144)
    raw_input('press any key to quit')
