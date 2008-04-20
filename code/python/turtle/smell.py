# smell.py

import iturtle, turtle, math

turtle.setup()
turtle = iturtle.iTurtle()

# location of the food
food = -100, 100

# remembers the distance of the smell the last time smell() was cal1ed;
# initially set to None
last_smell_dist = None

def dot_at(p, color = 'red', radius = 4):
    """ Draws a filled circle at point p.
    Returns the turtle to its original state after drawing the dot.
    """
    state = turtle.get_state()
    turtle.up()
    turtle.tracer(False)
    turtle.goto(p)
    turtle.color(color)
    turtle.begin_fill()
    turtle.circle(radius)
    turtle.end_fill()
    turtle.set_state(state)

def distance(p1, p2):
    """ Returns the Euclidean distance between two points.
    """
    d1, d2 = p1[0] - p2[0], p1[1] - p2[1]
    return math.sqrt(d1 ** 2 + d2 ** 2)

def smell1():
    """ Perform a single 'sniff'.
    If the scent is stronger than it was last time, returns 'stronger';
    otherwise returns 'weaker'.
    """
    global last_smell_dist
    dist_to_food = distance(turtle.position(), food)
    if last_smell_dist == None:
	result = 'stronger'
    elif dist_to_food > last_smell_dist:
	result = 'weaker'
    else:
	result = 'stronger'
    last_smell_dist = dist_to_food
    return result

def find_by_smell1(forward_amount = 1, turn_amount = 1):
    dot_at(food)
    while True:
	turtle.forward(forward_amount)
	if smell1() == 'weaker':
	    turtle.right(turn_amount)

def smell2():
    return distance(turtle.position(), food)

last_sniff = None

def find_by_smell2(turn_amount = 1):
    global last_sniff
    dot_at(food)
    while True:
	sniff = smell2()
	turtle.forward(sniff)
	if last_sniff == None or sniff > last_sniff:
	    turtle.right(turn_amount)
	last_sniff = sniff

def find_by_smell3(forward_amount = 5):
    global last_sniff
    dot_at(food)
    while True:
	sniff = smell2()
	turtle.forward(forward_amount)
	if last_sniff == None or sniff > last_sniff:
	    turtle.right(sniff)
	last_sniff = sniff

if __name__ == '__main__':
    turtle.tracer(False)
    find_by_smell1(1, 20)
    #find_by_smell2(90)
    #find_by_smell3()  # strange, unnatural behaviour!
