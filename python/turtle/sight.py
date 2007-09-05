# sight.py

import iturtle, turtle
turtle.setup()

turtle = iturtle.iTurtle()

food = 200, -100

#def bearing(p):
#    return turtle.towards(p) - turtle.heading()
#
#def face(p):
#    turtle.setheading(turtle.towards(p))

def keep_a_bearing(p, angle):
    dot_at(p)
    while True:
	turtle.face(p)
	turtle.left(angle)
	turtle.forward(1)

def right_eye_sees(p, fov = 45):
    return -fov/2 <= turtle.bearing(p) <= fov/2

def left_eye_sees(p, fov = 45):
    return -fov/2 <= turtle.bearing(p) <= fov/2

def head_for1(p = food):
    dot_at(p)
    while True:
	if left_eye_sees(p) or right_eye_sees(p):
	    turtle.forward(10)
	else:
	    turtle.left(10)

def head_for2(p = food):
    dot_at(p)
    while True:
	turtle.forward(10)
	left, right = left_eye_sees(p), right_eye_sees(p)
	if left and right:
            turtle.forward(10)
	elif left:
	    turtle.left(10)
	elif right:
	    turtle.right(10)
	else:
	    turtle.right(5)

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
    

if __name__ == '__main__':
    #keep_a_bearing(food, 40)
    head_for2(food)
