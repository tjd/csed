# together.py

from iturtle import iTurtle
from random import randint

def together(*args):
    """ Interleaves execution of the given turtle generators.
    When all the generators have finished, the function returns.
    The generators should be cooperative: they are executed in
    round-robin fashion in the order they are given, and the next
    one is executed only after a generator voluntarily yields.
    It is up to the caller to make sure that each generator takes
    about the same amount of time each step; otherwise the result
    might look oddly unparallel.
    """
    if len(args) == 1:
        gen = [g() for g in args[0]]
    else:
        gen = [g() for g in args]
    n = len(gen)
    done = False
    while not done:
        done = True
        for i in xrange(n):
            if gen[i] != None:
                try:
                    gen[i].next()
                    done = False
                except StopIteration:
                    gen[i] = None

def p1():
    t = iTurtle()
    for i in xrange(360):
        t.forward(1)
        t.left(1)
        yield
        
def p2():
    t = iTurtle()
    t.jump_to(10, 10)
    for i in xrange(360/2):
        t.forward(1)
        t.left(1)
        yield

def dot_poly(side, angle, x = 0, y = 0):
    turtle = iTurtle()
    turtle.speed('fastest')
    turtle.tracer(False)
    turtle.jump_to(x, y)
    def gen():
        turtle.forward(side)
#        for i in xrange(side):
#            turtle.forward(1)
#            yield
        turtle.dot_here()
        turtle.right(angle)
        turn = angle
        while turn % 360 != 0:
            turtle.stripe_forward(side)
#            for i in xrange(side):
#                turtle.forward(1)
            yield            
            turtle.dot_here()
            turtle.right(angle)
            turn += angle
    return gen

def demo_stars():
    # draw random polygons in parallel
    together(dot_poly(randint(50, 200),
                      150,
                      randint(-300, 300),
                      randint(-300, 300))
             for i in xrange(10))
    raw_input('press enter to quit ... ')

if __name__ == '__main__':
    #together((p1, p2, dot_poly(10, 100)))
    demo_stars()
