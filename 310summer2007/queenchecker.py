# queenchecker.py

"""

Checks if a given file of space-seperated numbers is a valid n-queens
solution. To run it at the (Linux) command line, type this:

$ python queenchecker.py filename

where filename is assumed to be a text file of space-seperated
numbers.

The numbers form a valid solution if:

 - They are a permutation of the numbers from 0 to n - 1.
 - All diagonal constraints are satisfied.

The function make_fn_sol_file creates random files for testing
purposes.

"""

import random
from math import *

def make_rnd_sol_file(n = 1000000, fname = 'queens.sol'):
    print 'Writing random permutation of 0 to %s into file %s ...' % (n - 1,
                                                                      fname)
    lst = [str(i) + ' ' for i in range(n)]
    print 'Shuffling ...'
    random.shuffle(lst)
    print 'Writing to %s ...' % fname
    f = open(fname, 'w')
    f.writelines(lst)
    f.close()
    print '%s completed' % fname

def main(fname = 'queens.sol'):
    """ fname must be the name of a text file containing space-seperated integers.
    """
    print 'Reading "%s" ...' % fname
    var = [int(s) for s in open(fname, 'r').read().split(' ') if s != '']
    n = len(var)
    print 'N = %s' % n
    
    print 'Checking for diagonal conflicts ...'
    for i in xrange(n):
        for j in xrange(i + 1, n):
            if abs(var[i] - var[j]) == abs(i - j):
                print 'Not a solution: diagonal conflict'
                print 'var[%s] = %s conflicts with var[%s] = %s' % (i, var[i],
                                                                    j, var[j])
                return

    print 'Checking uniqueness and completeness ...'
    print 'Sorting %s numbers ...' % n
    var.sort()
    print 'Scanning for incorrect values ...' 
    for i in xrange(n):
        if var[i] != i:
            print 'Not a solution: missing or repeated value'
            print 'var[%s] = %s' % (i, var[i])
            return

    print "!!! %s contains a valid solution to the %s-queens problem!" % (fname, n)
    
if __name__ == '__main__':
    # Import Psyco if available
    try:
        import psyco
        psyco.full()
        print '(full psyco optimizations)'
    except ImportError:
        print '(no psyco optimizations)'

    import sys
    main(sys.argv[1])
