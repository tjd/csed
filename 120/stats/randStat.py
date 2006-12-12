# randStat.py

import random, math
from stats import *

# pylab is a convenient graphing module base on matlab;
# you can download it from http://matplotlib.sourceforge.net/
# this is only used for checking results, and no pylab functions
# appear in the code below; so you can safely comment this line out
#from pylab import *

def box(s, c = '*'):
    line = (4 + len(s)) * '*'
    return line + ('\n* %s *\n' % s) + line

def percentile(S, p):
    """ Returns the item that is p-percent through the sorted version of S.
    """
    assert 0 <= p <= 1
    c = S[:]
    c.sort()
    if p == 1:
        return c[-1]
    else:
        return c[int(p * len(c))]
    
def mean_diff(S, T):
    return mean(S) - mean(T)

def samePopRand(A, B, stat_fn = mean_diff, reps = 1000):
    """ Test to determine if samples A and B come from the same population.

           Null hypothesis: samples A and B come from
                            the same population
    Alternative hypothesis: samples A and B come from
                            different populations

    A, B
    data samples; they need not be the same length

    stat_fn
    function that takes two data sample as input, and returns the
    statistic of interest; by default, stat_fn is the difference of
    the means
    
    reps
    number of times to repeat the basic loop; 1000 by default

    Returned is a distribution of stat_fn that can be used to
    determine the probability that the samples are drawn from the same
    distribution.

    This is based on procedure 5.7 of the book Empirical Artificial
    Intelligence.
    
    """
    na, nb = len(A), len(B)
    ab = A + B
    result = []
    for i in xrange(reps):
        random.shuffle(ab)
        astar = ab[:na]
        bstar = ab[na:]
        result.append(stat_fn(astar, bstar))
    return result
    
#
# the following data is based on running a system sort
# on a large text file
#

real = [1.436, 1.462, 1.443, 1.431, 1.417, 1.469, 1.463, 1.465, 1.496, 1.492]
user = [1.388, 1.412, 1.372, 1.364, 1.368, 1.436, 1.408, 1.388, 1.424, 1.432]

#
# Here's how to do a randomized version of a 2-sample t-test:
#
# >>> dist = samePopRand(real, user)
# >>> percentile(dist, .95)
# 0.029599999999999849
# >>> mean_diff(real, user)
# 0.058199999999999807
#
# This shows that the difference in means of the real and user samples is
# greater than 95% of the data in distribution resulting from samePopRand.
# Thus, with an alpha-level of .95, we should reject the null hypotehssi:
# it is likely that these two samples do not come from the same population,
# hence the difference in is significant.


def two_sample_randomized_t_test(A, B, alpha = 0.05, two_tailed = False,
                                 num_reps = 1000, return_dist = False):
    """
    >>> two_sample_randomized_t_test(user, real)

    Single-tailed t-test

    Null hypothesis: A and B have the same mean
    Alternative hypothesis: A and B have different means

    0.95 percentile = 0.03
    mean_diff(A, B) = -0.06
    ****************************************
    * Accept null hypothesis at 0.05-level *
    ****************************************

    (num_reps = 1000)
    """
    assert 0 < alpha < 1
    assert 0 < num_reps
    print
    if two_tailed:
        print 'Two-tailed t-test'
    else:
        print 'Single-tailed t-test'
    print
    print 'Null hypothesis: A and B have the same mean'
    print 'Alternative hypothesis: A and B have different means'
    print
    
    dist = samePopRand(A, B, stat_fn = mean_diff, reps = num_reps)
    
    if two_tailed:
        lo, hi = alpha / 2.0, 1 - alpha / 2.0
        p1 = percentile(dist, lo)
        p2 = percentile(dist, hi)
        md_ab = mean_diff(A, B)
        print '%.2f percentile = %.2f' %  (lo, p1)
        print '%.2f percentile = %.2f' %  (hi, p2)
        print 'mean_diff(A, B) = %.2f' % md_ab
        if p1 < md_ab < p2:
            print box('Accept null hypothesis at %.2f-level' % alpha)
        else:
            print box('Reject null hypothesis at %.2f-level' % alpha)
 
    else:
        p = percentile(dist, 1 - alpha)
        md_ab = mean_diff(A, B)
        print '%.2f percentile = %.2f' % (1 - alpha, p)
        print 'mean_diff(A, B) = %.2f' % md_ab
        if md_ab > p:
            print box('Reject null hypothesis at %.2f-level' % alpha)
        else:
            print box('Accept null hypothesis at %.2f-level' % alpha)
    print
    print '(num_reps = %s)' % num_reps

    if return_dist:
        return dist
