# stats.py

"""
Basic statistics.
"""
import math

def __implement_me__():
    print 'I am not yet implmented: please write me!'

def mean(lst):
    return sum(lst) / float(len(lst))

def median(raw_lst):
    n = len(raw_lst)
    lst = raw_lst[:]   # make a copy of the data
    lst.sort()         # sort the copy
    if n % 2 == 1:   # odd length?
        return lst[n / 2]  # median is the middle element
    else: # even number of values: no middle!
        return (lst[n / 2] + lst[1 + n / 2]) / 2.0

def mode(lst):
    """ Returns the mode (most frequent value) of lst.
    If there's a tie, only one mode is returned; while this is 
    a deterministic algorithm, there is no promise which one.

    >>> mode(range(10))
    9
    >>> mode([4, 6, 4, 5, 51, 15, 5, 6, 4, 4, ])
    4
    >>> mode([1, 1, 1, 1, 1])
    1
    >>> mode([1, 1, 2, 1, 2, 1, 1])
    1
    """
    vals = {}
    for item in lst:
	if item not in vals:
	    vals[item] = 1
	else:
	    vals[item] += 1
    srt = [(vals[key], key) for key in vals]
    srt.sort()
    srt.reverse()
    md = srt[0][1]
    return md

def variance(lst):
    m = mean(lst)
    return sum((x - m) ** 2 for x in lst) / (len(lst) - 1)

def std_dev(lst):
    return math.sqrt(variance(lst))

def rng(lst):
    return max(lst) - min(lst)

def skewness(lst):
    """ Returns the skewness of lst.

    ``Negative values for the skewness indicate data that are skewed
    left and positive values for the skewness indicate data that are
    skewed right. By skewed left, we mean that the left tail is long
    relative to the right tail. Similarly, skewed right means that the
    right tail is long relative to the left tail.''
    
    (from http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm)
    """
    s = std_dev(lst)
    n = len(lst)
    m = mean(lst)
    return sum((x - m) ** 3 for x in lst) / ((n - 1) * s ** 3)

def kurtosis(lst):
    """ Returns the skewness of lst.

    ``Positive kurtosis indicates a "peaked" distribution and negative
    kurtosis indicates a "flat" distribution.''
    
    (from http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm)
    """
    s = std_dev(lst)
    n = len(lst)
    m = mean(lst)
    return sum((x - m) ** 4 for x in lst) / ((n - 1) * s ** 4)

def excess_kurtosis(lst):
    """ Kurtosis of the normal distribution is 3, so the excess kurtosis
    makes the normal kurtosis 0.
    """
    return kurtosis(lst) - 3


# A more object-oriented approach to statistics is to follow the
# idea of a "frame" in the object-oriented statistical language R.
# An R frame can pre-calculate and store important values. The code
# is a first small step towards that goal.
class stat(object):
    def __init__(self, lst):
	n = len(lst)
	assert n > 0
	self.size = n
	self.raw = lst
	self.min = min(lst)
	self.max = max(lst)
        self.rng = self.max - self.min
	self.sum = sum(lst)
	self.mean = self.sum / float(n)
        self.median = median(lst)
	self.variance = sum((x - self.mean) ** 2 for x in lst) / float(n - 1)
	self.std_dev = math.sqrt(self.variance)
	self.zlist = [(x - self.mean) / self.std_dev for x in lst]

    def summary(self):
        print '  # items = %s' % self.size
        print
        print '      min = %.2f' % self.min
        print '      max = %.2f' % self.max
        print '    range = %.2f' % self.rng
        print
        print '     mean = %.2f' % self.mean
        print '   median = %.2f' % self.median
        print
        print ' variance = %.2f' % self.variance
        print 'std. dev. = %.2f' % self.std_dev

def summary(lst):
    """ Prints descriptive statistics.
    """
    stat(lst).summary()
