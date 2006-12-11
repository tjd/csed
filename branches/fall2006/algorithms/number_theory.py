# number_theory.py

# The GCD code is based on http://modular.fas.harvard.edu/ent/ent_py

def gcd(a, b):                                       
    """
    Returns the greatest commond divisor of a and b.

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
