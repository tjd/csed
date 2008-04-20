"""
This is a collection of functions related to prime numbers. 
numbers.
"""

# is_prime and next_prime are co-recursive(!)
def is_prime(a):
   p = 2
   while p <= int(math.sqrt(a)):
       if a % p == 0:
           return False
       p = next_prime(p)
   return True

def next_prime(n):
   """ Returns the smallest prime p such that p > n.
   """
   if is_prime(n + 1):
       return n + 1
   else:
       return next_prime(n + 1)
    
def is_composite(n):
    return not is_prime(n)
