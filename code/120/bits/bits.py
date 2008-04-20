import random, math

def bin_to_dec(b):
    """ Converts bit string b to its base-10 value.
    """
    total = 0
    p = 1
    for bit in reversed(b):
        if bit == '1':
            total += p
        p = 2 * p
    return total	

def num_bits(n):
    """ Returns the minimum number of bits needed to represented n
    in binary.
    """
    if n == 0:
        return 1
    else:
        return 1 + int(math.log(n, 2))

def dec_to_bin(n):
    """ Converts n to a binary bit string.
    """
    n = int(n)
    nb = num_bits(n)
    p = 2 ** (nb - 1) # biggest power of 2 <= n
    bin = ''
    for i in range(nb):
        if n >= p:
            bin += '1'
            n = n - p
        else:
            bin += '0'
        p = p / 2
    return bin

def random_bit():
    """ Returns '0' or '1', at random.
    """
    return str(random.randint(0, 1))

def random_bit_string(n):
    """ Returns a random n-bit string.
    """
    return ''.join([random_bit() for i in xrange(n)])

def flip(bit):
    if bit == '0':
        return '1'
    else:
        return '0'

def invert(bit_string):
    """ Flips all the bits in bit_string.
    """
    return ''.join([flip(bit) for bit in bit_string])

def pad(bit_string, n):
    """ Adds enough '0's to the left of bit_string to make its length n.
    """
    if len(bit_string) > n:
        return bit_string
    else:
        return ('0' * (n - len(bit_string))) + bit_string

def next_multiple_of_4(n):
    """ Return the first multiple of four f, such that f >= n.
    """
    if n % 4 == 0:
        return n
    else:
        return n + 4 - n % 4

def group_by_4s(bit_string):
    """ Returns bit_string copy with bits group by 4s.
    """
    n = len(bit_string)
    bit_string = pad(bit_string,
                     next_multiple_of_4(n))
    result = ''
    count = 0
    for bit in bit_string:
        if count % 4 == 0:
            result += ' '
        result += bit
        count += 1
    return result.strip()

def dec_to_bin32(n):
    return group_by_4s(pad(dec_to_bin(n), 32))

def gen_bits(n):
    """ Generates all bits strings of length n.
    """
    for i in xrange(2 ** n):
    	yield pad(dec_to_bin(i), n)

def gen_subsets(lst):
    """ Generates all subsets of lst.
    """
    n = len(lst)
    for s in gen_bits(n):
	yield [lst[i] for i, bit in enumerate(s) if bit == '1']
