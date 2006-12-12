# string_chunk.py

def __implement_me__():
    print 'I am not yet implemented. Please create me.'

def gen_string_chunk(s, chunk_size):
    """
    Returns substrings of s in sizes of chunk_size.
    >>> for chunk in gen_string_chunk('This is a test, this is only a test', 4):
    ...     print chunk
    ... 
    This
    is 
    a te
    st, 
    this
    is 
    only
    a t
    est
    """
    for i in xrange(0, len(s), chunk_size):
        yield s[i:i + chunk_size]

def all_indices_of(s, ch = ' '):
    """ Returns a list of the index values of where ch appears in s.
    """
    return [i for i, item in enumerate(s) if item == ch]

def get_chunk_to_sep(s, chunk_size, sep = ' '):
    """ Returns the largest substring s[:i] such that len(s[:i]) 
    """
    pass

def total_to(lst, total):
    """ Returns the largest i such that lst[0] + ... + lst[i] <= total.
    """
    running_sum = 0
    for i, item in enumerate(lst):
        running_sum += len(item)
        if running_sum > total:
            return i
    return len(lst) - 1

def gen_prefix(s, n):
    """
    Generates consecutive n-length substrings of s start at the
    beginning.

    >>> s = 'The big Kahuna, Bob!'
    >>> gen = gen_prefix(s, 5)
    >>> for pre in gen: print pre
    ... 
    The b
    ig Ka
    huna,
    Bob!
    >>> gen = gen_prefix('abc', 5)
    >>> for pre in gen: print pre
    ... 
    abc
    """
    while len(s) > 0:
	yield s[:n]
	s = s[n:]
    
def gen_word_chunk(s, chunk_size, sep = ' '):
    while True:
	if len(s) <= chunk_size:
	    yield s
	    raise StopIteration
	chunk = s[:chunk_size]
	break_at = chunk.rfind(sep)
	if break_at == -1:  # no sep
	    yield s
	    raise StopIteration
	else:
	    yield chunk[:break_at]
	s = s[break_at:]
	
def check_next(gen, val):
    next = gen.next()
    assert next == val, '"%s"' % next

def gen_word_chunk_test():
    print 'Testing gen_word_chunk ...',
    gen = gen_word_chunk('mustard', 10)
    check_next(gen, 'mustard')
    gen = gen_word_chunk('Louie is the king.', 9)
    check_next(gen, 'Louie is')
    check_next(gen, 'the king.')
    print 'done'
