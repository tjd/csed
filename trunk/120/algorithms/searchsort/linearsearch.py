# linearsearch.py

def linear_search(lst, x):
    """ Returns the index location of x in lst.
    Returns -1 if x is not in lst.

    >>> lst = range(10)
    >>> for i in lst: assert linear_search(lst, i) == i
    ...
    >>> linear_search(range(10), 15)
    -1
    """
    for i in range(len(lst)):
        if lst[i] == x:
            return i
    return -1

def reverse_linear_search(lst, x):
    """ Returns the index location of x in lst.
    Returns -1 if x is not in lst. Searches from
    right to left.

    >>> lst = range(10)
    >>> for i in lst: assert reverse_linear_search(lst, i) == i
    ...
    >>> lst = range(10) + range(10)
    >>> for i in lst: assert reverse_linear_search(lst, i) == 10 + i
    ... 
    """
    i = len(lst) - 1
    while i >= 0:
        if lst[i] == x:
            return i
        i -= 1
    return -1

def linear_search_num_comps(lst, x):
    """ Same as linear_search(lst, x), but returns the number of comparisons
    done.
    """
    comp_count = 0
    for i in range(len(lst)):
        comp_count += 1
        if lst[i] == x:
            return i, comp_count
    return -1, comp_count



