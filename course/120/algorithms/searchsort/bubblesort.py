# bubblesort.py

# Implements a variation of the classic bubblesort algorithm.
#
# This is not an efficient sorting algorithm, and should never be
# used to sort large amounts of data. It is only an illustration of
# basic Python programming.

from randdata import *

def bubble_up(lst):
    """ Scans through the list from left to right,
    swapping adjacent elements that are out of order.
    Returns True if one or more swaps were made, and
    False otherwise.
    
    This is done in-place, i.e. lst is modified.

    >>> lst = [7, 4, 1, 8]
    >>> bubble_up(lst)
    >>> lst
    [4, 1, 7, 8]
    >>> bubble_up(lst)
    >>> lst
    [1, 4, 7, 8]
    """
    i = 1
    any_swapped = False
    while i < len(lst):
        if lst[i - 1] > lst[i]:
            lst[i - 1], lst[i] = lst[i], lst[i - 1]  # swap
            any_swapped = True
        i += 1
    return any_swapped

def bubblesort(lst):
    """ Sorts lst in-place using bubblesort.
    Bubblesort is a very, very slow sorting algorithm:
    don't use it to sort large amounts of data.

    >>> import random
    >>> lst = range(10)
    >>> random.shuffle(lst)
    >>> lst
    [7, 4, 9, 1, 8, 5, 2, 3, 6, 0]
    >>> bubblesort(lst)
    >>> lst
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

    Note that this implementation is not quite as efficient as it
    could be.  For example, after each call to bubble_up, at least one
    element is in its "home" position on the right end of lst,
    so there is no need to scan over those already-placed
    elements in later iterations.
    """
    any_swapped = bubble_up(lst)
    while any_swapped:
        any_swapped = bubble_up(lst)

def bubble_up_num_comps(lst):
    """ Same as bubble_up, but returns the number of comparisons done.
    """
    num_comp = 0
    i = 1
    any_swapped = False
    while i < len(lst):
        num_comp += 1
        if lst[i - 1] > lst[i]:
            lst[i - 1], lst[i] = lst[i], lst[i - 1]  # swap
            any_swapped = True
        i += 1
    return any_swapped, num_comp

def bubblesort_num_comps(lst):
    """ Same as bubblesort, but returns the number of comparisons done.
    """
    any_swapped, c = bubble_up_num_comps(lst)
    num_comp = c
    while any_swapped:
        any_swapped, c = bubble_up_num_comps(lst)
        num_comp += c
    return num_comp
