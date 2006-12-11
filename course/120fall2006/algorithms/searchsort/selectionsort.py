# selectionsort.py

# Implements a variation of the classic selection sort algorithm.
#
# While inefficient for large lists, it is a simple and natural
# sorting algorithm; humans often use selection sort when sorting
# small lists, e.g. arranging their cards in a game of Bridge.

def index_of_min(begin, lst):
    """ Returns the index of the smallest value in lst, starting at begin.
    """
    assert 0 <= begin
    mi = begin
    i = begin
    while i < len(lst):
        if lst[i] < lst[mi]:
            mi = i
        i += 1
    return mi

def selectionsort(lst):
    """ Performs selection sort in-place on lst.

    >>> from selectionsort import *
    >>> lst = [5, 9, 0, 4, 2, 1, 5]
    >>> selectionsort(lst)
    >>> lst
    [0, 1, 2, 4, 5, 5, 9]
    """
    for i in range(len(lst)):
        mi = index_of_min(i, lst)
        lst[i], lst[mi] = lst[mi], lst[i]  # swap
        
