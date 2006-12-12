# binarysearch.py

def binary_search(lst, x):
    """ Using binary search, returns some i such that lst[i] == x.  If
    x is not in lst, then -1 is returned. IMPORTANT: elements of lst
    must be in ascending sorted order!

    Binary search is an important algorithm because it is extremely
    efficient: for a list of length n, it does, at *worst*, about log
    n iterations of the loop. So, for example, a list of a million
    items will require about 20 comparisons in the worst case.

    >>> for x in range(11): assert binary_search(range(11), x) == x
    ... 
    """
    begin, end = 0, len(lst) - 1
    while begin <= end:
        mid = (begin + end) / 2
        if lst[mid] == x:
            return mid
        elif x < lst[mid]:
            end = mid - 1
        else:
            begin = mid + 1
    return -1
