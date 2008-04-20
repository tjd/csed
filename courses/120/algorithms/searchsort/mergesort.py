def merge(A, B):
    """
    Returns a new list C that contains all the elements of A and B
    in sorted order.

    A and B must be in sorted order for this to work!

    >>> A = [3, 5, 6, 7, 32, 235]
    >>> B = [-1, 4, 5, 6, 7, 8, 9, 19, 65]
    >>> merge(A, B)
    [-1, 3, 4, 5, 5, 6, 6, 7, 7, 8, 9, 19, 32, 65, 235]
    """
    n = len(A) + len(B)
    C = range(n)
    a, b = 0, 0 
    for c in range(n):    # c is incremented each iteration
        if a == len(A):   # A is done
            C[c] = B[b]
            b = b + 1
        elif b == len(B): # B is done
            C[c] = A[a]
            a = a + 1
        elif A[a] < B[b]: # elements on both A and B
            C[c] = A[a]
            a = a + 1
        else:
            C[c] = B[b]
            b = b + 1

    return C

def mergesort(lst):
    """
    Performs mergesort on lst. Returns a new list that is in sorted
    order.  The passed-in lst is not modified.

    Note: This is not the most efficient version of mergesort. Far
    from it, in fact. It is written to be easy to understand. The
    slicing to create the left and right lists is inefficient, and
    should be replaced with in-place operations if you need it to
    run faster.
    """
    n = len(lst)
    if n <= 1:           # nothing to sort!
        return lst[:]    # so just return a copy and quit
    else:
        mid = n / 2        # split the list 
        left = lst[:mid]   # into two equal 
        right = lst[mid:]  # halves

        sorted_left = mergesort(left)     # recursively sort
        sorted_right = mergesort(right)   # each half

        return merge(sorted_left, sorted_right)
