# quicksort.py

# The following is based on recipe 5.11 of the Python Cookbook, 2nd edition.
# It is mainly an exercise in writing a short and consise version of
# quicksort comparable to the ones given in Haskell.

def quicksort(lst):
    if len(lst) <= 1:
        return lst
    else:
        pivot = lst[0]
        smaller = [small for small in lst[1:] if small < pivot]
        bigger = [big for big in lst[1:] if big >= pivot]
        return quicksort(smaller) + [pivot] + quicksort(bigger)
        

