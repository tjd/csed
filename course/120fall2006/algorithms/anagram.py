# anagram.py

"""
Gets anagrams of a word using an index.

For example:

>>> from anagram import *
Reading in words ...
Creating anagram index ...
>>> anagrams_of('retinas')
['asterin', 'eranist', 'restain', 'stainer', 'starnie', 'stearin']

"""

def anagrams_of(s):
    """ Returns all anagrams of s, if any.
    """
    sw = sort_letters(s)
    if sw in anagram_index:
        result = anagram_index[sw]
        return result
    else:
        return '%s has no anagrams' % s

def make_anagram_index(words):
    """ Returns a dictionary with keys being sorted words, and values
    the list of words with those letters.
    """
    idx = {}
    for w in words:
        sw = sort_letters(w)
        if sw in idx:
            idx[sw].append(w)
        else:
            idx[sw] = [w]
    return idx

def sort_letters(s):
    """ Returns a new string with the letters of s in sorted order.
    """
    lst = list(s)
    lst.sort()
    result = ''.join(lst)
    return result

def get_words(fname):
    """ Returns all words in fname as a list of strings.
    fname is assumed to have one word per line.
    """
    f = open(fname, 'r')
    all_words = []
    for word in f:
        all_words.append(word.strip())
    return all_words

#    
# open the dictionary and store all words in a list
#
print 'Reading in words ...'
all_words = get_words('web2')
print 'Creating anagram index ...'
anagram_index = make_anagram_index(all_words)    
