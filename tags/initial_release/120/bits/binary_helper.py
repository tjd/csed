"""
Binary Helper

A program to help you practice binary conversions and range functions.

To run this program, load it into the Python shell and type

>>> main()

There are still a number of small bugs in the programs, and so
any fixes or bug reports are gratefully accepted.

Thanks to the following people for finding errors and suggesting
improvements: Ivan, Justin Chan, Daryl Van Humbeck, Anne Dawson, and
Mike Malyuk.

Changes (Nov. 1 2004)

- Major changes to the internal structure so that it's easier to add
new types of questions.

- Three new kinds of indexing questions added.

- Improved the parsing of lists in the range() test question so that you
no longer need to type in the answer with perfect spacing.

"""

import random
import re   # regular expressions

# for debugging: when this is True, the correct answer to a question
# is shown above the prompt to the user to answer it
show_correct = False

# used for input filter functions that do nothing
def ident(s): return s

def main():
    """ Call this function to run the program.
    """
    done = False
    while not done:
        choice = show_menu()
        if choice == '1':
            ask_fn(bin_to_dec_prob, 'binary to decimal conversion')
        elif choice == '2':
            ask_fn(dec_to_bin_prob, 'decimal to binary conversion')
        elif choice == '3':
            ask_fn(comp_to_dec_prob, 'complement to decimal conversion')
        elif choice == '4':
            ask_fn(dec_to_comp_prob, 'decimal to complement conversion')
        elif choice == '5':
            ask_fn(range_prob, 'range questions')
        elif choice == '6':
            ask_fn(pos_index_prob, 'positive sequence indexing')
        elif choice == '7':
            ask_fn(neg_index_prob, 'negative sequence indexing')
        elif choice == '8':
            ask_fn(index_prob, 'positive and negative sequence indexing')
        elif choice in halt:
            done = True

def get_input(prompt):
    done = False
    while not done:
        result = raw_input(prompt).strip().lower()
        if result != "":
            done = True
    return result

# various strings that can be used to quite the program
halt = ['done', 'halt', 'stop', 'quit', 'end', 'finished', 'finish']

def show_menu():
    print """
    What do you want to practice?
    
        1. Binary to decimal
        2. Decimal to binary
        3. 2s complement to decimal
        4. Decimal to 2s complement
        5. The range() function
        6. Positive sequence indexing
        7. Negative sequence indexing
        8. Positive and negative sequence indexing

     Type "done" to quit.
      """
    choice = raw_input("      Enter the number of your choice: ")
    return choice.strip().lower()


def bin_to_dec_prob():
    binary = make_rand_bin()
    ans = int(binary, 2)
    quest = "What is %s in base 10? --> " % binary
    return quest, str(ans), ident


def make_rand_bin():
    """
    Returns a random bit-string of 4 or 5 bits.
    """
    r = random.randint(4, 5)
    return ''.join([`random.randint(0, 1)` for i in range(r)])


def dec_to_bin_prob():
    # choose an integer randomly from 0 to 31
    dec = random.randint(0, 31)
    
    quest = "What is %s in binary?" % dec
    
    binary = dec2bin(dec, '')
    
    binary = binary[:-1].lstrip('0') + binary[-1] # remove any leading 0s
    #ans = ans[:-1].lstrip('0') + ans[-1]          # remove any leading 0s
    return quest, binary, ident


def dec2bin(n, sep = ' '):
    """ Converts n to a binary string printed in 4-bit
    groups.

    >>> dec2bin(4)
    '0100'
    >>> dec2bin(25)
    '0001 1001'
    >>> dec2bin(38199)
    '1001 0101 0011 0111'
    """
    higit = {'0':'0000', '1':'0001', '2':'0010',
             '3':'0011', '4':'0100', '5':'0101',
             '6':'0110', '7':'0111', '8':'1000',
             '9':'1001', 'a':'1010', 'b':'1011',
             'c':'1100', 'd':'1101', 'e':'1110',
             'f':'1111'}
    base16_rep = hex(n)[2:]
    return sep.join([higit[d] for d in base16_rep])

def comp_to_dec_prob():
    comp = make_rand_bin()
    dec = comp2dec(comp)
    quest = "What is the %s-bit 2s complement bit-string %s in base 10? --> " % (len(comp), comp)
    return quest, str(dec), ident


def comp2dec(comp):
    """ Converts a 2s complement bit-string to its decimal value.
    """
    if comp.strip('0') == '':
        return 0
    elif comp[0] == '0':
        return int(comp, 2)
    else: # must be negative
        return -(int(invert(comp), 2) + 1)


def dec_to_comp_prob():
    dec = random.randint(-16, 15)
    comp = dec2comp(dec, 5)
    quest = "What is %s in 5-bit 2s complement?" % dec

    return quest, str(comp), ident


def dec2comp(dec, n):
    if dec >= 0:
        return dbin(dec, n)
    else:
        inv = invert(dbin(-dec, n))
        result = ''
        seen_zero = False
        for bit in inv[::-1]:
            if seen_zero:
                result += bit
            elif bit == '0' and not seen_zero:
                result += '1'
                seen_zero = True
            elif bit == '1' and not seen_zero:
                result += '0'
        return result[::-1]


def dbin(dec, n):
    assert n > 0
    pow = 2 ** (n - 1)
    assert dec <= pow
    bin = ''
    
    while pow >= 1:
        if pow <= dec:
            dec = dec - pow
            bin += '1'
        else:
            bin += '0'
        pow = pow / 2

    return bin


def invert(bin):
    """
    Assuming bin is a bit-string, changes 1s to 0s and 0s to 1s.
    """
    result = ""
    for bit in bin: 
        if bit == '0':
            result += '1'
        else:
            result += '0'
    return result

def range_prob():
    lo = random.randint(-10, 10)
    hi = lo + random.randint(0, 5)
    step = random.choice([1, 2, 3, -1, -2, -3])
    if step < 0 and random.randint(1, 4) == 1:
        hi, lo = lo, hi
    correct = filter_list(str(range(lo, hi, step)))
    ans = 'What does range(%s, %s, %s) return?' % (lo, hi, step)
    return ans, correct, filter_list

# used for splitting lists
comma_re = re.compile(r'\s*,\s*')

def filter_list(str_list):
    """
    Converts a string list to a Python list, e.g.

    >>> filter_list('[1, 2, 3]')
    ['1', '2', '3']
    >>> filter_list('[cat, mouse, dog]')
    ['cat', 'mouse', 'dog']
    """
    str_list = str_list.strip()  # remove leading/trailing spaces
    if str_list[0] == '[':       # remove initial '['
        str_list = str_list[1:]
    if str_list[-1] == ']':      # remove trailing ']'
        str_list = str_list[:-1]
    lst = comma_re.split(str_list)   # split the list on commas
    return [item.strip() for item in lst] # return a list with items stripped

def filter_tuple(s):
    s = s.strip()
    if s[0] == '(':
        s = s[1:]
        s = s.strip()        
    if s[-1] == ')':
        s = s[:-1]
        s = s.strip()
    if str_tuple[-1] == ',':
        s = s[:-1]
        s = s.strip()
    
    tup = comma_re.split(s)
    
    return [item.strip() for item in tup]


#
# Everything below has yet to be well tested!
#

def quote(s):
    if isinstance(s, str):
        return "'" + s + "'"
    else:
        return s

def rand_string(lo, hi):
    n = random.randint(lo, hi)
    result = ''
    for i in xrange(n):
        result += random.choice('abcd?123')
    return result

def rand_float():
    return random.choice([-1.2, 1.2, 0.01, 2.5])
    
def rand_flat_list(n):
    return [random.randint(-10, 10) for i in xrange(n)]

def rand_flat_tuple(n):
    return tuple(rand_flat_list(n))
    
def rand_list(lo, hi):
    n = random.randint(lo, hi)
    result = range(n)
    for i in xrange(n):
        choice = random.randint(1, 5)
        if choice == 1:
            item = random.randint(-10, 10)
        elif choice == 2:
            item = rand_float()
        elif choice == 3:
            item = rand_string(0, 4)
        elif choice == 4:
            item = rand_flat_list(random.randint(0, 3))
        elif choice == 5:
            item = rand_flat_tuple(random.randint(0, 3))
        result[i] = item
    return result
            
def rand_tuple(lo, hi):
    return tuple(rand_list(lo, hi))

def rand_sequence(lo = 0, hi = 6):
    choice = random.randint(1, 3)
    if choice == 1:
        seq = rand_list(lo, hi)
    elif choice == 2:
        seq = rand_tuple(lo, hi)
    elif choice == 3:
        seq = quote(rand_string(lo, hi))
    return seq

def rand_nonempty_sequence():
    return rand_sequence(1, 6)

def pos_index_prob():
    seq = rand_nonempty_sequence()
    idx = random.randint(0, len(seq) - 1)
    quest = "What does %s[%s] return?" % (seq, idx)
    ans = seq[idx]
    return quest, str(ans), ident

def neg_index_prob():
    seq = rand_nonempty_sequence()
    idx = random.randint(-len(seq), -1)
    quest = "What does %s[%s] return?" % (seq, idx)
    ans = seq[idx]
    return quest, str(ans), ident

def index_prob():
    seq = rand_nonempty_sequence()
    idx = random.randint(-len(seq), len(seq) - 1)
    quest = "What does %s[%s] return?" % (seq, idx)
    ans = seq[idx]
    return quest, str(ans), ident

def pos_slice_prob():
    seq = rand_nonempty_sequence()
    n = len(seq)
    start = random.randint(0, n - 1)
    end = random.randint(start, n)
    quest = "What does %s[%s:%s] return?" % (seq, start, end)
    ans = seq[start:end]
    return quest, str(ans), filter_list

def get_input(prompt):
    done = False
    while not done:
        result = raw_input(prompt).strip().lower()
        if result != "":
            done = True
    return result

def ask_fn(prob_fn, msg, filter_input = ident):
    """
    Generic question-asking and answering-checking function.

       prob_fn() returns a (question, correct_answer, input_filter)
                 tuple
    """
    print
    print 'Okay, %s ... type "quit" when you are done\n' % msg
    done = False
    while not done:
        quest, correct, filter_input = prob_fn()
        print
        if show_correct: print correct
        ans = get_input(quest.strip() + ' ')
        #print '(your filtered input:"%s")' % filter_input(ans)
        if ans in halt:
            done = True
        else:
            if filter_input(ans) == correct:
                print "Correct a mundo!"
            else:
                print "Oh, I'm sorry, that's incorrect :-("
                print "The correct answer is %s" % correct

