# casm.py

""" Simple assembler for the iRobot Create.

Drive in a square:
152 17 137 1 44 128 0 156 1 144 137 1 44 0 1 157 0 90 153

Here is the same program in a hypothetical assembly language, that
makes things more readable:

script 17         ;;; this is a 17-byte script
drive 300, 32767  ;;; drive forward with a velocity of 300 mm/s
wait_distance 400 ;;; don't execute the next command until you've gone 400mm
drive 300, 1      ;;; turn in-place counter-clockwise at 300 mm/s
wait_angle 90     ;;; don't execute the next command until you've turned 90 degrees
loop_forever      ;;; repeat this script
"""

import re   # regular expressions
import math

# Op codes come from p.22 of the iRobot Create Open Interface (OI)
# Specification. The commands words are assumed to be whitespace free.
op_code = {'start':128, 'baud':129, 'control':130, 'safe':131,
           'full':132, 'spot':134, 'cover':135, 'demo':136,
           'drive':137, 'low_side_drivers':138, 'leds':139,
           'song':140, 'play':141, 'sensors':142, 'cover_and_dock':143,
           'pwm_low_side_drivers':144, 'drive_direct':145,
           'digital_outputs':147, 'stream':148, 'query_list':149,
           'pause_resume_stream':150, 'send_ir':151, 'script':152,
           'play_script':153, 'show_script':154, 'wait_time':155,
           'wait_distance':156, 'wait_angle':157, 'wait_event':158
           }

def process_line(s):
    tokens = tokenize_line(s)
    return replace_tokens(tokens)

def replace_tokens(tokens):
    """ Replaces all tokens on the given list of tokens with their
    corresponding byte value.
    E.g.
    >>> process_line('demo 5')
    ['136', '5']
    >>> process_line('demo -1')
    ['136', 255]
    """
    n = len(tokens)
    first_word = tokens[0].lower()
    if n == 0:
        return ''
    elif n == 1:
        return [str(op_code[first_word])]
    else:
        result = []
        result.append(str(op_code[first_word]))  # first token is command word
        if first_word == 'baud':
            # 0 <= baud data byte <= 11
            result.append(tokens[1])
        elif first_word == 'demo':
            # -1 <= demo data byte <= 6
            if tokens[1] == '-1':
                result.append(255)
            else:
                result.append(tokens[1])
        return result

def error(s):
    print 'Error: %s' % s
        
def tokenize_line(s_raw):
    """ Returns a list of the tokens in line (comments stripped).
    A line has the form
           <command> [param]* [;;; comment]
    E.g.
    >>> tokenize_line('script 17         ;;; this is a 17-byte script')
    ['script', '17']
    >>> tokenize_line('drive 300, 32767  ;;; drive forward with a velocity of 300 mm/s')
    ['drive', '300', '32767']
    """
    s = strip_comment(s_raw).strip()
    tokens = [t.strip() for t in re.split('[, ]+', s)]
    return tokens

def strip_comment(s):
    """ Remove trailing ';;;'-style comment from s, if it has one. E.g.

    >>> strip_comment('start ;;; start it up!')
    'start '
    >>> strip_comment('move -220, 35')
    'move -220, 35'    
    """
    try:
        c_loc = s.index(';')
        return s[:c_loc]
    except:
        return s
    
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

def num_bits(n):
    """ Returns the minimum number of bits needed to represented n
    in binary.
    """
    if n == 0:
        return 1
    else:
        return 1 + int(math.log(n, 2))
