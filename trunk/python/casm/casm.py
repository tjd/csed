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

import re           # regular expressions
from bits import *  # bit-manipulation functions
import math


# Op codes come from p.22 of the iRobot Create Open Interface (OI)
# Specification. The commands words are assumed to be whitespace-free.
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

def process_file(fname):
    """
    Convert a casm file into a corresponding string of numbers.
    
    >>> from casm import *
    >>> process_file('forward_turn.cas')
    ['137', '8', '0', '0', '0', '156', '1', '144', '137', '1', '44', '0', '1', '157', '0', '90']
    """
    assert fname[-4:] == '.cas' and len(fname) > 0
    lines = re.split(r'\n+', open(fname, 'r').read())
    raw = [process_line(line) for line in lines if line != '']
    result = []
    for cmd in raw:
        result.extend(cmd)
    return result
    

def process_line(s):
    tokens = tokenize_line(s)
    return replace_tokens(tokens)

def replace_tokens(tokens):
    """ Replaces all tokens on the given list of tokens with their
    corresponding byte value.
    E.g.

    >>> process_line('baud 11')
    ['129', '11']
    >>> process_line('demo 5')
    ['136', '5']
    >>> process_line('demo -1')
    ['136', 255]
    >>> process_line('drive 32767')
    ['137', '8', '0', '0', '0']
    >>> process_line('drive 32768')
    ['137', '7', '15', '15', '15']
    >>> process_line('drive -200, 500   ;;; velocity = -200 mm/a, radius = 500 mm')
    ['137', '255', '56', '1', '244']
    >>> process_line('song 5, 5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10')
    ['140', '5', '5', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10']
    >>> process_line('play 5')
    ['141', '5']
    >>> process_line('sensors 12 ;;; get sensors data')
    ['142', '12']
    >>> process_line('pwm_low_side_drivers 8, 3, 2  ;;; set low side drivers')
    ['144', '8', '3', '2']
    >>> process_line('drive_direct -200, 418')
    ['145', '255', '56', '1', '162']
    >>> process_line('digital_outputs 5  ;;; intelligent comment goes here')
    ['147', '5']
    >>> process_line('stream 5, 1, 2, 3, 4, 5  ;;; get some data!')
    ['148', '5', '1', '2', '3', '4', '5']
    >>> process_line('query_list 5, 1, 2, 3, 4, 5  ;;; get some data!')
    ['149', '5', '1', '2', '3', '4', '5']
    >>> process_line('send_ir 88  ;;; intelligent comment goes here')
    ['151', '88']
    >>> process_line('wait_time 200  ;;; intelligent comment goes here')
    ['155', '200']
    >>> process_line('wait_distance 14200  ;;; intelligent comment goes here')
    ['156', '55', '120']
    >>> process_line('wait_distance -14200  ;;; intelligent comment goes here')
    ['156', '200', '136']
    >>> process_line('wait_event 18  ;;; intelligent comment goes here')
    ['158', '18']
    >>> process_line('wait_event -18  ;;; intelligent comment goes here')
    ['158', '238']
    """
    n = len(tokens)
    first_word = tokens[0].lower()
    if n == 0:
        return ''
    elif n == 1:
        # add error-checking for non-command words
        return [str(op_code[first_word])]
    else:
        result = [str(op_code[first_word])]    # first token is command word
        if first_word == 'baud':
            ensure(0 <= int(tokens[1]) <= 11,
                   'baud: parameter must be from 0 to 11 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'demo':
            ensure(0 <= int(tokens[1]) <= 11,
                   'demo: parameter must be from -1 to 6 (inclusive)')
            if tokens[1] == '-1':
                result.append(255)
            else:
                result.append(tokens[1])
        elif first_word == 'drive':
            if len(tokens) == 2:
                ensure(int(tokens[1]) in [32767, 32768],
                       "drive: single-parameter drive parameter must be 32767 or 32768")
                if int(tokens[1]) == 32767:
                    result += [8, 0, 0, 0]
                else:
                    result += [7, 15, 15, 15]
            else:
                velocity, radius = tokens[1], tokens[2]
                ensure(-500 <= int(velocity) <= 500,
                       'drive: velocity must be from -500 to 500 (inclusive)')
                ensure(-2000 <= int(radius) <= 2000,
                       'drive: radius must be from -2000 to 2000 (inclusive), or equal to 32767 or 32768')
                b1, b2 = byte_split(velocity)
                b3, b4 = byte_split(radius)
                result += [b1, b2, b3, b4]
        elif first_word == 'low_side_drivers':
            ensure(0 <= int(tokens[1]) <= 255,
                   'low_side_drivers: velocity must be from 0 to 255 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'leds':
            bits, color, intensity = int(tokens[1]), int(tokens[2]), int(tokens[3])
            ensure(0 <= bits <= 10,
                   'leds: bits must be from 0 to 10 (inclusive)')
            ensure(0 <= color <= 255,
                   'leds: color must be from 0 to 255 (inclusive)')
            ensure(0 <= intensity <= 255,
                   'leds: color must be from 0 to 255 (inclusive)')
            
            result += [bits, color, intensity]
        elif first_word == 'song':
            song_num, song_len, notes = tokens[1], tokens[2], tokens[3:]
            ensure(0 <= int(song_num) <= 15,
                   'song: song number must be from 0 to 15 (inclusive); song_num = %s' % song_num)
            ensure(1 <= int(song_len) <= 16,
                   'song: song length must be from 1 to 16 (inclusive)')
            ensure(len(notes) == 2 * int(song_len),
                   'song: must have 2n note and duration bytes')
            for n in notes:
                ensure(0 <= int(n) <= 255,
                   'song: notes and durations must be from 0 to 255 (inclusive)')
            result += [song_num] + [song_len] + notes
        elif first_word == 'play':
            ensure(0 <= int(tokens[1]) <= 15,
                   'play: song must be from 0 to 15 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'sensors':
            ensure(0 <= int(tokens[1]) <= 42,
                   'sensors: packet id must be from 0 to 42 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'pwm_low_side_drivers':
            lsd2, lsd1, lsd0 = tokens[1], tokens[2], tokens[3]
            ensure(0 <= int(lsd2) <= 128,
                   'pwm_low_side_drivers: duty cycle 2 must be from 0 to 128 (inclusive)')
            ensure(0 <= int(lsd1) <= 128,
                   'pwm_low_side_drivers: duty cycle 1 must be from 0 to 128 (inclusive)')
            ensure(0 <= int(lsd0) <= 128,
                   'pwm_low_side_drivers: duty cycle 0 must be from 0 to 128 (inclusive)')
            result += [lsd2, lsd1, lsd0]
        elif first_word == 'drive_direct':
            vel_right, vel_left = tokens[1], tokens[2]
            ensure(-500 <= int(vel_right) <= 500,
                   'drive_direct: velocity of right must be from -500 to 500 (inclusive)')
            ensure(-500 <= int(vel_left) <= 500,
                   'drive_direct: velocity of left must be from -500 to 500 (inclusive)')
            b1, b2 = byte_split(vel_right)
            b3, b4 = byte_split(vel_left)
            result += [b1, b2, b3, b4]
        elif first_word == 'digital_outputs':
            ensure(0 <= int(tokens[1]) <= 255,
                   'digital_outputs: parameter must be from 0 to 255 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'stream':
            num_packets, packet_ids = tokens[1], tokens[2:]
            ensure(int(num_packets) == len(packet_ids),
                   'stream: %s packets requested, but %s packet ids given' % (int(num_packets),
                                                                              len(packet_ids)))
            ensure(0 <= int(num_packets) <= 43,
                   'stream: number of packets must be from 0 to 43 (inclusive)')
            for pid in packet_ids:
                   ensure(0 <= int(pid) <= 42,
                          'stream: packet ids must be from 0 to 42 (inclusive)')
            result += [num_packets] + packet_ids
        elif first_word == 'query_list':
            num_packets, packet_ids = tokens[1], tokens[2:]
            ensure(int(num_packets) == len(packet_ids),
                   'query_list: %s packets requested, but %s packet ids given' % (int(num_packets),
                                                                                  len(packet_ids)))
            ensure(0 <= int(num_packets) <= 255,
                   'query_list: number of packets must be from 0 to 255 (inclusive)')
            for pid in packet_ids:
                   ensure(0 <= int(pid) <= 42,
                          'query_list: packet ids must be from 0 to 42 (inclusive)')
            result += [num_packets] + packet_ids
        elif first_word == 'pause_resume_stream':
            ensure(0 <= int(tokens[1]) <= 1,
                   'pause_resume_stream: parameter must be 0 or 1')
            result.append(tokens[1])
        elif first_word == 'send_ir':
            ensure(0 <= int(tokens[1]) <= 255,
                   'send_ir: parameter must be from 0 to 255 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'wait_time':
            ensure(0 <= int(tokens[1]) <= 255,
                   'wait_time: parameter must be from 0 to 255 (inclusive)')
            result.append(tokens[1])
        elif first_word == 'wait_distance':
            ensure(-32767 <= int(tokens[1]) <= 32768,
                   'wait_distance: distance must be from -32767 to 32768 (inclusive)')
            b1, b2 = byte_split(tokens[1])
            result += [b1, b2]
        elif first_word == 'wait_angle':
            ensure(-32767 <= int(tokens[1]) <= 32768,
                   'wait_angle: angle must be from -32767 to 32768 (inclusive)')
            b1, b2 = byte_split(tokens[1])
            result += [b1, b2]
        elif first_word == 'wait_event':
            # table on page 16 of OI spec goes up to 22
            ensure((-20 <= int(tokens[1]) <= -1) or (1 <= int(tokens[1]) <= 20),
                   'wait_event: event id must be from -20 to -1 (inclusive), or 1 to 20 (inclusive)')
            result.append(bin_to_dec(dec_to_twos_complement(tokens[1], 8)))
            
            
        # return the final values of as a list of base-10 strings
        return [str(x) for x in result]

def byte_split(w):
    """ Returns the decimal values of upper and lower bytes of w.
    Assumes w can be represented as a 16-bit 2s-complement value.
    """
    b = dec_to_twos_complement(w, 16)
    hi, lo = b[:8], b[8:]
    return bin_to_dec(hi), bin_to_dec(lo)

def ensure(b, s):
    """ If b is false, raises an error. Otherwise, does nothing.
    """
    if not b:
        error(s)
        
def error(s):
    raise RuntimeError('casm processing error:\n %s' % s)
        
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
