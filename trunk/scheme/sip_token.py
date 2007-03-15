# sip-token.py

digit_chars = '0123456789'
lower_alpha_chars = 'abcdefghijklmnopqrstuvwxyz'
upper_alpha_chars = lower_alpha_chars.upper()
alpha_chars = lower_alpha_chars + upper_alpha_chars
atom_chars = alpha_chars + '_-!?*' + digit_chars


def eval(e):
    print 'e = %s' % e
    if is_string(e):
	if is_int(e):
	    return int(e)
    elif is_list(e):
	head = e[0]
	if head == '+':
	    result = 0
	    for sub in e[1:]:
		result += eval(sub)
	    ret1urn result
	elif head == '-':
	    result = 0
	    for sub in e[1:]:
		result -= eval(sub)
	    return result
	elif head == '*':
	    result = 1
	    for sub in e[1:]:
		result *= eval(sub)
	    return result
	elif head == '/':
	    result = eval(e[1])
	    for sub in e[2:]:
		result /= eval(sub)
	    return result

def is_string(e):
    return isinstance(e, str)

def is_list(e):
    return isinstance(e, list)
	
def gen_tokens(s):
    """
    Returns a generator for the tokens of s.
    """
    i = 0
    while i < len(s):
        c = s[i] 
        if c in '()':
            yield c
            i += 1
        elif c in atom_chars:
            atom, next = get_atom(s, i)
            yield atom
            i = next
        else:
            i += 1

def get_atom(s, start):
    """
    Return the atom starting at s[start].
    """
    atom = ''
    while start < len(s) and s[start] in atom_chars:
        atom += s[start]
        start += 1
    return atom, start

def all_in(s, chars):
    """ Returns true iff all of s's characters are in chars.
    """
    for c in s:
        if not c in chars:
            return False
    return True

def is_atom(s):
    """ Returns true iff s is an atom.
    """
    if s == '':
        return False
    else:
        return all_in(s, atom_chars)

def is_int(s):
    if s == '':
        return False
    elif s[0] in '+-' and len(s) > 1:
        return all_in(s[1:], digit_chars)
    else:
        return all_in(s, digit_chars)

def gen_tokens_test():
    assert [t for t in gen_tokens('cat')] == ['cat']
    assert [t for t in gen_tokens('(cat)')] == ['(', 'cat', ')']
    assert [t for t in gen_tokens('(cat dog)')] == ['(', 'cat', 'dog', ')']
    assert [t for t in gen_tokens('(cat (dog woof))')] == ['(', 'cat',
                                                           '(', 'dog', 'woof',
                                                           ')', ')']
    assert [t for t in gen_tokens('()')] == ['(', ')']
    assert [t for t in gen_tokens('(())')] == ['(', '(', ')', ')']
    assert [t for t in gen_tokens('((()))')] == ['(', '(', '(', ')', ')', ')']

def get_atom_test():
    assert get_atom('a', 0) == ('a', 1)
    assert get_atom('apple', 0) == ('apple', 5)
    assert get_atom('apple ', 0) == ('apple', 5)
    assert get_atom('(apple)', 1) == ('apple', 6)

def is_atom_test():
    assert is_atom('a')
    assert is_atom('23')
    assert is_atom('Par-Con')
    assert is_atom('up_or_down!')
    assert is_atom('?---_!')

    assert not is_atom('')
    assert not is_atom('(')
    assert not is_atom(')')
    assert not is_atom('a b')
    assert not is_atom(' a')
    assert not is_atom('b ')

def is_int_test():
    assert is_int('0')
    assert is_int('+0')
    assert is_int('-0')    
    assert is_int('1')
    assert is_int('-1')
    assert is_int('+1')
    assert is_int('9363')
    assert is_int('-9363')
    assert not is_int('')
    assert not is_int(' 1')
    assert not is_int('1 ')
    assert not is_int('--1')
    assert not is_int('++1')
    assert not is_int('two')
    assert not is_int('+')
    assert not is_int('-')

if __name__ == '__main__':
    print 'Running tests ...'
    get_atom_test()
    gen_tokens_test()
    is_atom_test()
    is_int_test()
    print 'All tests passed'
