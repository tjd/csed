def simple_box(s, ch = '*'):
    """ Returns a string with s in a box of characters.
    s must not contain any \n characters.

    >>> print box('Hello world!')
    ****************
    * Hello world! * 
    ****************
    """
    n = len(s)
    bar = (n + 4) * ch
    mid = '%s %s %s' % (ch, s, ch)
    return '%s\n%s \n%s' % (bar, mid, bar)

def pad_left(s, n, ch = ' '):
    """ Adds spaces to s make it of length n.
    If len(s) > n, s is returned.
    """
    slen = len(s)
    if slen >= n:
	return s
    else:
	return (n - slen) * ch + s

def pad_right(s, n, ch = ' '):
    """ Adds spaces to s make it of length n.
    If len(s) > n, s is returned.
    """
    slen = len(s)
    if slen >= n:
	return s
    else:
	return s + (n - slen) * ch

def box(s, ch = '*'):
    """ Returns a string with s in a box of characters.
    s can have \n characters in it.

    >>> print box("This is a test\nthis is only\na test!!")
    ******************
    * This is a test *
    * this is only   *
    * a test!!       * 
    ******************
    """
    lines = s.split('\n')
    height = len(lines)
    width = max(len(line) for line in lines)
    bar = (width + 4) * ch
    mid = '\n'.join('%s %s %s' % (ch, pad_right(line, width), ch) 
		    for line in lines)
    return '%s\n%s \n%s' % (bar, mid, bar)
