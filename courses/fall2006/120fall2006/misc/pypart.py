# pypart.py

sample_code = """
def dec_to_bin(n):
    ## initialization
    nb = num_bits(n)
    p = 2 ** (nb - 1)
    bin = ''
    ##
    ## loop header
    for i in range(nb):
    ##
        ## inner loop
        ## bit chooser
	if n >= p:
	    bin += '1'
	    n = n - p
	else:
	    bin += '0'
        ##
	## loop control code
	p = p / 2
	##
        ##
    return bin
"""

sample_doc = """

<p>Before a loop we put code that initializes all important
variables:</p>
<pre>
## initialization
</pre>
<p>After the initialization we then have the loop, with the following
header:</p>
<pre>
## loop header
</pre>
<p>Inside the loop, there are typically two kinds of code: processing
code, and loop control code. In this case, the loop control code is
simple:</p>
<pre>
## loop control code
</pre>
<p>The processing code in this case decides what bit to add to the end 
of the string being constructed:</p>
<pre>
## bit chooser
</pre>
"""

def get_parts(s, cmd_token = '##', num_leading_spaces = 1, number_lines = True,
	      start_numbering_at = 1, number_style = 'space', indent_amount = 2):
    """ Returns a dictionary of names code parts.
    
    Mark the beginning of a part with a line like

        ## name for this part of the code

    and terminated some lines later with

        ##

    Only whitespaced may occur before '##' on a beginnng line, and the
    terminating line must not have any (non-whitespace) characters
    after the '##'.

    These lines can be nested. See the sample code above.
    """
    lines = s.split('\n')
    stack = []
    parts = {}
    for i, line in enumerate(lines):
	strip_line = line.strip()
	if strip_line == cmd_token:
	    name, start = stack.pop()
	    clean = [line for line in lines[start:i] 
		     if not line.strip().startswith(cmd_token)]
	    spaced = [add_leading_space(line, num_leading_spaces) 
		      for line in remove_leading_spaces(clean)]
	    if number_lines:
		parts[name] = add_numbers(spaced, start_numbering_at,
					  number_style)
	    else:
		parts[name] = spaced
	    parts[name] = indent_all(parts[name], indent_amount)
	elif strip_line.startswith(cmd_token):
	    n = len(cmd_token)
	    stack.append((strip_line[n + 1:].strip(), i + 1))
    return parts

style_to_char = {'space' : ' ', 'dot' : '.', 'colon' : ':', 'bracket' : ')'}

def add_numbers(lst, start = 1, number_style = 'space'):
    """ Returns a new list the same as lst, with numbers add to each element.
    """
    n = len(lst)
    width = len(str(n))
    return ['%*s%s%s' % (width, i + start, style_to_char[number_style], item) 
	    for i, item in enumerate(lst)]

def remove_leading_spaces(lst):
    """ Removes excess leading spaces.
    First determines which string in lst has the minimum nummber of spaces,
    and removes that many spaces from each string.
    """
    excess = min([(num_leading_spaces(item), item) for item in lst])[0]
    return [trim_leading_space(line, excess) for line in lst]

def trim_leading_space(s, trim_amount):
    """ Removes trim_amount spaces from the beginning of s.
    If s starts with fewer than trim_amount spaces, then all the leading
    spaces are removed.
    """
    amt = num_leading_spaces(s)
    return s[min(amt, trim_amount):]

def num_leading_spaces(s):
    """ Returns the number of spacing at the beginning of  s.
    """
    return len(s) - len(s.lstrip())

def add_leading_space(s, add_amount):
    """ Adds add_amount spaces to the beginning of s.
    """
    return ' ' * add_amount + s

def indent_all(lst, amount):
    """ Returns a copy lst with amount space to the each string.
    """
    return [add_leading_space(item, amount) for item in lst]

def replace_parts(s, parts, cmd_token = '##'):
    """ Replaces in s each line of the form

          ## name in parts

    See sample_doc and sample_code for an example of this works.
    """
    lines = s.split('\n')
    result = []
    for line in lines:
	strip_line = line.strip()
	if strip_line.startswith(cmd_token):
	    n = len(cmd_token)
	    name = strip_line[n + 1:]
	    code = parts[name]
	    result.append('\n'.join(code))
	else:
	    result.append(line)
    return result

def process_parts(in_code, in_doc, cmd_token = '##', num_leading_spaces = 1, 
		  number_lines = True, start_numbering_at = 1, 
		  number_style = 'space', indent_amount = 2):
    parts = get_parts(in_code, cmd_token = cmd_token, 
		      num_leading_spaces = num_leading_spaces,
		      number_lines = number_lines,
		      start_numbering_at = start_numbering_at,
		      number_style = number_style, 
		      indent_amount = indent_amount)
    doc = replace_parts(in_doc, parts, cmd_token = cmd_token)
    return doc

def main():
    doc = process_parts(sample_code, sample_doc)
    print '\n'.join(doc)

if __name__ == '__main__':
    main()
