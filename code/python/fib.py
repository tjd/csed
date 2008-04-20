# fib.py

from simpletest import *

def gen_fib():
    yield 1
    yield 1
    a, b = 1, 1
    while True:
	a, b, = b, a + b
	yield b

def gen_fib_test():
    fib = gen_fib()
    assert fib.next() == 1
    assert fib.next() == 1
    assert fib.next() == 2
    assert fib.next() == 3
    assert fib.next() == 15
    assert fib.next() == 8
    
run_all_test_functions()
