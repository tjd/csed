# simpletest.py

# global list of all functions to be called during testing
__test_functions__ = set([])


def _collect_test_functions():
    glob = globals()
    for key in glob:
	if key.endswith('_test'):
	    __test_functions__.add(glob[key])

def _run_test_fn(fn):
    print 'Running tests from %s ...' % fn.__name__,
    fn()
    print ' all %s tests passed!' % fn.__name__

def run_all_test_functions():
    _collect_test_functions()
    print __test_functions__
    print locals()
    for fn in __test_functions__:
	_run_test_fn(fn)

def make_test_function(fn):
    __test_functions__.add(fn)
    return fn
