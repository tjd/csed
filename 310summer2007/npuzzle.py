# npuzzle.py

import random, math

"""
>>> b = Board(3)
>>> b.display()
 0   1   2 
 3   4   5 
 6   7   * 
blank = (2, 2), misplaced = 0
Manhattan score = 0
>>> b.scramble(50)
['U', 'L', 'R', 'D', 'U', 'L', 'L', 'R', 'L', 'R', 'R', 'L', 'U', 'D', 'D', 'U', 'U', 'R', 'D', 'U', 'L', 'L', 'D', 'R', 'R', 'L', 'L', 'D', 'U', 'U', 'D', 'R', 'R', 'U', 'L', 'L', 'D', 'U', 'R', 'L', 'R', 'L', 'R', 'L', 'D', 'D', 'R', 'R', 'L', 'U']
>>> b.display()
 1   3   0 
 6   *   2 
 7   4   5 
blank = (1, 1), misplaced = 8
Manhattan score = 10

>>> b = Board([-1, 3, 5, 1, 2, 0, 6, 4, 7])
>>> b.display()
 *   3   5 
 1   2   0 
 6   4   7 
blank = (0, 0), misplaced = 7
Manhattan score = 12
"""


# represents an n-by-n n-puzzle board
class Board(object):
    def __init__(self, n):
        if isinstance(n, int):
            assert n > 2
            self.n = n
            self.board = [i for i in xrange(n * n)]
            self.board[-1] = -1   # -1 represents the blank
            self.blank_at = n * n - 1
            self.blank_row = n - 1
            self.blank_col = n - 1
            self.last_move = ''
        elif isinstance(n, list):
            lst = n
            self.n = int(math.sqrt(len(lst)))
            self.board = lst
            self.blank_at = lst.index(-1)
            self.blank_row = self.blank_at / self.n
            self.blank_col = self.blank_at % self.n
            self.last_move = ''

    def __eq__(self, other):
        return self.board == other.board

    def __hash__(self):
        if self.hash_val == None:
            self.hash_val = hash(tuple(self.board))
        return self.hash_val

    def copy(self):
        import copy
        return copy.deepcopy(self)

    def swap(self, i, j):
        self.board[i], self.board[j] = self.board[j], self.board[i]
    
    def move_up(self):
        if self.blank_row > 0:
            new_blank = self.blank_at - self.n
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_row -= 1
            self.last_move = 'U'

    def move_down(self):
        if self.blank_row < self.n - 1:
            new_blank = self.blank_at + self.n
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_row += 1
            self.last_move = 'D'

    def move_left(self):
        if self.blank_col > 0:
            new_blank = self.blank_at - 1
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_col -= 1
            self.last_move = 'L'

    def move_right(self):
        if self.blank_col < self.n - 1:
            new_blank = self.blank_at + 1
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_col += 1
            self.last_move = 'R'

    def make_move(self, m):
        if m == 'L':
            self.move_left()
        elif m == 'R':
            self.move_right()
        elif m == 'U':
            self.move_up()
        elif m == 'D':
            self.move_down()

    def make_moves(self, s):
        for m in s:
            self.make_move(m)

    def get_legal_move_names(self):
        """ Returns a string of the names of moves legal on this board.
        """
        moves = []
        if self.blank_row > 0: 
            moves.append('U')
        if self.blank_row < self.n - 1: 
            moves.append('D')
        if self.blank_col > 0: 
            moves.append('L')
        if self.blank_col < self.n - 1: 
            moves.append('R')
        return moves
                        
    def child_boards(self):
        """ Returns a list of Board objects representing the next states.
        """
        result = []
        for m in self.get_legal_move_names():
            b = self.copy()
            b.make_move(m)
            result.append(b)
        return result
        
    def make_one_random_move(self):
        move = random.choice(self.get_legal_move_names())
        self.make_move(move)
        return move

    def scramble(self, n = None):
        if n == None:
            n = (self.n + 2) ** 2
        moves = [self.make_one_random_move() for i in xrange(n)]
        return moves

    def tiles_at_home(self):
        count = 0
        for i in xrange(self.n * self.n):
            if self.board[i] == i:
                count += 1
        return count

    def misplaced(self):
        return self.n * self.n - 1 - self.tiles_at_home()

    def tile_index(self, tile):
        """ Returns the index of tile self.board.
        Could be made more efficient using a dictionary?
        """
        for i, t in enumerate(self.board):
            if t == tile:
                return i
        assert 1 == 2, 'error in tile_index'

    def tile_pos(self, tile):
        """ Returns the (row, col) position of tile.
        """
        ti = self.tile_index(tile)
        row = ti / self.n
        col = ti % self.n
        return row, col

    def tile_manhattan(self, tile):
        """ Return the Manhattan distance of tile to its home location.
        """
        r, c = self.tile_pos(tile)
        hr, hc = tile / self.n, tile % self.n
        return abs(r - hr) + abs(c - hc)

    def manhattan_score(self):
        """ Return the sum of the Manhattan scores of all the tiles.
        """
        return sum(self.tile_manhattan(tile) for tile in self.board 
		   if tile != -1)
                    
    def display(self):
        for i in xrange(self.n * self.n):
            if self.board[i] == -1:
                print ' * ',
            else:
                print '%2s ' % self.board[i],    
            if (i + 1) % self.n == 0: print
        print 'blank = (%s, %s), misplaced = %s' % (self.blank_row, self.blank_col,
                                                    self.misplaced())
        print 'Manhattan score = %s' % (self.manhattan_score())

