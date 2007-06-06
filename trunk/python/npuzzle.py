# npuzzle.py

# represents an n-by-n n-puzzle board
class Board(object):
    def __init__(self, n):
        assert n > 2
        self.board = [i for i in xrange(n * n)]
        self.board[-1] = -1   # -1 represents the blank
        self.blank_at = n * n - 1
        self.blank_row = n - 1
        self.blank_col = n - 1
        self.manhattan = 0  # running calculation of Manhattan score
        self.n = n

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

    def move_down(self):
        if self.blank_row < self.n - 1:
            new_blank = self.blank_at + self.n
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_row += 1

    def move_left(self):
        if self.blank_col > 0:
            new_blank = self.blank_at - 1
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_col -= 1

    def move_right(self):
        if self.blank_col < self.n - 1:
            new_blank = self.blank_at + 1
            self.swap(self.blank_at, new_blank)
            self.blank_at = new_blank
            self.blank_col += 1

    def legal_moves(self):
        """ Returns a list of the legal move methods for this object.
        """
        moves = []
        if self.blank_row > 0:
            moves.append(self.move_up)
        if self.blank_row < self.n - 1:
            moves.append(self.move_down)
        if self.blank_col > 0:
            moves.append(self.move_left)
        if self.blank_col < self.n - 1:
            moves.append(self.move_right)
        return moves

    def make_one_random_move(self):
        import random
        move_fn = random.choice(self.legal_moves())
        move_fn()        

    def scramble(self, n = 25):
        for i in xrange(n):
            self.make_one_random_move()        

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
        Calling self.tile_pos is somewhat inefficient. Better would be
        to calculate the Manhattan distances incrementally after each move.
        """
        r, c = self.tile_pos(tile)
        hr, hc = tile / self.n, tile % self.n
        return abs(r - hr) + abs(c - hc)

    def manhattan_score(self):
        """ Return the sum of the Manhattan scores of all the tiles.
        """
        score = 0
        for tile in self.board:
            if tile != -1:
                score += self.tile_manhattan(tile)
        return score
                    
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

def random_solver(n, mix_amount = 50):
    """ Tries solving a random n-puzzle by making random moves.
    """
    b = Board(n)
    b.scramble(mix_amount)
    print 'Starting board (%s random moves):' % mix_amount
    b.display()
    print '\nSearching for solution ...\n'
    count = 0
    while b.misplaced() > 0:
        b.make_one_random_move()
        count += 1
    print 'Found a solution after %s moves!' % count
    print 'Goal board:'
    b.display()
