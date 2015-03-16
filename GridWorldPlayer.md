The GridWorldPlayer is a simple tool for displaying routes through mazes. You give it a rectangular grid of walls and empty spaces as input, including the starting location of the robot that will be moving through the maze. Then you can specify a string of commands that will make an animated triangle move through the maze.

For example:

```
$ python gridworld.py grid25.txt uuulllu
```

The code for these examples is available in [gridworld.py](http://csed.googlecode.com/svn/trunk/310summer2007/gridworld/gridworld.py) and
[grid25.txt](http://csed.googlecode.com/svn/trunk/310summer2007/gridworld/grid25.txt).

The command string `uuullu` moves the robot up three times, then left three times, and then up once more. If the robot can't move due to a wall, then it stays where it is.

This tool is a quick and dirty piece of software based on Python's turtle graphics. Thus, the animated turtle's size never changes, which can be awkward in very large or very small mazes. It also lacks any significant error checking or fancy features; so if you want to do more than what it does out of the box, you will need to read the source code. Fortunately, it is quite straightforward.


