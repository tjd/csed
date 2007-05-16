# sound.py

from math import asin, pi, degrees

# See http://www.aip.org/pt/nov99/locsound.html
#
#  t = 3a/c sin theta
#
# Therefore:
#    ct / 3a = sin theta
#    theta = asin(ct/3a), where ct/3a < 1  (assuming t >= 0)
#                         or t < 3a/c = 0.000763 using default a, c values
#

# a is the radius (e.g. of the head); 8.75cm for people
# c is the speed of sound 34,400 cm/s
def angle_from(dt, c = 34400, a = 8.75):
    x = c * dt / (3 * a)
    return asin(x)

