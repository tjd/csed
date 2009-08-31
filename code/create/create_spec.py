"""

The following measurements were made by hand on an iRobot Create. They
are approximate to about 1cm.

Diameter: 33cm

The front bumper sticks out about 1cm (when not depressed).
So:          area = 855cm^2
    circumference = 104cm


Height: 6cm

It's slightly convex, so the sides are a little bit lower than the
middle.


Wheels: 4cm wide
        7cm long

        24cm between insides edges of two wheel
        .5cm from outside wheel edge to bot edge

        16cm from wheel bottom to bottom edge of bot measured
             perpendicularly

"""


operating_modes = ('off', 'passive', 'safe', 'full')
power_light_modes = ('off', 'slow_pulsing_orange', 'fast_pulsing_orange',
                     'green', 'amber', 'red', 'flashing_red')

class led(object):
    def __init__(self, states):
        self.possible_states = states
        self.state = states[0]



class cbot(object):
    def __init__(self):
        self.mode = 'off'
        self.power_led = 'off'
        self.play_led = 'off'
        self.advance_led = 'off'

    def press_power_button(self):
        self.mode = 'passive' if self.mode == 'off' else 'off'

    def press_play_button(self):
        pass

    def press_advance_button(self):
        pass
