# arena.py

import sys, os
import pygame
from pygame.locals import *

clock = pygame.time.Clock()
if not pygame.font: print 'Warning: fonts disabled'
if not pygame.mixer: print 'Warning: sound disabled'
black = 0, 0, 0
red = 255, 0, 0

def load_image(name, colorkey=None):
    """ Reads an image from the /data folder.
    """
    fullname = os.path.join('data', name)
    try:
        image = pygame.image.load(fullname)
    except pygame.error, message:
        print 'Cannot load image:', name
        raise SystemExit, message
    if image.get_alpha() is None:
        image = image.convert()
    else:
        image = image.convert_alpha()
    if colorkey is not None:
        if colorkey is -1:
            colorkey = image.get_at((0,0))
        image.set_colorkey(colorkey, RLEACCEL)
    return image, image.get_rect()

def load_sound(name):
    """ Reads a sound file from the /data folder.
    """
    class NoneSound:
        def play(self): pass
    if not pygame.mixer:
        return NoneSound()
    fullname = os.path.join('data', name)
    try:
        sound = pygame.mixer.Sound(fullname)
    except pygame.error, message:
        print 'Cannot load sound:', wav
        raise SystemExit, message
    return sound

class Bouncing_ball(pygame.sprite.Sprite):
    def __init__(self, screen_width, screen_height, 
                 start_x = 200, start_y = 200,
                 image_file = 'create.png',  #'tennis-ball.png',
                 scale_factor = 1.0, speed = [1, 1], 
                 rotate = True, bounding_box = False):
        assert 0.0 < scale_factor, '0.0 < scale_factor'
        pygame.sprite.Sprite.__init__(self)

        self.raw_image, self.raw_rect = load_image(image_file)
        self.image = None #self.raw_image.copy()

        self.speed = speed
        self.screen_width, self.screen_height = screen_width, screen_height
        self.rotate = rotate
        self.bounding_box = bounding_box

        self.scale_factor = scale_factor
        self.i = 0  # image counter
 
        self.make_rotated_images()
        self.rect = self.rotated_image[0].get_rect(center=(start_x, start_y))


    def make_rotated_images(self):
        num_images = 10
        self.rotated_image = range(360/num_images)
        #print '\n%s rotated images' % len(self.rotated_image)
        for i in xrange(len(self.rotated_image)):
            #print 'i = %s (%s deg)' % (i, i*num_images)
            self.rotated_image[i] = pygame.transform.rotozoom(self.raw_image, 
                                                              i*num_images,
                                                              self.scale_factor)
            if self.bounding_box:
                self.img = self.rotated_image[i]
                pygame.draw.rect(self.img, red, self.img.get_rect(), 1)

        self.rect0 = self.rotated_image[0].get_rect()
  
    def update(self):
        center = self.rect.move(self.speed).center
        self.image = self.rotated_image[self.i if self.rotate else 0]       
        self.i = (self.i + 1) % len(self.rotated_image)
        self.rect = self.image.get_rect(center=center)

        # self.rect0 is the rectangle of the first stored image
        if self.rect.centerx - self.rect0.width/2 < 0 or \
           self.rect.centerx + self.rect0.width/2 > self.screen_width:
               self.speed[0] = -self.speed[0]
        if self.rect.centery - self.rect0.height/2 < 0 or \
           self.rect.centery + self.rect0.height/2 > self.screen_height:
               self.speed[1] = -self.speed[1]

def main():
    pygame.init()    # initialize pygame (must always be called)
    size = width, height = 800, 600   # screen dimensions

    screen = pygame.display.set_mode(size)   # initialize the screen

    # create sprites
    ball = Bouncing_ball(width, height, rotate=True, bounding_box=False)
    ball2 = Bouncing_ball(width, height, scale_factor=0.5, speed=[2, 1],
                          rotate=False, bounding_box=True)
    allsprites = pygame.sprite.RenderPlain((ball, 
                                            ball2
                                            ))

    while True:
        clock.tick(60)  # 60fps max
        for event in pygame.event.get():
            if event.type == pygame.QUIT: sys.exit()

        allsprites.update()      # update sprite states

        screen.fill(black)       # paint entire screen black
        allsprites.draw(screen)  # draw sprites
        pygame.display.flip()    # display the screen


if __name__ == '__main__':
    main()
