#!/usr/bin/env python
"""
img_concat

Python script to concatenate a series of images together, and write them out
in the same format as the first given image.
"""

from PIL import Image
import sys
import uuid


def main(*args):
    images = []
    for filename in args:
        images.append(Image.open(filename).convert('RGBA'))

    new_width = 0
    for image in images:
        width, height = image.size
        new_width += width

    first_image = images[0]
    next_image_start, height = first_image.size
    new_dimensions = (0, 0, new_width, height)  # (startx, starty, endx, endy)
    new_image = first_image.crop(new_dimensions)  # actually expands it

    for image in images[1:]:
        print next_image_start
        new_image.paste(image, (next_image_start, 0))
        next_image_start += image.size[0]

    new_image.save(uuid.uuid1() + '.png', 'PNG')

if __name__ == '__main__':
    main(sys.argv[1:])
