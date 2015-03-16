#!/usr/bin/env python
from __future__ import print_function
import os

def truncate_cwd(begin, end):
    home_dir = os.environ['HOME']
    comps = os.getcwd().replace(home_dir, '~').split('/')
    return '/'.join(comps[:begin + 1] + ['...'] + comps[-(end):]
                    if (begin + end + 1) < len(comps)
                    else comps)

if __name__ == '__main__':
    print(truncate_cwd(1, 2))
