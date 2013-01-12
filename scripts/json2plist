#!/usr/bin/env python

import sys
import json
from plistlib import writePlist
import StringIO

json_file = open(sys.argv[1], "r").read()

in_file = StringIO.StringIO(json_file)
json_dict = json.load(in_file)

writePlist(json_dict, open(sys.argv[1] + ".plist", "w"))
