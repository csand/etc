#!/usr/bin/env python

import sys
import json
from plistlib import readPlist
import StringIO

plist = open(sys.argv[1], "r").read()

in_file = StringIO.StringIO(plist)
plist_dict = readPlist(in_file)

open(sys.argv[1] + ".json", "w").write(json.dumps(plist_dict))
