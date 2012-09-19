from pyth.plugins.rtf15.reader import Rtf15Reader
from pyth.plugins.xhtml.writer import XHTMLWriter
import sys

if __name__ == '__main__':
    with open(sys.argv[1]) as doc:
        print XHTMLWriter.write(Rtf15Reader.read(doc)).getvalue()
