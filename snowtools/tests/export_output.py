# -*- coding: utf-8 -*-

import sys


class exportoutput:
    def __init__(self, filename):
        self.output = filename

    def __enter__(self):
        self.f = open(self.output, 'a')
        sys.stdout = self.f
        sys.stderr = self.f

    def __exit__(self, e_type, e_value, e_traceback):
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        self.f.close()
