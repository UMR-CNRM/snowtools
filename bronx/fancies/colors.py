# -*- coding: utf-8 -*-

"""
Making things pretty and flashy.

TODO: Add an usage example
TODO: Add a simple unittest
"""

from __future__ import print_function, absolute_import, unicode_literals, division
import six

import re

#: No automatic export
__all__ = []


class termcolors(object):

    textset = dict(
        bold=1,
        dim=2,
        underlined=4,
        blink=5,
        reverse=7,
        hidden=8,
    )

    textreset = dict(
        default=0,
        bold=21,
        dim=22,
        underlined=24,
        blink=25,
        reverse=27,
        hidden=28,
    )

    foreground = dict(
        default=39,
        black=30,
        red=31,
        green=32,
        yellow=33,
        blue=34,
        magenta=35,
        cyan=36,
        lightgray=37,
        darkgray=90,
        lightred=91,
        lightgreen=92,
        lightyellow=93,
        lightblue=94,
        lightmagenta=95,
        lightcyan=96,
        white=97,
    )

    background = dict(
        default=49,
        black=40,
        red=41,
        green=42,
        yellow=43,
        blue=44,
        magenta=45,
        cyan=46,
        lightgray=47,
        darkgray=100,
        lightred=101,
        lightgreen=102,
        lightyellow=103,
        lightblue=104,
        lightmagenta=105,
        lightcyan=106,
        white=107,
    )

    @classmethod
    def clean(cls, text):
        return re.sub(r'\\033\[\d+(?:;\d+)*m', '', text)

    @classmethod
    def colored(cls, text, fgcolor=None, bgcolor=None, setfont=None):
        if any((setfont, fgcolor, bgcolor)):
            settings = list()
            if setfont is not None:
                settings.append(cls.textset.get(setfont.replace(' ', '').lower()))
            if fgcolor is not None:
                settings.append(cls.foreground.get(fgcolor.replace(' ', '').lower()))
            if bgcolor is not None:
                settings.append(cls.background.get(bgcolor.replace(' ', '').lower()))
            text = '\033[' + ';'.join([str(x) for x in settings]) + 'm' + six.text_type(text) + '\033[0m'
        return text

    @classmethod
    def markup(cls, text, item, enter_map, exit_map=None):
        if exit_map is None:
            exit_map = enter_map
        item = item.replace(' ', '').lower()
        return '\033[{0:s}m{1:s}\033[{2:s}m'.format(
            six.text_type(enter_map.get(item)),
            six.text_type(text),
            six.text_type(exit_map.get('default'))
        )

    @classmethod
    def setfont(cls, text, item):
        return cls.markup(text, item, cls.textset, cls.textreset)

    @classmethod
    def fgcolor(cls, text, item):
        return cls.markup(text, item, cls.foreground)

    @classmethod
    def bgcolor(cls, text, item):
        return cls.markup(text, item, cls.background)

    @classmethod
    def header(cls, text):
        return cls.colored(text, fgcolor='lightmagenta', setfont='bold')

    @classmethod
    def debug(cls, text):
        return cls.colored(text, fgcolor='lightblue', setfont='bold')

    @classmethod
    def info(cls, text):
        return cls.colored(text, fgcolor='white', setfont='bold')

    @classmethod
    def warning(cls, text):
        return cls.colored(text, fgcolor='lightyellow', setfont='bold')

    @classmethod
    def error(cls, text):
        return cls.colored(text, fgcolor='lightmagenta', setfont='bold')

    @classmethod
    def critical(cls, text):
        return cls.colored(text, fgcolor='lightred', setfont='bold')

    @classmethod
    def okblue(cls, text):
        return cls.colored(text, fgcolor='lightblue', setfont='bold')

    @classmethod
    def okgreen(cls, text):
        return cls.colored(text, fgcolor='lightgreen', setfont='bold')

    @classmethod
    def ok(cls, text):
        return cls.okgreen(text)

    @classmethod
    def dim(cls, text):
        return cls.setfont(text, 'dim')

    @classmethod
    def emph(cls, text):
        return cls.setfont(text, 'underlined')

    @classmethod
    def strong(cls, text):
        return cls.setfont(text, 'bold')

    @classmethod
    def reverse(cls, text):
        return cls.setfont(text, 'reverse')

    @classmethod
    def blink(cls, text):
        return cls.setfont(text, 'blink')

    @classmethod
    def hidden(cls, text):
        return cls.setfont(text, 'hidden')
