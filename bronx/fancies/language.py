# -*- coding: utf-8 -*-

"""
Various tools related to languages.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import locale
import re

#: No automatic export
__all__ = []

_VOWELS = set('aeiou')

_EN_ABERRANT_PLURAL_MAP = {
    'appendix': 'appendices',
    'barracks': 'barracks',
    'cactus': 'cacti',
    'child': 'children',
    'criterion': 'criteria',
    'deer': 'deer',
    'echo': 'echoes',
    'elf': 'elves',
    'embargo': 'embargoes',
    'focus': 'foci',
    'fungus': 'fungi',
    'goose': 'geese',
    'hero': 'heroes',
    'hoof': 'hooves',
    'index': 'indices',
    'knife': 'knives',
    'leaf': 'leaves',
    'life': 'lives',
    'man': 'men',
    'mouse': 'mice',
    'nucleus': 'nuclei',
    'person': 'people',
    'phenomenon': 'phenomena',
    'potato': 'potatoes',
    'self': 'selves',
    'syllabus': 'syllabi',
    'tomato': 'tomatoes',
    'torpedo': 'torpedoes',
    'veto': 'vetoes',
    'woman': 'women',
}


class Pluralise(object):
    """Given a singular word, returns the plural form.

    We try to preserve the case as much as possible

    English example::

        >>> pl = Pluralise(curlocale='en_GB')
        >>> print(pl(''))
        <BLANKLINE>
        >>> print(pl('Goose'))
        Geese
        >>> print(pl('DOLLY'))
        DOLLIES
        >>> print(pl('GEnius'))
        GEnii
        >>> print(pl('jones'))
        joneses
        >>> print(pl('pass'))
        passes
        >>> print(pl('zero'))
        zeros
        >>> print(pl('casino'))
        casinos
        >>> print(pl('hero'))
        heroes
        >>> print(pl('church'))
        churches
        >>> print(pl('x'))
        xs
        >>> print(pl('car'))
        cars

    For now, only the english language is support::

        >>> pl = Pluralise(curlocale='fr_FR')
        Traceback (most recent call last):
        ...
        ValueError: This class does not supports the "fr_FR" locale

    Single words are the only accepted values::

        >>> pl = Pluralise(curlocale='en_US')
        >>> pl("This is a complete sentence")
        Traceback (most recent call last):
        ...
        ValueError: "This is a complete sentence" is not a single word

    """

    _locale_mapping = dict()
    _language_mapping = dict(en='_en_pluralise')

    def __init__(self, curlocale=None):
        """
        :param curlocale: The locale to be considered. If ``None``, the object
                          will try to guess the current locale using the
                          :mod:`locale` package.
        """
        if curlocale is None:
            curlocale = locale.getlocale()[0]
            if curlocale is None:
                curlocale = locale.getdefaultlocale()[0]
            if curlocale is None:
                raise ValueError('Unable to detect the current locale')
        pl_mtd = self._locale_mapping.get(curlocale, None)
        if pl_mtd is None:
            pl_mtd = self._language_mapping.get(curlocale.split('_')[0], None)
        if pl_mtd is None:
            raise ValueError('This class does not supports the "{:s}" locale'.format(curlocale))
        else:
            self._pl_mtd = getattr(self, pl_mtd)

    def __call__(self, singular):
        """Return the plural form of **singular**."""
        if re.search(r'[^\w]+', singular):
            raise ValueError('"{:s}" is not a single word'.format(singular))
        if not singular:
            return ''
        is_last_uc = singular[-1].isupper()
        plural_lc = self._pl_mtd(singular.lower())
        plural = ''
        for i, c in enumerate(plural_lc):
            if i < len(singular):
                plural += c.upper() if singular[i].isupper() else c
            else:
                plural += c.upper() if is_last_uc else c
        return plural

    def _en_pluralise(self, singular):
        """Return plural form of given lowercase singular word (English only).

        Based on https://code.activestate.com/recipes/577781-pluralize-word-convert-singular-word-to-its-plural/
        """
        plural = _EN_ABERRANT_PLURAL_MAP.get(singular, None)
        if plural:
            return plural
        root = singular
        try:
            if singular[-1] == 'y' and singular[-2] not in _VOWELS:
                root = singular[:-1]
                suffix = 'ies'
            elif singular[-1] == 's':
                if singular[-2] in _VOWELS:
                    if singular[-3:] == 'ius':
                        root = singular[:-2]
                        suffix = 'i'
                    else:
                        root = singular[:-1]
                        suffix = 'ses'
                else:
                    suffix = 'es'
            elif singular[-2:] in ('ch', 'sh'):
                suffix = 'es'
            else:
                suffix = 's'
        except IndexError:
            suffix = 's'
        plural = root + suffix
        return plural


if __name__ == '__main__':
    import doctest
    doctest.testmod()
