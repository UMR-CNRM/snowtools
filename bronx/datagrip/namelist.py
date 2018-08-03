#!/usr/bin/env python
#  -*- coding: utf-8 -*-

"""
This module contains:

* The :class:`LiteralParser` class in charge of type conversion
  between FORTRAN literals, represented as strings, and corresponding
  python types. A detailed exemple is given in the class's documentation.
* The :class:`NamelistBlock` class that contains a single namelist and allows
  to modify its content.
* The :class:`NamelistSet` class that holds a collection of namelist blocks
  (described by :class:`NamelistBlock` objects).
* The :class:`NamelistParser` class that parse a string, a physical file or
  File-like object and look for namelist blocks. Upon success, it returns a
  :class:`NamelistSet` object

Inital author: Joris Picot (2010-12-08 / CERFACS)
"""

from __future__ import print_function, absolute_import, division, unicode_literals
import six

import collections
import copy
from decimal import Decimal
import io
import re

import six
import numpy

#: No automatic export
__all__ = []

_RE_FLAGS = re.IGNORECASE + re.DOTALL

# Processor
_LETTER             = "[A-Z]"
_DIGIT              = "[0-9]"
_UNDERSCORE         = "[_]"
_LETTER_UNDERSCORE  = "[A-Z_]"
_SPECIAL_CHARACTERS = "[ =+-*/(),.':!\"%&;<>?$]"
_OTHER_CHARACTERS   = "[^A-Z0-9_ =+-*/(),.':!\"%&;<>?$]"
_GRAPHIC_CHARACTERS = ".|\n"

_ALPHANUMERIC_CHARACTER = "[A-Z0-9_]"
_CHARACTER              = "[A-Z0-9_ =+-*/(),.':!\"%&;<>?$]"

# Low-lever
_QUOTE      = "'"
_DQUOTE     = '"'
_STRDELIM_B = "(?P<STRB>[" + _QUOTE + _DQUOTE + "])"
_STRDELIM_E = "(?(STRB)[" + _QUOTE + _DQUOTE + "])"
_NAME       = _LETTER + _ALPHANUMERIC_CHARACTER + '*'
_MACRONAME  = (_STRDELIM_B + r'?\$?' +
               "(?P<NAME>" + _LETTER_UNDERSCORE + _ALPHANUMERIC_CHARACTER + '*' + ")" +
               _STRDELIM_E)
_FREEMACRONAME  = (_STRDELIM_B + r'?' + _UNDERSCORE + r'{2}' +
                   "(?P<NAME>" + _LETTER + _ALPHANUMERIC_CHARACTER + '*' + ")" +
                   _UNDERSCORE + r'{2}' + _STRDELIM_E)

# Operators
_POWER_OP  = "[*][*]"
_MULT_OP   = "[*/]"
_ADD_OP    = "[+-]"
_CONCAT_OP = "[/][/]"
_REL_OP    = r"\.EQ\.|\.NE\.|\.LT\.|\.GT\.|\.GE\.|[=][=]|[/][=]|[<]|[<][=]|[>]|[>][=]"
_NOT_OP    = r"\.NOT\."
_AND_OP    = r"\.AND\."
_OR_OP     = r"\.OR\."
_EQUIV_OP  = r"\.EQV\.|\.NEQV\."
_INTRINSIC_OPERATOR = '|'.join((_POWER_OP, _MULT_OP, _ADD_OP, _CONCAT_OP, _REL_OP, _NOT_OP,
                                _AND_OP, _OR_OP, _EQUIV_OP))

# Labels
_LABEL = _DIGIT + "{1,5}"

# Integers
_SIGN                        = "[+-]"
_DIGIT_STRING                = _DIGIT + "+"
_SIGNED_DIGIT_STRING         = _SIGN + "?" + _DIGIT_STRING
_KIND_PARAM                  = "[A-Z0-9]+"
_INT_LITERAL_CONSTANT        = _DIGIT_STRING + "(?:_" + _KIND_PARAM + ")?"
_SIGNED_INT_LITERAL_CONSTANT = _SIGN + "?" + _INT_LITERAL_CONSTANT

# BOZ
_BINARY_DIGIT         = "[0-1]"
_OCTAL_DIGIT          = "[0-7]"
_HEX_DIGIT            = "[ABCDEF0-9]"
_BINARY_CONSTANT      = "B" + "(?:'|\")" + _BINARY_DIGIT + "+" + "(?:'|\")"
_OCTAL_CONSTANT       = "O" + "(?:'|\")" + _OCTAL_DIGIT + "+" + "(?:'|\")"
_HEX_CONSTANT         = "Z" + "(?:'|\")" + _HEX_DIGIT + "+" + "(?:'|\")"
_BOZ_LITERAL_CONSTANT = "(?:" + _BINARY_CONSTANT + "|" + _OCTAL_CONSTANT + "|" + _HEX_CONSTANT + ")"

# Real
_SIGNIFICAND                  = "(?:" + _DIGIT_STRING + r"\." + "(?:" + _DIGIT_STRING + ")?" + \
                                "|" + r"\." + _DIGIT_STRING + ")"
_EXPONENT_LETTER              = "[DE]"
_EXPONENT                     = _SIGNED_DIGIT_STRING
_REAL_LITERAL_CONSTANT        = "(?:" + _SIGNIFICAND + "(?:" + _EXPONENT_LETTER + _EXPONENT + \
                                ")?" + "(?:_" + _KIND_PARAM + ")?" + "|" + _DIGIT_STRING +    \
                                _EXPONENT_LETTER + _EXPONENT + "(?:_" + _KIND_PARAM + ")?" + ")"
_SIGNED_REAL_LITERAL_CONSTANT = _SIGN + "?" + _REAL_LITERAL_CONSTANT

# Complex
_REAL_PART                = "(?:" + _SIGNED_INT_LITERAL_CONSTANT + "|" + \
                            _SIGNED_REAL_LITERAL_CONSTANT + ")"
_IMAG_PART                = "(?:" + _SIGNED_INT_LITERAL_CONSTANT + "|" + \
                            _SIGNED_REAL_LITERAL_CONSTANT + ")"
_COMPLEX_LITERAL_CONSTANT = "[(]" + _REAL_PART + "," + _IMAG_PART + "[)]"

# Character
_CHAR_LITERAL_CONSTANT = "(?:" + "(?:" + _KIND_PARAM + "_)?" + "'[^']*'" + "|" + "(?:" + \
                         _KIND_PARAM + "_)?" + "\"[^\"]*\"" + ")"

# Logical
_LOGICAL_LITERAL_CONSTANT = "(?:" + r"\.TRUE\." + "(?:_" + _KIND_PARAM + ")?" + "|" + \
                            r"\.FALSE\." + "(?:_" + _KIND_PARAM + ")?" + "|" + \
                            "[TF]" + ")"

# Constants
_LITERAL_CONSTANT = "(?:" + _SIGNED_INT_LITERAL_CONSTANT + "|" + _BOZ_LITERAL_CONSTANT + "|" + \
                    _SIGNED_REAL_LITERAL_CONSTANT + "|" + _COMPLEX_LITERAL_CONSTANT + "|" + \
                    _CHAR_LITERAL_CONSTANT + "|" + _LOGICAL_LITERAL_CONSTANT + ")"

#: Do not sort anything
NO_SORTING = 0
#: Sort all keys
FIRST_ORDER_SORTING = 1
#: Sort only within indexes or attributes of the same key.
SECOND_ORDER_SORTING = 2


class LiteralParser(object):
    """
    Object in charge of parsing literal fortran expressions that could be found
    in a namelist.

    For each literal type (integer, boz, real, complex, character and
    logical), there is a corresponding function parse_* and a global
    parser, simply called :meth:`parse`, choose automatically the
    appropriate literal type. Here is the type conversion table:

    * integer   -> int
    * boz       -> int
    * real      -> float or Decimal
    * complex   -> complex
    * character -> string
    * logical   -> bool

    For python data, functions are provided for conversion into FORTRAN
    literals through a LiteralParser. Each literal type has its encode_*
    function and a global encoder, simply called :meth:`encode`, chooses
    automatically the appropriate encoder; python integers will be converted
    into a FORTRAN integer, hence the only way to produce a FORTRAN boz is
    to use encode_boz directly.

    :example: A :class:`LiteralParser` can easily be created:

        >>> lp = LiteralParser()
        >>> lp #doctest: +ELLIPSIS
        <bronx.datagrip.namelist.LiteralParser object at 0x...>

        Basic fortran types could be checked agains a string value:

        >>> lp.check_integer('2')
        True
        >>> lp.check_integer('2.')
        False
        >>> lp.check_integer('2x')
        False
        >>> lp.check_real('1.2E-6')
        True
        >>> lp.check_logical('.T.')
        False
        >>> lp.check_logical('.True.')
        True
        >>> lp.check_complex('(2.,0.)')
        True
        >>> lp.check_boz("B'11'")
        True

        After a successful type check, one can parse a specific fortran type
        according to the corresponding method of the :class:`LiteralParser`:

        >>> s = '1.2E-6'
        >>> lp.check_real(s)
        True
        >>> x = lp.parse_real(s)
        >>> print(x)
        0.0000012

        It could be more convenient to use the generic :meth:`parse` method:

        >>> x = lp.parse('.true.')
        >>> print(x)
        True
        >>> type(x).__name__
        'bool'
        >>> x = lp.parse('2.')
        >>> print(x)
        2
        >>> type(x)
        <class 'decimal.Decimal'>
        >>> x = lp.parse('Z"1F"')
        >>> print(x)
        31
        >>> type(x).__name__
        'int'

        The reverse operation could be achieved through a specific encodingfunction:

        >>> x = 2
        >>> print(lp.encode_real(x))
        2.
        >>> print(lp.encode_integer(x))
        2
        >>> print(lp.encode_complex(x))
        (2.,0.)
        >>> print(lp.encode_logical(x))
        .TRUE.

        It is possible to rely on the internal python type to decide which
        is the appropriate encoding through the generic :meth:`encode` method:

        >>> x = 2
        >>> print(lp.encode(x))
        2
        >>> z = 1 - 2j
        >>> print(lp.encode(z))
        (1.,-2.)
        >>> x = 2.
        >>> print(lp.encode(x))
        2.

    """
    def __init__(self,
                 re_flags     = _RE_FLAGS,
                 re_integer   = '^' + _SIGNED_INT_LITERAL_CONSTANT + '$',
                 re_boz       = '^' + _BOZ_LITERAL_CONSTANT + '$',
                 re_real      = '^' + _SIGNED_REAL_LITERAL_CONSTANT + '$',
                 re_complex   = '^' + _COMPLEX_LITERAL_CONSTANT + '$',
                 re_character = '^' + _CHAR_LITERAL_CONSTANT + '$',
                 re_logical   = '^' + _LOGICAL_LITERAL_CONSTANT + '$',
                 re_true      = r'\.T(?:RUE)?\.|T',
                 re_false     = r'\.F(?:ALSE)?\.|F'):
        self._re_flags     = re_flags
        self._re_integer   = re_integer
        self._re_boz       = re_boz
        self._re_real      = re_real
        self._re_complex   = re_complex
        self._re_character = re_character
        self._re_logical   = re_logical
        self._re_true      = re_true
        self._re_false     = re_false
        self._log = list()
        self._recompile()

    def _recompile(self):
        """Recompile regexps according to internal characters strings by literal types."""
        self.integer = re.compile(self._re_integer, self._re_flags)
        self.boz = re.compile(self._re_boz, self._re_flags)
        self.real = re.compile(self._re_real, self._re_flags)
        self.complex = re.compile(self._re_complex, self._re_flags)
        self.character = re.compile(self._re_character, self._re_flags)
        self.logical = re.compile(self._re_logical, self._re_flags)
        self.true = re.compile(self._re_true, self._re_flags)
        self.false = re.compile(self._re_false, self._re_flags)

    # Fast check

    def check_integer(self, string):
        """Returns True if ``string`` could be an integer."""
        return bool(self.integer.match(string))

    def check_boz(self, string):
        """Returns True if ``string`` could be a binary, octal or hexa number."""
        return bool(self.boz.match(string))

    def check_real(self, string):
        """Returns True if ``string`` could be a real number."""
        return bool(self.real.match(string))

    def check_complex(self, string):
        """Returns True if ``string`` could be a complex number."""
        return bool(self.complex.match(string))

    def check_character(self, string):
        """Returns True if ``string`` could be a character string."""
        return bool(self.character.match(string))

    def check_logical(self, string):
        """Returns True if ``string`` could be a logical value."""
        return bool(self.logical.match(string))

    # Atomic type parsing

    def parse_integer(self, string):
        """If the argument looks like a FORTRAN integer, returns the matching python integer."""
        if self.integer.match(string):
            # Removes the kind parameter.
            cleaned_string = re.sub("_" + _KIND_PARAM, "", string, self._re_flags)
            return int(cleaned_string)
        raise ValueError("Literal %s doesn't represent a FORTRAN integer" % string)

    def parse_boz(self, string):
        """If the argument looks like a FORTRAN boz, returns the matching python integer."""
        if self.boz.match(string):
            if string[0] == "B":
                return int(string[2:-1], 2)
            elif string[0] == "O":
                return int(string[2:-1], 8)
            elif string[0] == "Z":
                return int(string[2:-1], 16)
        raise ValueError("Literal %s doesn't represent a FORTRAN boz" % string)

    def parse_real(self, string):
        """If the argument looks like a FORTRAN real, returns the matching python float."""
        if self.real.match(string):
            # Removes the kind parameter.
            string = re.sub("_" + _KIND_PARAM, "", string, self._re_flags)
            # Changes the exponent d to e.
            cleaned_string = re.sub("d|D", "E", string, self._re_flags)
            return Decimal(cleaned_string)
        raise ValueError("Literal %s doesn't represent a FORTRAN real" % string)

    def parse_complex(self, string):
        """If the argument looks like a FORTRAN complex, returns the matching python complex."""
        if self.complex.match(string):
            # Splits real and imag parts.
            (real_string, imag_string) = string[1:-1].split(',')
            # Parse real part
            if self.integer.match(real_string):
                real = self.parse_integer(real_string)
            else:
                real = self.parse_real(real_string)
            # Parse imag part
            if self.integer.match(imag_string):
                imag = self.parse_integer(imag_string)
            else:
                imag = self.parse_real(imag_string)
            return complex(real, imag)
        raise ValueError("Literal %s doesn't represent a FORTRAN complex" % string)

    def parse_character(self, string):
        """If the argument looks like a FORTRAN character, returns the matching python string."""
        if self.character.match(string):
            # Removes the kind parameter.
            cleaned_string = re.sub("^" + _KIND_PARAM + "_", "", string, self._re_flags)
            return cleaned_string[1:-1]
        raise ValueError("Literal %s doesn't represent a FORTRAN character" % string)

    def parse_logical(self, string):
        """If the argument looks like a FORTRAN logical, returns the matching python boolean."""
        if self.logical.match(string):
            # Removes the kind parameter.
            cleaned_string = re.sub("_" + _KIND_PARAM, "", string, self._re_flags)
            if self.true.match(cleaned_string):
                return True
            elif self.false.match(cleaned_string):
                return False
            else:
                raise ValueError("Literal %s is a weirdFORTRAN logical" % cleaned_string)
        raise ValueError("Literal %s doesn't represent a FORTRAN logical" % string)

    def parse(self, string):
        """
        Parse a FORTRAN literal and returns the corresponding python
        type. Resolution order is: integer, boz, real, complex, character
        and logical.
        """
        if self.check_integer(string):
            return self.parse_integer(string)
        elif self.check_boz(string):
            return self.parse_boz(string)
        elif self.check_real(string):
            return self.parse_real(string)
        elif self.check_complex(string):
            return self.parse_complex(string)
        elif self.check_character(string):
            return self.parse_character(string)
        elif self.check_logical(string):
            return self.parse_logical(string)
        else:
            raise ValueError("Literal %s doesn't represent a FORTRAN literal" % string)

    # Python types envoding

    @staticmethod
    def encode_integer(value):
        """Returns the string form of the integer ``value``."""
        return six.text_type(value)

    @staticmethod
    def encode_boz(value):
        """Returns the string form of the BOZ ``value``."""
        return six.text_type(value)

    @staticmethod
    def encode_real(value):
        """Returns the string form of the real ``value``."""
        if value == 0.:
            real = '0.'
        else:
            if isinstance(value, numpy.float32):
                real = '{0:.7G}'.format(value).replace('E', 'D')
            else:
                real = '{0:.15G}'.format(value).replace('E', 'D')
        if '.' not in real:
            real = re.sub('D', '.0D', real)
            if '.' not in real:
                real += '.'
        return real.rstrip('0')

    def encode_complex(self, value):
        """Returns the string form of the complex ``value``."""
        return "(%s,%s)" % (self.encode_real(value.real), self.encode_real(value.imag))

    @staticmethod
    def encode_character(value):
        """Returns the string form of the character string ``value``."""
        if "'" in value and '"' in value:
            return '"%s"' % value.replace('"', '""')
        elif "'" in value:
            return '"%s"' % value
        elif '"' in value:
            return "'%s'" % value
        else:
            return "'%s'" % value

    @staticmethod
    def encode_logical(value):
        """Returns the string form of the logical ``value``."""
        if value:
            return '.TRUE.'
        else:
            return '.FALSE.'

    def encode(self, value):
        """Returns the string form of the specified ``value`` according to its type."""
        if isinstance(value, bool):
            return self.encode_logical(value)
        elif isinstance(value, int):
            return self.encode_integer(value)
        elif isinstance(value, float) or isinstance(value, numpy.float32):
            return self.encode_real(value)
        elif isinstance(value, Decimal):
            return self.encode_real(value)
        elif isinstance(value, complex):
            return self.encode_complex(value)
        elif isinstance(value, six.string_types):
            return self.encode_character(value)
        else:
            raise ValueError("Type %s cannot be FORTRAN encoded" % type(value))


class NamelistBlock(collections.MutableMapping):
    """
    This class represent a FORTRAN namelist block.

    This class defines all the methods of a usual Python's dictionary. The
    keys being the namelist's variable names.

    Macros are special values prefixed and suffixed with ``__`` (e.g. ``__MYMACRO__``)
    that can be substituted at any time using the :meth:`addmacro` method. NB: They
    need to be declared using the :meth:`addmacro` method prior to being used.

    :example: To create an empty :class:`NamelistBlock` object, just provide the name:

        >>> nb1 = NamelistBlock('MYNAM')
        >>> nb1 # doctest: +ELLIPSIS
        <bronx.datagrip.namelist.NamelistBlock object at 0x... | name=MYNAM len=0>

        From now and on, it's possible to play arround with the namelist block:

        >>> nb1['A'] = 1
        >>> nb1.B = 2.
        >>> nb1.text = 'MyBad'
        >>> print(nb1)
         &MYNAM
           A=1,
           B=2.,
           TEXT='MyBad',
         /
        <BLANKLINE>
        >>> for k in nb1:
        ...     print('Entry {:s}: Value is {!s}'.format(k, nb1[k]))
        ...
        Entry A: Value is 1
        Entry B: Value is 2.0
        Entry TEXT: Value is MyBad
        >>> for k, v in nb1.items():
        ...     print('Entry {:s}: Value is {!s}'.format(k, v))  # doctest: +ELLIPSIS
        ...
        Entry A: Value is [1]
        Entry B: Value is [2.0]
        Entry TEXT: Value is [...'MyBad']
        >>> del nb1.B

        An exemple of namelist blocks merge:

        >>> nb2 = NamelistBlock('MYNAM')
        >>> nb2.A = 3
        >>> nb2.todelete('text')
        >>> nb1.merge(nb2)
        >>> print(nb1)
         &MYNAM
           A=3,
         /
        <BLANKLINE>

        Macros can be defined in a namelist blocks using the __MACRONAME__ syntax.
        They can be substituted at anytime:

        >>> nb3 = NamelistBlock('MYNAM')
        >>> nb3.addmacro('MACRO_A')
        >>> nb3.A = '__MACRO_A__'
        >>> print(nb3)
         &MYNAM
           A=__MACRO_A__,
         /
        <BLANKLINE>
        >>> nb3.addmacro('MACRO_A', 3)
        >>> print(nb3)
         &MYNAM
           A=3,
         /
        <BLANKLINE>

    """

    _RE_FREEMACRO = re.compile(r'^' + _FREEMACRONAME + r'$')

    def __init__(self, name='UNKNOWN'):
        self.__dict__['_name'] = name.upper()
        self.__dict__['_keys'] = list()
        self.__dict__['_pool'] = dict()
        self.__dict__['_mods'] = set()
        self.__dict__['_dels'] = set()
        self.__dict__['_subs'] = dict()
        self.__dict__['_declared_subs'] = set()
        self.__dict__['_literal'] = None

    def __getstate__(self):
        """For deepcopy and pickle."""
        st = dict(self.__dict__)
        st['_literal'] = None  # It's recreated on the fly when needed...
        return st

    def __setstate__(self, state):
        """For deepcopy and pickle."""
        self.__dict__.update(state)

    def set_name(self, name):
        """Change the namelist block name."""
        self.__dict__['_name'] = name.upper()

    @property
    def name(self):
        """The namelist block name."""
        return self._name

    def __repr__(self):
        """Returns a formated id of the current namelist block, including number of items."""
        parent_repr = super(NamelistBlock, self).__repr__().rstrip('>')
        return '{0:s} | name={1:s} len={2:d}>'.format(parent_repr,
                                                      self.name,
                                                      len(self._pool))

    def __str__(self):
        """Returns a text dump of the namelist (see the :meth:`dumps` method)."""
        return self.dumps()

    def setvar(self, varname, value, index=None):
        """
        Insert or change a namelist block variable.

        :param str varname: the variable name
        :param value: the variable value
        :param int index: if given, set the key to the given index in block.
        """
        varname = varname.upper()
        if not isinstance(value, list):
            value = [value, ]
        # Automatically add free macros to the macro list
        for v in [self._RE_FREEMACRO.match(v) for v in value
                  if isinstance(v, six.string_types)]:
            if v and v.group('NAME') not in self.macros():
                self.addmacro(v.group('NAME'))
        # Process the given value...
        self._pool[varname] = value
        if varname not in self._keys:
            if index is None:
                self._keys.append(varname)
            else:
                self._keys.insert(index, varname)
        elif index is not None:
            self._keys.remove(varname)
            self._keys.insert(index, varname)
        self._mods.add(varname)
        self._dels.discard(varname)

    def __setitem__(self, varname, value):
        """Insert or change a namelist block variable."""
        return self.setvar(varname, value)

    def __setattr__(self, varname, value):
        """Insert or change a namelist block variable."""
        return self.setvar(varname, value)

    def getvar(self, varname):
        """Get ``varname`` variable's value (this is not case sensitive)."""
        varname = varname.upper()
        if varname in self._pool:
            if len(self._pool[varname]) == 1:
                return self._pool[varname][0]
            else:
                return self._pool[varname]
        else:
            raise AttributeError("Unknown Namelist variable")

    def __getitem__(self, varname):
        """Get ``varname`` variable's value (this is not case sensitive)."""
        return self.getvar(varname)

    def __getattr__(self, varname):
        """Get ``varname`` variable's value (this is not case sensitive)."""
        return self.getvar(varname)

    def delvar(self, varname):
        """Delete the specified ``varname`` variable from this block."""
        varname = varname.upper()
        if varname in self._pool:
            del self._pool[varname]
            self._keys.remove(varname)

    def __delitem__(self, varname):
        """Delete the specified ``varname`` variable from this block."""
        self.delvar(varname)

    def __delattr__(self, varname):
        """Delete the specified ``varname`` variable from this block."""
        self.delvar(varname)

    def __len__(self):
        """The number of variable within the namelist block."""
        return len(self._pool)

    def __iter__(self):
        """Iterate through variable names."""
        for t in self._keys:
            yield t

    def keys(self):
        """Returns the ordered variable names of the namelist block."""
        if six.PY3:
            return self.__iter__()
        else:
            return self._keys[:]

    iterkeys = __iter__

    def __contains__(self, item):
        """Returns whether ``item`` value is defined as a namelist variable or not."""
        return item.upper() in self._pool

    def has_key(self, item):
        """Returns whether ``item`` value is defined as a namelist variable or not."""
        return item in self

    def __call__(self):
        return self.pool()

    def values(self):
        """Returns the values of the internal pool of variables."""
        return self._pool.values()

    def items(self):
        """Iterate over the namelist block's variables."""
        if six.PY3:
            return self.iteritems()
        else:
            return [(k, self._pool[k]) for k in self._keys]

    def iteritems(self):
        """Iterate over the namelist block's variables."""
        for k in self._keys:
            yield (k, self._pool[k])

    def pool(self):
        """Returns the reference of the internal pool of variables."""
        return self._pool

    def get(self, *args):
        """Proxy to the dictionary ``get`` mechanism on the internal pool of variables."""
        return self._pool.get(*args)

    def update(self, dico):
        """Updates the pool of keys, and keeps as much as possible the initial order."""
        for var, value in six.iteritems(dico):
            self.setvar(var, value)

    def clear(self, rmkeys=None):
        """Remove specified keys (**rmkeys**) or completely clear the namelist block."""
        if rmkeys:
            for k in rmkeys:
                self.delvar(k)
        else:
            self.__dict__['_keys'] = list()
            self.__dict__['_pool'] = dict()

    def todelete(self, varname):
        """Register a key to be deleted."""
        self._dels.add(varname.upper())

    def rmkeys(self):
        """Returns a set of key to be deleted in a merge or dump."""
        return self._dels

    def macros(self):
        """Returns list of used macros in this block."""
        return self._subs.keys()

    def declaredmacros(self):
        """Returns list of old-style declared macros in this block."""
        return self._declared_subs

    def addmacro(self, macro, value=None):
        """Add a new macro to this definition block, and/or set a value."""
        self._subs[macro] = value

    def add_declaredmacro(self, macro, value=None):
        """
        Add a new old-style declared macro to this definition block,
        and/or set a value.
        """
        self.addmacro(macro, value)
        self._declared_subs.add(macro)

    def possible_macroname(self, item):
        """Find whether *item* is a macro or not."""
        if item in self._declared_subs:
            return item
        elif isinstance(item, six.string_types):
            fm_match = self._RE_FREEMACRO.match(item)
            if fm_match:
                return fm_match.group('NAME') if fm_match else None
        else:
            return None

    def nice(self, item, literal=None):
        """Nice encoded value of the item, possibly substitute with macros."""
        if literal is None:
            if self._literal is None:
                self.__dict__['_literal'] = LiteralParser()
            literal = self._literal
        if isinstance(item, six.string_types):
            itemli = item[:]
            # Ignore quote and double-quote when mathing macro's name
            if ((itemli.startswith("'") and itemli.endswith("'")) or
                    (itemli.startswith('"') and itemli.endswith('"'))):
                itemli = itemli[1:-1]
            # Ignore the dollar sign before a macro name
            if itemli.startswith('$'):
                itemli = itemli[1:]
        else:
            itemli = item
        macroname = self.possible_macroname(itemli)
        if macroname is not None:
            if self._subs.get(macroname, None) is None:
                return item
            else:
                macrovalue = self._subs[macroname]
                if isinstance(macrovalue, (list, tuple)):
                    return ','.join([literal.encode(value) for value in macrovalue])
                else:
                    return literal.encode(self._subs[macroname])
        else:
            return literal.encode(item)

    def dumps_values(self, key, literal=None):
        """Nice encoded values (incl. list of)."""
        return ','.join([self.nice(value, literal) for value in self._pool[key]])

    def dumps(self, literal=None, sorting=NO_SORTING):
        """
        Returns a string of the namelist block that will be readable by fortran parsers.

        :param sorting: Sorting option. One of :py:data:`NO_SORTING`,
                        :py:data:`FIRST_ORDER_SORTING` (sort based on variable names) or
                        :py:data:`SECOND_ORDER_SORTING` (sort only within indexes or attributes
                        of the same variable: usefull with arrays).
        """
        namout = " &{0:s}\n".format(self.name)
        if literal is None:
            if self._literal is None:
                self.__dict__['_literal'] = LiteralParser()
            literal = self._literal
        if sorting:
            def str2tup(k):
                k_by_attr = k.split('%')
                split_k = []
                for a in k_by_attr:
                    table = re.match(r'(?P<radic>\w+)\((?P<indexes>.+)\)', a)
                    if table is None:  # scalar
                        split_k.append(a)
                    else:
                        split_k.append(table.group('radic'))
                        strindexes = table.group('indexes')
                        if all([s in strindexes for s in (':', ',')]):
                            raise NotImplementedError("both ':' and ',' in array indexes")
                        elif ':' in strindexes:
                            split_k.extend([int(i) for i in strindexes.split(':')])
                        elif ',' in strindexes:
                            split_k.extend([int(i) for i in strindexes.split(',')])
                        else:
                            split_k.append(int(strindexes))
                return tuple(split_k)
            if sorting == FIRST_ORDER_SORTING:
                keylist = sorted(self._keys, key=str2tup)
            elif sorting == SECOND_ORDER_SORTING:
                tuples = [str2tup(k) for k in self._keys]
                radics = [t[0] for t in tuples]
                radics = sorted(list(set(radics)), key=lambda x: radics.index(x))
                byradics = {r: sorted([{'indexes': tuples[i][1:], 'fullkey': self._keys[i]}
                                       for i in range(len(self._keys)) if tuples[i][0] == r],
                                      key=lambda x: x['indexes'])
                            for r in radics}
                keylist = []
                for r in radics:
                    keylist.extend([b['fullkey'] for b in byradics[r]])
            else:
                raise ValueError('unknown value for **sorting**:' + str(sorting))
        else:
            keylist = self._keys
        for key in keylist:
            value_strings = self.dumps_values(key, literal=literal)
            namout += '   {0:s}={1:s},\n'.format(key, value_strings)
        return namout + " /\n"

    def merge(self, delta):
        """Merge the delta provided to the current block.

        :param NamelistBlock delta: The namelist block to merge in.
        """
        self.update(delta.pool())
        for dkey in [x for x in delta.rmkeys() if x in self]:
            self.delvar(dkey)
        for dkey in delta.rmkeys():
            self.todelete(dkey)
        # Preserve macros
        for skey in delta.macros():
            self._subs[skey] = delta._subs[skey]
            self._declared_subs.update(delta._declared_subs)


class NamelistSet(collections.MutableMapping):
    """A set of namelist blocks (see :class:`NamelistBlock`).

    This class defines all the methods of a usual Python's dictionary. The
    keys being the namelist names and the values the corresponding namelist
    blocks.

    :example: To create a :class:`NamelistSet` object populated with a pre-existing
        namelist block:

        >>> nb = NamelistBlock('MYNAM')
        >>> nb.A = 3
        >>> nset = NamelistSet([nb, ])

        It's possible to add a new block and customise it:

        >>> newblock = nset.newblock('TESTNAM')
        >>> newblock.A = 1
        >>> nset['TESTNAM'].B = 5.
        >>> print(nset.dumps())
         &MYNAM
           A=3,
         /
         &TESTNAM
           A=1,
           B=5.,
         /
        <BLANKLINE>

        For instance, this block can be renamed and the previous one deleted:

        >>> nset.mvblock('TESTNAM', 'MYNAM2')
        >>> del nset['MYNAM']
        >>> print(nset.dumps())
         &MYNAM2
           A=1,
           B=5.,
         /
        <BLANKLINE>

        The :meth:`merge` method allows to merge two namelist sets:

        >>> nset2 = NamelistSet([nb, ])
        >>> newblock = nset2.newblock('MYNAM2')
        >>> newblock.A = 999
        >>> print(nset2.dumps())
         &MYNAM
           A=3,
         /
         &MYNAM2
           A=999,
         /
        <BLANKLINE>
        >>> nset.merge(nset2)
        >>> print(nset.dumps())
         &MYNAM
           A=3,
         /
         &MYNAM2
           A=999,
           B=5.,
         /
        <BLANKLINE>

    """

    def __init__(self, blocks_set=None):
        """
        :param list[NamelistBlock] blocks_set: A list of :class:`NamelistBlock` objects (if
                                               missing, an empty list is assumed).
        """
        # For later use
        self._automkblock = 1
        # Initialise the set content
        if isinstance(blocks_set, (list, tuple)):
            if any([not isinstance(i, NamelistBlock) for i in blocks_set]):
                raise ValueError("When *blocks_set* is a list, its elements must be NamelistBlock objects")
            blocks_set = blocks_set
        elif isinstance(blocks_set, NamelistSet):
            blocks_set = blocks_set.as_list()
        elif blocks_set is None:
            blocks_set = []
        else:
            raise ValueError("Incorrect value for *blocks_set*: {!s}".format(blocks_set))
        # Generate a mapping based on the namelist blocks names
        self._mapping_dict = collections.OrderedDict()
        for nb in blocks_set:
            self._mapping_dict[nb.name] = nb

    def __contains__(self, key):
        return key.upper() in self._mapping_dict

    def __len__(self):
        return len(self._mapping_dict)

    def __iter__(self):
        for nbk in self._mapping_dict.keys():
            yield nbk

    def __getitem__(self, key):
        return self._mapping_dict[key.upper()]

    def __setitem__(self, key, value):
        assert isinstance(value, NamelistBlock)
        key = key.upper()
        if value.name != key:
            # To be safe...
            value = copy.deepcopy(value)
            value.set_name(key)
        self._mapping_dict[key] = value

    def __delitem__(self, key):
        del self._mapping_dict[key.upper()]

    def add(self, namblock):
        """Add a namelist block object to the present namelist set.

        :param NamelistBlock namblock: The namelist block to add.
        """
        self[namblock.name] = namblock

    def newblock(self, name=None):
        """Construct a new block.

        :param str name: the name of the new block. If omitted a block new block
            name will be generated (something like AUTOBLOCKnnn).
        """
        if name is None:
            name = 'AUTOBLOCK{0:03d}'.format(self._automkblock)
            while name in self:
                self._automkblock += 1
                name = 'AUTOBLOCK{0:03d}'.format(self._automkblock)
        if name not in self:
            self[name] = NamelistBlock(name=name)
        return self[name]

    def mvblock(self, sourcename, destname):
        """Rename a namelist block."""
        assert destname not in self, "Block {:s} already exists".format(destname)
        self[destname] = self.pop(sourcename)

    def setmacro(self, item, value):
        """
        Set macro value for further substitution (in all of the
        namelist blocks).
        """
        for namblock in filter(lambda x: item in x.macros(), self.values()):
            namblock.addmacro(item, value)

    def merge(self, delta, rmkeys=None, rmblocks=None, clblocks=None):
        """
        Merge of the current namelist set with the set of namelist blocks
        provided.
        """
        assert isinstance(delta, NamelistSet) or delta == dict()
        for namblock in delta.values():
            if namblock.name in self:
                self[namblock.name].merge(namblock)
            else:
                self.add(copy.deepcopy(namblock))
        if rmblocks is not None:
            for item in [x for x in rmblocks if x in self]:
                del self[item]
        if clblocks is not None:
            for item in [x for x in clblocks if x in self]:
                self[item].clear()
        if rmkeys is not None:
            for item in self:
                self[item].clear(rmkeys)

    def dumps(self, sorting=NO_SORTING, block_sorting=True):
        """
        Join the fortran's strings dumped by each namelist block.

        :param sorting: Sorting option. One of :py:data:`NO_SORTING`,
                        :py:data:`FIRST_ORDER_SORTING` (sort based on variable names) or
                        :py:data:`SECOND_ORDER_SORTING` (sort only within indexes or attributes
                        of the same variable: usefull with arrays).
        :param bool block_sorting: if True, namelist blocks are ordered based
                                   on their name.
        """
        if block_sorting:
            return ''.join([self[nblock_k].dumps(sorting=sorting)
                            for nblock_k in sorted(self.keys())])
        else:
            return ''.join([nblock.dumps(sorting=sorting)
                            for nblock in self.values()])

    def as_dict(self, deepcopy=False):
        """Return the actual namelist set as a dictionary."""
        if deepcopy:
            return copy.deepcopy(self._mapping_dict)
        else:
            return dict(self._mapping_dict)

    def as_list(self, deepcopy=False):
        """Return the actual namelist set as a list."""
        if deepcopy:
            return copy.deepcopy(list(self.values()))
        else:
            return list(self.values())


class NamelistParser(object):
    """
    Parser that creates a :class:`NamelistSet` object from a namelist file or
    a string.

    Macros (i.e. __SOMETHING__ values) are properly dealt with.

    :example: To get a :class:`NamelistSet` object from a string:

    >>> np = NamelistParser()
    >>> nset = np.parse('&NAM1 A=5.69, / &NAM2 B=__MYMACRO__ /')
    >>> print(nset.dumps())
     &NAM1
       A=5.69,
     /
     &NAM2
       B=__MYMACRO__,
     /
    <BLANKLINE>
    >>> nset.setmacro('MYMACRO', 'toto')
    >>> print(nset.dumps())
     &NAM1
       A=5.69,
     /
     &NAM2
       B='toto',
     /
    <BLANKLINE>

    """

    def __init__(self,
                 literal  = LiteralParser(),
                 macros = None,
                 re_flags = None,
                 re_clean = r"^(\s+|![^\n]*\n)",
                 re_block = r'&.*/',
                 re_endblock = r"^/(end)?",
                 re_bname = _NAME,
                 re_entry = _LETTER + r'[ A-Z0-9_,\%\(\):]*' + r"(?=\s*=)",
                 re_macro = _MACRONAME,
                 re_freemacro = _FREEMACRONAME,
                 re_endol = r"(?=\s*(,|/|\n))",
                 re_comma = r"\s*,"):
        self._literal = literal
        if macros:
            self._declaredmacros = set(macros)
        else:
            self._declaredmacros = set()
        if re_flags:
            self._re_flags = re_flags
        else:
            self._re_flags = literal._re_flags
        self._re_clean = re_clean
        self._re_block = re_block
        self._re_endblock = re_endblock
        self._re_bname = re_bname
        self._re_entry = re_entry
        self._re_macro = re_macro
        self._re_freemacro = re_freemacro
        self._re_endol = re_endol
        self._re_comma = re_comma
        self._recompile()

    def _recompile(self):
        """Recompile regexps according to internal characters strings by namelist entity."""
        self.clean = re.compile(self._re_clean, self._re_flags)
        self.block = re.compile(self._re_block, self._re_flags)
        self.endblock = re.compile(self._re_endblock, self._re_flags)
        self.bname = re.compile(self._re_bname, self._re_flags)
        self.entry = re.compile(self._re_entry, self._re_flags)
        self.macro_eol = re.compile(self._re_macro + self._re_endol, self._re_flags)
        self.freemacro_eol = re.compile(self._re_freemacro + self._re_endol, self._re_flags)
        self.comma = re.compile(self._re_comma, self._re_flags)
        self.deladd = re.compile(r'\-+' + self._re_endol, self._re_flags)
        # Element matching, ...
        self._SIGNED_INT_LCRE = re.compile(_SIGNED_INT_LITERAL_CONSTANT + self._re_endol, self._re_flags)
        self._BOZ_LCRE = re.compile(_BOZ_LITERAL_CONSTANT + self._re_endol, self._re_flags)
        self._SIGNED_REAL_LCRE = re.compile(_SIGNED_REAL_LITERAL_CONSTANT + self._re_endol, self._re_flags)
        self._COMPLEX_LCRE = re.compile(_COMPLEX_LITERAL_CONSTANT + self._re_endol, self._re_flags)
        self._CHAR_LCRE = re.compile(_CHAR_LITERAL_CONSTANT + self._re_endol, self._re_flags)
        self._LOGICAL_LCRE = re.compile(_LOGICAL_LITERAL_CONSTANT + self._re_endol, self._re_flags)

    def addmacro(self, macro):
        """Add an extra declared macro name (without associated value)."""
        self._declaredmacros.add(macro)

    @property
    def literal(self):
        """The literal parser used to process variable values."""
        return self._literal

    def _namelist_parse(self, source):
        """Parse the all bunch of source as a dict of namelist blocks."""
        namelists = list()
        while source:
            if self.block.search(source):
                namblock, source = self._namelist_block_parse(source)
                namelists.append(namblock)
            else:
                break
        return NamelistSet(namelists)

    def _namelist_clean(self, dirty_source, extraclean=()):
        """Removes spaces and comments before data."""
        cleaner_source = self.clean.sub('', dirty_source)
        while cleaner_source != dirty_source:
            dirty_source = cleaner_source
            cleaner_source = self.clean.sub('', dirty_source)
            for cleaner in extraclean:
                cleaner_source = cleaner.sub('', cleaner_source)
        return cleaner_source

    def _namelist_block_parse(self, source):
        """Parse a block of namelist."""
        source = self._namelist_clean(source, extraclean=(self.endblock, ))
        block_name = self.bname.match(source[1:]).group(0)
        source = self._namelist_clean(source[1 + len(block_name):])
        namelist = NamelistBlock(block_name)

        current = None
        values = list()

        while source:

            if self.entry.match(source) and not self._LOGICAL_LCRE.match(source):
                # Got a new entry in the namelist block
                if current:
                    namelist.update({current: values})
                current = self.entry.match(source).group(0).strip()
                values = list()
                source = self._namelist_clean(source[len(current):])
                # Removes equal
                source = self._namelist_clean(source[1:])
                continue

            elif self.endblock.match(source):
                if current:
                    namelist.update({current: values})
                source = source[1:]
                if re.match(r'end', source, self._re_flags):
                    source = source[3:]
                break

            elif self.deladd.match(source):
                item = self.deladd.match(source).group(0)
                namelist.todelete(current)
                current = None
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))
                continue

            elif self.freemacro_eol.match(source):
                rmatch = self.freemacro_eol.match(source)
                values.append(rmatch.group(0))
                source = self._namelist_clean(source[len(rmatch.group(0)):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))
                continue

            elif self.macro_eol.match(source):
                rmatch = self.macro_eol.match(source)
                if rmatch.group('NAME') in self._declaredmacros:
                    namelist.add_declaredmacro(rmatch.group('NAME'), None)
                    values.append(rmatch.group(0))
                    source = self._namelist_clean(source[len(rmatch.group(0)):])
                    if self.comma.match(source):
                        source = self._namelist_clean(self.comma.sub('', source, 1))
                    continue

            if self._SIGNED_INT_LCRE.match(source):
                item = self._SIGNED_INT_LCRE.match(source).group(0)
                values.append(self.literal.parse_integer(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            elif self._BOZ_LCRE.match(source):
                item = self._BOZ_LCRE.match(source).group(0)
                values.append(self.literal.parse_boz(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            elif self._SIGNED_REAL_LCRE.match(source):
                item = self._SIGNED_REAL_LCRE.match(source).group(0)
                values.append(self.literal.parse_real(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            elif self._COMPLEX_LCRE.match(source):
                item = self._COMPLEX_LCRE.match(source).group(0)
                values.append(self.literal.parse_complex(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            elif self._CHAR_LCRE.match(source):
                item = self._CHAR_LCRE.match(source).group(0)
                values.append(self.literal.parse_character(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            elif self._LOGICAL_LCRE.match(source):
                item = self._LOGICAL_LCRE.match(source).group(0)
                values.append(self.literal.parse_logical(item))
                source = self._namelist_clean(source[len(item):])
                if self.comma.match(source):
                    source = self._namelist_clean(self.comma.sub('', source, 1))

            else:
                raise ValueError("Badly formatted FORTRAN namelist: [[%s]]" % source[:32])

        return (namelist, source)

    def parse(self, obj):
        """Parse a string or a file.

        Returns a :class:`NamelistSet` object.
        """
        if isinstance(obj, six.string_types):
            if not self.block.search(obj):
                obj = obj.strip()
                with io.open(obj, 'r') as iod:
                    obj = iod.read()
            return self._namelist_parse(obj)

        elif hasattr(obj, 'seek') and hasattr(obj, 'read'):
            obj.seek(0)
            return self._namelist_parse(obj.read())
        else:
            raise ValueError("Argument %s cannot be parsed." % str(obj))


def namparse(obj, **kwargs):
    """Raw parsing with an default anonymous fortran parser.

    This function is a shortcut to the :meth:`NamelistParser.parse` method.

    :example: To get a :class:`NamelistSet` object from a string:

        >>> nset = namparse('&NAM1 A=5.69, / &NAM2 B=1 /')
        >>> print(nset.dumps())
         &NAM1
           A=5.69,
         /
         &NAM2
           B=1,
         /
        <BLANKLINE>

    """
    namp = NamelistParser(**kwargs)
    return namp.parse(obj)
