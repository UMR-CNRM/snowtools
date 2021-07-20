# -*- coding:Utf-8 -*-
# pylint: disable=unused-argument

"""
Data dumper... The (challenging) idea is to be able to dump any object to many
different formats.

It is mostly used in objects' docstring within the footprints package.

:note: Dumper objects are managed using :mod:`bronx.patterns.getbytag`;
       consequently, they are associated with a ``tag`` and can be re-used.

Example::

    >>> class Foo(object):
    ...     a = 1
    ...
    ...     def __str__(self):
    ...         return str(self.a)

    >>> somelist = [dict(akey=Foo(),bkey=['item1', 'item2'],
    ...                  ckey=dict(other=1, sutff=2)), 'a_string',
    ...             ['another', 'list', tuple([1, 2, 3])]]

    # A Txt Dumper object can be created directly
    >>> tdumper = TxtDumper()
    >>> print(tdumper.tag)
    default

    # Or using get(). Since getbytag is used, the same objet is dumped
    >>> tdumper_bis = get()
    >>> tdumper_bis is tdumper
    True

    >>> print(tdumper.cleandump(somelist)) # doctest: +ELLIPSIS
          [dict(
                  akey = ...Foo::1,
                  bkey = ['item1', 'item2'],
                  ckey = dict(
                      other = 1,
                      sutff = 2,
                  ),
              ), 'a_string', ['another', 'list', (1, 2, 3)]]

    # The Jsonable Dumper will produce something that can safely be dumped to
    # a JSON File
    >>> jdumper = JsonableDumper(tag='testdumper')
    >>> jdumper.cleandump(somelist) # doctest: +ELLIPSIS +NORMALIZE_WHITESPACE
    [{'akey': ...'...Foo::1',
      'bkey': [...'item1', ...'item2'],
      'ckey': {'sutff': 2, 'other': 1}}, ...
     'a_string', [...'another', ...'list', [1, 2, 3]]]

    # The XML Dumper returns a xml.dom's Document object
    >>> xdumper = XmlDomDumper()
    >>> xd = xdumper.cleandump(somelist, 'testxml')
    >>> xd # doctest: +ELLIPSIS
    <xml.dom.minidom.Document instance at 0x...>
    >>> print(xd.toprettyxml(indent='  ', encoding='utf-8')) # doctest: +ELLIPSIS
    <?xml version="1.0" encoding="utf-8"?>
    <testxml>
      <generic_item>
        <akey>
          <generic_object>
            <overview>1</overview>
            <type>...Foo</type>
          </generic_object>
        </akey>
        <bkey>item1</bkey>
        <bkey>item2</bkey>
        <ckey>
          <other>1</other>
          <sutff>2</sutff>
        </ckey>
      </generic_item>
      <generic_item>a_string</generic_item>
      <generic_item>
        <generic_item>another</generic_item>
        <generic_item>list</generic_item>
        <generic_item>
          <generic_item>1</generic_item>
          <generic_item>2</generic_item>
          <generic_item>3</generic_item>
        </generic_item>
      </generic_item>
    </testxml>
    <BLANKLINE>

    # Interface functions can be used to obtain quickly a text dump
    >>> print(fulldump(somelist)) # doctest: +ELLIPSIS
          [dict(
                  akey = ...Foo::1,
                  bkey = ['item1', 'item2'],
                  ckey = dict(
                      other = 1,
                      sutff = 2,
                  ),
              ), 'a_string', ['another', 'list', (1, 2, 3)]]

"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

import re
from xml.dom import minidom

from bronx.patterns import getbytag

#: No automatic export
__all__ = []


def _DEBUG(msg, obj=None, level=None):
    """Fake method for debug purpose (then should provide a print statement)."""
    # print(msg, six.text_type(obj))
    pass


def is_an_instance(val):
    """Detect if a given object is an instance (as opposed to being a class).

    :param val: The object to analyse
    """
    # Change: This routine will no longer detect old-style classes !
    #         (because the support of old-style classes will be removed)
    # instance of extension class, but not an actual extension class
    if (hasattr(val, '__class__') and
            hasattr(val, '__dict__') and
            not hasattr(val, '__bases__')):
        return True
    else:
        return False


def is_class(val):
    """Detect if a given object is a class (as opposed to being an instance).

    :param val: The object to analyse
    """
    return hasattr(val, '__bases__')


def get(**kw):
    """Return an actual TxtDumper object matching the description."""
    return TxtDumper(**kw)


class _AbstractDumper(getbytag.GetByTag):
    """Could dump almost anything."""

    def __init__(self):
        """
        No arguments.
        """
        self.reset()

    def reset(self):
        """Clear the Dumper's object cache."""
        self.seen = dict()

    def _dump_internal_dict(self, obj, level=0, nextline=True):
        return self.dump_dict(obj, level + 1, nextline)

    def _dump_as_proxy(self, proxy, obj, level=0, nextline=True):
        return getattr(self, 'dump_' + proxy,
                       self._lazzy_dump)(obj, level + 1, nextline)

    def _unknown_obj_overview(self, obj):
        strobj = six.text_type(obj)
        reprobj = repr(obj)
        if '\n' not in strobj and strobj != reprobj:
            return strobj
        else:
            return reprobj

    def _dump_unknown_obj(self, obj, level=0, nextline=True):
        return "{:s}.{:s}::{:s}".format(type(obj).__module__, type(obj).__name__,
                                        self._unknown_obj_overview(obj))

    def _dump_class(self, obj, level=0, nextline=True):
        return '{0:s}.{1:s}'.format(obj.__module__, obj.__name__)

    def _dump_builtin(self, obj, level=0, nextline=True):
        return obj.__name__

    def _dump_obj_shortcut(self, obj, level=0, nextline=True):
        return "{:s}.{:s}::{:s}".format(type(obj).__module__, type(obj).__name__,
                                        obj.as_dump())

    def dump_default(self, obj, level=0, nextline=True):
        _DEBUG('dump_default')
        # Great, obj as a as_dict method: top choice
        if hasattr(obj, '__dict__') and hasattr(obj, 'as_dict'):
            return self._dump_internal_dict(obj.as_dict(), level + 1)
        # Rely on parent classes: ok it should work
        if isinstance(obj, dict):
            return self._dump_as_proxy('dict', obj, level + 1, nextline)
        if isinstance(obj, set):
            return self._dump_as_proxy('set', obj, level + 1, nextline)
        if isinstance(obj, list):
            return self._dump_as_proxy('list', obj, level + 1, nextline)
        if isinstance(obj, tuple):
            return self._dump_as_proxy('tuple', obj, level + 1, nextline)
        # Can't do anything better, sorry !
        return self._dump_unknown_obj(obj, level, nextline)

    def _lazzy_dump(self, obj, level=0, nextline=True):
        return obj

    dump_dict = _lazzy_dump
    dump_int = _lazzy_dump
    dump_long = _lazzy_dump
    dump_float = _lazzy_dump
    dump_bool = _lazzy_dump
    dump_str = _lazzy_dump
    dump_unicode = _lazzy_dump

    def _recursive_dump(self, obj, level=0, nextline=True):
        """This routine can be called recursively (if necessary)."""
        _DEBUG('dump top', obj)

        this_id = id(obj)

        if this_id in self.seen:
            return self.seen[this_id]

        if is_an_instance(obj) and hasattr(obj, 'as_dump'):
            _DEBUG('dump shortcut', obj)
            self.seen[this_id] = self._dump_obj_shortcut(obj, level, nextline)
            return self.seen[this_id]

        if is_class(obj):
            if obj.__module__ in ('__builtin__', 'builtins'):
                _DEBUG('builtin')
                self.seen[this_id] = self._dump_builtin(obj, level, nextline)
            else:
                _DEBUG('class ' + six.text_type(obj))
                self.seen[this_id] = self._dump_class(obj, level, nextline)
            return self.seen[this_id]

        name = type(obj).__name__
        dump_func = getattr(self, "dump_%s" % name, self.dump_default)
        return dump_func(obj, level, nextline)

    def dump(self, obj, level=0, nextline=True):
        """Call this method to dump ``obj`` (or at least try to...).

        :param obj: The object that will be dumped
        :param int level: For internal use only.
        :param bool nextline: For internal use only.
        """
        return self._recursive_dump(obj, level=level, nextline=nextline)

    def cleandump(self, obj):
        """Clear cache dump and provide a dump of the provided ``obj``.

        :param obj: The object that will be dumped
        """
        self.reset()
        return self.dump(obj)


class JsonableDumper(_AbstractDumper):
    """Return a dump consisting of a pure mix of dictionaries and lists.

    The resulting dump can be serialised using the standard pickle or json module.
    """

    def dump_dict(self, obj, level=0, nextline=True):
        return {self._recursive_dump(k, level, nextline):
                self._recursive_dump(v, level + 1, nextline)
                for k, v in six.iteritems(obj)}

    def dump_list(self, obj, level=0, nextline=True):
        return [self._recursive_dump(v, level + 1, nextline) for v in obj]

    dump_tuple = dump_list
    dump_set = dump_list

    def dump_NoneType(self, obj, level=0, nextline=True):
        return 'None'


class XmlDomDumper(JsonableDumper):
    """Return a dump as an XML DOM object (instance of :class:`xml.minidom.Document`)."""

    def __init__(self, named_nodes=()):
        """
        :param tuple named_nodes: List of XML nodes that support a `name` attribute.
            For such nodes, a dictionary will be converted as follows :
            ``attr=dict(toto="BlaBla",titi="BlaBla")`` becomes
            ``<attr name="toto">BlaBla</attr><attr name="titi">BlaBla</attr>``

        """
        super(XmlDomDumper, self).__init__()
        self._named_nodes = named_nodes

    def _unknown_obj_overview(self, obj):
        return re.sub(r'^<(.*)>$', r'\1',
                      super(XmlDomDumper, self)._unknown_obj_overview(obj))

    def _dump_unknown_obj(self, obj, level=0, nextline=True):
        return dict(generic_object=dict(type='{}.{}'.format(type(obj).__module__,
                                                            type(obj).__name__),
                                        overview=self._unknown_obj_overview(obj)))

    def _dump_as_proxy(self, proxy, obj, level=0, nextline=True):
        if proxy in ('list', 'set', 'tuple') or type(obj).__name__.startswith('FP'):
            return self._dump_unknown_obj(obj, level, nextline)
        else:
            return super(XmlDomDumper, self)._dump_as_proxy(proxy, obj, level, nextline)

    def _dump_obj_shortcut(self, obj, level=0, nextline=True):
        return dict(generic_object=dict(type='{}.{}'.format(type(obj).__module__,
                                                            type(obj).__name__),
                                        overview=obj.as_dump()))

    def _dump_class(self, obj, level=0, nextline=True):
        return {'class': super(XmlDomDumper, self)._dump_class(obj, level, nextline)}

    def _dump_builtin(self, obj, level=0, nextline=True):
        return {'builtin': super(XmlDomDumper, self)._dump_builtin(obj, level, nextline)}

    def _xdump_dict(self, xdoc, xroot, obj, myname):
        for k, v in sorted(obj.items(), key=lambda x: x[0]):
            if not isinstance(v, list):
                if myname in self._named_nodes:
                    xnode = xdoc.createElement(myname)
                    xnode.setAttribute('name', six.text_type(k))
                else:
                    if six.text_type(k) in self._named_nodes:
                        xnode = xroot
                    else:
                        xnode = xdoc.createElement(six.text_type(k))
            else:
                xnode = xroot
            self._xdump(xdoc, xnode, v, myname=six.text_type(k))
            if xnode is not xroot:
                xroot.appendChild(xnode)

    def _xdump_list(self, xdoc, xroot, obj, myname, topelt=False):
        for v in obj:
            if topelt:
                xnode = xdoc.createElement('generic_item')
            else:
                xnode = xdoc.createElement(myname)
            self._xdump(xdoc, xnode, v, myname=six.text_type(v), topelt=True)
            xroot.appendChild(xnode)

    def _xdump(self, xdoc, xroot, obj, myname, topelt=False):
        if isinstance(obj, list):
            self._xdump_list(xdoc, xroot, obj, myname, topelt=topelt)
        elif isinstance(obj, dict):
            self._xdump_dict(xdoc, xroot, obj, myname)
        else:
            # Generic case
            xroot.appendChild(xdoc.createTextNode(six.text_type(obj)))

    def dump(self, obj, root, rootattr=None, level=0, nextline=True):
        """Call this method to dump ``obj`` (or at least try to...).

        :param obj: The object that will be dumped
        :param str root: Name of the XML root node
        :param dict rootattr: dictionary of attributes that will be added to the
            XML root element.
        :param int level: For internal use only.
        :param bool nextline: For internal use only.
        """
        parent_dump = self._recursive_dump(obj, level, nextline)
        xdoc = minidom.Document()
        xroot = xdoc.createElement(root)
        if rootattr is not None and isinstance(rootattr, dict):
            for k, v in six.iteritems(rootattr):
                xroot.setAttribute(k, v)
        self._xdump(xdoc, xroot, parent_dump, myname=root, topelt=True)
        xdoc.appendChild(xroot)
        return xdoc

    cleandump = dump


class TxtDumper(_AbstractDumper):
    """Dump a text representation of almost any object..."""

    indent_first = 6
    indent_size = 4
    indent_space = ' '

    max_depth = 32

    break_base = False
    break_string = False
    break_bool = False
    break_default = True
    break_proxies = True

    break_before_list_item = False
    break_before_list_begin = False
    break_after_list_begin = False
    break_before_list_end = False
    break_after_list_end = False

    break_before_set_item = False
    break_before_set_begin = False
    break_after_set_begin = False
    break_before_set_end = False
    break_after_set_end = False

    break_before_tuple_item = False
    break_before_tuple_begin = False
    break_after_tuple_begin = False
    break_before_tuple_end = False
    break_after_tuple_end = False

    break_before_dict_key = True
    break_before_dict_value = False
    break_before_dict_begin = False
    break_after_dict_begin = False
    break_before_dict_end = True
    break_after_dict_end = False

    def _indent(self, level=0, nextline=True):
        if nextline:
            return "\n" + self.indent_space * (self.indent_first +
                                               self.indent_size * level)
        else:
            return ""

    def _dump_internal_dict(self, obj, level=0, nextline=True):
        parent_dump = super(TxtDumper, self)._dump_internal_dict(obj, level + 1,
                                                                 nextline and self.break_proxies)
        return "<<{:s}__dict__:: {!s}{:s}>>".format(self._indent(level + 1, self.break_proxies),
                                                    parent_dump,
                                                    self._indent(level, self.break_proxies))

    def _dump_as_proxy(self, proxy, obj, level=0, nextline=True):
        parent_dump = super(TxtDumper, self)._dump_as_proxy(proxy, obj, level + 1,
                                                            nextline and self.break_proxies)
        return "<<{:s}as_{:s}:: {!s}{:s}>>".format(self._indent(level + 1, self.break_proxies),
                                                   proxy, parent_dump,
                                                   self._indent(level, self.break_proxies),)

    def _dump_unknown_obj(self, obj, level=0, nextline=True):
        return self._unknown_obj_overview(obj)

    def dump_default(self, obj, level=0, nextline=True):
        _DEBUG('dump_default')
        if level + 1 > self.max_depth:
            return " <%s...>" % type(obj).__class__
        else:
            parent_dump = super(TxtDumper, self).dump_default(obj, level,
                                                              nextline and self.break_default)
            return "{:s}.{:s}::{!s}".format(type(obj).__module__, type(obj).__name__,
                                            parent_dump)

    def dump_base(self, obj, level=0, nextline=True):
        _DEBUG('dump base ' + type(obj).__name__)
        return "%s%s" % (self._indent(level, self.break_base), obj)

    dump_NoneType = dump_base
    dump_int = dump_base
    dump_long = dump_base
    dump_float = dump_base

    def dump_str(self, obj, level=0, nextline=True):
        _DEBUG('dump_str', obj)
        return "%s'%s'" % (self._indent(level, self.break_string), obj)

    dump_unicode = dump_str

    def dump_bool(self, obj, level=0, nextline=True):
        _DEBUG('dump_bool', obj)
        return "%s%s" % (self._indent(level, self.break_bool), six.text_type(obj))

    def dump_tuple(self, obj, level=0, nextline=True):
        _DEBUG('dump_tuple', obj)
        if level + 1 > self.max_depth:
            return "%s(...)%s" % (
                self._indent(level, self.break_before_tuple_begin),
                self._indent(level, self.break_after_tuple_end)
            )
        else:
            items = ["%s%s" % (self._indent(level + 1, self.break_before_tuple_item),
                               self._recursive_dump(x, level + 1))
                     for x in obj]
            return "%s(%s%s%s)%s" % (
                self._indent(level, nextline and self.break_before_tuple_begin),
                self._indent(level + 1, self.break_after_tuple_begin), ', '.join(items),
                self._indent(level, self.break_before_tuple_end),
                self._indent(level, self.break_after_tuple_end)
            )

    def dump_list(self, obj, level=0, nextline=True):
        _DEBUG('dump_list', obj)
        if level + 1 > self.max_depth:
            return "%s[...]%s" % (
                self._indent(level, self.break_before_list_begin),
                self._indent(level, self.break_after_list_end)
            )
        else:
            items = ["%s%s" % (self._indent(level + 1, self.break_before_list_item),
                               self._recursive_dump(x, level + 1))
                     for x in obj]
            return "%s[%s%s%s]%s" % (
                self._indent(level, nextline and self.break_before_list_begin),
                self._indent(level + 1, self.break_after_list_begin), ', '.join(items),
                self._indent(level, self.break_before_list_end),
                self._indent(level, self.break_after_list_end)
            )

    def dump_set(self, obj, level=0, nextline=True):
        _DEBUG('dump_set', obj)
        if level + 1 > self.max_depth:
            return "%sset([...])%s" % (
                self._indent(level, self.break_before_set_begin),
                self._indent(level, self.break_after_set_end)
            )
        else:
            items = [
                "%s%s" % (
                    self._indent(level + 1, self.break_before_set_item),
                    self._recursive_dump(x, level + 1)
                ) for x in obj
            ]
            return "%sset([%s%s%s])%s" % (
                self._indent(level, nextline and self.break_before_set_begin),
                self._indent(level + 1, self.break_after_set_begin), ', '.join(items),
                self._indent(level, self.break_before_set_end),
                self._indent(level, self.break_after_set_end)
            )

    def dump_dict(self, obj, level=0, nextline=True):
        _DEBUG('dump_dict', obj)
        if level + 1 > self.max_depth:
            return "%s{...}%s" % (
                self._indent(level, self.break_before_dict_begin),
                self._indent(level, self.break_after_dict_end)
            )
        else:
            items = ["%s%s = %s%s," % (self._indent(level + 1, self.break_before_dict_key),
                                       six.text_type(k),
                                       self._indent(level + 2, self.break_before_dict_value),
                                       self._recursive_dump(v, level + 1))
                     for k, v in sorted(obj.items())]
            breakdict = self.break_before_dict_end
            if not len(obj):
                breakdict = False
            return "%sdict(%s%s%s)%s" % (
                self._indent(level, nextline and self.break_before_dict_begin),
                self._indent(level + 1, self.break_after_dict_begin), ' '.join(items),
                self._indent(level, breakdict),
                self._indent(level, self.break_after_dict_end)
            )

    def cleandump(self, obj):
        """Clear cache dump and provide a top indented dump of the provided ``obj``.

        :param obj: The object that will be dumped
        """
        parent_dump = super(TxtDumper, self).cleandump(obj)
        return self.indent_space * self.indent_first + parent_dump


class OneLineTxtDumper(TxtDumper):
    """Dump single-line text representation of almost any object..."""

    indent_first = 0
    indent_size = 0

    break_default = False
    break_proxies = False

    break_before_dict_key = False
    break_before_dict_end = False

    def _dump_obj_shortcut(self, obj, level=0, nextline=True):
        return "{:s}::{:s}".format(type(obj).__name__, obj.as_dump())

    def dump_default(self, obj, level=0, nextline=True):
        _DEBUG('dump_default')
        if level + 1 > self.max_depth:
            return " <%s...>" % type(obj).__class__
        else:
            parent_dump = _AbstractDumper.dump_default(self, obj, level,
                                                       nextline and self.break_default)
            return "{:s}::{!s}".format(type(obj).__name__, parent_dump)


def fulldump(obj, startpos=TxtDumper.indent_first, reset=True):
    """Entry point: Return a text dump of the provided ``obj``.

    :param obj: The object that will be dumped
    :param int startpos: Number of blank characters that will be added to the
        first line of the text dump
    :param bool reset: Reset the TxtDumper object's cache before dumping ``obj``
    """
    d = TxtDumper()
    if reset:
        d.reset()
    return TxtDumper.indent_space * startpos + d.dump(obj)


def lightdump(obj, break_before_dict_key=True, break_before_dict_value=False):
    """Entry point: Have a quick glance to an assumed 1-depth dictionary.

    :param obj: The object that will be dumped
    """
    _DEBUG('dump_dict', obj)
    d = TxtDumper()
    items = [
        "%s%s = %s%s," % (
            d._indent(0, break_before_dict_key),
            six.text_type(k),
            d._indent(1, break_before_dict_value),
            six.text_type(v)
        ) for k, v in sorted(obj.items(), key=lambda x: x[0])
    ]
    return ''.join(items)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
