# -*- coding:Utf-8 -*-

"""
GetByTag does not provide any "official" design pattern but we consider it to be
somehow an extension of the Singleton pattern.

The main objective is to manage and keep alive a collection of objects that are
identified by a unique **tag**. For a given **tag**, only one object can be created.
If one attempts to create an object with an already existing **tag**, the
constructor will return the previously created object that matches the **tag**.

Basic example (see :class:`GetByTag` class documentation for more advanced features)::

    >>> class A(GetByTag):
    ...     def __init__(self, data=None):
    ...         self.data = data

    >>> class A1(A):
    ...     # A1 and A will share the same list of tags...
    ...     _tag_topcls = False

    >>> class B(GetByTag):
    ...     def __init__(self, data=None):
    ...         self.data = data


    >>> a = A(data=1)
    >>> print(a.tag)
    default
    >>> b = B(data=1)
    >>> print(b.tag)
    default
    >>> b is not a
    True
    >>> a1 = A1(data=10)
    >>> print(a1.tag)
    default
    >>> a1 is a  # Chocking? No... since a and a1 have been created using the same default tag.
    True

    # A new object of tag 'toto' can be created as follows:
    >>> t = A1('toto')
    >>> t is not a
    True
    >>> tbis = A1(tag='toto')
    >>> t is tbis
    True

"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

from weakref import WeakSet

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)


class GetByTagMeta(type):
    """Meta class constructor for :class:`GetByTag`.

    The purpose is quite simple : to set a dedicated shared table in the new
    class in construction.
    """

    def __new__(cls, n, b, d):
        """Create a new class object."""
        logger.debug('Base class for getbytag usage "%s / %s", bc = ( %s ), internal = %s', cls, n, b, d)
        if d.setdefault('_tag_topcls', True):
            d['_tag_table'] = dict()
            d['_tag_focus'] = dict(default=None)
            d['_tag_class'] = WeakSet()
        realnew = super(GetByTagMeta, cls).__new__(cls, n, b, d)
        realnew._tag_class.add(realnew)
        return realnew

    def __call__(self, *args, **kw):
        """Create a new class object."""
        return self.__new__(self, *args, **kw)


@six.add_metaclass(GetByTagMeta)
class GetByTag(object):
    """
    Utility to retrieve a new/existing object by a special argument named ``tag``.
    If an object had already been created with that tag, return this object.

    Optionally, :class:`GetByTag` can manage the active/passive state of the
    created objects. It's done using the notion of **focus**. **Focus** can be
    set, caught or checked using the :meth:`set_focus` class method, the
    :meth:`catch_focus` method and the :meth:`has_focus` method. Some actions
    can be launched when **focus** is gained or lost simply by subclassing the
    :meth:`focus_gain_allow`, :meth:`focus_gain_hook` and :meth:`focus_loose_hook`
    methods.
    """

    _tag_default = 'default'

    _tag_implicit_new = True

    def __new__(cls, *args, **kw):
        """
        Some class variables may have an impact on GetByTag behaviour:

        * ``_tag_default``: Sets the default ``tag`` (if the ``tag`` attribute is
          omitted when calling the constructor, the ``_tag_default`` string will be used.
        * ``_tag_implicit_new``: If set to ``False``, unless ``new=True`` is specified when
          calling the constructor, it won't be allowed to create new objects (a
          RuntimeError exception will be thrown).
        * ``_tag_topcls``: if set to ``False``, the tags list will be shared with
          the parent class (and possibly other siblings). The :meth:`tag_classes` class
          method allows to retrieve the list of Classes sharing the same list of tags

        """
        tag = kw.pop('tag', None)
        if tag is None:
            if args:
                args = list(args)
                tag = args.pop(0)
            else:
                tag = cls._tag_default
        tag = cls.tag_clean(tag)
        new = kw.pop('new', False)
        if not new and tag in cls._tag_table:
            newobj = cls._tag_table[tag]
        else:
            if not cls._tag_implicit_new and not new:
                cls._tag_implicit_new_error(tag)
            newobj = super(GetByTag, cls).__new__(cls)
            newobj._tag = tag
            cls._tag_table[tag] = newobj
            newobj.__init__(*args, **kw)
        return newobj

    @classmethod
    def _tag_implicit_new_error(cls, tag):
        """Called whenever a tag does not exist and _tag_implicit_new = False."""
        raise RuntimeError(("It's not allowed to create a new {:s} object (new tag={:s}) "
                            "without an explicit new=True argument.").format(cls.__name__, tag))

    @property
    def tag(self):
        """The current object's tag."""
        return self._tag

    @classmethod
    def tag_clean(cls, tag):
        """By default, return the actual tag."""
        return tag

    @classmethod
    def tag_keys(cls):
        """Return an alphabetically ordered list of actual keys of the objects instantiated."""
        return sorted(cls._tag_table.keys())

    @classmethod
    def tag_values(cls):
        """Return a non-ordered list of actual values of the objects instantiated."""
        return list(cls._tag_table.values())

    @classmethod
    def tag_items(cls):
        """Proxy to the ``items`` method of the internal dictionary table of objects."""
        return list(cls._tag_table.items())

    @classmethod
    def tag_check(cls, tag):
        """Check if the tag is in list of actual keys of the objects instanciated."""
        return (tag in cls._tag_table.keys())

    @classmethod
    def tag_focus(cls, select='default'):
        """Return the tag value of the actual object with focus according to the ``select`` value."""
        return cls._tag_focus[select]

    @classmethod
    def set_focus(cls, obj, select='default'):
        """Define a new tag value for the focus in the scope of the ``select`` value."""
        # Do the sanity checks
        obj.focus_gain_allow()
        # Call the hook on the previous default object
        prev_focus = cls._tag_focus[select]
        if prev_focus is not None:
            prev_obj = cls(prev_focus)
            prev_obj.focus_loose_hook()
        # Actually change the default
        cls._tag_focus[select] = obj.tag
        # Call the hook on the new default object
        obj.focus_gain_hook()

    def has_focus(self, select='default'):
        """Return a boolean value on equality of current tag and focus tag."""
        return self.tag == self.__class__._tag_focus[select]

    def catch_focus(self, select='default'):
        """The current object decides to be on focus !"""
        self.set_focus(self, select)

    @classmethod
    def tag_clear(cls):
        """Clear all internal information about objects and focus for that class."""
        cls._tag_table = dict()
        cls._tag_focus = dict(default=None)

    @classmethod
    def tag_classes(cls):
        """Return a list of current classes that have been registered with the same GetByTag root."""
        return list(cls._tag_class)

    def __copy__(self):
        """I don't know how to deep copy a GetByTag..."""
        logger.debug("There is no trivial way to copy a GetByTag instance: returning self")
        return self

    def __deepcopy__(self, memo):
        """I don't know how to deep copy a GetByTag..."""
        logger.debug("There is no trivial way to deepcopy a GetByTag instance: returning self")
        memo[id(self)] = self
        return self

    def focus_loose_hook(self):
        """This method is called when an object looses the focus."""
        pass

    def focus_gain_allow(self):
        """This method is called on the target object prior to any focus change.

        It might be useful if one wants to perform checks and raise an exception.
        """
        pass

    def focus_gain_hook(self):
        """This method is called when an object gains the focus."""
        pass


if __name__ == '__main__':
    import doctest
    doctest.testmod()
