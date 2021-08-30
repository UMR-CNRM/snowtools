# -*- coding: utf-8 -*-

"""
A templating system for nested dictionaries and lists.

The :class:`DefaultTemplate` class should be used directly. See its documentation.
"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

import copy
import re

from bronx.syntax.iterators import izip_pcn
from bronx.syntax.minieval import SingleLineStatement


class TemplateRenderingError(ValueError):
    """Any exception raised by the :class:`DefaultTemplate` class."""
    pass


class TemplateLoopRenderingError(TemplateRenderingError):
    """Any exception raised by the :class:`DefaultTemplate` class. during loop expansion"""

    def __init__(self, msg):
        super(TemplateLoopRenderingError, self).__init__('loop rendering: {:s}'.format(msg))


class DefaultTemplate(object):
    """Recursively walk into nested dictionaries and/or lists templates.

    This templating class provides two features:

        * String substitution: If a string looks like ``{{some_statement}}``,
          ``some_statement`` is evaluated and the returned data replaces the
          original string.
        * Loop expansion: Portions of the template will be duplicated according
          to a loop iterator (see below for more details).

    The use of this class is very simple: create an object by providing the
    nested dictionaries and/or lists template. Then, call the
    :meth:`~DefaultTemplate.render` method with a list of variables that will
    be used when processing the template.

    Example::

        >>> tpl = ['Any static string.',
        ...        dict(inputs=dict(__bronx_tpl_engine__='loop',
        ...                         __loopiterator__='zip(members, physics)',
        ...                         __loopvariables__='member, physic',
        ...                         __body__=dict(kind='member_spec',
        ...                                       msg=('{{"Member number: {:03d} next one is {!s}"' +
        ...                                            '.format(member, member_next)}}'),
        ...                                       member='{{member}}',
        ...                                       physic_id='{{physic}}',
        ...                                       terms=dict(__bronx_tpl_engine__='loop',
        ...                                                  __loopiterator__='enumerate(terms)',
        ...                                                  __loopvariables__='i, term',
        ...                                                  __body__=['{{i}}', '{{term.fmthm}}']),
        ...                                       )
        ...                         ),
        ...             nmembers='{{len(members)}}',
        ...             nterms='{{len(terms)}}'
        ...             ),
        ...        ]
        >>> dt = DefaultTemplate(tpl)
        >>> from bronx.stdtypes.date import Time
        >>> tpl_r = dt.render(members=[1, 2, 3],
        ...                   physics=['turb1', 'turb2', 'turb3'],
        ...                   terms=[Time(3, 0), Time(6, 0)])
        >>> (tpl_r ==
        ...  ['Any static string.',
        ...   {'inputs': [{'kind': 'member_spec',
        ...                'member': 1,
        ...                'msg': 'Member number: 001 next one is 2',
        ...                'physic_id': 'turb1',
        ...                'terms': [[0, '0003:00'], [1, '0006:00']]},
        ...               {'kind': 'member_spec',
        ...                'member': 2,
        ...                'msg': 'Member number: 002 next one is 3',
        ...                'physic_id': 'turb2',
        ...                'terms': [[0, '0003:00'], [1, '0006:00']]},
        ...               {'kind': 'member_spec',
        ...                'member': 3,
        ...                'msg': 'Member number: 003 next one is None',
        ...                'physic_id': 'turb3',
        ...                'terms': [[0, '0003:00'], [1, '0006:00']]}],
        ...    'nmembers': 3,
        ...    'nterms': 2}])
        True

    .. note:: More details on loop expansion:

       * The loop expansion is triggered by a dictionary that contains the
         ``__bronx_tpl_engine__ = 'loop'`` entry;
       * The ``__loopiterator__`` (mandatory) item is a python code statement that
         returns any kind of iterable objects. The loop will iterate on it;
       * The ``__loopvariables__`` (mandatory) item is a comma-separated list where
         the loop control variable names are specified. These control variables,
         will be updated at each iteration of the loop with the current value
         of the iterator. In addition, variables suffixed by ``_prev`` and
         ``_next`` will contain the previous and next iterator items;
       * The ``__body__`` (mandatory) item will be duplicated at each iteration
         of the loop;
       * The ``__body_first__`` and ``__body_last__`` (optional) items may be
         added in order to specify a specfic ``body`` for respectively the first
         and last iteration of the loop;
       * The ``__extra_vars__`` (optional) item is a dictionary that contains
         variable names (keys) associated with a Python code statement (values).
         These variables will be evaluated at each iteration of the loop and could
         be used later on in string substitutions.

    .. note::

       It heavily relies on Python code statements that are checked and evaluate
       using the :mod:`bronx.syntax.minieval` module. Consequently, exceptions based
       on :mod:`bronx.syntax.minieval.SingleLineStatementError` may be raised.

    """

    _TEMPLATING_KEY = '__bronx_tpl_engine__'
    _INLINE_EVAL_RE = re.compile(r'^{{(.*)}}$')

    def __init__(self, base):
        """
        :param tpl: The template to work with (nested dictionaries and/or lists)
        """
        self._tplbase = base
        self._sls = SingleLineStatement()

    def render(self, ** kwargs):
        """Renders the template for a specific set **kwargs** of variables.

        :param kwargs: Variables usable during template rendering
        :return: The processed template
        :raises bronx.syntax.minieval.SingleLineStatementError: if the
            parsing/evaluation of any Python code statement fails
        :raises TemplateRenderingError: if the template rendering fails (generic)
        :raises TemplateLoopRenderingError: if the template rendering fails during
            loop expansion
        """
        return self._recursive_render(self._tplbase, kwargs)

    def _recursive_render(self, tpl, subs):
        if isinstance(tpl, dict):
            if self._TEMPLATING_KEY in tpl:
                # Do some templating :-)
                try:
                    pmethod = getattr(self, '_render_{:s}'.format(tpl[self._TEMPLATING_KEY]))
                except AttributeError:
                    raise TemplateRenderingError('Do not know what to do with: {:s}={:s}.'
                                                 .format(self._TEMPLATING_KEY,
                                                         tpl[self._TEMPLATING_KEY]))
                return pmethod(tpl, subs)
            else:
                # This is a normal dictionary
                return {self._recursive_render(k, subs): self._recursive_render(v, subs)
                        for k, v in tpl.items()}

        elif isinstance(tpl, list):
            return [self._recursive_render(v, subs) for v in tpl]

        elif isinstance(tpl, six.string_types):
            e_match = self._INLINE_EVAL_RE.match(tpl)
            if e_match:
                return self._sls(e_match.group(1), ** subs)
            else:
                return tpl

        else:
            return tpl

    def _render_loop(self, tpl, subs):
        """Deal with "loop" directives."""
        # Check arguments
        if '__loopiterator__' not in tpl:
            raise TemplateLoopRenderingError('The __loopiterator__ key is required')
        if '__loopvariables__' not in tpl:
            raise TemplateLoopRenderingError('The __loopvariables__ key is required')
        if '__body__' not in tpl:
            raise TemplateLoopRenderingError('The __body__ key is required')
        literator = self._sls(tpl['__loopiterator__'], ** subs)
        lvariables = re.split(r'\s*,\s*', tpl['__loopvariables__'])
        lbody = tpl['__body__']
        lbodyfirst = tpl.get('__body_first__', lbody)
        lbodylast = tpl.get('__body_last__', lbody)
        lextras = tpl.get('__extra_vars__', dict())
        if isinstance(lextras, dict):
            allowed = subs.copy()
            for lvar in lvariables:
                allowed[lvar] = None
            for v in lextras.values():
                self._sls.check(v, ** allowed)
        else:
            raise TemplateLoopRenderingError('The __extra_vars__ key must be a dictionary')
        # Expand the loop
        outlist = []
        for ivars_p, ivars, ivars_n in izip_pcn(literator):
            # Generate the variables list
            n_subs = subs.copy()
            if len(lvariables) == 1:
                n = lvariables[0]
                n_subs[n + '_prev'] = ivars_p[0]
                n_subs[n] = ivars[0]
                n_subs[n + '_next'] = ivars_n[0]
            else:
                for i, n in enumerate(lvariables):
                    n_subs[n + '_prev'] = ivars_p[0][i] if ivars_p[0] is not None else None
                    n_subs[n] = ivars[0][i]
                    n_subs[n + '_next'] = ivars_n[0][i] if ivars_n[0] is not None else None
            # Populate extra variables
            e_subs = n_subs.copy()
            for k, v in lextras.items():
                e_subs[k] = self._sls(v, ** n_subs)
            n_subs.update(e_subs)
            # Let's expand !
            if all([v is None for v in ivars_p]):
                # Fist one
                outlist.append(self._recursive_render(copy.copy(lbodyfirst), n_subs))
            elif all([v is None for v in ivars_n]):
                # Last one
                outlist.append(self._recursive_render(copy.copy(lbodylast), n_subs))
            else:
                outlist.append(self._recursive_render(copy.copy(lbody), n_subs))
        # Done !
        return outlist


if __name__ == '__main__':
    import doctest
    doctest.testmod()
