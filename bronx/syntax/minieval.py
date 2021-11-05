# -*- coding: utf-8 -*-

"""
Parse, check and execute single-line Python's statements.

The Python ast module is leveraged in order to parse the code and look for
potential security hazards. Notably, access to attributes prefixed by '_' are
forbidden and a few builtin method like ``globals`` or ``import`` are
deactivated.

The :func:`safe_eval` module level object provides access to the default
checker.

Examples::

    # Security threats ?
    >>> safe_eval('globals()[1].clear()')  # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
        ...
    SingleLineStatementSecurityError: The "globals" builtin is not allowed
    >>> safe_eval('stuff.__dict__.clear()', stuff='anyobject')  # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
        ...
    SingleLineStatementSecurityError: The "__dict__" attribute is not allowed

    # Missing variables ?
    >>> safe_eval('a + 1')  # doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
        ...
    SingleLineStatementSecurityError: The "a" variable is not allowed at this particular location

    # Let's show some usefull things...
    >>> safe_eval('a + 1', a=1)
    2
    >>> print(safe_eval('["Member{:02d}".format(m) for m in members if m < 2]',
    ...                 members=range(0, 3)))
    ['Member00', 'Member01']

"""

from __future__ import print_function, absolute_import, unicode_literals, division

import ast
from contextlib import contextmanager
import sys


class SingleLineStatementError(RuntimeError):
    """
    Any exception raised by the :class:`SingleLineStatement` or
    :class:`SafetyCheckNodeVisitor` classes.
    """
    pass


class SingleLineStatementParsingError(SingleLineStatementError):
    """The ast module was unable to parse the code statement (Syntax Error)."""
    pass


class SingleLineStatementSecurityError(SingleLineStatementError):
    """An unauthorised piece of code has been detected."""
    pass


class SingleLineStatementEvalError(SingleLineStatementError):
    """The evaluation (i.e. execution) of the code statement failed."""
    pass


class SafetyCheckNodeVisitor(ast.NodeVisitor):
    """Look for security threats/errors in a given AST node.

    In case something wrong is detected, a :class:`SingleLineStatementSecurityError`
    exception will be raised.
    """

    #: Allowed AST nodes (regardless of their content)
    _GENERIC_WHITELIST = set([
        ast.Expression,
        # Literals
        ast.Num, ast.Str, ast.List, ast.Tuple, ast.Set, ast.Dict,
        ast.Ellipsis,
        # Variables
        ast.Load,
        # Expressions
        ast.Expr, ast.UnaryOp, ast.UAdd, ast.USub, ast.Not, ast.Invert,
        ast.BinOp, ast.Add, ast.Sub, ast.Mult, ast.Div, ast.FloorDiv,
        ast.Mod, ast.Pow, ast.LShift, ast.RShift, ast.BitOr, ast.BitXor,
        ast.BitAnd, ast.BoolOp, ast.And, ast.Or, ast.Compare,
        ast.Eq, ast.NotEq, ast.Lt, ast.LtE, ast.Gt, ast.GtE, ast.Is,
        ast.IsNot, ast.In, ast.NotIn, ast.IfExp,
        # Calls
        ast.keyword,
        # Subscripting
        ast.Subscript, ast.Index, ast.Slice, ast.ExtSlice,
    ])

    # Some new AST nodes came up with Python 3
    if sys.version_info.major >= 3:
        _GENERIC_WHITELIST.update([ast.Bytes, ast.Starred, ])
        if sys.version_info.major > 3 or sys.version_info.minor >= 4:
            _GENERIC_WHITELIST.add(ast.NameConstant)
        if sys.version_info.major > 3 or sys.version_info.minor >= 5:
            _GENERIC_WHITELIST.add(ast.MatMult)
        if sys.version_info.major > 3 or sys.version_info.minor >= 6:
            _GENERIC_WHITELIST.update([ast.FormattedValue, ast.JoinedStr, ])

    _GENERIC_WHITELIST = tuple(_GENERIC_WHITELIST)

    #: The list of allowed builtins
    _BUILTINS_WHITELIST = set([
        'abs', 'all', 'any', 'ascii', 'bin', 'bool', 'bytearray',
        'bytes', 'chr', 'complex', 'dict', 'divmod', 'enumerate',
        'filter', 'float', 'frozenset', 'hasattr', 'hash', 'hex',
        'int', 'isinstance', 'issubclass', 'iter', 'len', 'list',
        'map', 'max', 'min', 'next', 'oct', 'ord', 'pow', 'range',
        'reversed', 'round', 'set', 'sorted', 'str', 'sum', 'tuple',
        'zip'
    ])

    def __init__(self, varnames):
        """
        :param varnames: A list of allowed global variable names
        """
        super(SafetyCheckNodeVisitor, self).__init__()
        self._varnames = varnames
        self._allowednames = set(varnames)
        if not (sys.version_info.major > 3 or
                (sys.version_info.major == 3 and sys.version_info.minor >= 4)):
            # In Python >= 3.4, this is described by the ast.NameConstant node
            self._allowednames.update(['False', 'True', 'None'])

    @contextmanager
    def _add_named(self, *kargs):
        """Temporarily add a bunch of global variable names to the allowed list."""
        todo = [k for k in kargs if k not in self._allowednames]
        self._allowednames.update(* todo)
        try:
            yield
        finally:
            for k in todo:
                self._allowednames.discard(k)

    def generic_visit(self, node):
        """Generic method that check AST nodes types agains the whitelist."""
        if not isinstance(node, self._GENERIC_WHITELIST):
            raise SingleLineStatementSecurityError('The "{:s}" AST node is not allowed'
                                                   .format(type(node)))
        super(SafetyCheckNodeVisitor, self).generic_visit(node)

    def visit_Call(self, node):
        """Check :class:`ast.Call` objects."""
        # Whitelisted builtins or allowed Attributes only...
        if isinstance(node.func, ast.Name):
            if node.func.id not in self._BUILTINS_WHITELIST:
                raise SingleLineStatementSecurityError('The "{0.id:s}" builtin is not allowed'
                                                       .format(node.func))
        elif isinstance(node.func, ast.Attribute):
            self.visit(node.func)
        else:
            raise RuntimeError('Unexpected type for node.func: {:s}'
                               .format(type(node.func)))
        # Recursively check the other attributes of the object
        for anode in node.args:
            self.visit(anode)
        for knode in node.keywords:
            self.visit(knode)
        if hasattr(node, 'starargs') and node.starargs:
            self.visit(node.starargs)
        if hasattr(node, 'kwargs') and node.kwargs:
            self.visit(node.kwargs)

    def visit_Name(self, node):
        """Check :class:`ast.Name` objects."""
        if node.id not in self._allowednames:
            raise SingleLineStatementSecurityError('The "{0.id:s}" variable is not allowed at this particular location'
                                                   .format(node))
        if not isinstance(node.ctx, ast.Load):
            raise SingleLineStatementSecurityError('Del/Store context not allowed for variable "{0.id:s}"'
                                                   .format(node))

    def visit_Attribute(self, node):
        """Check :class:`ast.Attribute` objects."""
        if node.attr.startswith('_'):
            raise SingleLineStatementSecurityError('The "{0.attr:s}" attribute is not allowed'
                                                   .format(node))
        if not isinstance(node.ctx, ast.Load):
            raise SingleLineStatementSecurityError('Del/Store context not allowed for attribute "{0.attr:s}"'
                                                   .format(node))
        self.visit(node.value)

    def _comprehension_generator1(self, generators):
        """
        Find out the name of control variables when using List, Set or Dict
        comprehension + check that the iterators are valid.
        """
        controlvariables = set()
        for agenerator in generators:
            self.visit(agenerator.iter)
            if isinstance(agenerator.target, ast.Name):
                controlvariables.add(agenerator.target.id)
            elif isinstance(agenerator.target, ast.Tuple):
                for aname in agenerator.target.elts:
                    controlvariables.add(aname.id)
            else:
                raise RuntimeError('Unexpected type for generator.target: {:s}'
                                   .format(type(agenerator.target)))
        return controlvariables

    def _comprehension_generator2(self, generators):
        """Check that the if clauses are valid when using List, Set or Dict comprehension."""
        for agenerator in generators:
            for ifnode in agenerator.ifs:
                self.visit(ifnode)

    def visit_ListComp(self, node):
        """Check :class:`ast.listComp` objects (list comprehension)."""
        with self._add_named(self._comprehension_generator1(node.generators)):
            self._comprehension_generator2(node.generators)
            self.visit(node.elt)

    def visit_SetComp(self, node):
        """Check :class:`ast.SetComp` objects (set comprehension)."""
        with self._add_named(self._comprehension_generator1(node.generators)):
            self._comprehension_generator2(node.generators)
            self.visit(node.elt)

    def visit_DictComp(self, node):
        """Check :class:`ast.DictComp` objects (dict comprehension)."""
        with self._add_named(self._comprehension_generator1(node.generators)):
            self._comprehension_generator2(node.generators)
            self.visit(node.key)
            self.visit(node.value)

    def visit_Lambda(self, node):
        """Check :class:`ast.Lambda` objects."""
        if sys.version_info.major >= 3:
            argsbase = node.args.args + node.args.kwonlyargs
            defaultbase = node.args.defaults + node.args.kw_defaults
        else:
            argsbase = list(node.args.args)
            defaultbase = list(node.args.defaults)
        # Find out the argument names and defaults
        if node.args.vararg is not None:
            argsbase.append(node.args.vararg)
        if node.args.kwarg is not None:
            argsbase.append(node.args.kwarg)
        if sys.version_info.major >= 3:
            controlvariables = set([a.arg for a in argsbase])  # arg objects
        else:
            controlvariables = set([a.id for a in argsbase])  # Name objects
        for d in defaultbase:
            if d is not None:
                self.visit(d)
        # Check the lambda's body
        with self._add_named(controlvariables):
            self.visit(node.body)


class SingleLineStatement(object):
    """Safely parse, check and evaluate a code statement.

    The interface of such a class is fairly simple. One just needs to create an
    object with no arguments and use:

        * The :meth:`check` method in order to verify the syntax and look for
          potential security threats in a code statement.
        * Just call the object (:meth:`__call__`) in order to perform all checks
          and actually run the statement.

    With both methods, a list of variables can be provided : only these variables
    will be allowed and usable in the code statement.

    .. warning:: Do not use in sensitive/exposed softwares since the
                 security provided by this class is probably very thin!

    :example: See the top module examples.
    """

    def __init__(self, visit_cls=SafetyCheckNodeVisitor):
        """
        :param ast.NodeVisitor visit_cls: The class that is used to check the
                                          code statement
        """
        self.__visit_cls = visit_cls

    @property
    def _visit_cls(self):
        """The checker class."""
        return self.__visit_cls

    def _parse_and_check(self, statement, varnames):
        """Parse and Check a code statement.

        :param str statement: The code statement
        :param set varnames: The set of allowed global variable names
        """
        try:
            tree = ast.parse(statement, mode='eval')
        except SyntaxError as e:
            raise SingleLineStatementParsingError('The parsing of < {:s} > failed: {!s}'
                                                  .format(statement, e))
        vobj = self._visit_cls(varnames)
        vobj.visit(tree)
        return tree

    def check(self, statement, ** kwargs):
        """Parse and Check a code statement.

        :param str statement: The code statement
        :param kwargs: The variables that can be used in the code statement
        :return: The ast node object representing the parsed statement
        :raises SingleLineStatementParsingError: if the ast package fails to
            parse the code statement (syntax error)
        :raises SingleLineStatementSecurityError: if something odd/forbidden is
            spotted in the code statement
        """
        return self._parse_and_check(statement, set(kwargs.keys()))

    def __call__(self, statement, ** kwargs):
        """Parse, Check and Evaluate a code statement.

        :param str statement: The code statement
        :param kwargs: The variables that can be used in the code statement
        :return: Any data returned by the evaluation of the code statement
        :raises SingleLineStatementParsingError: if the ast package fails to
            parse the code statement (syntax error)
        :raises SingleLineStatementSecurityError: if something odd/forbidden is
            spotted in the code statement
        :raises SingleLineStatementEvalError: if the evaluation of the code
            statement fails
        """
        tree = self._parse_and_check(statement, set(kwargs.keys()))
        try:
            return eval(compile(tree, filename='<ast>', mode='eval'), kwargs)
        except Exception as e:
            raise SingleLineStatementEvalError('The evaluation of < {:s} > failed: {!s}'
                                               .format(statement, e))


#: An object instantiated from the :class:`SingleLineStatement` checker class.
safe_eval = SingleLineStatement()


if __name__ == '__main__':
    import doctest
    doctest.testmod()
