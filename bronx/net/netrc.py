# -*- coding: utf-8 -*-

"""An object-oriented interface to .netrc files.

This code has been extacted from Python 3.5 and two patches have been applied:

* With this class, the user **login** taken into accout when looking for .netrc
  lines (see the **login** argument for the :func:`netrc.authenticators` method.
* Password can be surrounded with simple or double quotes (for compatibility
  with the standard Linux FTP client).

"""

# Module and documentation by Eric S. Raymond, 21 Dec 1998

# 09/02/2017
# Original version: Python 3.5
#                   Copyright 2001-2017 Python Software Foundation; All Rights Reserved
# Proposed patch applied: https://bugs.python.org/issue11416
# LFM: netrc._passwd_clean regex introduced to remove matching quotes

from __future__ import absolute_import, division, print_function, unicode_literals

import io
import os
import re
import shlex
import stat

#: Wildchar exports
__all__ = ["netrc", "NetrcParseError"]


class NetrcParseError(Exception):
    """
    Exception raised by the :class:`netrc` class when syntactical errors are
    encountered in source text. Instances of this exception provide three
    interesting attributes: ``msg`` is a textual explanation of the error,
    ``filename`` is the name of the source file, and ``lineno`` gives the
    line number on which the error was found.
    """

    def __init__(self, msg, filename=None, lineno=None):
        self.filename = filename
        self.lineno = lineno
        self.msg = msg
        Exception.__init__(self, msg)

    def __str__(self):
        return "%s (%s, line %s)" % (self.msg, self.filename, self.lineno)


class netrc:
    """
    A :class:`netrc` instance or subclass instance encapsulates data from a
    netrc file.

    """

    _passwd_clean = re.compile(r'''^(?P<quote>["'])(?P<pass>.*)(?P=quote)$''')

    def __init__(self, file=None):
        """
        The initialisation argument, if present, specifies the file to parse.
        If no argument is given, the file ``.netrc`` in the user's home directory
        will be read. Parse errors will raise :class:`NetrcParseError` with diagnostic
        information including the file name, line number, and terminating token.
        If no argument is specified on a POSIX system, the presence of passwords
        in the ``.netrc`` file will raise a :class:`NetrcParseError` if the file
        ownership or permissions are insecure (owned by a user other than the
        user running the process, or accessible for read or write by any other
        user). This implements security behavior equivalent to that of ftp and
        other programs that use ``.netrc``.

        .. attribute:: netrc.hosts

           Dictionary mapping host names to ``(login, account, password)`` tuples.  The
           'default' entry, if any, is represented as a pseudo-host by that name.

        """
        default_netrc = file is None
        if file is None:
            try:
                file = os.path.join(os.environ['HOME'], ".netrc")
            except KeyError:
                raise OSError("Could not find .netrc: $HOME is not set")
        self.hosts = {}
        self.allhosts = {}
        self.macros = {}
        with io.open(file, 'r') as fp:
            self._parse(file, fp, default_netrc)

    def _parse(self, file, fp, default_netrc):
        lexer = shlex.shlex(fp)
        lexer.wordchars += r"""!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
        lexer.commenters = lexer.commenters.replace('#', '')
        while 1:
            # Look for a machine, default, or macdef top-level keyword
            saved_lineno = lexer.lineno
            toplevel = tt = lexer.get_token()
            if not tt:
                break
            elif tt[0] == '#':
                if lexer.lineno == saved_lineno and len(tt) == 1:
                    lexer.instream.readline()
                continue
            elif tt == 'machine':
                entryname = lexer.get_token()
            elif tt == 'default':
                entryname = 'default'
            elif tt == 'macdef':  # Just skip to end of macdefs
                entryname = lexer.get_token()
                self.macros[entryname] = []
                lexer.whitespace = ' \t'
                while 1:
                    line = lexer.instream.readline()
                    if not line or line == '\012':
                        lexer.whitespace = ' \t\r\n'
                        break
                    self.macros[entryname].append(line)
                continue
            else:
                raise NetrcParseError(
                    "bad toplevel token %r" % tt, file, lexer.lineno)

            # We're looking at start of an entry for a named machine or default.
            login = ''
            account = password = None
            self.allhosts.setdefault(entryname, [])
            self.hosts[entryname] = {}
            while 1:
                tt = lexer.get_token()
                if (tt.startswith('#') or tt in {'', 'machine', 'default', 'macdef'}):
                    if password:
                        current_entry = (login, account, password)
                        self.allhosts[entryname].append(current_entry)
                        self.hosts[entryname] = current_entry
                        lexer.push_token(tt)
                        break
                    else:
                        raise NetrcParseError(
                            "malformed %s entry %s terminated by %s"
                            % (toplevel, entryname, repr(tt)),
                            file, lexer.lineno)
                elif tt == 'login' or tt == 'user':
                    login = lexer.get_token()
                elif tt == 'account':
                    account = lexer.get_token()
                elif tt == 'password':
                    if os.name == 'posix' and default_netrc:
                        prop = os.fstat(fp.fileno())
                        if prop.st_uid != os.getuid():
                            import pwd
                            try:
                                fowner = pwd.getpwuid(prop.st_uid)[0]
                            except KeyError:
                                fowner = 'uid %s' % prop.st_uid
                            try:
                                user = pwd.getpwuid(os.getuid())[0]
                            except KeyError:
                                user = 'uid %s' % os.getuid()
                            raise NetrcParseError(
                                ("~/.netrc file owner (%s) does not match"
                                 " current user (%s)") % (fowner, user),
                                file, lexer.lineno)
                        if prop.st_mode & (stat.S_IRWXG | stat.S_IRWXO):
                            raise NetrcParseError(
                                "~/.netrc access too permissive: access"
                                " permissions must restrict access to only"
                                " the owner", file, lexer.lineno)
                    password = self._passwd_clean.sub(r'\g<pass>', lexer.get_token())
                else:
                    raise NetrcParseError("bad follower token {!r} in {!r} line {}".format(
                        tt, file, lexer.lineno))

    def authenticators(self, host, login=None):
        """
        Return a 3-tuple ``(login, account, password)`` of authenticators
        for host. If the netrc file did not contain an entry for the given
        host, return the tuple associated with the 'default' entry. If neither
        matching host nor default entry is available, return ``None``.

        When provided, the optional login argument selects a tuple
        with a matching user name.
        """
        if host in self.hosts:
            if login is not None:
                for host_login in self.allhosts[host]:
                    if host_login[0] == login:
                        return host_login
                return None
            else:
                return self.hosts[host]
        elif 'default' in self.hosts:
            return self.hosts['default']
        else:
            return None

    def __repr__(self):
        """
        Dump the class data as a string in the format of a netrc file.
        (This discards comments and may reorder the entries.)
        """
        rep = ""
        for host, attrlist in self.allhosts.items():
            for attrs in attrlist:
                rep = rep + "machine " + host + "\n\tlogin " + repr(attrs[0]) + "\n"
                if attrs[1]:
                    rep = rep + "account " + repr(attrs[1])
                rep = rep + "\tpassword " + repr(attrs[2]) + "\n"
        for macro in self.macros.keys():
            rep = rep + "macdef " + macro + "\n"
            for line in self.macros[macro]:
                rep = rep + line
            rep = rep + "\n"
        return rep


if __name__ == '__main__':
    print(netrc())
