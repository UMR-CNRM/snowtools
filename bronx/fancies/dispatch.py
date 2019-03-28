#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
An extended cmd line action dispatcher for interactive usage.

The first argument is the so-called action to perform.
All other command-line arguments are supposed to be options
given to action-methods of the dispatcher.

.. warning:: This module is under heavy development consequently significant
             will be made in future versions. DO NOT USE YET.

"""

from __future__ import absolute_import, unicode_literals, print_function, division
import six

import os
import sys
import subprocess
import shlex
import re
import io
import cmd
import pickle

import footprints

from bronx.stdtypes.history import PrivateHistory as Histo
from bronx.fancies import loggers, dump
from bronx.fancies.colors import termcolors
from bronx.fancies.wrapcmd import WrapCmdLineArgs
import bronx.fancies.multicfg  # @UnusedImport
from bronx.syntax.decorators import secure_getattr

logger = loggers.getLogger(__name__)


class StdColorFilter(object):
    """
    Color filtering for stdout calls.
    """

    def __init__(self, stdpipe=None, fgcolor='green', setfont='bold'):
        self.setfont = setfont
        self.fgcolor = fgcolor
        self.stdpipe = stdpipe

    def write(self, *args):
        text = ' '.join(args)
        if text.startswith('***'):
            fgcolor = 'red'
        else:
            fgcolor = self.fgcolor
        self.stdpipe.write(termcolors.colored(text, fgcolor=fgcolor, setfont=self.setfont))

    @secure_getattr
    def __getattr__(self, attr):
        return getattr(self.stdpipe, attr)


class CmdLiner(cmd.Cmd):
    """
    Abstract class for dealing with shell-like options oriented command line tasks.
    """

    def __init__(self, name='cmdliner', prompt=None, maxlen=999, logfile=None,
                 fgcolor='lightyellow', fgshell='lightcyan'):
        """
        It should be a good habit to give a significant name to the ``CmdLiner`` object
        so that history files are not mixed (read at initialisation).
        The prompt is derivated from name if not provided.
        """
        cmd.Cmd.__init__(self, stdout=StdColorFilter(stdpipe=sys.stdout))
        if prompt is None:
            prompt = name
        if logfile is None:
            logfile = name + '.log'
        self._logfile = os.path.realpath(logfile)
        self._prompt = prompt.strip()
        self._hfile = os.path.join(os.environ['HOME'], '.' + name + '.history')
        if os.path.exists(self._hfile):
            with io.open(self._hfile, 'rb') as fd:
                self._history = pickle.load(fd)
        else:
            self._history = Histo(maxlen=maxlen)
        self.fgcolor = fgcolor
        self.fgshell = fgshell
        self.setprompt()

    @property
    def history(self):
        return self._history

    @property
    def hfile(self):
        return self._hfile

    @property
    def logfile(self):
        return self._logfile

    def report(self, info):
        """Append ``info`` to the logfile."""
        with io.open(self.logfile, 'a') as fd:
            fd.write(six.text_type(info).rstrip() + u'\n')

    def emptyline(self):
        """Just do nothing when line is empty."""
        pass

    def setprompt(self):
        """Build a prompt with history count."""
        self.prompt = '[{0:s}][{1:d}] '.format(self._prompt, self.history.count + 1)

    def precmd(self, line):
        """Save non-empty command lines in history."""
        if len(self.history):
            if re.match('^!!', line):
                line = re.sub('^!!', self.history.nice(self.history.last), line)
            elif re.match(r'^!\d+', line):
                line = re.sub(r'^!(\d+)', lambda m: self.history.nice(self.history.getbynumber(int(m.group(
                    1)))), line)
        if line:
            self.history.append(line)
            self.setprompt()
        return line

    def stdlog(self, *args, **kw):
        """Print to stdout and possibly logfile the current arguments."""
        text = ' '.join([six.text_type(x) for x in args])
        if kw.get('raw', False):
            print(text)
        else:
            print(termcolors.colored(text,
                                     fgcolor=kw.get('fgcolor', self.fgcolor),
                                     setfont=kw.get('setfont', 'bold')))
        if self.logflag:
            self.report(termcolors.clean(text))

    def nicelist(self, itervalue, **opts):
        """Return a comma separated list of iterable values according to discard and grep options."""
        return ', '.join(sorted([x for x in itervalue
                                 if x not in opts['discard'] and opts['grep'].search(x)]))

    def do_shell(self, line):
        """Simply run the command line as a shell subprocess."""
        self.stdlog(subprocess.check_output(shlex.split(line)), fgcolor=self.fgshell)

    def do_reset(self, line):
        """Clear the history."""
        if os.path.exists(self._hfile):
            os.unlink(self._hfile)
        self.history.reset()

    @WrapCmdLineArgs(addhelp=True)
    def do_logfile(self, **opts):
        """Display the log filename."""
        self.stdlog(self.logfile)

    def do_report(self, line):
        """Add the current line of text to the logfile."""
        self.report(line)

    def do_log(self, line):
        """Cat the logfile."""
        if os.path.exists(self.logfile):
            self.stdlog(subprocess.check_output(['cat', self.logfile]), fgcolor=self.fgshell)

    def do_clearlog(self, line):
        """Remove the logfile."""
        if os.path.exists(self.logfile):
            os.unlink(self.logfile)

    @WrapCmdLineArgs('_all', addhelp=True)
    def do_echo(self, **opts):
        """Display all the registered arguments and their default values."""
        invopts = {v.dest: v for v in self.defined_opts.values()}
        maxname = max([len(x.optname) for x in invopts.values()])
        maxclean = max([len(x.optclean) for x in invopts.values()])
        for k, v in sorted(opts.items()):
            storeaction = invopts.get(k)
            if storeaction.optclean == storeaction.optname:
                alternate = ''
            else:
                alternate = '[' + storeaction.optname + ']'
            self.stdlog('{0:{widthclean}s} {1:{widthname}s} :'.format(
                storeaction.optclean,
                alternate,
                widthclean=maxclean,
                widthname=maxname + 2,
            ), opts[storeaction.dest])

    def save_history(self):
        """Dump actual history logfile."""
        with io.open(self._hfile, 'wb') as fd:
            pickle.dump(self._history, fd)

    @WrapCmdLineArgs(addhelp=True, strict=True)
    def do_exit(self, **opts):
        """Nice way to exit from the cmd loop."""
        self.save_history()
        return True

    @WrapCmdLineArgs(addhelp=True, strict=True)
    def do_quit(self, **opts):
        """Yet another nice way to exit from the cmd loop."""
        self.save_history()
        return True

    @WrapCmdLineArgs('discard', 'grep', addhelp=True)
    def do_commands(self, **opts):
        """List of available commands.

        Default discard: commands
        """
        self.stdlog(self.nicelist([x.split('_', 1)[-1] for x in dir(self) if x.startswith('do_')], **opts))

    @WrapCmdLineArgs('grep', 'resize', 'focus', 'delta', addhelp=True, strict=True)
    def do_history(self, **opts):
        """Resize or print history with an optional grep selection or focus.

        Default resize: -1
        """
        if opts['resize'] > 0:
            self.history.resize(maxlen=opts['resize'])
            self.stdlog(self.history.size)
        elif opts['focus'] is None and opts['delta'] is None:
            self.history.showmatch(opts['grep'])
        else:
            if opts['focus'] is not None:
                opts['focus'] = opts['focus'].as_datetime()
            if opts['delta'] is None:
                opts['delta'] = 120
            self.history.around(focus=opts['focus'], delta=opts['delta'])

    @WrapCmdLineArgs(addhelp=True, strict=True)
    def do_hfile(self, **opts):
        """Print history file path. No options."""
        self.stdlog(self.hfile)

    @WrapCmdLineArgs(addhelp=True, strict=True)
    def do_htimer(self, **opts):
        """Switch verbose time mode for history log. No options."""
        self.history.timer = not self.history.timer
        self.stdlog(self.history.timer)


class ExtendedCmdLiner(CmdLiner):
    """An other CmdLiner combined with a configuration manager and auxilary task handler."""

    def __init__(self, **kw):
        self._cfg = kw.pop('cfg', None)
        self._job = kw.pop('job', None)
        CmdLiner.__init__(self, **kw)
        self.buildgates()

    def buildgates(self):
        """Cross-link cfg and job."""
        if self.cfg is not None and self.job is not None:
            self.cfg.job = self.job
            self.job.cfg = self.cfg

    def setcfg(self, **kw):
        self._cfg = footprints.proxy.ymlconf(**kw)
        self.buildgates()

    @property
    def cfg(self):
        return self._cfg

    @WrapCmdLineArgs('cfgname', 'cfgtag', 'cfgdir', 'cfgroot', 'cfgext')
    def do_setcfg(self, **opts):
        """Define a new basis for configuration files inquiry."""
        self.setcfg(**opts)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfg(self, **opts):
        """Display the current configuration object."""
        self.stdlog(self.cfg)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgname(self, **opts):
        """Display the current configuration radical name."""
        self.stdlog(self.cfg.cfgname)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgpath(self, **opts):
        """Display the current path to configuration files."""
        self.stdlog(self.cfg.cfgpath)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgfile(self, **opts):
        """Display the current configuration file path."""
        self.stdlog(self.cfg.cfgfile)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgtag(self, **opts):
        """Display the current configuration tag."""
        self.stdlog(self.cfg.cfgtag)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgcat(self, **opts):
        """Cat the current configuration file without extra processing."""
        self.stdlog(self.cfg.raw)

    @WrapCmdLineArgs(addhelp=True)
    def do_cfginfo(self, **opts):
        """Display the parsed configuration file information."""
        self.stdlog(dump.fulldump(self.cfg.info))

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgdefaults(self, **opts):
        """Display defaults value extracted from the configuration file."""
        self.stdlog(dump.fulldump(self.cfg.defaults))

    @WrapCmdLineArgs(addhelp=True)
    def do_cfgtmp(self, **opts):
        """
        Display internal temporary values stored while processing the
        configuration file.
        """
        self.stdlog(dump.fulldump(self.cfg.tmp))

    @WrapCmdLineArgs('grep', 'discard', addhelp=True)
    def do_excluded(self, **opts):
        """
        Display internal keys that should be discarded when dealing with
        configuration information.

        Default discard: None
        """
        self.stdlog(self.nicelist(self.cfg.excluded, **opts))

    def setjob(self, **kw):
        """Define a associated job digger for the current cmd driver."""
        self._job = footprints.proxy.digger(**kw)
        self.buildgates()

    @property
    def job(self):
        return self._job

    @WrapCmdLineArgs(addhelp=True)
    def do_setjob(self, **opts):
        """Define a associated job digger for the current cmd driver."""
        self.setjob(**opts)

    @WrapCmdLineArgs(addhelp=True)
    def do_job(self, **opts):
        """Display the current associated job digger."""
        self.stdlog(self.job)
