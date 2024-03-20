#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
A small tool to get information from git. Useful to reuse it in python scripts
(and get a trace of the version of the code used when doing a simulation).

Can also be used as a script passing the path as an argument.
It will print the main informations.

.. note::
    This class does nothing when run with python 2

:Authors:
    Léo Viallon-Galinier
"""

import subprocess
import logging
import os

logger = logging.getLogger('snowtools.gitutils')

timeout = 1  # s
CMD_DISCOVER = ['git', 'rev-parse', '--show-toplevel']
#: The keys that :class:`git_infos` get from git log
COMMAND_OUTPUT = ('commit', 'short_commit', 'author', 'author_email', 'date',
                  'committer', 'committer_email', 'committer_date',
                  'branch_raw', 'message', 'message_full')
COMMAND = ['git', 'log', '-1', '--date=iso', '--format=%H%n%h%n%an%n%ae%n%ad%n%cn%n%ce%n%cd%n%D%n%s%n%B']
COMMAND_CLEAN = ['git', 'status', '--porcelain', '--untracked-files=no']
COMMAND_TAG = ['git', 'describe', '--tags', '--abbrev=0']


class _ChdirException(OSError):
    """
    Exception when changing directory
    """
    pass


class _chdir:
    """
    Change directory with a correct management of errors
    """
    def __init__(self, path=None):
        self.path = path
        self.oldpwd = os.getcwd()
        self.chdir = False

    def __enter__(self):
        if self.path is None:
            return self
        try:
            os.chdir(self.path)
            self.chdir = True
            return self
        except (OSError, FileNotFoundError, PermissionError, NotADirectoryError) as e:
            logger.error('Incorrect path: {}'.format(e))
            raise _ChdirException('Could not change directory to {}'.format(self.path)) from e

    def __exit__(self, e_type, e_value, e_traceback):
        if self.chdir:
            os.chdir(self.oldpwd)


def current_git_repo(path=None):
    """
    Get the git repo path if we are in a git repo
    else return None

    :param path: Path where to search for git repo (default is curent directory)
    :type path: str or path-like

    :returns: Path of git repository, or None if an error occurs or not in a git repository
    :rtype: str
    """
    try:
        with _chdir(path):
            spg = subprocess.run(CMD_DISCOVER, timeout=timeout, capture_output=True, check=True, encoding='utf-8')
        if spg.stdout is not None and len(spg.stdout) > 0:
            lines = spg.stdout.splitlines()
            if len(lines) > 0 and len(lines[0]) > 0:
                return lines[0]
    except subprocess.TimeoutExpired:
        logger.warning('Timeout when discovering git repository')
    except subprocess.CalledProcessError:
        logger.warning('Error when discovering git repository')
    except _ChdirException:
        logger.error('Could not reach target directory {}'.format(path))
    except FileNotFoundError:
        logger.warning('GIT is not installed in your environment')
    except TypeError:
        logger.warning('GIT folder not found. Maybe too old version of python3')
    return None


class git_infos:
    """
    Get git informations and put it in a dictionary
    ``self.dict``.

    :param path: Path where to search for git repo (if None, search in curent directory)
    :type path: str or path-like

    To get access to all information in a dictionnary, look at the
    ``dict`` attribute. Information can also be get by subscripting
    the object directly.

    All data is given as a string. A uncomplete list of available keys
    is available :data:`here<COMMAND_OUTPUT>`. Parsed data includes:

     - ``path``: it is the only one to be guaranteed to be present. If ``None``, the git repository have not been found.
     - ``commit``: the full commit hash
     - ``short_commit``: the short commit hash
     - ``author``, ``author_email``
     - ``committer``, ``committer_email``, ``committer_date``
     - ``date`` : the date, as a string but on iso format
     - ``branch`` : the branch name, if the branch label is on the current commit only
     - ``message`` : the commit header message
     - ``message_full`` : the full commit message
     - ``last_tag`` : the last tag on the history
     - ``clean`` : a boolean, True if the followed files have not been modified since last commit

    Note that some of the keys may not be defined if the git repository was not found.

    Example :

    .. code-block:: python

       import os
       from snowtools.utils.git import git_infos

       gi = git_infos(os.environ.get('EXESURFEX'))

    you can then access information :

    .. code-block:: python

       >>> gi['commit']
       98730c7c7f486dc85462b0427ff2ff9275bd5537

    print all what was collected:

    .. code-block:: python

       >>> print(gi)
       path: /home/viallonl/bin/snowtools
       commit: 98730c7c7f486dc85462b0427ff2ff9275bd5537
       short_commit: 98730c7
       author: Matthieu Lafaysse
       author_email: matthieu.lafaysse@meteo.fr
       date: 2021-07-08 16:53:15 +0200
       committer: Matthieu Lafaysse
       committer_email: matthieu.lafaysse@meteo.fr
       committer_date: 2021-07-08 16:53:15 +0200
       branch_raw: HEAD -> master, origin/master, origin/HEAD
       message: bdpe for post-processing
       message_full: bdpe for post-processing
       branch: master
       last_tag: s2m_reanalysis_2020.2
       clean: True

    or get a dictionnary for further use:

    .. code-block:: python

       >>> gi.dict
       {'path': '/home/viallonl/bin/snowtools',
        'commit': '98730c7c7f486dc85462b0427ff2ff9275bd5537',
        'short_commit': '98730c7',
        'author': 'Matthieu Lafaysse',
        'author_email': 'matthieu.lafaysse@meteo.fr',
        'date': '2021-07-08 16:53:15 +0200',
        'committer': 'Matthieu Lafaysse',
        'committer_email': 'matthieu.lafaysse@meteo.fr',
        'committer_date': '2021-07-08 16:53:15 +0200',
        'branch_raw': 'HEAD -> master, origin/master, origin/HEAD',
        'message': 'bdpe for post-processing',
        'message_full': 'bdpe for post-processing',
        'branch': 'master',
        'last_tag': 's2m_reanalysis_2020.2',
        'clean': False}

    Some very often used strings are also computed by pre-coded functions with
    management of lacking keys:

    .. code-block:: python

       >>> gi.pretty_str_commit()
       c46bb7a088fc182950621f6849e64d2654987325 (master) by Matthieu Lafaysse on 2021-07-20 (from s2m_reanalysis_2020.2)
       >>> gi.get_commit()
       c46bb7a088fc182950621f6849e64d2654987325

    """

    def __init__(self, path=None):
        self.path = current_git_repo(path=path)
        self.dict = {'path': self.path}
        if self.path is not None:
            self.parse_commit_infos()
            self.dict['branch'] = self._extract_branch()
            self.dict['last_tag'] = self.last_tag()
            self.dict['clean'] = self.is_clean()

    def parse_commit_infos(self):
        """
        Parse commit information, executing ``git log -1``
        and fill the dict containing all git informations
        """
        try:
            with _chdir(self.path):
                spg = subprocess.run(COMMAND, timeout=timeout, capture_output=True, check=True, encoding='utf-8')
            if spg.stdout is not None and len(spg.stdout) > 0:
                lines = spg.stdout.splitlines()
                for i in range(len(COMMAND_OUTPUT)-1):
                    if len(lines) > i:
                        self.dict[COMMAND_OUTPUT[i]] = lines[i]
                if len(lines) >= len(COMMAND_OUTPUT):
                    self.dict[COMMAND_OUTPUT[-1]] = '\n'.join(lines[len(COMMAND_OUTPUT)-1:])
        except subprocess.TimeoutExpired:
            logger.warning('Timeout when discovering git repository')
        except subprocess.CalledProcessError:
            logger.warning('Error when discovering git repository')
        except _ChdirException:
            logger.error('Could not reach target directory {}'.format(self.path))

    def _extract_branch(self):
        """
        Parse branch_raw key and return main branch name
        """
        if 'branch_raw' in self.dict:
            sp = self.dict['branch_raw'].split(',')
            if len(sp) > 0 and len(sp[0]) > 0:
                if ' -> ' in sp[0]:
                    sp1 = sp[0].split(' -> ')
                    return sp1[-1]
                else:
                    return sp
        return None

    def is_clean(self):
        """
        Check if there is modified files in the repo.

        :returns: True if the repository is clean (no modified file), else False.
                  Could also return None in case of error in command execution or git repo not found.
        :rtype: bool
        """
        try:
            with _chdir(self.path):
                spg = subprocess.run(COMMAND_CLEAN, timeout=timeout, capture_output=True, check=True, encoding='utf-8')
            if spg.stdout is None:
                return None
            if len(spg.stdout) == 0:
                return True
            lines = spg.stdout.splitlines()
            if len(lines) == 1 and len(lines[0]) == 0:
                return True
            return False

        except subprocess.TimeoutExpired:
            logger.warning('Timeout when discovering git repository')
        except subprocess.CalledProcessError:
            logger.warning('Error when discovering git repository')
        except _ChdirException:
            logger.error('Could not reach target directory {}'.format(self.path))
        return None

    def last_tag(self):
        """
        Get the last available tag from current commit

        :returns: The last available tag
        :rtype: str
        """
        try:
            with _chdir(self.path):
                spg = subprocess.run(COMMAND_TAG, timeout=timeout, capture_output=True, check=True, encoding='utf-8')
            if spg.stdout is None or len(spg.stdout) == 0:
                return None
            lines = spg.stdout.splitlines()
            if len(lines) > 0 and len(lines[0]) > 0:
                return lines[0]
        except subprocess.TimeoutExpired:
            logger.warning('Timeout when discovering git repository')
        except subprocess.CalledProcessError:
            logger.warning('Error when discovering git repository')
        except _ChdirException:
            logger.error('Could not reach target directory {}'.format(self.path))
        return None

    def pretty_str_commit(self, short=False, default=''):
        """
        Return a pretty one-line string to describe the commit

        Example: ``c46bb7a088fc182950621f6849e64d2654987325 (master) by Matthieu Lafaysse on 2021-07-20 (from s2m_reanalysis_2020.2)``

        :param short: If True, return the short commit format
        :type short: bool
        :param default: Default string to be returned if commit could not be read
        :type default: str

        :returns: A pretty string describing the commit
        :rtype: str
        """
        if self.dict['path'] is None:
            return default
        r = []
        key_commit = 'short_commit' if short else 'commit'
        if key_commit in self.dict:
            r.append(self.dict[key_commit])
        if 'branch' in self.dict:
            r.append('({})'.format(self.dict['branch']))
        if 'author' in self.dict:
            r.append('by {}'.format(self.dict['author']))
        if 'date' in self.dict and len(self.dict['date']) >= 10:
            r.append('on {}'.format(self.dict['date'][:10]))
        if 'last_tag' in self.dict:
            r.append('(from {})'.format(self.dict['last_tag']))
        if 'clean' in self.dict:
            if not self.dict['clean']:
                r.append('[+]')
        else:
            r.append('[?]')

        if len(r) == 0:
            return default
        else:
            return ' '.join(r)

    def get_commit(self, short=False, default=''):
        """
        Return the commit number if present, and a zero-length
        string else (or default if specified).

        :param short: If True, return the short commit format
        :type short: bool
        :param default: Default string to be returned if commit could not be read
        :type default: str

        :returns: commit string
        :rtype: str
        """
        key = 'short_commit' if short else 'commit'
        if key in self.dict:
            return self.dict[key]
        else:
            return default

    def __str__(self):
        r = ""
        for key, value in self.dict.items():
            r += '{}: {}\n'.format(key, value)
        return r

    def __getitem__(self, key):
        return self.dict[key]

    def __contains__(self, key):
        return self.dict.__contains__(key)

    def __iter__(self):
        return self.dict.__iter__()

    def __next__(self):
        return self.dict.__next__()


def get_summary_git(path=None):
    """
    Return a short string describing the state of git repository.

    Try to use git command. If this fails, search for a .git_info file
    and return its content.

    :param path: Path where to search for git repo (if None, search in curent directory)
    :type path: str or path-like
    """
    # Disable messages
    logging.getLogger('snowtools.gitutils').setLevel(logging.ERROR)
    # Check if we are in a git repository
    git_repo = current_git_repo(path=path)
    if git_repo is not None:
        # Use above defined tool with git tool
        try:
            text = git_infos(path=path).pretty_str_commit(short=True)
        except:
            text = ''
        return text
    else:
        # Search for a .git_info file
        path = path if path is not None else ''
        gitinfos_file_path = os.path.join(path, '.git_info')
        if os.path.isfile(gitinfos_file_path):
            try:
                with open(gitinfos_file_path, 'r') as f:
                    text = f.readline()
                    if len(text) > 1:
                        return text.replace('\n', '')
            except:
                pass
    return ''


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Tool to read git informations easily (last commit)")
    parser.add_argument("path", default=None, help="Path for git repository. Default to current directory", nargs='?')
    parser.add_argument("--summary", action="store_true", default=False, help="Get a summary "
                        "(compatible with .git_info files) rather than full information.")
    args = parser.parse_args()
    if args.summary:
        print(get_summary_git(path=args.path))
    else:
        gi = git_infos(args.path)
        if args.path is None:
            print('Git info on current directory:')
        else:
            print('Git info on {}:'.format(args.path))
        print(gi)
