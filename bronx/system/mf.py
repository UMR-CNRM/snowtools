# -*- coding: utf-8 -*-

"""
Meteo France specific system related tools.
"""

from __future__ import print_function, absolute_import, unicode_literals, division
from six import BytesIO

import ftplib
import netrc
import re
import uuid

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)

#: No automatic export
__all__ = []


def prestage(resource_paths,
             mail=None,
             archive_machine='hendrix',
             stagedir='/DemandeMig/ChargeEnEspaceRapide'):
    """
    Puts a pre-staging request on **archive_machine** for the given list of
    resources **resource_paths**, and return the path to the submitted request
    file.

    :param resource_paths: list of paths to requested resources
    :param mail: if given, used for informing about the request progress.
    :param archive_machine: name of the archive machine. Will probably not work
                            for other than *hendrix* for now...
    :param stagedir: directory in which prestaging request are to be put
                     on **archive_machine**

    .. note::
        Uses *~/.netrc* to connect to **archive_machine**.
    """
    # build request
    if mail is not None:
        if re.match(r'([a-zA-Z\-]+)\.([a-zA-Z\-]+)\@meteo.fr', mail):
            request = ["#MAIL=" + mail + '\n', ]
        else:
            logger.warning('invalid **mail** format: ' + mail)
            request = []
    else:
        request = []
    request += [r + '\n' for r in resource_paths]
    # connect to archive
    try:
        (_login, _, _passwd) = netrc.netrc().authenticators(archive_machine)
    except TypeError:
        if netrc.netrc().authenticators(archive_machine) is None:
            raise IOError("host " + archive_machine + " is unknown in .netrc")
        else:
            raise
    ftp = ftplib.FTP(archive_machine)
    ftp.login(_login, _passwd)
    # send request
    request_filename = '.'.join([_login,
                                 'staging_request',
                                 uuid.uuid4().hex[:8],  # [:8] safe enough ?
                                 'MIG'])
    f = BytesIO()
    f.writelines([line.encode('utf-8') for line in request])
    f.seek(0)
    ftp.cwd(stagedir)
    ftp.storbinary('STOR ' + request_filename, f)
    f.close()
    ftp.quit()
    # send back request identifier
    return '/'.join([stagedir, request_filename])
