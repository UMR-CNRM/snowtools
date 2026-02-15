"""
This module contains the services specifically needed by Olive.
"""

from vortex.tools.services import AbstractRdTemplatedMailService

#: Export nothing
__all__ = []


class CenMailService(AbstractRdTemplatedMailService):
    """Class responsible for sending predefined mails.

    This class should not be called directly.
    """

    _footprint = dict(
        info = 'CEN predefined mail services class',
        attr = dict(
            kind = dict(
                values   = ['cenmail'],
            ),
        )
    )

    _TEMPLATES_SUBDIR = 'cenmails'
