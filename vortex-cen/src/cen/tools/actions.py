"""
Actions specific to CEN needs.
"""

from vortex.tools.actions import TemplatedMail, actiond

#: Export nothing
__all__ = []


class CenMail(TemplatedMail):
    """
    Class responsible for sending pre-defined mails for CEN.
    """

    def __init__(self, kind='cenmail', service='cenmail', active=True,
                 catalog=None, inputs_charset=None):
        super().__init__(kind=kind, active=active, service=service,
                         catalog=catalog, inputs_charset=inputs_charset)
        self.off()  # Inactive by default


actiond.add(CenMail(inputs_charset='utf-8'))
