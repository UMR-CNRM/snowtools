"""
TODO: module documentation.
"""

from bronx.fancies import loggers

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class _ReportContext:
    """Context manager that prints a report."""

    def __init__(self, task, ticket):
        self._task = task
        self._ticket = ticket

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_value, traceback):
        self._report(self._ticket, exc_type is None)

    def _report(self, t, try_ok=True):
        """Report status of the session (IO review)."""
        raise NotImplementedError("To be overwritten...")


class InputReportContext(_ReportContext):
    """Context manager that prints a report on inputs."""

    def _report(self, t, try_ok=True, **kw):
        """Report status of the session (input review)."""
        t.sh.header('Input review')
        report = t.context.sequence.inputs_report()
        report.print_report(detailed=True)
        if try_ok:
            if any(report.active_alternates()):
                t.sh.header('Input informations: active alternates were found')
            elif any(report.missing_resources()):
                t.sh.header('Input informations: missing resources')
            else:
                t.sh.header('Input informations: everything is ok')
        else:
            t.sh.header('Input informations: one of the input failed')


class OutputReportContext(_ReportContext):
    """Context manager that prints a report on outputs."""

    def _report(self, t, try_ok=True, **kw):
        """Report status of the session (output review)."""
        if try_ok:
            t.sh.header('Output informations: everything is ok')
        else:
            t.sh.header('Output informations: one of the output failed')


class TestReportContext(_ReportContext):
    """Context manager that prints a report on tests results."""

    def _report(self, t, try_ok=True, **kw):
        """Report status of the session (test review)."""
        if try_ok:
            t.sh.header('Test informations: everything is ok')
        else:
            t.sh.header('Test informations: the test failed')
