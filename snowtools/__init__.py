
from . import utils  # noqa

# disble if unavailable
from bronx.syntax.externalcode import ExternalCodeImportChecker

vortex_checker = ExternalCodeImportChecker('vortex')
with vortex_checker:
    from . import algo  # noqa
