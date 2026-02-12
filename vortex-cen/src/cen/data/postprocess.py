"""
SURFEX/Crocus post-processing resources.
"""

from vortex_cen.data.flow import SurfaceIO


class Postproc(SurfaceIO):
    """Class for post-processed SURFEX/Crocus output files."""

    # TODO : Il n'y a pas vraiment d'intéret à avoir une classe distincte de "SurfaceIO"  ou "Pro" en l'état
    # La logique voudrait que :
    # kind  = 'postproc'
    # model = 'surfex'
    # Pourquoi ne pas hériter de "SurfexPeriodDiagnostics" / "SurfexDiagnostics" (common/data/diagnostics.py)
    # --> cela permettrait d'ajouter le type de diagnostique / post-process ("scope")

    _footprint = [
        dict(
            info = 'Post-processed Surfex-simulated snowpack files',
            attr = dict(
                kind = dict(
                    values = ['SnowpackSimulation'],
                    # values = ['SnowpackSimulation', 'ProPostProcess'], ?
                ),
                model = dict(
                    values = ['postproc'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "POSTPROC"


class ScoresSnow(SurfaceIO):
    """Class for scores of snow simulations."""

    _footprint = [
        dict(
            info = 'Safran-produced forcing file',
            attr = dict(
                kind = dict(
                    values = ['ScoresSnow'],
                ),
                model = dict(
                    values = ['surfex'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "scores"
