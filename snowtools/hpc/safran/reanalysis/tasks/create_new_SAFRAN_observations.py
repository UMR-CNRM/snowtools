from vortex.layout.nodes import Driver

from snowtools.tasks.research.obs.reconstruct_obs_safran import Reconstruct_SAFRAN_Obs


def setup(t, **kw):
    return Driver(
        tag='safran_obs',
        ticket=t,
        nodes=[
            Reconstruct_SAFRAN_Obs(tag='safran_obs', ticket=t, **kw),
        ],
        options=kw,
    )
