from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex_preprocess import Prep


def setup(t, **kw):
    return Driver(
        tag='Prep',
        ticket=t,
        nodes=[
            Prep(tag='prep', ticket=t, **kw),
        ],
        options=kw
    )
