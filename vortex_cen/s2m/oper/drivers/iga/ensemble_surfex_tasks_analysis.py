# -*- coding: utf-8 -*-

from .ensemble_surfex_tasks_common import Ensemble_Surfex_Task
from .hydro_task import Hydro_Task
from .ensemble_surfex_tasks_bdpe import Rapatrie_Forcing, Rapatrie_Prep, Rapatrie_Pro, Rapatrie_Forcing_Deterministic, Rapatrie_Pro_Deterministic
from mkjob.nodes import Driver


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
            Hydro_Task(tag='S2M_Hydro_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
            Rapatrie_Forcing_Deterministic(tag='Rapatrie_Forcing_Deterministic', ticket=t, **kw),
            Rapatrie_Pro_Deterministic(tag='Rapatrie_Pro_Deterministic', ticket=t, **kw),
            Rapatrie_Forcing(tag='Rapatrie_Forcing', ticket=t, **kw),
            Rapatrie_Pro(tag='Rapatrie_Pro', ticket=t, **kw),
            Rapatrie_Prep(tag='Rapatrie_Prep', ticket=t, **kw),
        ],
        options=kw
    )
