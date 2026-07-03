# -*- coding: utf-8 -*-
"""
pre_process.py
--------------

Tasks designed to launch the pre-process the OPTIONS.nam namelist before any SURFEX binary execution.

.. inheritance-diagram:: vortex_cen.tasks.surfex.pre_process
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: _Preprocess
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: Preprocess_Uenv_Namelist
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: Preprocess_Local_Namelist
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: Soda_Namelist_Preprocess
   :no-members:
   :class-doc-from: class
   :show-inheritance:

"""

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class _Preprocess(SurfexCommonsMixin, _CenResearchTask):
    """
    **Task: _Preprocess**

    Abstract task for pre-processing namelist:
    add infos like points and dates from forcing to namelist.

    **Input:**

    - SURFEX namelist (OPTIONS.nam)
    - FORCING file

    **Output:**

    - Modified and ready-to-use SURFEX namelist

    """
    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "diff_xpid",
            "diff_user",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Change the namelist when forcings and namelist are here
        """
        avail_forcings = self.ticket.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            raise InputCheckerError('No FORCING file present, the task can not run properly')

        # Algo component to preprocess the namelist (adjust dates, etc.)
        self.sh.title('Toolbox algo preprocess')
        preprocess_tba = vortex.task(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo preprocess =', preprocess_tba)
        print()
        return preprocess_tba

    def launch_algo(self, algo):
        """
        Launch an algo component.
        :param algo: algo component
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the changed namelist in cache -> namespace = 'vortex.CACHE.fr'
        """
        self.sh.title('Toolbox output Namelist after modification (local cache only)')
        namelist_tbo = vortex.output(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',  # Never put a namelist on Hendrix
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist_tbo =', namelist_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        pass
#        self.sh.title("Reproductibility check : OPTIONS.nam")
#        diff = vortex.diff(
#            role         = 'Nam_surfex',
#            kind         = 'namelist',
#            model        = 'surfex',
#            local        = 'OPTIONS.nam',
#            experiment   = self.conf.diff_xpid,
#            username     = self.conf.get('diff_user', None),
#            namespace    = 'vortex.multi.fr',  # A single reference namelist should be on Hendrix
#            block        = 'namelist',
#            nativefmt    = 'nam',
#        ),
#        print(self.ticket.prompt, 'diff =', diff)
#        print()


class Preprocess_Uenv_Namelist(_Preprocess):
    """
    **Task: Preprocess_Uenv_Namelist**

    Task for pre-processing a namelist coming from a User Environment.
    NB : This is the task to use to guarantee the simulation's reproductibility
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
            "namelist_source",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get namelist from a User Environment.
        """
        super().get_remote_inputs()
        self.get_namelist_from_uenv()


class Preprocess_Local_Namelist(_Preprocess):
    """
    **Task: Preprocess_Local_Namelist**

    Task for pre-processing a namelist coming from any user-defined absolute path.
    WARNING : The simulation's reproductibility can not be guaranteed with this task !
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "namelist_path",

        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get namelist from a user-provided absolute path.
        """
        super().get_remote_inputs()
        self.get_namelist_from_path()


class Soda_Namelist_Preprocess(SurfexCommonsMixin, _CenResearchTask):
    """
    **Task: Soda_Namelist_Preprocess**

    Pre-process SURFEX namelist for SODA executable

    **Input:**

    - SODA namelist (OPTIONS.nam)

    **Outputs:**

    - SODA namelist (OPTIONS.nam)
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
            "namelist_source",
            "nmembers",

        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_namelist_from_uenv()

    def get_local_inputs(self):
        pass

    def algo(self):

        self.sh.title('Algo soda preprocess')
        algo = vortex.task(
            kind         = 'soda_preprocess',
            nmembers      = self.conf.nmembers,
        )
        print(self.ticket.prompt, 'Algo soda preprocess =', algo)
        print()

        return algo

    def launch_algo(self, algo):
        """
        Launch a soda namelist preprocess algorithm.
        :param algo: algorithm to launch
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):

        self.sh.title('Output namelist')
        namelist = vortex.output(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS_OUT.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print(self.ticket.prompt, 'Namelist = ', namelist)
        print()
