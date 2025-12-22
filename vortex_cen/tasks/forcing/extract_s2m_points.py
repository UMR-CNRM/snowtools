# -*- coding: utf-8 -*-
'''
'''

from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex import toolbox
import vortex_cen  # noqa


class ExtractS2MForcing(_CenResearchTask):
    '''
    Extract a list of massifs from a forcing file
    TODO
    '''

    def get_remote_inputs(self):

        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc')

    def algo(self):
        """
        TODO
        """

        for footprint in ['massifs', 'slopes', 'elevations', 'aspects']:
            if footprint not in self.conf:
                self.conf[footprint] = None

        self.sh.title('Toolbox algo')
        algo = toolbox.algo(
            kind         = 'ExtractMassifs',
            massifs      = self.conf.massifs,
            slopes       = self.conf.slopes,
            elevations   = self.conf.elevations,
            aspects      = self.conf.aspects,
            role_members = 'Forcing',
            engine       = 'algo',
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()
        algo.run()

    def put_remote_outputs(self):
        """
        TODO
        """

        # TODO : ajouter unesécurité pour ne pas risque d'écraser le fichier d'origine
        self.sh.title('Toolbox input FORCING')
        forcing_out = toolbox.output(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = self.conf.xpid,
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = 'meteo',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()
