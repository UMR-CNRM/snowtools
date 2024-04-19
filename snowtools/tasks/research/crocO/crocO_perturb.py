# -*- coding: utf-8 -*-
"""
Created on 27 mars 2019

@author: lafaysse
Vortex task for perturbation of forcings

"""

from vortex import toolbox

from vortex.layout.nodes import Driver, Task
from vortex import toolbox

from cen.layout.nodes import S2MTaskMixIn


from bronx.stdtypes.date import Date
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend


def setup(t, **kw):
    return Driver(
        tag='Perturb_Driver',
        ticket=t,
        nodes=[
            Perturb_Task(tag='Perturb_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Perturb_Task(Task, S2MTaskMixIn):
    """
    Task for ensemble offline sequence between 2 assimilation dates
    """

    def process(self):
        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.07@CONST_CEN'

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc)

        members = list(range(0, int(self.conf.nmembers) + 1))

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # Try to find a forcing covering the full simulation period
            tb01 = toolbox.input(
                role           = 'Forcing',
                kind           = 'MeteorologicalForcing',
                vapp           = self.conf.meteo,
                vconf          = '[geometry:area]' if source_safran == 'safran' else '[geometry:tag]',
                source_app     = dict_source_app_safran if source_safran == 'safran' else None,
                source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
                cutoff         = 'assimilation',
                local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' \
                                 if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.forcingid,
                block          = block_safran,
                geometry       = list_geometry,
                nativefmt      = 'netcdf',
                model          = 'safran',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                intent         = 'inout',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                fatal          = False,
            ),

            if tb01[0]:
                oneforcing = True
            else:
                oneforcing = False
                # Look for yearly forcing files
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Forcing',
                    kind           = 'MeteorologicalForcing',
                    vapp           = self.conf.meteo,
                    vconf          = '[geometry:area]' if source_safran == 'safran' else '[geometry:tag]',
                    source_app     = dict_source_app_safran if source_safran == 'safran' else None,
                    source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
                    cutoff         = 'assimilation',
                    local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' \
                                     if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.forcingid,
                    block          = block_safran,
                    geometry       = list_geometry,
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    intent         = 'inout',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                ),

                print(t.prompt, 'tb01 =', tb01)
                print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tbalgo')
            tbalgo = toolbox.algo(
                engine='s2m',
                kind='perturbforcing',
                members = members,
                datebegin=self.conf.datebegin,
                dateend=self.conf.dateend,
                ntasks=self.conf.nmembers,
                geometry_in=list_geometry,
                geometry_out=self.conf.geometry.tag,
                reprod_info=self.get_reprod_info,
            )
            print(t.prompt, 'tbalgo =', tbalgo)
            print()
            tbalgo.run()

        if 'backup' in self.steps or 'late-backup' in self.steps:

            tbout = toolbox.output(
                role           = 'Forcing',
                local          = 'mb[member%04d]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                vapp           = self.conf.meteo,
                experiment     = self.conf.xpid,
                member         = members,
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_forc,
                dateend        = dict_dates_end_forc,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                model          = 'safran',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',  # ???
                block          = 'meteo'
            ),
            print(t.prompt, 'tbout =', tbout)
            print()
