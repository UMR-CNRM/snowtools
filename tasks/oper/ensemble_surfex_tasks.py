'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import daterange, yesterday, tomorrow
import footprints


def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Ensemble_Surfex_Task(Task, S2MTaskMixIn):
    '''

    '''

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep = self.get_rundate_prep()
        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()

        pearpmembers, members = self.get_list_members()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = toolbox.input(
                role           = 'Forcing',
                local          = 'mb035/[geometry::area]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if self.conf.geometry.area == 'postes' else 'mb035/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:area]',
                block          = block_safran,
                member         = 35 if source_safran == 's2m' else None,
                source_app     = 'arpege' if source_safran == 'safran' else None,
                source_conf    = '4dvarfr' if source_safran == 'safran' else None,
                experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                geometry       = list_geometry,
                date           = rundate_forcing,
                datebegin      = datebegin if source_safran == 'safran' else yesterday(base=datebegin),
                dateend        = dateend,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                namespace      = 'vortex.multi.fr',
                model          = source_safran,
                cutoff         = 'production' if self.conf.previ else 'assimilation'
            ),
            print(t.prompt, 'tb01 =', tb01)
            print()

            self.sh.title('Toolbox input tb01b')
            tb01b = toolbox.input(
                role           = 'Forcing',
                local          = 'mb[member]/[geometry::area]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if self.conf.geometry.area == 'postes' else 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:area]',
                block          = block_safran,
                source_app     = 'arpege' if source_safran == 'safran' else None,
                source_conf    = 'pearp' if source_safran == 'safran' else None,
                experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                geometry       = list_geometry,
                date           = rundate_forcing,
                datebegin      = datebegin if source_safran == 'safran' else yesterday(base=datebegin),
                dateend        = dateend,
                member         = pearpmembers,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                namespace      = 'vortex.multi.fr',
                model          = source_safran,
                cutoff         = 'production' if self.conf.previ else 'assimilation'
            ),
            print(t.prompt, 'tb01b =', tb01b)
            print()

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                geometry       = self.conf.geometry,
                genv            = 'uenv:cen.01@CONST_CEN',
                gvar           = 'pgd_[geometry::area]',
                model          = 'surfex',
                fatal          = True,
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datevalidity   = datebegin,
                date           = rundate_prep,
                member         = 35,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                fatal          = True,
                cutoff         = 'assimilation'
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv            = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb05 =', tb05)
            print()

            self.sh.title('Toolbox input tb06')
            tb06 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = 'uenv:cen.01@CONST_CEN',
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model          = 'surfex',
            )
            print(t.prompt, 'tb06 =', tb06)
            print()

            self.sh.title('Toolbox input tb07')

            tb07 = toolbox.input(
                role            = 'Nam_surfex',
                source          = 'OPTIONS_default.nam',
                genv            = 'uenv:cen.01@CONST_CEN',
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
            )

            self.sh.title('Toolbox input tb07')

            tb07 = toolbox.input(
                role            = 'Nam_surfex',
                source          = 'OPTIONS_sytron.nam',
                genv            = 'uenv:cen.01@CONST_CEN',
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS_sytron.nam',
            )

            print(t.prompt, 'tb07 =', tb07)
            print()

            self.sh.title('Toolbox executable tb08= tbx1')
            tb08 = tbx1 = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = 'uenv:cen.01@CONST_CEN',
                gvar           = 'master_offline_nompi',
            )

            print(t.prompt, 'tb08 =', tb08)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb09 = OFFLINE')

            tb09 = tbalgo1 = toolbox.algo(
                engine         = 'blind',
                kind           = "ensmeteo+sytron",
                datebegin      = datebegin,
                dateend        = dateend,
                dateinit       = datebegin,
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(members),
                geometry       = list_geometry,
                ntasks         = 40,
                daily          = not self.conf.previ,
            )
            print(t.prompt, 'tb09 =', tb09)
            print()
            self.component_runner(tbalgo1, tbx1)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:
            print "source_safran"
            print source_safran
            if source_safran != 's2m':
                self.sh.title('Toolbox output tb10')
                tb10 = toolbox.output(
                    local          = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'meteo',
                    geometry       = self.conf.geometry,
                    date           = self.conf.rundate,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                ),
                print(t.prompt, 'tb10 =', tb10)
                print()

            self.sh.title('Toolbox output tb11')
            tb11 = toolbox.output(
                local          = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'pro',
                geometry       = self.conf.geometry,
                date           = self.conf.rundate,
                datebegin      = datebegin if self.conf.previ else '[dateend]/-PT24H',
                dateend        = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                member         = members,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'production' if self.conf.previ else 'assimilation',
            ),
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox output tb12')
            tb12 = toolbox.output(
                local          = 'mb[member]/PREP_[datevalidity:ymdh].nc',
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                block          = 'prep',
                geometry       = self.conf.geometry,
                datevalidity   = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                date           = self.conf.rundate,
                member         = members,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'production' if self.conf.previ else 'assimilation',
            ),
            print(t.prompt, 'tb12 =', tb12)
            print()
