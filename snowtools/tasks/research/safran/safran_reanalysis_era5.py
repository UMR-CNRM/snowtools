# -*- coding:Utf-8 -*-

__all__ = []

from bronx.stdtypes.date import Date
from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from vortex.tools.systems import ExecutionError


logger = footprints.loggers.getLogger(__name__)





def setup(t, **kw):
    return Driver(
        tag='safran_reana',
        ticket=t,
        nodes=[
            Safran(tag='safran_reana', ticket=t, **kw),
        ],
        options=kw,
    )


class Safran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Safran analysis"""

        t = self.ticket

        rundate = self.conf.datebegin
        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
        if len(list_dates) > 120:
            raise ExecutionError("Too many years")

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            for rundate in list_dates:
                datebegin = rundate
                dateend = rundate.replace(year = rundate.year + 1)
#                dateend = rundate + Period(years=1)
                season = datebegin.nivologyseason
                next_season = dateend.nivologyseason
                y1 = datebegin.year
                y2 = dateend.year

                self.sh.title(f'Toolbox input tb01 - {y1:d}/{y2:d}')
                tb01 = toolbox.input(
                    role           = 'Ebauche',
                    kind           = 'packedguess',
                    local          = '{0:s}_{1:s}/guess{2:s}.tar'.format(datebegin.ymd6h, dateend.ymd6h, season),
                    namespace      = 's2m.archive.fr',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'tar',
                    model          = 'safran',
                    source         = 'arpege' if y1 > 2020 else 'era5',
                    now            = True,
                    fatal          = False,
                    date           = dateend.ymdh,
                    datebegin      = datebegin.ymdh,
                    dateend        = dateend.ymdh,
                ),
                print(t.prompt, 'tb01 =', tb01)
                print()

                # TODO : Gérer le fait de ne pas prendre le fichier 2 de 6h le 01/08
                self.sh.title(f'Toolbox input tb02 - {y1:d}/{y2:d}')
                tb02 = toolbox.input(
                    role           = 'Observations',
                    part           = 'all',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    kind           = 'packedobs',
                    #nativefmt      = 'tar',
                    #unknownflow    = True,
                    local          = '{0:s}_{1:s}/rs{2:s}.tar'.format(datebegin.ymd6h, dateend.ymd6h, season),
                    namespace      = 's2m.archive.fr',
                    #remote         = '/home/vernaym/s2m/[geometry]/obs/rs{0:s}.tar'.format(season),
                    #hostname       = 'hendrix.meteo.fr',
                    date           = dateend.ymdh,
                    datebegin      = datebegin.ymdh,
                    dateend        = dateend.ymdh,
                    #tube           = 'ftp',
                    model          = self.conf.model,
                    source         = 'surfaceobs',
                    now            = True,
                )
                print(t.prompt, 'tb02 =', tb02)
                print()

                if datebegin >= Date(1991, 8, 1, 6) and dateend <= Date(2018, 8, 1, 6) and self.conf.vconf in ['alp', 'pyr', 'cor']:

                    self.sh.title('Toolbox input tb03 - ' + datebegin.ymdh)
                    tb03 = toolbox.input(
                        role           = 'Nebul',
                        part           = 'all',
                        geometry       = self.conf.geometry[self.conf.vconf],
                        kind           = 'packedobs',
                        #nativefmt      = 'tar',
                        #unknownflow    = True,
                        local          = '{0:s}_{1:s}/n{2:s}.tar'.format(datebegin.ymd6h, dateend.ymd6h, season),
                        namespace      = 's2m.archive.fr',
                        #remote         = '/home/vernaym/s2m/[geometry]/obs/n{0:s}.tar'.format(season),
                        #hostname       = 'hendrix.meteo.fr',
                        date           = dateend.ymdh,
                        datebegin      = datebegin.ymdh,
                        dateend        = dateend.ymdh,
                        source         = 'neb',
                        #tube           = 'ftp',
                        model          = self.conf.model,
                        now            = True,
                        fatal          = False,
                    )
                    print(t.prompt, 'tb03 =', tb03)
                    print()


            self.sh.title('Toolbox input tb07')
            tb07 = toolbox.input(
                role            = 'ListeMassif',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'listem',
                model           = self.conf.model,
                local           = 'listem',
            )
            print(t.prompt, 'tb07 =', tb07)
            print()

            self.sh.title('Toolbox input tb08')
            tb08 = toolbox.input(
                role            = 'ListeLimitesMassif',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'listeml',
                model           = self.conf.model,
                local           = 'listeml',
            )
            print(t.prompt, 'tb08 =', tb08)
            print()

#             self.sh.title('Toolbox input tb08')
#             tb08 = toolbox.input(
#                 role            = 'NormalesClim',
#                 genv            = self.conf.cycle,
#                 gdomain         = self.conf.vconf,
#                 geometry        = '[gdomain]',
#                 kind            = 'NORELm',
#                 model           = self.conf.model,
#                 local           = 'NORELm',
#             )
#             print t.prompt, 'tb08 =', tb08
#             print

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'ListePost',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'listeo',
                model           = self.conf.model,
                local           = 'listeo',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            if self.conf.vconf in ['alp', 'pyr']:

                self.sh.title('Toolbox input tb09')
                tb09 = toolbox.input(
                    role            = 'MoyennesMensuellesRR',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'NORELot',
                    model           = self.conf.model,
                    local           = 'NORELot',
                )
                print(t.prompt, 'tb09 =', tb09)
                print()

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'SurfZ',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'surfz',
                model           = self.conf.model,
                local           = 'surfz',
                nativefmt       = 'ascii',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

#             self.sh.title('Toolbox input tb09')
#             tb09 = toolbox.input(
#                 role            = 'MoyennesMensuellesRR_TT',
#                 genv            = self.conf.cycle,
#                 gdomain         = self.conf.vconf,
#                 geometry        = '[gdomain]',
#                 kind            = 'NORELo',
#                 model           = self.conf.model,
#                 local           = 'NORELo',
#             )
#             print t.prompt, 'tb09 =', tb09
#             print

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'carac_post',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'carpost',
                model           = self.conf.model,
                local           = 'carpost.tar',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            if self.conf.vconf in ['alp', 'pyr']:

                self.sh.title('Toolbox input tb12')
                tb12 = toolbox.input(
                    role            = 'BlackList',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'blacklist',
                    model           = self.conf.model,
                    local           = 'BLACK',
                    fatal           = False,
                )
                print(t.prompt, 'tb12 =', tb12)
                print()

                self.sh.title('Toolbox input tb08')
                tb08 = toolbox.input(
                    role            = 'NormalesClimTT',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = self.conf.geometry[self.conf.vconf],
                    kind            = 'NORELmt',
                    model           = self.conf.model,
                    local           = 'NORELmt',
                )
                print(t.prompt, 'tb08 =', tb08)
                print()

            self.sh.title('Toolbox input tb11')
            tb11 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'rsclim',
                model           = self.conf.model,
                local           = 'rsclim.don',
                fatal           = False,
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox input tb12')
            tb12 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'icrccm',
                model           = self.conf.model,
                local           = 'icrccm.don',
                fatal           = False,
            )
            print(t.prompt, 'tb12 =', tb12)
            print()

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_sorties',
                source          = 'namelist_sorties_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'SORTIES',
                fatal           = False,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_analyse',
                source          = 'namelist_analyse_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ANALYSE',
                fatal           = False,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_adapt',
                source          = 'namelist_adapt',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ADAPT',
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_melange',
                source          = 'namelist_melange_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'MELANGE',
                fatal           = False,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_observr',
                source          = 'namelist_observr_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'OBSERVR',
                fatal           = False,
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_ebauche',
                source          = 'namelist_ebauche_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'EBAUCHE',
                fatal           = True,
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

#            self.sh.title('Toolbox executable tb17 = tbx0')
#            tb17 = tbx0 = toolbox.executable(
#                role           = 'Binary',
#                genv           = self.conf.cycle,
#                kind           = 'intercep',
#                #local          = 'intercep_era40',
#                local          = 'intercep_era5',
#                model          = self.conf.model,
#            )
#            print(t.prompt, 'tb17 =', tb17)
#            print()

            self.sh.title('Toolbox executable tb17 = tbx1')
            tb17 = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'safrane',
                local          = 'safrane',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb17 =', tb17)
            print()

            self.sh.title('Toolbox executable tb18 = tbx2')
            tbx2 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrpluie',
                local          = 'syrpluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb18 =', tbx2)
            print()

            self.sh.title('Toolbox executable tb18_b = tbx3')
            tb18_b = tbx3 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sypluie',
                local          = 'sypluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb18_b =', tb18_b)
            print()

            self.sh.title('Toolbox executable tb19 = tbx4')
            tb19 = tbx4 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvapr',
                local          = 'syvapr',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb19 =', tb19)
            print()

            self.sh.title('Toolbox executable tb20 = tbx5')
            tb20 = tbx5 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvafi',
                local          = 'syvafi',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb20 =', tb20)
            print()

            self.sh.title('Toolbox executable tb21 = tbx6')
            tb21 = tbx6 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sytist',
                local          = 'sytist',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb21 =', tb21)
            print()

        if 'compute' in self.steps:

#            if self.conf.datebegin < Date(2002, 8, 1):
#
#                self.sh.title('Toolbox algo tb22 = INTERCEP')
#                tb22 = tbalgo0 = toolbox.algo(
#                    engine         = 's2m',
#                    kind           = 'intercep',
#                    datebegin      = self.conf.datebegin.ymd6h,
#                    dateend        = self.conf.dateend.ymd6h,
#                    ntasks         = self.conf.ntasks,
#                    execution      = self.conf.execution,
#                )
#                print(t.prompt, 'tb22 =', tb22)
#                print()
#
#                self.component_runner(tbalgo0, tbx0)

            # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
            self.sh.title('Toolbox algo tb22 = SAFRANE')
            tb22 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = 'safrane',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb22 =', tb22)
            print()

            self.component_runner(tbalgo1, tbx1)

            if self.conf.execution == 'analysis':

                # Cas d'une execution où l'on veut utiliser les rr ARPEGE comme guess
                # A lancer avec un job_name=with_rr_arpege (cf fichier de conf)
                # WARNING : si execution="analysis" l'algo component définit les
                # observations comme des fichiers "communs", ce qui n'est pas le
                # cas.
                # Les obs sont néanmoins bien assimilées car si les fichiers d'obs ne sont pas 
                # au chemin indiqué dans les fichiers OP* (ce qui dépend de la variable "execution"),
                # SAFRAN les cherche dans le répertoire courrant.
                self.sh.title('Toolbox algo tb23 = SYRPLUIE')
                tb23 = tbalgo2 = toolbox.algo(
                    engine         = 's2m',
                    kind           = 'syrpluie',
                    datebegin      = self.conf.datebegin.ymd6h,
                    dateend        = self.conf.dateend.ymd6h,
                    # members        = footprints.util.rangex(self.conf.members),
                    ntasks         = self.conf.ntasks,
                    execution      = self.conf.execution,
                )
                print(t.prompt, 'tb23 =', tb23)
                print()

                self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb23_b = SYPLUIE')
            tb23 = tbalgo3 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sypluie',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb23 =', tb23)
            print()

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb24 = SYVAPR')
            tb24 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvapr',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb24 =', tb24)
            print()

            self.component_runner(tbalgo4, tbx4)

            self.sh.title('Toolbox algo tb25 = SYVAFI')
            tb25 = tbalgo5 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvafi',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
            )
            print(t.prompt, 'tb25 =', tb25)
            print()

            self.component_runner(tbalgo5, tbx5)

            self.sh.title('Toolbox algo tb26 = SYTIST')
            tb26 = tbalgo6 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sytist',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = self.conf.execution,
                metadata       = 'StandardSAFRAN',
            )
            print(t.prompt, 'tb26 =', tb26)
            print()

            self.component_runner(tbalgo6, tbx6)

        if 'late-backup' in self.steps:

            for rundate in list_dates:
                datebegin = rundate
                dateend = min(datebegin.replace(year=datebegin.year + 1), self.conf.dateend)
                season = datebegin.nivologyseason
                next_season = dateend.nivologyseason

                if rundate >= Date(2021, 8, 1):
                    source_app = 'arpege'
                    source_conf = '4dvarfr'
                else:
                    source_app = 'ifs'
                    source_conf = 'era5'

                y1 = datebegin.year
                y2 = y1 + 1

                self.sh.title('Toolbox output tb27')
                tb27 = toolbox.output(
                    role           = 'Ana_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = source_app,
                    source_conf    = source_conf,
                    cutoff         = 'assimilation',
                    # local          = 'mb035/FORCING_massif.nc',
                    local          = '{0:s}_{1:s}/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc'.format(datebegin.ymdh, dateend.ymd6h),
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    date           = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                    namebuild      = 'flat@cen',
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

                self.sh.title('Toolbox output tb28')
                tb27 = toolbox.output(
                    role           = 'Ana_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = source_app,
                    source_conf    = source_conf,
                    cutoff         = 'assimilation',
                    # local          = 'mb035/FORCING_postes.nc',
                    local          = '{0:s}_{1:s}/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc'.format(datebegin.ymd6h, dateend.ymd6h),
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry       = self.conf.geometry[self.conf.vconf],
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    date           = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                    namebuild      = 'flat@cen',
                ),
                print(t.prompt, 'tb28 =', tb27)
                print()

                self.sh.title('Toolbox output tb32')
                tb29 = toolbox.output(
                    role           = 'Liste_obs',
                    block          = 'liste_obs',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry[self.conf.vconf],
                    cutoff         = 'assimilation',
                    nativefmt      = 'tar',
                    model          = self.conf.model,
                    kind           = 'listobs',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    date           = dateend.ymd6h,
                    local          = '{0:s}_{1:s}/liste_obs_[datebegin::ymd6h]_[dateend::ymd6h].tar.gz'.format(datebegin.ymd6h, dateend.ymd6h),
                    namespace      = self.conf.namespace,
                    namebuild      = 'flat@cen',
                )
                print(t.prompt, 'tb32 =', tb29)
                print()

            raise ExecutionError('')
