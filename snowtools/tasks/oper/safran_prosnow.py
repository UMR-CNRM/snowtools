# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn

import ftplib
from ftplib import FTP,all_errors
from netrc import netrc


def ftpconnect(machine):
    username, account, password = netrc().authenticators(machine)
    ftp = FTP(machine)
    ftp.set_debuglevel(1)
    ftp.login(username, password)
    return ftp


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            Safran(tag='prvsaf', ticket=t, **kw, delay_component_errors=True),
        ],
        options=kw,
    )


class Safran(Task, S2MTaskMixIn):

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error

    def process(self):
        """Safran"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # P12 à P108 du réseau 18h (J-1)
            self.sh.title('Toolbox intput tb01')
            tb01 = toolbox.input(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
                experiment     = self.conf.xpid,
                block          = self.conf.guess_block,
                geometry       = self.conf.vconf,
                date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms),
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace_in,
                member         = footprints.util.rangex(self.conf.pearp_members),
                fatal          = False,
            ),
            print(t.prompt, 'tb01 =', tb01)
            print()

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role            = 'ListeMassif',
                genv            = self.conf.cycle,
                kind            = 'listem',
                model           = self.conf.model,
                local           = 'listem',
                geometry        = self.conf.vconf,
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role            = 'ListeLimitesMassif',
                genv            = self.conf.cycle,
                kind            = 'listeml',
                model           = self.conf.model,
                local           = 'listeml',
                geometry        = self.conf.vconf,
            )
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role            = 'ListePost',
                genv            = self.conf.cycle,
                kind            = 'listeo',
                model           = self.conf.model,
                local           = 'listeo',
                geometry        = self.conf.vconf,
            )
            print(t.prompt, 'tb05 =', tb05)
            print()

            self.sh.title('Toolbox input tb07')
            tb07 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                kind            = 'rsclim',
                model           = self.conf.model,
                local           = 'rsclim.don',
                geometry        = self.conf.vconf,
                fatal           = False,
            )
            print(t.prompt, 'tb07 =', tb07)
            print()

            self.sh.title('Toolbox input tb08')
            tb08 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                kind            = 'icrccm',
                model           = self.conf.model,
                local           = 'icrccm.don',
                geometry        = self.conf.vconf,
                fatal           = False,
            )
            print(t.prompt, 'tb08 =', tb08)
            print()

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'Nam_sorties',
                source          = 'namelist_sorties_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'SORTIES',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_adapt',
                source          = 'namelist_adapt',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ADAPT',
                fatal           = False,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb10')
            tb10 = toolbox.input(
                role            = 'Nam_melange',
                source          = 'namelist_melange_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'MELANGE',
                fatal           = False,
            )
            print(t.prompt, 'tb10 =', tb10)
            print()

            self.sh.title('Toolbox input tb11')
            tb11 = toolbox.input(
                role            = 'carac_post',
                genv            = self.conf.cycle,
                geometry        = self.conf.vconf,
                kind            = 'carpost',
                model           = self.conf.model,
                local           = 'carpost.tar',
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_observr',
                source          = 'namelist_observr_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'OBSERVR',
                fatal           = False,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_analyse',
                source          = 'namelist_analyse_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ANALYSE',
                fatal           = False,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_ebauche',
                source          = 'namelist_ebauche_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'EBAUCHE',
                fatal           = False,
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.sh.title('Toolbox executable tb11 = tbx1')
            tb11 = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'safrane',
                local          = 'safrane',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox executable tb12 = tbx2')
            tb12 = tbx2 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrpluie',
                local          = 'syrpluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb12 =', tb12)
            print()

            self.sh.title('Toolbox executable tb13 = tbx3')
            tb13 = tbx3 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrmrr',
                local          = 'syrmRR',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox executable tb14 = tbx4')
            tb14 = tbx4 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sytist',
                local          = 'sytist',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb15 = SAFRANE')
            tb15 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = 'safrane',
                datebegin      = self.conf.rundate.ymd6h,
                dateend        = '{0:s}/+PT96H'.format(self.conf.rundate.ymd6h),
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb15 =', tb15)
            print()

            self.component_runner(tbalgo1, tbx1)

            self.sh.title('Toolbox algo tb16 = SYRPLUIE')
            tb16 = tbalgo2 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syrpluie',
                datebegin      = self.conf.rundate.ymd6h,
                dateend        = '{0:s}/+PT96H'.format(self.conf.rundate.ymd6h),
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb17 = SYRMRR')
            tb17 = tbalgo3 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syrmrr',
                datebegin      = self.conf.rundate.ymd6h,
                dateend        = '{0:s}/+PT96H'.format(self.conf.rundate.ymd6h),
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb17 =', tb17)
            print()

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb18 = SYTIST')
            tb18 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sytist',
                datebegin      = self.conf.rundate.ymd6h,
                dateend        = '{0:s}/+PT96H'.format(self.conf.rundate.ymd6h),
                ntasks         = self.conf.ntasks,
                execution      = 'forecast',
            )
            print(t.prompt, 'tb18 =', tb18)
            print()

            self.component_runner(tbalgo4, tbx4)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb27')
            tb27 = toolbox.output(
                role           = 'Prv_massifs',
                kind           = 'MeteorologicalForcing',
                source_app     = 'arpege',
                source_conf    = 'pearp',
                local          = 'mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'massifs',
                geometry       = self.conf.vconf,
                nativefmt      = 'netcdf',
                model          = self.conf.model,
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                namespace      = self.conf.namespace_out,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print(t.prompt, 'tb27 =', tb27)
            print()

            self.sh.title('Toolbox output tb28')
            tb27 = toolbox.output(
                role           = 'Prv_postes',
                kind           = 'MeteorologicalForcing',
                source_app     = 'arpege',
                source_conf    = 'pearp',
                local          = 'mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                experiment     = self.conf.xpid,
                block          = 'postes',
                geometry       = self.conf.vconf,
                nativefmt      = 'netcdf',
                model          = self.conf.model,
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                namespace      = self.conf.namespace_out,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print(t.prompt, 'tb28 =', tb27)
            print()

#            print('==================================================================================================')
#            print('INFO :The execution went well, do not take into account the following error')
#            print('==================================================================================================')
#            from vortex.tools.systems import ExecutionError
#            raise ExecutionError('')
