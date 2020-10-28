#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from bronx.stdtypes.date import Date
from bronx.stdtypes.date import Period
from cen.layout.nodes import S2MTaskMixIn
import footprints
import glob
import os
import tarfile
from vortex import toolbox
from vortex.layout.nodes import Driver, Task

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):

    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            PrepSafran(tag='prepsaf', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        # En laçant cette tâche au réseau de 12h on obtient datebegin=1er aout precedent et dateend=J-4
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps:

            tbarp   = list()
            missing_dates = list()
            # On veut commencer les guess au 31/07 6h
            rundate = datebegin - Period(days=1)
            # On s'arrête à J-4 pour produire le même fichier que le 'refill' journalier
            # TODO --> il serait plus logique d'aller jusqu'à 6H (J) pour avoir un mode secours
            # ==> Modification simultanéee de prepsaf_reana ET safran_ana (12H) 
            while rundate < dateend + Period(days=4):

                # 1. Check if guess file already exists
                self.sh.title('Toolbox input guess {0:s}'.format(rundate.ymdh))
                tb01 = toolbox.input(
                    role           = 'Ebauche',
                    local          = '[date::ymdh]/P[date::addcumul_yymdh]',
                    # geometry       = self.conf.domains,
                    geometry       = self.conf.vconf,
                    vapp           = 's2m',
                    vconf          = '[geometry:area]',
                    experiment     = 'OPER@vernaym',
                    cutoff         = 'assimilation',
                    block          = 'guess',
                    date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print t.prompt, 'tb01 =', tb01
                print

                if len(tb01[0]) < 5:

                    # 2. Get ARPEGE file
                    # Recuperation de A6 du réseau H-6 pour H in [0, 6, 12, 18]
                    if rundate < Date(2019, 7, 1, 0):

                        self.sh.title('Toolbox input ARPEGE {0:s}'.format(rundate.ymdh))
                        tb02 = toolbox.input(
                            role           = 'Gridpoint',
                            format         = 'grib',
                            geometry       = 'euroc25',
                            kind           = 'gridpoint',
                            local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                            date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                            term           = self.conf.cumul,
                            nativefmt      = '[format]',
                            # remote         = '/home/mrns/vernaym/extraction_bdap/[vconf]/arpege_[date::ymdh]_[term::hour].grib',
                            # hostname       = 'guppy.meteo.fr',
                            # tube           = 'ftp',
                            # origin         = 'arpege',
                            # fatal          = False,
                            suite          = 'oper',
                            cutoff         = 'assimilation',
                            # local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                            namespace      = 'oper.multi.fr',
                            origin         = 'historic',
                            model          = '[vapp]',
                            vapp           = self.conf.source_app,
                            vconf          = self.conf.deterministic_conf,
                            fatal          = False,
                        )
                        print t.prompt, 'tb02 =', tb02
                        print

                    else:

                        self.sh.title('Toolbox input arpege {0:s}'.format(rundate.ymdh))
                        tb02 = toolbox.input(
                            role           = 'Gridpoint',
                            format         = 'grib',
                            geometry       = 'glob025',
                            kind           = 'gridpoint',
                            # filtername     = 'concatenate',
                            suite          = 'oper',
                            local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                            date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                            # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                            term           = self.conf.cumul,
                            namespace      = 'vortex.multi.fr',
                            block          = 'forecast',
                            cutoff         = 'assimilation',
                            nativefmt      = '[format]',
                            origin         = 'historic',
                            model          = '[vapp]',
                            vapp           = self.conf.source_app,
                            vconf          = self.conf.deterministic_conf,

                            fatal          = False,
                        )
                        print t.prompt, 'tb02 =', tb02
                        print

                        if len(tb02) == 5:
                            tbarp.extend(tb02)
                        else:
                            # En dernier recours on va chercher si une extraction BDAP a été faite pour les dates plus anciennes
                            self.sh.title('Toolbox input BDAP {0:s}'.format(rundate.ymdh))
                            tbarp.extend(toolbox.input(
                                role           = 'Gridpoint',
                                format         = 'grib',
                                geometry       = 'euroc25',
                                kind           = 'gridpoint',
                                local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                                date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                                term           = self.conf.cumul,
                                nativefmt      = '[format]',
                                remote         = '/home/mrns/vernaym/extraction_bdap/[vconf]/arpege_[date::ymdh]_[term::hour].grib',
                                hostname       = 'guppy.meteo.fr',
                                tube           = 'ftp',
                                origin         = 'arpege',
                                fatal          = False,
                            ))
                            print t.prompt, 'tbarp =', tbarp
                            print

                        missing_dates.append(rundate)

                rundate = rundate + Period(days=1)

            self.sh.title('Toolbox input tb03 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -a -o -i IDW -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp)]))),
            )
            print t.prompt, 'tb03 =', tb03
            print

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print t.prompt, 'tb04 =', expresso
            print

#             self.sh.title('Toolbox algo tb04')
#             expresso = toolbox.algo(
#                 vconf          = self.conf.vconf,
#                 engine         = 'exec',
#                 kind           = 'guess',
#                 #terms          = footprints.util.rangex(self.conf.prv_terms),
#                 #members        = footprints.util.rangex(1, 40, 1),
#                 interpreter    = script[0].resource.language,
#                 ntasks         = self.conf.ntasks,
#                 # members        = footprints.util.rangex(self.conf.members)
#             )
#             print t.prompt, 'tb04 =', expresso
#             print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            # WARNING : The following only works for a 1-year execution and one domain
            season = Date.nivologyseason.fget(self.conf.rundate)
            # for dom in self.conf.domains:
            # tarname = 'p{0:s}_{1:s}.tar'.format(season, self.conf.domains)
            tarname = 'p{0:s}_{1:s}.tar'.format(season, self.conf.vconf)
            # thisdir = os.getcwd()
            with tarfile.open(tarname, mode='w') as tarfic:
                for f in glob.glob('*/P????????'):
                    # oldname = os.path.basename(f).split('_')[0]
                    # date = Date.strptime(oldname[1:], '%y%m%d%H') + Period(hours=6)
                    # arcname = 'P{0:s}'.format(date.yymdh)
                    arcname = os.path.basename(f)
                    tarfic.add(f, arcname=arcname)

            self.sh.title('Toolbox output tb05')
            tb05 = toolbox.output(
                role           = 'Reanalyses',
                kind           = 'packedguess',
                local          = tarname,
                experiment     = self.conf.xpid,
                block          = 'guess',
                nativefmt      = 'tar',
                namespace      = 'vortex.multi.fr',
                geometry       = self.conf.vconf,
                begindate      = datebegin.ymd6h,
                enddate        = dateend.ymd6h,
                model          = 'safran',
                date           = self.conf.rundate.ymdh,
                vapp           = self.conf.vapp,
                vconf          = self.conf.vconf,
                fatal          = True,
            ),
            print t.prompt, 'tb05 =', tb05
            print

            for f in glob.glob('*/ARPEGE*'):

                rundate = Date(f.split('/')[0])

                self.sh.title('Toolbox output tb06')
                tb06 = toolbox.output(
                    role           = 'Ebauche',
                    local          = '[date::ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.vconf,
                    vapp           = 's2m',
                    vconf          = '[geometry:area]',
                    experiment     = 'OPER@vernaym',
                    cutoff         = 'assimilation',
                    block          = 'guess',
                    date           = rundate.ymdh,
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb06 =', tb06
                print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
            pass
