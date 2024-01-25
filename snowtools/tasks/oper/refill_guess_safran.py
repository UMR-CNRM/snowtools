# -*- coding:Utf-8 -*-


__all__ = []

from bronx.stdtypes.date import Date
from bronx.stdtypes.date import Period
from cen.layout.nodes import S2MTaskMixIn
import footprints
#import glob
#import os
import tarfile
from vortex import toolbox
from vortex.layout.nodes import Driver, Task

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):

    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            # Do not add delay_component_errors=True in the kw because
            # if one file hasn't been produced nothing must be saved
            # (otherwise it could lead to keep an incomplete tar).
            PrepSafran(tag='prepsaf', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error

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

                refill = False
                for dom in self.conf.domains:

                    # 1. Check if guess file already exists
                    self.sh.title(f'Toolbox input guess {rundate.ymdh} {dom}')
                    tb01 = toolbox.input(
                        role           = 'Ebauche',
                        local          = '[date::ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                        # geometry       = self.conf.domains,
                        geometry       = dom,
                        vapp           = 's2m',
                        vconf          = '[geometry:area]',
                        experiment     = self.conf.xpid_guess,
                        cutoff         = 'assimilation',
                        block          = 'guess',
                        date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.deterministic_conf,
                        namespace      = self.conf.namespace_in,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb01 =', tb01)
                    print()

                    if len(tb01[0]) < 5:
                        refill = True

                if refill:
                    # 2. Get ARPEGE file
                    # Recuperation de A6 du réseau H-6 pour H in [0, 6, 12, 18]
                    if rundate < Date(2019, 7, 1, 0):

                        self.sh.title('Toolbox input ARPEGE {0:s}'.format(rundate.ymdh))
                        tb02 = toolbox.input(
                            role           = 'Gridpoint',
                            format         = 'grib',
                            geometry       = self.conf.cpl_geometry,
                            kind           = 'gridpoint',
                            local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                            date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                            term           = self.conf.cumul,
                            nativefmt      = '[format]',
                            # remote         = '/home/mrns/vernaym/extraction_bdap/[vconf]/ \
                            # arpege_[date::ymdh]_[term::hour].grib',
                            # hostname       = 'guppy.meteo.fr',
                            # tube           = 'ftp',
                            # origin         = 'arpege',
                            # fatal          = False,
                            suite          = 'oper',  # Force oper file to avoid mixing experiments
                            cutoff         = 'assimilation',
                            # local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                            namespace      = self.conf.namespace_in,
                            origin         = 'historic',
                            model          = '[vapp]',
                            vapp           = self.conf.source_app,
                            vconf          = self.conf.deterministic_conf,
                            fatal          = False,
                        )
                        print(t.prompt, 'tb02 =', tb02)
                        print()

                    else:

                        self.sh.title('Toolbox input arpege {0:s}'.format(rundate.ymdh))
                        tb02 = toolbox.input(
                            role           = 'Gridpoint',
                            format         = 'grib',
                            geometry       = self.conf.cpl_geometry,
                            kind           = 'gridpoint',
                            # filtername     = 'concatenate',
                            suite          = 'oper',  # Force oper files to avoid mixing experiments
                            local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                            date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                            # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                            term           = self.conf.cumul,
                            namespace      = self.conf.namespace_in,
                            block          = 'forecast',
                            cutoff         = 'assimilation',
                            nativefmt      = '[format]',
                            origin         = 'historic',
                            model          = '[vapp]',
                            vapp           = self.conf.source_app,
                            vconf          = self.conf.deterministic_conf,
                            fatal          = False,
                        )
                        print(t.prompt, 'tb02 =', tb02)
                        print()

                        if len(tb02) == 5:
                            tbarp.extend(tb02)
                        else:
                            # En dernier recours on va chercher si une extraction BDAP a été faite
                            # pour les dates plus anciennes
                            self.sh.title('Toolbox input BDAP {0:s}'.format(rundate.ymdh))
                            tbarp.extend(toolbox.input(
                                role           = 'Gridpoint',
                                format         = 'grib',
                                geometry       = self.conf.cpl_geometry,
                                kind           = 'gridpoint',
                                local          = '[date::ymdh]/ARPEGE[date::ymdh]_[term::hour]',
                                date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d))
                                                  for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                                term           = self.conf.cumul,
                                nativefmt      = '[format]',
                                remote         = '/home/mrns/vernaym/extraction_bdap/[vconf]/ \
                                arpege_[date::ymdh]_[term::hour].grib',
                                hostname       = 'sotrtm35-sidev.meteo.fr',
                                tube           = 'ftp',
                                origin         = 'arpege',
                                fatal          = False,
                            ))
                            print(t.prompt, 'tbarp =', tbarp)
                            print()

                        missing_dates.append(rundate)

                rundate = rundate + Period(days=1)

            self.sh.title('Toolbox input metadata')
            tbmeta = toolbox.input(
                role           = 'Metadata',
                format         = 'grib',
                genv            = self.conf.cycle,
                geometry       = self.conf.arpege_geometry,  #EURAT01
                gdomain        = '[geometry:area]',
                kind           = 'relief',
                local          = 'METADATA.grib',
                fatal          = True,
            )
            print(t.prompt, 'tbmeta =', tbmeta)
            print()

            self.sh.title('Toolbox input shapefile')
            tbshp = toolbox.input(
                role            = 'Shapefile',
                genv            = self.conf.cycle,
                gdomain         = 'all_massifs',
                geometry        = '[gdomain]',
                kind            = 'shapefile',
                model           = self.conf.model,
                local           = 'massifs_safran.tar',
            )
            print(t.prompt, 'tbshp =', tbshp)
            print()

            self.sh.title('Toolbox input tb03 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp)])),
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                # Need to extend pythonpath to be independant of the user environment
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is
                # eventually launched in a 'user-defined' environment
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d)
                                  for d in ['vortex/src', 'vortex/site', 'epygram',
                                            'epygram/site', 'epygram/eccodes_python']],
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

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
            # thisdir = os.getcwd()

            for dom in self.conf.domains:
                tarname = f'p{season}_{dom}.tar'
                with tarfile.open(tarname, mode='w') as tarfic:

                    rundate = datebegin - Period(days=1) - Period(hours=6)
                    while rundate < dateend + Period(days=4):

                        # On archive d'abord le fichier guess
                        self.sh.title(f'Toolbox output tbguess {rundate.ymdh} {dom}')
                        tbguess = toolbox.output(
                            role           = 'Ebauche',
                            local          = '[date::ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                            #local          = '[date::ymdh]/P[date::addcumul_yymdh]',
                            geometry       = dom,
                            vapp           = 's2m',
                            vconf          = '[geometry:area]',
                            experiment     = self.conf.xpid_guess,
                            cutoff         = 'assimilation',
                            block          = 'guess',
                            date           = rundate.ymdh,
                            cumul          = self.conf.cumul,
                            nativefmt      = 'ascii',
                            kind           = 'guess',
                            model          = 'safran',
                            source_app     = self.conf.source_app,
                            source_conf    = self.conf.deterministic_conf,
                            namespace      = self.conf.namespace_out,
                        ),
                        print(t.prompt, 'tbguess =', tbguess)
                        print()

                        validitydate = rundate + Period(hours=6)

                        guessname = tbguess[0][0].container.localpath()
                        arcname   = f'P{validitydate.yymdh}'
                        # Puis on renome le fichier guess au format P????????
                        tarfic.add(guessname, arcname=arcname)

                        rundate = rundate + Period(hours=6)

                self.sh.title('Toolbox output tb05')
                tb05 = toolbox.output(
                    role           = 'Reanalyses',
                    kind           = 'packedguess',
                    local          = tarname,
                    experiment     = self.conf.xpid_guess,
                    block          = 'guess',
                    nativefmt      = 'tar',
                    namespace      = self.conf.namespace_out,
                    geometry       = dom,
                    datebegin      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                    dateend        = '{0:s}/+PT96H'.format(dateend.ymd6h),
                    model          = 'safran',
                    date           = self.conf.rundate.ymdh,
                    vapp           = self.conf.vapp,
                    vconf          = '[geometry:area]',
                    fatal          = True,
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

#            print('==================================================================================================')
#            print('==================================================================================================')
#            raise Exception('INFO :The execution went well, do not take into account the following error')
