#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn

from bronx.stdtypes.date import Period


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

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            tbarp = list()
            tbpearp = list()
            rundate = self.conf.datebegin
            while rundate <= self.conf.dateend:

                # I- Guess ANALYSE ARPEGE
                # -----------------------

                # Recuperation de A6 du réseau H-6 pour H in [0, 6, 12, 18]
                self.sh.title('Toolbox input tb01')
                tbarp.extend(toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    cutoff         = 'assimilation',
                    # local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                    local          = '[date::ymdh]/mb035/ARPEGE[date::ymdh]_[term::hour]',
                    date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                ))
                print t.prompt, 'tb01_a =', tbarp
                print

                # II- Guess PEARP (membres 0 à 34)
                # --------------------------------

                # Récupération du réseau 6h (J-1) (utilisée seulement à partir du run de 6h) pour l'analyse (J-1) 6h -> J 6h
                self.sh.title('Toolbox input tb02')
                tbpearp.extend(toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    suite          = 'oper',
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = '[date::ymdh]/mb[member]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}'.format(rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                    fatal          = False,
                ))
                print t.prompt, 'tb02 =', tbpearp
                print

                rundate = rundate + Period(days=1)

                # II- Guess FORECAST ARPEGE
                # -------------------------

#                 # Récupération du réseau 0h
#                 # Récupération des échéances de 6h à 102h du réseau 0h d'ARPEGE
#                 self.sh.title('Toolbox input tb01')
#                 tbarp = toolbox.input(
#                     alternate      = 'Gridpoint',
#                     format         = 'grib',
#                     geometry       = self.conf.cpl_geometry,
#                     kind           = 'gridpoint',
#                     suite          = 'oper',
#                     cutoff         = 'production',
#                     local          = 'mb035/ARPEGE[date::addterm_ymdh]',
#                     date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
#                     term           = footprints.util.rangex(self.conf.prv_terms)[2:27],
#                     namespace      = 'oper.multi.fr',
#                     nativefmt      = '[format]',
#                     origin         = 'historic',
#                     model          = '[vapp]',
#                     vapp           = self.conf.source_app,
#                     vconf          = self.conf.deterministic_conf,
#                     fatal          = False,
#                 )
#                 print t.prompt, 'tb01 =', tbarp
#                 print

                # II- PEARP
                # Récupération des réseaus 6h
#                 self.sh.title('Toolbox input tb02')
#                 tbpearp = toolbox.input(
#                     role           = 'Gridpoint',
#                     block          = 'forecast',
#                     suite          = 'oper',
#                     cutoff         = 'production',
#                     format         = 'grib',
#                     geometry       = self.conf.cpl_geometry,
#                     kind           = 'gridpoint',
#                     local          = 'mb[member]/PEARP[date::addterm_ymdh]',
#                     # date           = [rundate.ymd6h, '{0:s}/+PT12H'.format(rundate.ymd6h)],
#                     date           = rundate.ymd6h, # Pour l'analyse
#                     term           = footprints.util.rangex(self.conf.prv_terms),
#                     member         = footprints.util.rangex(self.conf.pearp_members),
#                     namespace      = 'vortex.multi.fr',
#                     nativefmt      = '[format]',
#                     origin         = 'historic',
#                     model          = '[vapp]',
#                     vapp           = self.conf.source_app,
#                     vconf          = self.conf.eps_conf,
#                     fatal          = False,
#                 )
#                 print t.prompt, 'tb02 =', tbpearp
#                 print

            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                # rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
                rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
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
                #terms          = footprints.util.rangex(self.conf.prv_terms),
                #members        = footprints.util.rangex(1, 40, 1),
                interpreter    = script[0].resource.language,
                ntasks         = self.conf.ntasks,
                # members        = footprints.util.rangex(self.conf.members)
            )
            print t.prompt, 'tb04 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            rundate = self.conf.datebegin
            while rundate <= self.conf.dateend:

                self.sh.title('Toolbox output tb05')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    #local          = 'mb[date::day]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    local          = '[date::ymdh]/mb035/P[date::yymdh]_[cumul::hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::area]',
                    cutoff         = 'assimilation',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = ['{0:s}/-PT6H/+PT{1:s}H'.format(rundate.ymd6h, str(d)) for d in footprints.util.rangex(0, 24, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    #namespace      = 's2m.archive.fr',
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print t.prompt, 'tb05 =', tb05
                print

                self.sh.title('Toolbox output tb06')
                tb06 = toolbox.output(
                    role           = 'Ebauche',
                    #local          = 'mb[date::day]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    local          = '[date::ymdh]/mb0[date::dd]/P[date::yymdh]_[cumul::hour]_[vconf]_production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::area]',
                    cutoff         = 'production',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}'.format(rundate.ymd6h),
                    cumul          = footprints.util.rangex(footprints.util.rangex(self.conf.ana_terms)),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    #namespace      = 's2m.archive.fr',
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print t.prompt, 'tb06 =', tb06
                print

    #             self.sh.title('Toolbox output tb05b')
    #             tb05b = toolbox.output(
    #                 role           = 'Ebauche',
    #                 local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_[cutoff]',
    #                 experiment     = self.conf.xpid,
    #                 block          = self.conf.block,
    #                 cutoff         = 'production',
    #                 geometry       = self.conf.domains,
    #                 vconf          = '[geometry::area]',
    #                 date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
    #                 cumul          = footprints.util.rangex(self.conf.prv_terms)[2:27],
    #                 nativefmt      = 'ascii',
    #                 kind           = 'guess',
    #                 model          = 'safran',
    #                 source_app     = self.conf.source_app,
    #                 source_conf    = self.conf.deterministic_conf,
    #                 namespace      = self.conf.namespace,
    #                 fatal          = False,
    #             ),
    #             print t.prompt, 'tb05b =', tb05b
    #             print

    #             self.sh.title('Toolbox output tb06')
    #             tb06 = toolbox.output(
    #                 role           = 'Ebauche',
    #                 local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_[cutoff]',
    #                 experiment     = self.conf.xpid,
    #                 block          = self.conf.block,
    #                 geometry       = self.conf.domains,
    #                 cutoff         = 'production',
    #                 vconf          = '[geometry::area]',
    #                 #date           = [rundate.ymd6h, '{0:s}/+PT12H'.format(rundate.ymd6h)],
    #                 date           = rundate.ymd6h,
    #                 cumul          = footprints.util.rangex(self.conf.prv_terms),
    #                 nativefmt      = 'ascii',
    #                 kind           = 'guess',
    #                 model          = 'safran',
    #                 source_app     = self.conf.source_app,
    #                 source_conf    = self.conf.eps_conf,
    #                 namespace      = self.conf.namespace,
    #                 member         = footprints.util.rangex(self.conf.pearp_members),
    #                 fatal          = False,
    #             ),
    #             print t.prompt, 'tb06 =', tb06
    #             print

                rundate = rundate + Period(days=1)

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
            pass
