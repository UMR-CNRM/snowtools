# -*- coding:Utf-8 -*-


__all__ = []

import os
import tarfile
import glob

import footprints

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from bronx.stdtypes.date import Period


class PrepSafran(_CenResearchTask):

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket

        if 'early-fetch' in self.steps:

            ###########################
            #  I) FICHIER de METADONNES
            ###########################

            # On commence par récupérer un fichier à échéance 0h qui sert à lire le métédonnées
            # (infos sur la grille en particulier).
            # Ce fichier supplémentaire est indispensable pour toujours travailler avec la bonne grille du modèle,
            # même en cas d'évolution de la géométrie ARPEGE.
            self.sh.title('Input metadata')
            tbmeta = vortex.input(
                role           = 'Metadata',
                format         = 'grib',
                genv            = self.conf.uenv,
                geometry       = self.conf.arpege_geometry,  # EURAT01
                gdomain        = '[geometry:area]',
                kind           = 'relief',
                local          = 'METADATA.grib',
                fatal          = True,
            )
            print(t.prompt, 'tbmeta =', tbmeta)
            print()

            tbarp = list()
            tbpearp = list()
            rundate = self.conf.datebegin
            while rundate <= self.conf.dateend:

                # Récupération du réseau ARPEGE de 0:00 (J) pour couvrir J 6h -> (J+4) 6h
                self.sh.title('Input arpege 0h')
                tbarp.extend(vortex.input(
                    role           = 'Gridpoint',
                    kind           = 'gridpoint',
                    cutoff         = 'production',
                    format         = 'grib',
                    nativefmt      = '[format]',
                    experiment     = self.conf.guess_xpid,
                    block          = 'forecast',
                    namespace      = 'vortex.multi.fr',  # permet d'utiliser le cache inline pour les relances
                    geometry       = self.conf.arpege_geometry,
                    local          = '[date::ymdh]/mb035/[term:fmthour]/ARPEGE.grib',
                    origin         = 'historic',
                    date           = rundate,
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    model          = '[vapp]',
                    vapp           = 'arpege',
                    vconf          = '4dvarfr',
                ))

                # Récupération du réseau PEARP de 0:00 (J) pour couvrir J 6h -> (J+4) 6h
                self.sh.title('Input pearp 0h')
                tbpearp.extend(vortex.input(
                    role           = 'Gridpoint',
                    kind           = 'gridpoint',
                    cutoff         = 'production',
                    format         = 'grib',
                    nativefmt      = '[format]',
                    experiment     = self.conf.guess_xpid,
                    block          = 'forecast',
                    namespace      = 'vortex.multi.fr',  # permet d'utiliser le cache inline pour les relances
                    geometry       = self.conf.pearp_geometry,
                    local          = '[date::ymdh]/mb[member%03]/[term:fmthour]/PEARP.grib',
                    origin         = 'historic',
                    date           = rundate,
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    member         = footprints.util.rangex(self.conf.members),
                    model          = '[vapp]',
                    vapp           = 'arpege',
                    vconf          = 'pearp',
                ))

                rundate = rundate + Period(days=1)

            ###########################
            #        SHAPEFILE
            ###########################
            # Dans tous les cas de figure on aura besoin du shapefile des massifs SAFRAN
            self.sh.title('Input shapefile')
            shp = vortex.input(
                role            = 'Shapefile',
                genv            = self.conf.uenv,
                gdomain         = 'all_massifs',
                geometry        = '[gdomain]',
                kind            = 'shapefile',
                model           = 'safran',
                local           = 'massifs_safran.tar',
            )
            print(t.prompt, 'Shapefile =', shp)
            print()

            self.sh.title('Input PRE-TRAITEMENT FORCAGE script')
            script = vortex.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.uenv,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ARPEGE.grib PEARP.grib',
            )
            print(t.prompt, 'script =', script)
            print()

        if 'compute' in self.steps:

            # Tar guess files in parallel over the different rundates
            print('DBUG ntasks=', type(self.conf.ntasks))
            print('DBUG nnodes=', type(self.conf.nnodes))

            self.sh.title('Algo Guess')
            expresso = vortex.task(
                engine         = 'exec',
                kind           = 'guess',
                terms          = footprints.util.rangex(self.conf.prv_terms),
                interpreter    = 'current',
                ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
                reforecast     = True,
            )
            print(t.prompt, 'algo =', expresso)
            print()

            self.component_runner(expresso, script, fortran=False)

            self.sh.title('Algo Tar')
            tar = vortex.task(
                engine         = 'algo',
                kind           = 'TarSafranGuess',
                domains        = [geometry.domain for geometry in self.conf.geometries],
                ntasks         = int(self.conf.ntasks) * int(self.conf.nnodes),
                role_members   = 'Gridpoint',
            )
            print(t.prompt, 'tar =', tar)
            print()

            tar.run()

        if 'backup' in self.steps or 'late-backup' in self.steps:

            rundate = self.conf.datebegin
            while rundate <= self.conf.dateend:

                for geometry in self.conf.geometries:

                    # self.tar_date(rundate, geometry)

                    self.sh.title(f'Output guess {rundate} {geometry.domain}')
                    vortex.output(
                        role           = 'Ebauche',
                        local          = f'ebauches_[geometry:domain]_{rundate.ymdh}.tar',
                        kind           = 'packedguess',
                        experiment     = self.conf.xpid,
                        block          = 'guess',
                        geometry       = geometry,
                        nativefmt      = 'tar',
                        namespace      = 'vortex.multi.fr',
                        namebuild      = 'flat@cen',
                        datebegin      = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[0]),
                        dateend        = rundate + Period(hours=footprints.util.rangex(self.conf.prv_terms)[-1]),
                        model          = 'safran',
                    ),

                rundate = rundate + Period(days=1)

    def tar_date(self, datepivot, geometry):

        tarname = f'ebauches_{geometry.domain}_{datepivot.ymdh}.tar'
        with tarfile.open(tarname, mode='w') as tarfic:
            for f in glob.glob(f'{datepivot.ymdh}/*/*/P????????*{geometry.domain}*'):
                # f = 'YYYYMMDD00/mbXXX/ECH/PYYMMDDHH_E_dom_production'
                ech = int(f.split('/')[2])  # ECH
                # On veut organiser le tar pour qu'il soit directement exploitable par
                # l'algo SAFRAN arpès détarrage : toutes les échéances issues d'un même
                # réseau doivent être regroupées dans le même répertoire et le nom
                # du fichier guess de la forme PYYMMDDHH correspondant à la date
                # de validité du guess
                validity = datepivot + Period(hours=ech)
                arcname = os.path.join(f.split('/')[0], f.split('/')[1], f'P{validity.yymdh}')
                tarfic.add(f, arcname=arcname)
