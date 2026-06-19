# -*- coding:Utf-8 -*-
"""
SAFRAN input mixin
"""

import vortex


class SafranMixIn:

    def get_const_safran(self):

        t = vortex.ticket()

        self.sh.title('Toolbox input listem')
        tb03 = vortex.input(
            role            = 'ListeMassif',
            genv            = self.conf.cycle,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            kind            = 'listem',
            model           = 'safran',
            local           = 'listem',
        )
        print(t.prompt, 'tb03 =', tb03)
        print()

        self.sh.title('Toolbox input listeml')
        tb04 = vortex.input(
            role            = 'ListeLimitesMassif',
            genv            = self.conf.cycle,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            kind            = 'listeml',
            model           = 'safran',
            local           = 'listeml',
        )
        print(t.prompt, 'tb04 =', tb04)
        print()

        self.sh.title('Toolbox input listeo')
        tb05 = vortex.input(
            role            = 'ListePost',
            genv            = self.conf.cycle,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            kind            = 'listeo',
            model           = 'safran',
            # local           = 'listeo' if self.conf.vconf == 'alp' else 'lysteo',
            local           = 'listeo',
        )
        print(t.prompt, 'tb05 =', tb05)
        print()

        self.sh.title('Toolbox input carpost')
        tb06 = vortex.input(
            role            = 'carac_post',
            genv            = self.conf.cycle,
            gdomain         = '[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            kind            = 'carpost',
            model           = 'safran',
            local           = 'carpost.tar',
        )
        print(t.prompt, 'tb06 =', tb06)
        print()

        # WARNING : Le ressoucre rsclim  sert pas dans le cas nominal mais
        # constitue un mode secours pour SAFRAN si il rencontre un problème pour faire son guess
        # A partir des fichiers P
#                self.sh.title('Toolbox input rsclim')
#                tb09 = vortex.input(
#                    role            = 'Clim',
#                    genv            = self.conf.cycle,
#                    gvar            = '[kind]',
#                    geometry        = self.conf.geometry[self.conf.vconf],
#                    kind            = 'rsclim',
#                    model           = 'safran',
#                    local           = 'rsclim.don',
#                )
#                print(t.prompt, 'tb09 =', tb09)
#                print()

        self.sh.title('Toolbox input icrccm')
        tb10 = vortex.input(
            role            = 'Clim',
            genv            = self.conf.cycle,
            gvar            = '[kind]',
            geometry        = self.conf.geometry[self.conf.vconf],
            kind            = 'icrccm',
            model           = 'safran',
            local           = 'icrccm.don',
        )
        print(t.prompt, 'tb10 =', tb10)
        print()

        self.sh.title('Toolbox input namelist sorties')
        tb11 = vortex.input(
            role            = 'Nam_sorties',
            source          = 'namelist_sorties_[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            genv            = self.conf.cycle,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'SORTIES',
            fatal           = False,
        )
        print(t.prompt, 'tb11 =', tb11)
        print()

        self.sh.title('Toolbox input namelist analyse')
        tb12 = vortex.input(
            role            = 'Nam_analyse',
            source          = 'namelist_analyse_[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            genv            = self.conf.cycle,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'ANALYSE',
            fatal           = False,
        )
        print(t.prompt, 'tb12 =', tb12)
        print()

        self.sh.title('Toolbox input namelist melange')
        tb13 = vortex.input(
            role            = 'Nam_melange',
            source          = 'namelist_melange_[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            genv            = self.conf.cycle,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'MELANGE',
            fatal           = False,
        )
        print(t.prompt, 'tb13 =', tb13)
        print()

        self.sh.title('Toolbox input namelist adapt')
        tb14 = vortex.input(
            role            = 'Nam_adapt',
            source          = 'namelist_adapt',
            geometry        = self.conf.geometry[self.conf.vconf],
            genv            = self.conf.cycle,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'ADAPT',
            fatal          = False,
        )
        print(t.prompt, 'tb14 =', tb14)
        print()

        if self.conf.vconf == 'pyr':

            self.sh.title('Toolbox input namelist observr')
            tb15 = vortex.input(
                role            = 'Nam_observr',
                source          = 'namelist_observr_[geometry:domain]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = 'safran',
                local           = 'OBSERVR',
                fatal           = False,
            )
            print(t.prompt, 'tb15 =', tb15)
            print()

        self.sh.title('Toolbox input namelist ebauche')
        tb16 = vortex.input(
            role            = 'Nam_ebauche',
            source          = 'namelist_ebauche_[geometry:domain]',
            geometry        = self.conf.geometry[self.conf.vconf],
            genv            = self.conf.cycle,
            kind            = 'namelist',
            model           = 'safran',
            local           = 'EBAUCHE',
            fatal           = False,
        )
        print(t.prompt, 'tb16 =', tb16)
        print()
