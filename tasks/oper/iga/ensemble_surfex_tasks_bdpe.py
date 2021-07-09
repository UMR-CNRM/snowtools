'''
Created on 7 nov. 2017

@author: lafaysse
'''


import iga.tools.op as op
from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad

from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from snowtools.bronx.stdtypes.date import daterange, tomorrow, Date
import footprints


class Rapatrie_Postproc(S2MTaskMixIn, OpTask):

    def process(self):
        t = self.ticket
        transfernode = t.sh.target().inetname + 'transfert-agt'
        datebegin, dateend = self.get_period()

        self.sh.title('Toolbox output tb03')
        tb03 = toolbox.input(
            role='Postproc_output',
            intent='out',
            local='postproc.nc',
            experiment=self.conf.xpid_postpr,
            block='postproc',
            geometry=self.conf.geometry,
            date=self.conf.rundate,
            datebegin=datebegin,
            dateend=dateend,
            nativefmt='netcdf',
            kind='SnowpackSimulation',
            model='postproc',
            namespace='vortex.multi.fr',
            cutoff='production',
            fatal=True
        ),
        print(t.prompt, 'tb03 =', tb03)
        print()

        ad.route(kind='bdpe', productid=self.conf.num_bdpe_postproc[self.conf.xpid], sshhost=transfernode,
         soprano_target=self.conf.soprano_target[self.conf.suitebg], routingkey='bdpe', term=0,
         filename='postproc.nc')


class Rapatrie_Forcing(S2MTaskMixIn, OpTask):

    def process(self):

        t = self.ticket
        transfernode = t.sh.target().inetname + 'transfert-agt'
        datebegin, dateend = self.get_period()

        source_safran, block_safran = self.get_source_safran()

        pearpmembers, members = self.get_list_members()

        if source_safran != 's2m':

            for m in members:
                try:

                    self.sh.title('Toolbox output tb10')
                    tb10 = toolbox.input(
                        local          = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment     = self.conf.xpid,
                        block          = 'meteo',
                        geometry       = self.conf.geometry,
                        date           = self.conf.rundate,
                        datebegin      = datebegin,
                        dateend        = dateend,
                        member         = m,
                        nativefmt      = 'netcdf',
                        intent         = 'inout',
                        namespace      = 'vortex.cache.fr',
                        kind           = 'MeteorologicalForcing',
                        model          = 's2m',
                        store_compressed = 'gzip',
                        cutoff         = 'production' if self.conf.previ else 'assimilation',
                        fatal          = False
                    ),
                    print((t.prompt, 'tb10 =', tb10))
                    print()

                    self.sh.tar('FORCING_{:03}.tar'.format(m), *[item for sublist in (['mb{:03}'.format(m)], ['FORCING']) for item in sublist], output= False)
                    self.sh.gzip('FORCING_{:03}.tar'.format(m), output = False)
                    ad.route(kind='bdpe', productid=self.conf.num_bdpe_for[self.conf.xpid], sshhost=transfernode, soprano_target=self.conf.soprano_target[self.conf.suitebg], routingkey='bdpe',term=m, filename='FORCING_{:03}.tar.gz'.format(m)) 
                except:
                    print(("Oops! The file FORCING_{:03}.tar is missing...".format(m)))


class Rapatrie_Pro(S2MTaskMixIn, OpTask):

    def process(self):

        t = self.ticket
        transfernode = t.sh.target().inetname + 'transfert-agt'
        datebegin, dateend = self.get_period()

        pearpmembers, members = self.get_list_members()

        for m in members:
            try:

                self.sh.title('Toolbox input tb11')
                tb11 = toolbox.input(
                    local            = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment       = self.conf.xpid,
                    block            = 'pro',
                    geometry         = self.conf.geometry,
                    date             = self.conf.rundate,
                    datebegin        = datebegin if self.conf.previ else '[dateend]/-PT24H',
                    dateend          = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                    member           = m,
                    nativefmt        = 'netcdf',
                    namespace        = 'vortex.cache.fr',
                    store_compressed = 'gzip',
                    kind             = 'SnowpackSimulation',
                    intent           = 'inout', 
                    model            = 'surfex',
                    cutoff           = 'production' if self.conf.previ else 'assimilation',
                    fatal            = False
                ),
                print((t.prompt, 'tb11 =', tb11))
                print()

                self.sh.tar('PRO_{:03}.tar'.format(m), *[item for sublist in (['mb{:03}'.format(m)], ['PRO']) for item in sublist], output= False)
                self.sh.gzip('PRO_{:03}.tar'.format(m), output = False)
                ad.route(kind='bdpe', productid=self.conf.num_bdpe_pro[self.conf.xpid], sshhost=transfernode, soprano_target=self.conf.soprano_target[self.conf.suitebg], routingkey='bdpe',term=m, filename='PRO_{:03}.tar.gz'.format(m))
            except:
                print(("Oops! The file PRO_{:03}.tar is missing...".format(m)))


class Rapatrie_Prep(S2MTaskMixIn, OpTask):

    def process(self):

        t = self.ticket
        transfernode = t.sh.target().inetname + 'transfert-agt'
        datebegin, dateend = self.get_period()

        pearpmembers, members = self.get_list_members()

        for m in members:
            try:

                self.sh.title('Toolbox input tb12')
                tb12 = toolbox.input(
                    local            = 'mb[member]/PREP_[datevalidity:ymdh].nc',
                    role             = 'SnowpackInit',
                    experiment       = self.conf.xpid,
                    block            = 'prep',
                    geometry         = self.conf.geometry,
                    datevalidity     = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                    date             = self.conf.rundate,
                    member           = m,
                    nativefmt        = 'netcdf',
                    namespace        = 'vortex.cache.fr',
                    intent           = 'inout',
                    store_compressed = 'gzip',
                    kind             = 'PREP',
                    model            = 'surfex',
                    cutoff           = 'production' if self.conf.previ else 'assimilation',
                    fatal            = False
                ),
                print((t.prompt, 'tb12 =', tb12))
                print()

                self.sh.tar('PREP_{:03}.tar'.format(m), *[item for sublist in (['mb{:03}'.format(m)], ['PREP']) for item in sublist], output= False)
                self.sh.gzip('PREP_{:03}.tar'.format(m), output = False)
                ad.route(kind='bdpe', productid=self.conf.num_bdpe_prep[self.conf.xpid], sshhost=transfernode, soprano_target=self.conf.soprano_target[self.conf.suitebg], routingkey='bdpe',term=m, filename='PREP_{:03}.tar.gz'.format(m))
            except:
                print(("Oops! The file PRO_{:03}.tar is missing...".format(m)))

        # When the simulation is an analysis covering August 1st, (on August 2nd, 3rd and 4th)
        # also route member 35 to BDPE in a specific product because this is the PREP file which is needed
        # to initialize future monthly reanalyses
        # This product will be read once a month in extract_for_reanalyse.py
        if not self.conf.previ and Date(dateend.year, 8, 1, 6) in list(daterange(tomorrow(base=datebegin), dateend)):
            self.sh.title('Toolbox input tb12')
            tb12 = toolbox.input(
                local='PREP_for_reanalysis.nc',
                role='SnowpackInitForMonthlyReanalysis',
                experiment=self.conf.xpid,
                block='prep',
                geometry=self.conf.geometry,
                datevalidity=dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                date=self.conf.rundate,
                member=35,  # deterministic run
                nativefmt='netcdf',
                namespace='vortex.cache.fr',
                intent='inout',
                kind='PREP',
                model='surfex',
                cutoff='assimilation',
                fatal=True
            ),
            print((t.prompt, 'tb12 =', tb12))
            print()
            ad.route(kind='bdpe', productid=self.conf.num_bdpe_initrea[self.conf.xpid], sshhost=transfernode, soprano_target=self.conf.soprano_target[self.conf.suitebg], routingkey='bdpe',term=0, filename='PREP_for_reanalysis.nc')