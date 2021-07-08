'''
Created on 7 nov. 2017

@author: lafaysse
'''


import iga.tools.op as op
from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad

from vortex.layout.nodes import Driver
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from snowtools.bronx.stdtypes.date import daterange, yesterday, tomorrow, Period
import footprints
from vortex.algo.components import DelayedAlgoComponentError

import snowtools
import re

def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw),
            Rapatrie_Forcing(tag='Rapatrie_Forcing', ticket=t, **kw),
            Rapatrie_Pro(tag='Rapatrie_Pro', ticket=t, **kw),
            Rapatrie_Prep(tag='Rapatrie_Prep', ticket=t, **kw),
        ],
        options=kw
    )


class Rapatrie_Forcing(S2MTaskMixIn, OpTask):


    def process(self):

        t = self.ticket
        transfernode = t.sh.target().inetname + 'transfert-agt'
        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False

        pearpmembers, members = self.get_list_members()


        if source_safran != 's2m' or exceptional_save_forcing:


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
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False

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
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False

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
