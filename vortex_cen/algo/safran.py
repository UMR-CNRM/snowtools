# -*- coding: utf-8 -*-

"""
safran.py
---------

SAFRAN related algo Components.

.. inheritance-diagram:: vortex_cen.algo.safran
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun,
                 vortex.algo.components.ParaExpresso, vortex.tools.parallelism.TaylorVortexWorker,
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:
   :parts: 1
"""

import os
import glob
import tarfile
from collections import defaultdict
import xarray as xr

from bronx.fancies import loggers
from bronx.stdtypes.date import Date, Period
from vortex.util.helpers import InputCheckerError
from footprints.stdtypes import FPList
from vortex.syntax.stdattrs import a_date
from vortex.algo.components import ParaExpresso
from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker, _CenWorkerBlindRun
from vortex.tools.systems import ExecutionError
from vortex_cen.algo.ensemble import S2MExecutionError
from snowtools.utils import xarray_snowtools  # noqa

logger = loggers.getLogger(__name__)


class GuessWorker(_CenWorkerBlindRun):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['guess', 'intercep']
            ),
            interpreter = dict(
                values = ['python', 'current']
            ),
            reforecast = dict(
                type     = bool,
                default  = False,
                optional = True,
            ),
            gribname = dict(
                type = str,
                default = False,
                optional = False,
            ),
        )
    )

    def vortex_task(self, **kwargs):
        ebauche = self.find_ebauche()
        super().vortex_task(ebauche=ebauche)

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        ebauche = kwargs['ebauche']
        if ebauche and not self.system.path.exists(ebauche):
            self.system.symlink(self.system.path.join(rundir, ebauche), ebauche)
        self.link_ifnotprovided(self.system.path.join(rundir, 'METADATA.grib'), 'METADATA.grib')
        for suffix in ['dbf', 'prj', 'qgs', 'qpj', 'shp', 'shx']:
            shapefile = 'massifs_safran.{:s}'.format(suffix)
            self.link_ifnotprovided(self.system.path.join(rundir, shapefile), shapefile)
        list_name = self.system.path.join(thisdir, self.kind + '.out')
        # La chaine en double 2021/2022 produit des fichiers GRIB eclatés,
        # il faut donc commencer par les concaténer. Cette concaténation est faite
        # dans l'algo pour profiter de la parallélisation.
        concat = self.system.forcepack(source=self.gribname, fmt='grib')
        if concat != self.gribname:
            self.system.rm(self.gribname, fmt='grib')
            self.system.mv(concat, self.gribname, fmt='grib')
        try:
            self.local_spawn(list_name)
            self.postfix()
        except ExecutionError:
            rdict['rc'] = S2MExecutionError(self.progname, self.deterministic, self.subdir,
                                            self.datebegin, self.dateend)
        finally:
            return rdict  # Note than in the other case return rdict is at the end

    def find_ebauche(self, opts=None):
        """Find ebauche namelist in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind='namelist')]
        self.system.subtitle('Namelist candidates')
        ebauche = None
        for nam in namcandidates:
            nam.quickview()
            if nam.container.basename.startswith('EBAUCHE_'):
                ebauche = nam.container.basename

        return ebauche


class Guess(ParaExpresso):
    """AlgoComponent that runs several executions of a guess-making script."""

    _footprint = dict(
        info = 'AlgoComponent that runs several executions of a guess-making script',
        attr = dict(
            kind = dict(
                values = ['guess'],
            ),
            interpreter = dict(
                values = ['python', 'current']
            ),
            reforecast = dict(
                type     = bool,
                optional = True,
                default  = False,
            ),
        )
    )

    def prepare(self, rh, opts):
        """Set some variables according to target definition."""
        super().prepare(rh, opts)
        self.env.DR_HOOK_NOT_MPI = 1

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        ddict['interpreter'] = self.interpreter
        ddict['reforecast'] = self.reforecast
        return ddict

    def _default_pre_execute(self, rh, opts):
        """Add concatenation of the 'METADATA' grib file here since it is a common ressource"""
        concat = self.system.forcepack(source='METADATA.grib', fmt='grib')
        if concat != 'METADATA.grib':
            self.system.rm('METADATA.grib', fmt='grib')
            self.system.mv(concat, 'METADATA.grib', fmt='grib')
        super()._default_pre_execute(rh, opts)

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        # Note: The number of members and the name of the subdirectories could be
        # auto-detected using the sequence
        cpl_model = self.get_origin(rh, opts)
        subdirs, gribnames = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs, gribname=gribnames, deterministic=cpl_model))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """Get the subdirectories from the effective inputs"""
        avail_members = self.context.sequence.effective_inputs(role=self.role_ref_namebuilder())
        subdirs = list()
        gribnames = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)
                gribnames.append(am.rh.container.basename)

        return subdirs, gribnames

    def get_origin(self, rh, opts):
        """Get the subdirectories from the effective inputs"""
        avail_members = self.context.sequence.effective_inputs(role=self.role_ref_namebuilder())
        subdirs = list()
        cpl_model = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)
                cpl_model.append(am.rh.provider.vconf == '4dvarfr')

        return cpl_model

    def role_ref_namebuilder(self):
        return 'Gridpoint'

    def postfix(self, rh, opts):
        pass


class _SafranWorker(_CenWorkerBlindRun):
    """TODO: Class documentation."""

    _abstract = True
    _footprint = dict(
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            day_begins_at = dict(
                type     = int,
                optional = True,
                default  = 6,
            ),
            posts = dict(
                info = "Switch to activate posts chain (=1) or not (=0)",
                type = int,
                optional = True,
                default = 1,
            ),
            execution = dict(
                values = ['analysis', 'forecast', 'reanalysis', 'reforecast'],
                optional = True,
            ),
        )
    )

    def __init__(self, *kargs, **kwargs):
        super().__init__(*kargs, **kwargs)
        self.set_actual_period()

    def set_actual_period(self):
        """Guess the dates that are to be covered by the forecast."""
        if self.datebegin.hour > self.day_begins_at:
            self.datebegin = self.datebegin + Period(days=1)
        self.datebegin.replace(hour=self.day_begins_at, minute=0, second=0, microsecond=0)
        if self.dateend.hour < self.day_begins_at:
            self.dateend = self.dateend - Period(days=1)
        self.dateend.replace(hour=self.day_begins_at, minute=0, second=0, microsecond=0)

    @property
    def days(self):
        self._days = defaultdict(list)
        ndays = (self.dateend - self.datebegin).days
        d = self.datebegin
        if ndays > 0:
            for n in range(1, ndays + 1):
                try_dates = [d + Period(hours=h) for h in range(0, 25, 3)]  # We check for 3-hours guess
                self._days[n] = self.get_guess(try_dates, fatal=False)
                d = d + Period(days=1)
        elif ndays == 0:
            logger.warning('The given time period is too short, doing nothing.')
        else:
            logger.warning('datebegin argument must be before dateend argument')
        return self._days

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        _Safran_namelists = ['ANALYSE', 'CENPRAA', 'OBSERVA', 'OBSERVR', 'IMPRESS',
                             'ADAPT', 'SORTIES', 'MELANGE', 'EBAUCHE', 'rsclim.don']
        for nam in _Safran_namelists:
            self.link_ifnotprovided(self.system.path.join(rundir, nam), nam)

        # Generate the 'OPxxxxx' files containing links for the safran execution.
        _OP_files_common = ['OPlisteo', 'OPlysteo', 'OPlistem', 'Oplystem', 'OPlisteml', 'OPlysteml',
                            'OPclim', 'OPNOmt', 'OPsat', 'OPnoir', 'OPposte']
        _OP_files_individual = ['OPguess', 'OPprevi', 'OPMET', 'OPSA', 'OPSG', 'OPSAP', 'OPSAN']
        if self.execution == 'reanalysis':
            # In reanalysis tasks the parallelisation is made over the seasons so
            # the observations are "individal files"
            _OP_files_individual.extend(['OPA', 'OPR', 'OPS', 'OPT'])
            # Un-comment the following lines to run a re-analysis without observation assimilation.
            # It is also necessary to modify the safran_reanalysis task to force the execution
            # of syrpluie and prevent the execution of sypluie
            # import glob
            # for obs in glob.glob('S????????') + glob.glob('T????????') + glob.glob('R????????'):
            #     self.system.remove(obs)
            # Add 'weather type' normals
            _OP_files_common.extend(['OPNOot', 'OPNOmt'])
        else:
            # In case no observation file is found at the given path, SAFRAN also check if it is
            # in the current repository, so the following is optionnal (that's the reason the
            # "reanalysis_with_rr_arpege" works even if the execution is "analysis" and
            # observation files are individual ones).
            _OP_files_common.extend(['OPA', 'OPR', 'OPS', 'OPT'])

        for op_file in _OP_files_common:
            if not self.system.path.isfile(op_file):
                with open(op_file, 'w') as f:
                    f.write(rundir.rstrip('/') + '@\n')

        for op_file in _OP_files_individual:
            if not self.system.path.isfile(op_file):
                with open(op_file, 'w') as f:
                    f.write(thisdir.rstrip('/') + '@\n')

        self.system.remove('sapfich')

        print('Running task {:s}'.format(self.kind))
        for day, dates in self.days.items():
            nech = len(dates) if len(dates) == 9 else 5
            self.sapdat(dates[-1], nech)
            rdict = self._safran_task(rundir, thisdir, day, dates, rdict)

        self.postfix()

        return rdict

    def _safran_task(self, rundir, thisdir, rdict):
        """The piece of code specific to a Safran submodule does here."""
        raise NotImplementedError()

    def check_mandatory_resources(self, rdict, filenames):
        outcome = True
        missing_files = list()
        for filename in filenames:
            if not self.system.path.exists(filename):
                # SAFRAN guess files can be named 'PYYMMDDHH' or 'EYYMMDDHH'
                if not (filename.startswith('P') and self.system.path.exists('E' + filename[1:])):
                    missing_files.append(filename)
        if len(missing_files) > 0:
            if self.execution not in ['reforecast', ]:
                rdict['rc'] = InputCheckerError('The following mandatory flow resource are missing : \n' +
                                                '\n'.join(missing_files))
                # TODO : Faire planter maintenant sans essayer de lancer SAFRAN ?
            # In analysis cases (oper or research) missing guess are not fatal since SAFRAN uses
            # a climatological guess that is corrected by the observations
            if self.execution not in ['analysis', 'reanalysis']:
                outcome = False
        return rdict, outcome

    def sapdat(self, thisdate, nech=5):
        # Creation of the 'sapdat' file containing the exact date of the file to be processed.
        self.system.remove('sapdat')

        # A PASSER EN NAMELIST OU A PARAMETRISER POUR D'AUTRES APPLICATIONS
        with open('sapdat', 'w') as d:
            d.write(thisdate.strftime('%y,%m,%d,%H,') + str(nech) + '\n')
            # In reanalysis execution the RR guess comes from a "weather types" analysis
            d.write('0,0,0\n')
            d.write('3,1,3,3\n')

    def get_guess(self, dates, prefix='P', fatal=False, dt=3):
        """Try to guess the corresponding input file."""
        actual_dates = list()
        # Control de cohérence sur les cumuls : on ne doit pas mélanger des cumuls sur 6h
        # avec des cumuls sur 24h. Le bool cumul permet de forcer la recherche de guess
        # de précipittion cumulées dès lors qu'une échéance 6h est absente.
        cumul = False
        for i, date in enumerate(dates):
            p = '{:s}{:s}'.format(prefix, date.yymdh)
            # Cas d'un fichier P ou E unique par echeance et utilisable par SAFRAN
            if self.system.path.exists(p) and not self.system.path.islink(p):
                actual_dates.append(date)
            # Cas d'un fichier P ou E nommé avec l'annee sur 4 digits (simulations Benedicte)
            elif self.system.path.exists('{:s}{:s}'.format(prefix, date.ymdh)):
                self.link_in('{:s}{:s}'.format(prefix, date.ymdh), prefix + date.yymdh)
                actual_dates.append(date)
            # Recherche d'un fichier P ou E correspondant à la date voulue en fonction du type d'execution
            else:
                if self.system.path.islink(p):
                    self.system.remove(p)
                # We try to find the P file with format Pyymmddhh_tt (yymmddhh + tt = date)
                # The maximum time is 108h (4 days)
                if self.execution == 'reforecast':
                    # We look for the first forecast run before the begining of the target period
                    t = int((date - self.datebegin).days * 24 + (date - self.datebegin).seconds / 3600)
                elif self.execution == 'forecast':
                    # In operational task the datebegin is 24h earlier (pseudo-forecast from 6h J-1 to 6h J)
                    # The forecast perdiod is split into two parts :
                    #     1) From J-1 6h to J 6h
                    #        The 'deterministic member' takes the 6h ARPEGE analysis
                    #        All PEARP members take the forecasts from the 6h J lead time
                    #     2) From J 6h to J+4 6h
                    #        The deterministic member takes the forecasts from the (D, 0:00)  lead time
                    #        All PEARP members now also take the forecats from the (D, 0:00) lead time
                    #        but used to take he forecasts from (D-1, 18:00) lead time before the 2022
                    #        PNT DBLE chain. This code works for both cases
                    d = date - Period(hours=6)
                    oldp = '{:s}{:s}_{!s}'.format(prefix, d.yymdh, 6)
                    if self.system.path.exists(oldp) and not cumul:
                        # This part is still necessary for the ARPEGE-based member that uses 6h assimilation guess
                        # from (D-1, 6:00) to (D, 6:00)
                        self.link_in(oldp, p)
                        actual_dates.append(date)
                    else:
                        cumul = True    # If no 6h forecast is available at 1 ech, SAFRAN needs 24h cumulates
                        # precipitation guess for the whole day.
                        # The goal of the following is to find the first ech "t" that could be available
                        # for the current date
                        if dates[0] == self.datebegin:  # Cas de la pseudo prévision de J-1 6h à J 6h
                            # t = number of hours since self.datebegin (D-1 at 6:00)
                            t = int((date - self.datebegin).days * 24 +
                                    (date - self.datebegin).seconds / 3600)  # (date - self.datebegin).seconds
                        # returns the number of hours since last 6:00.
                        else:  # Cas de la prévision dec J 6h à J+4 6h
                            # t = number of hours since (D-1) at 18:00 (PEARP lead time used until the 2022 PNT
                            # DBLE chain. This works also with the (D, 6:00) lead time used for ARPEGE (and PEARP
                            # from the 2022 PNT DBLE chain on).
                            t = int((date - self.datebegin).days * 24 +
                                    (date - self.datebegin).seconds / 3600) - 18  # 18 is the difference between
                            # D-1 (6:00) and D (0:00)
                else:  # Analysis execution
                    if date == dates[-1]:
                        # Avoid to take the first P file of the next day
                        # Check for a 6-hour analysis
                        d = date - Period(hours=6)
                        oldp = '{:s}{:s}_{!s}'.format(prefix, d.yymdh, 6)
                        if self.system.path.exists(oldp):
                            self.link_in(oldp, p)
                            actual_dates.append(date)
                        else:
                            # If there is no 6-hour analysis we need at least a 24h forecast
                            # to have a cumulate rr24
                            t = 24
                    else:
                        t = 0
                while not self.system.path.islink(p) and (t <= 102):
                    d = date - Period(hours=t)
                    oldp = '{:s}{:s}_{!s}'.format(prefix, d.yymdh, t)
                    if self.system.path.exists(oldp):
                        self.link_in(oldp, p)
                        actual_dates.append(date)
                    t = t + dt  # 3-hours check

        if 5 < len(actual_dates) < 9:
            # We must have either 5 or 9 dates, if not we only keep synoptic ones
            for date in actual_dates:
                if date.hour not in [0, 6, 12, 18]:
                    actual_dates.remove(date)
        if len(actual_dates) < 5:
            # print("WARNING : Not enough guess for date {0:s}, expecting at least 5, "
            #      "got {1:d}".format(dates[0].ymdh, len(actual_dates)))
            # In this case, actual_dates is filled with the mandatory dates
            if prefix == 'P':
                actual_dates = self.get_guess(dates, prefix='E', fatal=False)
            else:
                logger.warning('No guess files found for date {:s}, '.format(date.ymdh) +
                               'SAFRAN will run with climatological guess')
                actual_dates = [d for d in dates if d.hour in [0, 6, 12, 18]]

        return actual_dates


class InterCEPWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['intercep']
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        _Safran_namelists = ['ANALYSE', 'CENPRAA', 'OBSERVA', 'OBSERVR', 'IMPRESS',
                             'ADAPT', 'SORTIES', 'MELANGE', 'EBAUCHE', 'surfz']
        for nam in _Safran_namelists:
            self.link_in(self.system.path.join(rundir, nam), nam)

        # Generate the 'OPxxxxx' files containing links for the safran execution.
        _OP_files_individual = ['OPguess']
        _OP_files_common = ['OPcep']

        for op_file in _OP_files_individual:
            if not self.system.path.isfile(op_file):
                with open(op_file, 'w') as f:
                    f.write(thisdir + '@\n')

        for op_file in _OP_files_common:
            if not self.system.path.isfile(op_file):
                with open(op_file, 'w') as f:
                    f.write(rundir + '@\n')

        if self.datebegin < Date(2002, 8, 1):
            print('Running task {:s}'.format(self.kind))
            rundate = self.datebegin.replace(hour=self.day_begins_at)
            while rundate <= self.dateend and rundate < Date(2002, 8, 1):
                self.sapdat(rundate)
                list_name = self.system.path.join(thisdir, self.kind + rundate.ymdh + '.out')
                self.local_spawn(list_name)
                rundate = rundate + Period(hours=6)
        else:
            print('Guess should already be there, doing nothing')

        self.postfix()

        return rdict

    def sapdat(self, thisdate, nech=5):
        # Creation of the 'sapdat' file containing the exact date of the file to be processed.
        self.system.remove('sapdat')

        # A PASSER EN NAMELIST OU A PARAMETRISER POUR D'AUTRES APPLICATIONS
        with open('sapdat', 'w') as d:
            d.write(thisdate.strftime('%y,%m,%d,%H,') + str(nech) + '\n')


class SafraneWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['safrane']
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        nech = len(dates) if len(dates) == 9 else 5
        self.get_guess(dates)
        mandatory_dates = [d for d in dates if d.hour in [0, 6, 12, 18]]
        rdict, go = self.check_mandatory_resources(rdict, ['P{:s}'.format(d.yymdh)
                                                           for d in mandatory_dates])
        if go:
            for d in dates:
                logger.info('Running date : {:s}'.format(d.ymdh))
                self.sapdat(d, nech)
                # Creation of the 'sapfich' file containing the name of the output file
                with open('sapfich', 'w') as f:
                    f.write('SAFRANE_d{!s}_{:s}'.format(day, d.ymdh))
                list_name = self.system.path.join(thisdir, self.kind + d.ymdh + '.out')
                try:
                    self.local_spawn(list_name)
                    # Reanalysis : if the execution was allright we don't need the log file
                    # if self.execution in ['reanalysis', 'reforecast']:
                    #     self.system.remove(list_name)
                except ExecutionError:
                    self.system.remove('SAFRANE_d{!s}_{:s}'.format(day, d.ymdh))
                    rdict['rc'] = S2MExecutionError(self.progname, self.deterministic,
                                                    self.subdir,
                                                    self.datebegin, self.dateend)
        return rdict


class SypluieWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['sypluie']
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        self.link_in('SAPLUI5' + dates[-1].ymdh, 'SAPLUI5_ARP')
        # Creation of the 'sapfich' file containing the name of the output file
        with open('sapfich', 'w') as f:
            f.write('SAPLUI5' + dates[-1].ymdh)
        list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
        try:
            self.local_spawn(list_name)
            # Reanalysis : if the execution was allright we don't need the log file
            # if self.execution in ['reanalysis', 'reforecast']:
            #     self.system.remove(list_name)
        except ExecutionError:
            self.system.remove('SAPLUI5' + dates[-1].ymdh)
            rdict['rc'] = S2MExecutionError(self.progname, self.deterministic, self.subdir,
                                            self.datebegin, self.dateend)
        finally:
            return rdict  # Note than in the other case return rdict is at the end

    def sapdat(self, thisdate, nech=5):
        # Creation of the 'sapdat' file containing the exact date of the file to be processed.
        self.system.remove('sapdat')

        # A PASSER EN NAMELIST OU A PARAMETRISER POUR D'AUTRES APPLICATIONS
        with open('sapdat', 'w') as d:
            d.write(thisdate.strftime('%y,%m,%d,%H,') + str(nech) + '\n')
            # i1=0 pour lecture fichier P ou E
            # i2=0 pour obs rr dans fichier R
            # i3:
            # =0 Ebauche par moyenne mensuelle
            # =1 Ebauche par type de temps
            # =2 Ebauche clim constante (à éviter)
            # =3 pour lecture fichier produit par syrpluie
            d.write('0,0,3\n')
            d.write('3,1,3,3\n')


class SyrpluieWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['syrpluie']
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        self.get_guess(dates)
        # Creation of the 'sapfich' file containing the name of the output file
        with open('sapfich', 'w') as f:
            f.write('SAPLUI5' + dates[-1].ymdh)
        list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
        mandatory_dates = [d for d in dates if d.hour in [0, 6, 12, 18]]
        rdict, go = self.check_mandatory_resources(rdict, ['P{:s}'.format(d.yymdh)
                                                           for d in mandatory_dates])
        if go:
            try:
                self.local_spawn(list_name)
                # Reanalysis : if the execution was allright we don't need the log file
                # if self.execution in ['reanalysis', 'reforecast']:
                #     self.system.remove(list_name)
            except ExecutionError:
                self.system.remove('SAPLUI5' + dates[-1].ymdh)
                rdict['rc'] = S2MExecutionError(self.progname, self.deterministic, self.subdir,
                                                self.datebegin, self.dateend)
            finally:
                return rdict  # Note than in the other case return rdict is at the end
        else:
            return rdict  # Note than in the other case return rdict is at the end

    def sapdat(self, thisdate, nech=5):
        # Creation of the 'sapdat' file containing the exact date of the file to be processed.
        self.system.remove('sapdat')

        # A PASSER EN NAMELIST OU A PARAMETRISER POUR D'AUTRES APPLICATIONS
        with open('sapdat', 'w') as d:
            d.write(thisdate.strftime('%y,%m,%d,%H,') + str(nech) + '\n')
            # RR guess are not available with ERA-40, the guess comes from a "weather types" analysis
            # Except for more recent years for which ARPEGE rr guess are available
            # if self.execution == 'reanalysis' and self.datebegin < Date(2017, 8, 1, 0):
            #     d.write('0,0,1\n')
            # else:
            #     d.write('0,0,3\n')
            # Update 13/02/2026 : RR guess are now available with ERA-5, we use them !
            # i1=0 --> lecture guess dans fichier P ou E
            # i2 inutilisé
            # i3:
            # =0 --> répartition verticale par moyenne mensuelle
            # =1 --> répartition verticale par type de temps
            # =2 --> répartition verticale par gradient standard
            # =3 --> répartition verticale déduit du modèle
            d.write('0,0,3\n')
            d.write('3,1,3,3\n')


class SyvaprWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['syvapr']
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        rdict, go = self.check_mandatory_resources(rdict, ['SAFRANE_d{!s}_{:s}'.format(day, d.ymdh) for d in dates])
        if go:
            for j, d in enumerate(dates):
                self.link_in('SAFRANE_d{!s}_{:s}'.format(day, d.ymdh), 'SAFRAN' + str(j + 1))
            self.link_in('SAPLUI5' + dates[-1].ymdh, 'SAPLUI5')
            list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
            try:
                self.local_spawn(list_name)
                # Reanalysis : if the execution was allright we don't need the log file
                # if self.execution in ['reanalysis', 'reforecast']:
                #     self.system.remove(list_name)
                for suffix in ['HA', 'HS', 'NA', 'TA', 'TS', 'UA', 'US', 'VA', 'VS']:
                    self.mv_if_exists('SAF4D_{:s}'.format(suffix),
                                      'SAF4D_{:s}_{:s}'.format(suffix, dates[-1].ymdh))
            except ExecutionError:
                rdict['rc'] = S2MExecutionError(self.progname, False, self.subdir,
                                                self.datebegin, self.dateend)

        return rdict  # Note than in the other case return rdict is at the end


class SyvafiWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['syvafi']
            ),
            deterministic = dict(
                default  = False,
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        # if self.check_mandatory_resources(rdict, ['SAPLUI5' + str(day), ]):
        for j, d in enumerate(dates):
            self.link_in('SAFRANE_d{!s}_{:s}'.format(day, d.ymdh), 'SAFRAN' + str(j + 1))
        self.link_in('SAPLUI5' + dates[-1].ymdh, 'SAPLUI5')
        for suffix in ['HA', 'HS', 'NA', 'TA', 'TS', 'UA', 'US', 'VA', 'VS']:
            self.link_in('SAF4D_{:s}_{:s}'.format(suffix, dates[-1].ymdh), 'SAF4D_{:s}'.format(suffix))
        list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
        try:
            self.local_spawn(list_name)
            self.mv_if_exists('fort.90', 'TAL' + dates[-1].ymdh)
            # if self.execution in ['reanalysis', 'reforecast']:
            #     self.system.remove(list_name)
        except ExecutionError:
            rdict['rc'] = S2MExecutionError(self.progname, False, self.subdir,
                                            self.datebegin, self.dateend)

        return rdict


class SyrmrrWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['syrmrr']
            ),
        )
    )

    def _safran_task(self, rundir, thisdir, day, dates, rdict):

        rdict, go = self.check_mandatory_resources(rdict, ['SAPLUI5' + dates[-1].ymdh])
        if go:
            self.link_in('SAPLUI5' + dates[-1].ymdh, 'fort.12')
            list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
            try:
                self.local_spawn(list_name)
                self.mv_if_exists('fort.13', 'SAPLUI5' + dates[-1].ymdh)
                self.mv_if_exists('fort.14', 'SAPLUI5_ARP' + dates[-1].ymdh)
                self.mv_if_exists('fort.15', 'SAPLUI5_ANA' + dates[-1].ymdh)
                # if self.execution in ['reanalysis', 'reforecast']:
                #     self.system.remove(list_name)
            except ExecutionError:
                rdict['rc'] = S2MExecutionError(self.progname, self.deterministic, self.subdir,
                                                self.datebegin, self.dateend)

        return rdict


class SytistWorker(_SafranWorker):
    """TODO: Class documentation."""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['sytist']
            ),
            metadata = dict(
                values   = ['StandardSAFRAN', 'StandardPROSNOW'],
                optional = True,
            ),
        )
    )

    @property
    def get_standard_metadata_section(self):
        """
        Return the section name to use in the Standard_Output_Metadata.ini configuration file
        """
        if self.reprod_info.get('vapp', None) == 's2m':
            if self.reprod_info.get('vconf', None) == 'reanalysis':
                product = "S2MReanalysis"
            elif self.reprod_info.get('vconf', None) in ['alp', 'pyr', 'cor', 'mac', 'vog', 'jur', 'postes']:
                product = "S2MOper"
        else:
            product = None
        return product

    def postfix(self, rdict):
        if self.metadata:
            for forcing_name in ['FORCING_massif.nc', 'FORCING_postes.nc']:
                if self.system.path.isfile(forcing_name):
                    product = self.get_standard_metadata_section
                    with xr.open_dataset(forcing_name, engine='snowtools') as forcing:
                        getattr(forcing, self.metadata).GlobalAttributes(product=product, **self.reprod_info)
                        #getattr(forcing, self.metadata).add_standard_names()  # Already called by GlobalAttributes
                        forcing.to_netcdf(forcing_name)

        if 'rc' in rdict.keys() and (isinstance(rdict['rc'], S2MExecutionError) or
                                     isinstance(rdict['rc'], InputCheckerError)):
            self.system.remove('FORCING_massif.nc')
            self.system.remove('FORCING_postes.nc')

        self.mv_if_exists('FORCING_massif.nc',
                          'FORCING_massif_{:s}_{:s}.nc'.format(self.datebegin.ymd6h, self.dateend.ymd6h))
        self.mv_if_exists('FORCING_postes.nc',
                          'FORCING_postes_{:s}_{:s}.nc'.format(self.datebegin.ymd6h, self.dateend.ymd6h))

        if self.execution in ['analysis', 'reanalysis']:
            # Ensure that at least one listing file has been created, otherwise the tar command raises an
            # ExecutionError that isn't filtered by the DelayedAlgoError mechanism and make the algo component
            # (even other workers that were fine) crash.
            # This issue has been identified when trying the #2079 vortex issue that should allow the task
            # to go on until produced resources are archived even if some members have crashed.
            if len(self.system.ffind('liste_obs*')) > 0:
                self.system.tar('liste_obs_{:s}_{:s}.tar.gz'.format(self.datebegin.ymd6h, self.dateend.ymd6h),
                                'liste_obs*')
        # Ensure that at least one listing file has been created, otherwise the tar command raises an
        # ExecutionError that isn't filtered by the DelayedAlgoError mechanism and make the algo component
        # (even other workers that were fine) crash.
        # This issue has been identified when trying the #2079 vortex issue that should allow the task
        # to go on until produced resources are archived even if some members have crashed.
        if len(self.system.ffind('*.out')) > 0:
            self.system.tar('listings_safran_{:s}_{:s}.tar.gz'.format(self.datebegin.ymd6h, self.dateend.ymd6h),
                            '*.out')

        super().postfix()

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        self.system.remove('sapfich')
        print('Running task {:s}'.format(self.kind))
        for day, dates in self.days.items():
            nech = len(dates) if len(dates) == 9 else 5
            self.sapdat(dates[-1], nech)
            rdict = self._safran_task(rundir, thisdir, day, dates, rdict)

        self.postfix(rdict)
        return rdict

    def _safran_task(self, rundir, thisdir, day, dates, rdict):
        self.link_in('SAPLUI5' + dates[-1].ymdh, 'SAPLUI5')
        self.link_in('SAPLUI5_ARP' + dates[-1].ymdh, 'SAPLUI5_ARP')
        self.link_in('SAPLUI5_ANA' + dates[-1].ymdh, 'SAPLUI5_ANA')
        for suffix in ['HA', 'HS', 'NA', 'TA', 'TS', 'UA', 'US', 'VA', 'VS']:
            self.link_in('SAF4D_{:s}_{:s}'.format(suffix, dates[-1].ymdh), 'SAF4D_{:s}'.format(suffix))
        rdict, go = self.check_mandatory_resources(rdict,
                                                   ['SAPLUI5'] + ['SAFRANE_d{!s}_{:s}'.format(day, d.ymdh)
                                                                  for d in dates])
        if go:
            for j, d in enumerate(dates):
                self.link_in('SAFRANE_d{!s}_{:s}'.format(day, d.ymdh), 'SAFRAN' + str(j + 1))
            list_name = self.system.path.join(thisdir, self.kind + dates[-1].ymd + '.out')
            try:
                self.local_spawn(list_name)
            except ExecutionError:
                rdict['rc'] = S2MExecutionError(self.progname, self.deterministic, self.subdir,
                                                self.datebegin, self.dateend)

        return rdict

    def sapdat(self, thisdate, nech=5):
        # Creation of the 'sapdat' file containing the exact date of the file to be processed.
        self.system.remove('sapdat')

        # A PASSER EN NAMELIST OU A PARAMETRISER POUR D'AUTRES APPLICATIONS
        with open('sapdat', 'w') as d:
            d.write(thisdate.strftime('%y,%m,%d,%H,') + str(nech) + '\n')
            if self.execution in ['forecast', 'reforecast']:
                d.write('0,0,0\n')
            elif self.execution in ['analysis', 'reanalysis']:
                d.write('0,1,0\n')
            d.write('3,1,3,3\n')
            d.write('0\n')
            d.write('1,1,{!s}\n'.format(self.posts))


class TarGuess(_CenTaylorRun):
    """
    Create a tar archive from an ensemble of SAFRAN guess files.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several concatenations in parallel.',
        attr = dict(
            kind  = dict(
                values     = ['TarSafranGuess'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Gridpoint'],
            ),
            domains = dict(
                info     = "List of domains covered by the guess files to tar",
                type     = FPList,
            )
        ),
    )

    def get_subdirs(self, rh, opts):
        """
        """
        avail_members = self.context.sequence.effective_inputs(role=self.role_members)

        print('----------------------------------------------------------------------')
        print('List of Workers :')
        print('-----------------')
        if len(avail_members) > 0:
            subdirs = list()
            # Retrive the subdirectory asociated to each identified RH
            for am in avail_members:
                if am.rh.resource.date.ymdh not in subdirs:
                    subdirs.append(am.rh.resource.date.ymdh)
                    print('* ', am.rh.resource.date.ymdh)
        else:
            subdirs = ['.']
            print('* .')
        print('----------------------------------------------------------------------')
        # logger.info('Workers : \n' + '\n'.join(subdirs))

        return subdirs


class TarGuessWorker(_CenTaylorVortexWorker):
    """
    Create a tar archive from an ensemble of SAFRAN guess files.
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['TarSafranGuess']
            ),
            domains = dict(
                info     = "List of domains covered by the guess files to tar",
                type     = FPList,
            )
        )
    )

    def vortex_task(self, **kwargs):
        """
        """
        for geometry in self.domains:
            tarname = f'ebauches_{geometry}_{self.subdir}.tar'
            with tarfile.open(tarname, mode='w') as tarfic:
                for f in glob.glob(f'{self.subdir}/*/*/P????????*{geometry}*'):
                    # f = 'YYYYMMDD00/mbXXX/ECH/PYYMMDDHH_E_dom_production'
                    ech = int(f.split('/')[2])  # ECH
                    # On veut organiser le tar pour qu'il soit directement exploitable par
                    # l'algo SAFRAN arpès détarrage : toutes les échéances issues d'un même
                    # réseau doivent être regroupées dans le même répertoire et le nom
                    # du fichier guess de la forme PYYMMDDHH correspondant à la date
                    # de validité du guess
                    validity = Date(self.subdir) + Period(hours=ech)
                    arcname = os.path.join(f.split('/')[0], f.split('/')[1], f'P{validity.yymdh}')
                    tarfic.add(f, arcname=arcname)
