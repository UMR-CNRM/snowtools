"""
Algo Components for ensemble S2M simulations.
"""

from bronx.fancies import loggers
from bronx.stdtypes.date import Date, Period, tomorrow
from bronx.syntax.externalcode import ExternalCodeImportChecker
from collections import defaultdict
import footprints
from vortex.algo.components import ParaBlindRun, ParaExpresso, TaylorRun, DelayedAlgoComponentError
from vortex.syntax.stdattrs import a_date
from vortex.tools.parallelism import VortexWorkerBlindRun, TaylorVortexWorker
from vortex.tools.systems import ExecutionError
from vortex.util.helpers import InputCheckerError

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.tools.change_prep import prep_tomodify
    from snowtools.utils.resources import get_file_period, save_file_period, save_file_date
    from snowtools.tools.update_namelist import update_surfex_namelist_object
    from snowtools.tools.change_forcing import forcinput_select, forcinput_applymask, forcinput_extract,\
        forcinput_changedates
    from snowtools.utils.infomassifs import infomassifs
    from snowtools.tools.massif_diags import massif_simu
    from snowtools.utils.ESCROCsubensembles import ESCROC_subensembles
    from snowtools.utils import S2M_standard_file
    from snowtools.utils.FileException import TimeListException, FileNameException


class _S2MWorkerMixIn(object):

    def vortex_task(self, **kwargs):
        """
        Main method, the first that is executed at the initialization of the
        worker.

        Its purpose is to set the worker's specific sub-environment (move to
        the potential corresponding sub-directory) and to return the output
        state of the worker (stored in the `rdict` dictionary) to the main Algo
        Component.

        Any overloading of this method or any sub-method called must return
        such a dictionary storing potential execution errors (in the `rc` entry
        of the dictionary). As a consequence, any part of code within the worker
        that might raise a python Exception must be encapsulated in a
        try/except instruction in order to catch such exception, put it into
        the `rdict` output and return it to the main Algo Component where all
        exceptions should be managed properly.

        :note: Any un-catched exception will cause a (clean) shutdown of the
               :mod:`taylorism` system (e.g. All the other workers will be killed
               and the main Algo Component will exit).
        """

        rdict = dict(rc=True)
        rundir = self.system.getcwd()
        if self.subdir is not self.system.path.dirname(rundir):
            thisdir = self.system.path.join(rundir, self.subdir)
            with self.system.cdcontext(self.subdir, create=True):
                rdict = self._commons(rundir, thisdir, rdict, **kwargs)
        else:
            thisdir = rundir
            rdict = self._commons(rundir, thisdir, rdict, **kwargs)

        return rdict

    def _commons(self, rundir, thisdir, rdict):
        """
        Abstract method called by the main **vortex_task** method to set up the
        worker's environment (links to common files, name of execution listings,
        ...) and launch the executable, call the method that launches it or
        apply algo instructions.
        """
        raise NotImplementedError

    def exists(self, target):
        """Check if a target file exists."""
        if self.system.path.islink(target) or self.system.path.isfile(target):
            return True
        else:
            return False

    def mv_if_exists(self, local, dest):
        """Move a file if it exists (intended to deal with output files)."""
        if self.system.path.isfile(local):
            self.system.mv(local, dest)

    def copy_if_exists(self, local, dest):
        """Copy a file if it exists (intended to deal with input files)."""
        if self.system.path.isfile(local):
            self.system.cp(local, dest)

    def link_in(self, local, dest):
        """Link a file (the target is cleaned first)."""
        self.system.remove(dest)
        if self.system.path.isfile(local):
            self.system.symlink(local, dest)

    def link_ifnotprovided(self, local, dest):
        """Link a file if the target does not already exist."""
        if not self.system.path.islink(dest) and not self.system.path.isfile(dest):
            if self.system.path.isfile(local):
                self.system.symlink(local, dest)

    def copy_ifnotprovided(self, local, dest):
        """Link a file if the target does not already exist."""
        if not self.system.path.islink(dest) and not self.system.path.isfile(dest):
            if self.system.path.isfile(local):
                self.system.cp(local, dest)

    def postfix(self):
        self.system.subtitle('{:s} : directory listing (post-run)'.format(self.kind))


class _CENWorkerBlindRun(_S2MWorkerMixIn, VortexWorkerBlindRun):
    """
    This abstract worker is designed to drive the launch of any S2M executable
    without MPI parallelization (deterministic or ensemble-like simulations) in
    association with an Algo Component inheriting from an :class:`_CENParaBlindRun`
    or :class:`Guess` Algo Component.

    A single worker is thus a deterministic execution of a given binary with a
    specific environment that can be run in parallel with other workers
    (executions of the same binary with different environments).
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of S2M ensemble experiment associated to an executable'
               'without MPI parallelization.',
        attr = dict(
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = True
            ),
            deterministic = dict(
                type     = bool,
                default  = True,
                optional = True,
            ),
            reprod_info = dict(
                info     = "Informations that must be stored in output files for reproductibility",
                type     = dict,
                optional = True,
                default  = dict(),
            ),
        )
    )


class _CENTaylorVortexWorker(_S2MWorkerMixIn, TaylorVortexWorker):
    """
    This abstract worker is designed to drive the launch of any S2M task not associated
    to an executable with MPI parallelization (deterministic or ensemble-like simulations) in
    association with an Algo Component inheriting from an :class:`_CENTaylorRun`

    A single worker is thus a deterministic execution of a list of python commands in a
    specific environment that can be run in parallel with other workers.
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of S2M ensemble experiment NOT associated to an executable'
               'with MPI parallelization.',
        attr = dict(
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = False
            ),
        )
    )


class GuessWorker(_CENWorkerBlindRun):
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


class _SafranWorker(_CENWorkerBlindRun):
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

    def postfix(self, rdict):
        if self.metadata:
            for f in ['FORCING_massif.nc', 'FORCING_postes.nc']:
                if self.system.path.isfile(f):
                    forcing_to_modify = getattr(S2M_standard_file, self.metadata)(f, "a")
                    forcing_to_modify.GlobalAttributes(**self.reprod_info)
                    forcing_to_modify.add_standard_names()
                    forcing_to_modify.close()

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


class S2MExecutionError(ExecutionError):
    """Execution Error in S2M algo component that should be catched and delayed"""

    def __init__(self, model, deterministic, subdir, datebegin, dateend):
        self.model = model
        self.deterministic = deterministic  # Key used for delayed exception management
        self.subdir = subdir
        self.datebegin = datebegin
        self.dateend = dateend
        super().__init__(self.model + ' execution failed.')

    def __str__(self):
        return ("Error while running " + self.model + " for member " + self.subdir + " for period " +
                self.datebegin.ymdh + " - " + self.dateend.ymdh)

    def __reduce__(self):
        red = list(super().__reduce__())
        red[1] = tuple([self.model, self.deterministic, self.subdir, self.datebegin,
                        self.dateend])  # Les arguments qui seront passes a __init__
        return tuple(red)


class S2MMissingDeterministicError(DelayedAlgoComponentError):
    """
    Exception raised when no resource is found for a mandatory role although
    fatal is False in toolbox inputs.
    """

    def __init__(self, role):
        self.role = role
        self.deterministic = True

    def __str__(self):
        return "Unable to find the mandatory resource of role : " + self.role

    def __reduce__(self):
        red = list(super().__reduce__())
        red[1] = tuple([self.role, self.deterministic])
        return tuple(red)


@echecker.disabled_if_unavailable
class SurfexWorker(_CENWorkerBlindRun):
    """
    This algo component is designed to run a SURFEX experiment without
    MPI parallelization.
    """

    _footprint = dict(
        info = 'AlgoComponent designed to run a SURFEX experiment without MPI parallelization.',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            dateinit  = a_date,
            kind = dict(
                values = ['deterministic', 'escroc', 'ensmeteo', 'ensmeteonodet', 'ensmeteo+sytron', 'ensmeteo+escroc',
                          'croco'],
            ),
            threshold = dict(
                info = "Threshold to initialise snowdepth",
                type = int,
                optional = True,
                default = -999
            ),
            physical_options = dict(
                info = "Dictionnary of ESCROC physical options",
                type = dict,
                optional = True,
                default = {}
            ),
            snow_parameters = dict(
                info = "Dictionnary of ESCROC snow physical parameters",
                type = dict,
                optional = True,
                default = {}
            ),
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = False
            ),
            geometry_in=dict(
                info="Area information in case of an execution on a massif geometry",
                type=footprints.stdtypes.FPList,
            ),
            geometry_out=dict(
                info="The resource's massif geometry.",
                type=str,
            ),
            daily = dict(
                info = "If True, split simulations in daily runs",
                type = bool,
                optional = True,
                default = False,
            ),
            dailynamelist = dict(
                info = "If daily is True, possibility to provide a list of namelists to change each day",
                type = list,
                optional = True,
                default = [],
            ),
        )
    )

    def modify_prep(self, datebegin_this_run):
        """
        The PREP file needs to be modified if the init date differs from the starting
        date or if a threshold needs to be applied on snow water equivalent.
        """
        modif_swe = self.threshold > 0 and datebegin_this_run.month == 8 and datebegin_this_run.day == 1
        modif_date = datebegin_this_run == self.datebegin and self.datebegin != self.dateinit
        modif = modif_swe or modif_date

        if modif:
            prep = prep_tomodify("PREP.nc")

            if modif_swe:
                print("APPLY THRESHOLD ON SWE.")
                prep.apply_swe_threshold(self.threshold)

            if modif_date:
                print("CHANGE DATE OF THE PREP FILE.")
                prep.change_date(self.datebegin)

            prep.close()
        else:
            print("DO NOT CHANGE THE PREP FILE.")

    def _commons(self, rundir, thisdir, rdict, **kwargs):

        if len(self.dailynamelist) > 1:
            list_files_copy = self.dailynamelist
        else:
            list_files_copy = ["OPTIONS.nam"]
        list_files_link = ["PGD.nc", "METADATA.xml", "ecoclimapI_covers_param.bin",
                           "ecoclimapII_eu_covers_param.bin", "drdt_bst_fit_60.nc"]
        if self.kind in ['escroc', 'croco'] and (self.datebegin != self.dateinit or self.threshold > 0):
            list_files_copy_ifnotprovided = ["PREP.nc"]
            list_files_link_ifnotprovided = []
        else:
            list_files_copy_ifnotprovided = []
            list_files_link_ifnotprovided = ["PREP.nc"]
        # here Bertrand also created links towards the forcings but in a non-standard way --> to be checked

        for required_copy in list_files_copy:
            self.copy_if_exists(self.system.path.join(rundir, required_copy), required_copy)
        for required_link in list_files_link:
            self.link_in(self.system.path.join(rundir, required_link), required_link)
        for required_link in list_files_link_ifnotprovided:
            self.link_ifnotprovided(self.system.path.join(rundir, required_link), required_link)
            # For reforecast:
            self.link_ifnotprovided(self.system.path.join(self.system.path.dirname(thisdir), required_link),
                                    required_link)
        for required_copy in list_files_copy_ifnotprovided:
            self.copy_ifnotprovided(self.system.path.join(rundir, required_copy), required_copy)
            self.copy_ifnotprovided(self.system.path.join(self.system.path.dirname(thisdir), required_copy),
                                    required_link)

        rdict = self._surfex_task(rundir, thisdir, rdict)
        self.postfix()
        return rdict

    def _surfex_task(self, rundir, thisdir, rdict):
        # ESCROC cases: each member will need to have its own namelist
        # meteo ensemble cases: the forcing modification must be applied to all members and the namelist
        # generation requires that the forcing generation has already be done. Therefore, preprocessing
        # is done in the offline algo in all these cases
        # Determinstic cases : the namelist is prepared in the preprocess algo component in order to allow
        # to build PGD and PREP
        namelist_ready = self.kind == 'deterministic'
        need_other_run = True
        need_other_forcing = True
        need_save_forcing = False
        updateloc = True
        datebegin_this_run = self.datebegin

        sytron = self.kind == "ensmeteo+sytron" and self.subdir == "mb036"

        changenamelistdaily = self.daily and (len(self.dailynamelist) > 1)

        while need_other_run:

            # Modification of the PREP file
            self.modify_prep(datebegin_this_run)

            if need_other_forcing:

                if self.kind == "escroc":
                    # ESCROC only: the forcing files are in the father directory (same forcing for all members)
                    forcingdir = rundir
                elif sytron:
                    # ensmeteo+sytron: the forcing files are supposed to be in the subdirectories
                    # of each member except for the sytron member
                    forcingdir = rundir + "/mb035"
                else:
                    # ensmeteo or ensmeteo+escroc or croco: the forcing files are supposed to be in the subdirectories
                    # of each member
                    # determinstic case: the forcing file(s) is/are in the only directory
                    forcingdir = thisdir

                if len(self.geometry_in) > 1:
                    print("FORCING AGGREGATION")
                    forcinglist = []
                    for massif in self.geometry_in:
                        try:
                            dateforcbegin, dateforcend = get_file_period(
                                "FORCING",
                                forcingdir + "/" + massif,
                                datebegin_this_run,
                                self.dateend)
                        except FileNameException:
                            deterministic = self.subdir == "mb035"
                            rdict['rc'] = S2MExecutionError("missing forcing file in directory " + forcingdir + "/" +
                                                            massif, deterministic, self.subdir, datebegin_this_run,
                                                            self.dateend)
                            return rdict  # Note than in the other case return rdict is at the end
                        forcingname = "FORCING_" + massif + ".nc"
                        self.system.mv("FORCING.nc", forcingname)
                        forcinglist.append(forcingname)

                    print(forcinglist)
                    try:
                        forcinput_applymask(forcinglist, "FORCING.nc", **self.reprod_info)
                    except TimeListException:
                        deterministic = self.subdir == "mb035"
                        rdict['rc'] = S2MExecutionError("merge of forcings", deterministic, self.subdir,
                                                        dateforcbegin, dateforcend)
                        return rdict  # Note than in the other case return rdict is at the end

                    need_save_forcing = True
                else:
                    # Get the first file covering part of the whole simulation period
                    print("LOOK FOR FORCING")
                    try:
                        dateforcbegin, dateforcend = get_file_period(
                            "FORCING",
                            forcingdir,
                            datebegin_this_run,
                            self.dateend)
                    except FileNameException:
                        deterministic = self.subdir == "mb035"
                        rdict['rc'] = S2MExecutionError("missing forcing file in directory " + forcingdir,
                                                        deterministic, self.subdir, datebegin_this_run,
                                                        self.dateend)
                        return rdict
                    print("FORCING FOUND")

                    if "flat" in self.geometry_in[0] and "allslopes" in self.geometry_out:
                        print("FORCING EXTENSION")
                        liste_massifs = infomassifs().dicArea[self.geometry_in[0]]
                        liste_aspect = infomassifs().get_list_aspect(8, ["0", "20", "40"])
                        self.mv_if_exists("FORCING.nc", "FORCING_OLD.nc")
                        forcinput_select('FORCING_OLD.nc', 'FORCING.nc', liste_massifs, 0, 5000,
                                         ["0", "20", "40"], liste_aspect, **self.reprod_info)
                        need_save_forcing = True

            if self.daily:
                dateend_this_run = min(tomorrow(base=datebegin_this_run), min(self.dateend, dateforcend))
                need_other_forcing = False
            else:
                dateend_this_run = min(self.dateend, dateforcend)

            if not namelist_ready:
                if sytron:
                    self.copy_if_exists(self.system.path.join(rundir, "OPTIONS_sytron.nam"), "OPTIONS.nam")

                available_namelists = self.find_namelists()
                if len(available_namelists) > 1:
                    print("WARNING SEVERAL NAMELISTS AVAILABLE !!!")
                for namelist in available_namelists:
                    # Update the contents of the namelist (date and location)
                    # Location taken in the FORCING file.
                    print("MODIFY THE NAMELIST:" + namelist.container.basename)
                    newcontent = update_surfex_namelist_object(
                        namelist.contents,
                        datebegin_this_run,
                        dateend=dateend_this_run,
                        updateloc=updateloc,
                        physicaloptions=self.physical_options,
                        snowparameters=self.snow_parameters)
                    newnam = footprints.proxy.container(filename=namelist.container.basename)
                    newcontent.rewrite(newnam)
                    newnam.close()
                if self.daily:
                    updateloc = True

            if changenamelistdaily:
                # Change the namelist
                self.link_in(self.dailynamelist.pop(0), "OPTIONS.nam")

            # Run surfex offline
            list_name = self.system.path.join(thisdir, 'offline.out')

            try:
                self.local_spawn(list_name)
                # Uncomment these lines to test the behaviour in case of failure of 1 member
                # if self.subdir == "mb006": # To test warnings
                # if self.subdir == "mb035": # To test errors
                #     deterministic = self.subdir == "mb035"
                #     # print("DEBUGINFO")
                #     # print(dir(self))
                #     rdict['rc'] = S2MExecutionError(self.progname, deterministic,
                #                                     self.subdir, datebegin_this_run, dateend_this_run)
                #     return rdict

            except ExecutionError:
                deterministic = self.subdir == "mb035"
                rdict['rc'] = S2MExecutionError(self.progname, deterministic, self.subdir,
                                                datebegin_this_run, dateend_this_run)
                return rdict  # Note than in the other case return rdict is at the end

            # Copy the SURFOUT file for next iteration
            self.system.cp("SURFOUT.nc", "PREP.nc")

            # Post-process
            pro = massif_simu("ISBA_PROGNOSTIC.OUT.nc", openmode='a')
            pro.massif_natural_risk()
            pro.dataset.GlobalAttributes(**self.reprod_info)
            pro.dataset.add_standard_names()
            pro.close()

            # Rename outputs with the dates
            save_file_date(".", "SURFOUT", dateend_this_run, newprefix="PREP")
            save_file_period(".", "ISBA_PROGNOSTIC.OUT", datebegin_this_run, dateend_this_run,
                             newprefix="PRO")

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_run = dateend_this_run < self.dateend

            print(dateend_this_run, self.dateend)
            print("INFO SAVE FORCING", need_save_forcing, need_other_run, need_other_forcing)

            if need_save_forcing and not (need_other_run and not need_other_forcing):
                save_file_period(".", "FORCING", dateforcbegin, dateforcend)
            # Remove the symbolic link for next iteration (not needed since now we rename the forcing just before
#             self.system.remove("FORCING.nc")

        return rdict


@echecker.disabled_if_unavailable
class PrepareForcingWorker(TaylorVortexWorker):
    """This algo component is designed to prepare a SURFEX Forcing file (change of geometry)."""

    _footprint = dict(
        info = 'AlgoComponent designed to run a SURFEX experiment without MPI parallelization.',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            kind = dict(
                values = ['prepareforcing'],
            ),
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = False
            ),
            geometry_in = dict(
                info = "Area information in case of an execution on a massif geometry",
                type = footprints.stdtypes.FPList,
                optional = True,
                default = None
            ),
            geometry_out = dict(
                info = "The resource's massif geometry.",
                type = str,
            ),
            reprod_info = dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
        )
    )

    def vortex_task(self, **kwargs):
        rdict = dict(rc=True)
        rundir = self.system.getcwd()
        if self.subdir is not self.system.path.dirname(rundir):
            thisdir = self.system.path.join(rundir, self.subdir)
            with self.system.cdcontext(self.subdir, create=True):
                rdict = self._commons(rundir, thisdir, rdict, **kwargs)

        else:
            thisdir = rundir
            rdict = self._commons(rundir, thisdir, rdict, **kwargs)

        return rdict

    def _commons(self, rundir, thisdir, rdict, **kwargs):

        rdict = self._prepare_forcing_task(rundir, thisdir, rdict)
        self.postfix()

        return rdict

    def forcingdir(self, rundir, thisdir):
        return rundir

    def _prepare_forcing_task(self, rundir, thisdir, rdict):

        need_other_run = True
        need_other_forcing = True
        need_save_forcing = False
        datebegin_this_run = self.datebegin

        while need_other_run:

            if need_other_forcing:

                forcingdir = self.forcingdir(rundir, thisdir)

                if len(self.geometry_in) > 1:
                    print("FORCING AGGREGATION")
                    forcinglist = []
                    for massif in self.geometry_in:
                        dateforcbegin, dateforcend = get_file_period("FORCING", forcingdir + "/" + massif,
                                                                     datebegin_this_run, self.dateend)
                        forcingname = "FORCING_" + massif + ".nc"
                        self.system.mv("FORCING.nc", forcingname)
                        forcinglist.append(forcingname)

                    forcinput_applymask(forcinglist, "FORCING.nc", **self.reprod_info)
                    need_save_forcing = True
                else:
                    # Get the first file covering part of the whole simulation period
                    print("LOOK FOR FORCING")
                    dateforcbegin, dateforcend = get_file_period("FORCING", forcingdir,
                                                                 datebegin_this_run, self.dateend)
                    print("FORCING FOUND")

                    print("flat" in self.geometry_in[0])
                    print("allslopes" in self.geometry_out)

                    print(self.geometry_in[0], self.geometry_out)

                    if "flat" in self.geometry_in[0] and "allslopes" in self.geometry_out:

                        list_slopes = ["0", "20", "40"]

                        print("FORCING EXTENSION")
                        liste_massifs = infomassifs().dicArea[self.geometry_in[0]]
                        liste_aspect = infomassifs().get_list_aspect(8, list_slopes)
                        self.system.mv("FORCING.nc", "FORCING_OLD.nc")
                        forcinput_select('FORCING_OLD.nc', 'FORCING.nc', liste_massifs,
                                         0, 5000, list_slopes, liste_aspect, **self.reprod_info)
                        need_save_forcing = True

            dateend_this_run = min(self.dateend, dateforcend)

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_run = dateend_this_run < self.dateend

            if need_save_forcing and not (need_other_run and not need_other_forcing):
                save_file_period(forcingdir, "FORCING", dateforcbegin, dateforcend)

        return rdict

    def postfix(self):
        self.system.subtitle('{:s} : directory listing (post-run)'.format(self.kind))
        for line in self.system.dir():
            print(line)


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


class _CENParaBlindRun(ParaBlindRun):
    """
    This abstract algo component defines common methods for all CEN ensemble simulations
    with a parallelisation over ensemble members and no MPI parallelization.
    Algo components deriving from ParaBlindRun (and thus from this class) are associated to an executable.
    """

    _abstract = True
    _footprint = dict(
        info = 'AlgoComponent that runs several executions of a CEN model in parallel.',
        attr = dict(
            engine = dict(
                # TODO : modification majeure, à discuter avec Matthieu
                # Engine should stay 'blind' to indicate that the core of the algo component is
                # an executable (thus the Vortex object is 'blind' to the execution)
                # This footprint should be used to chose between *_CENParaBlindRun* algo components
                # and *_CENTaylorRun* algo components
                values   = ['blind', 's2m']  # s2m for backward compatibility
            ),
            metadata = dict(
                values   = ['StandardSAFRAN', 'StandardPROSNOW'],
                optional = True,
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
        for attribute in self.footprint_attributes:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def postfix(self, rh, opts):
        pass


class S2MComponent(_CENParaBlindRun):
    """
    This Algo Component is designed to manage any S2M task without MPI
    parallelization (deterministic or ensemble-like simulations).

    Ensemble-like simulations include real ensemble simulations (many executions
    of the same simulation with different initial conditions or configurations)
    and multi-year simulations (associating each year to one member) that can run
    in parallel.

    The different members of an ensemble simulation are identified by an input
    resource that differ between the members (defined by the method
    **role_ref_namebuilder** that can be overloaded). For each identified
    member, a worker object (whose class inherits from :class:`_CENWorkerBlindRun`)
    is generated and the different workers run in parallel.

    The :class:`S2MComponent` class (that relies on the :mod:`taylorism`
    package):

    * allocates the different executions of the same binary to workers
    * analyses their feedbacks to look for execution errors
      (thanks to the inherited :meth:`~ParaBlindRun._default_post_execute`
      method): should the `rc` entry of the dictionary returned by the
      worker's :meth:`_CENWorkerBlindRun.vortex_task` method be an Exception, it is
      captured and stored.

    When the execution of all members finishes, the captured exceptions (see
    above) are wrapped in a
    :class:`~vortex.algo.components.DelayedAlgoComponentError` exception that
    is ultimately raised.

    Consequently, we can rely on that to:

    * filter/ignore some errors,
    * send notifications...

    For CEN R&D needs, this can be easily implemented in the Task classes
    provided they inherit from the  :class:`cen.layout.nodes.S2MTaskMixIn`
    mixin. For example:

    * To filter execution errors, just define
      ``filter_execution_error`` in your Task class:
      ``filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error``

    * To send notification emails about fatal and non-fatal errors:
      ``report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning``
      and
      ``report_execution_errors = S2MTaskMixIn.s2moper_report_execution_errors``

    """

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            kind = dict(
                values   = ['safrane', 'syrpluie', 'syrmrr', 'sytist', 'sypluie', 'syvapr',
                            'syvafi', 'intercep'],
            ),
            datebegin = a_date,
            dateend   = a_date,
            execution = dict(
                values   = ['analysis', 'forecast'],
                optional = True,
            ),
            metadata = dict(
                values   = ['StandardSAFRAN', 'StandardPROSNOW'],
                optional = True,
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        cpl_model = self.get_origin(rh, opts)
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs, deterministic=cpl_model))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """Get the different member's subdirectories.

        One member is associated to each 'effective input' (inputs that where
        actually retrieved during the fetch step) Section with a role matching
        the one defined by the **role_ref_namebuilder** method.
        """
        deterministic_member = self.context.sequence.effective_inputs(role=self.role_deterministic_namebuilder())
        # Produce a delayed algo component error if no deterministic member in order to let the members run
        # but crash at the end
        if len(deterministic_member) < 1 and self.kind != 'ensmeteonodet':
            self.delayed_exception_add(S2MMissingDeterministicError(self.role_deterministic_namebuilder()),
                                       traceback=True)
        avail_members = deterministic_member +\
            self.context.sequence.effective_inputs(role=self.role_members_namebuilder())

        subdirs = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)

        # Add a sytron member (only for child SurfexComponent but done here to test availability of deterministic mb)
        if self.kind == "ensmeteo+sytron" and len(deterministic_member) == 1:
            subdirs.append('mb036')

        # Ca partait d'une bonne idee mais en pratique il y a plein de cas particuliers
        # pour lesquels ca pose probleme : reanalyse safran, surfex postes, etc
        # self.algoassert(len(set(subdirs)) == len(set([am.rh.provider.member for am in avail_members])))

        # Crash if subdirs is an empty list (it means that there is not any input available)
        # TODO : modifier en python3
        self.algoassert(len(subdirs) >= 1)
        return subdirs

    def get_origin(self, rh, opts):
        """Get the status (essential or secondary) of the different members.

        Ultimately:

        * A failure on an essential member automatically leads to the
          crash of the whole task (for example the deterministic member of
          the PEARP-S2M operational chain)
        * A failure on a secondary member is not automatically fatal
          for the task (it depends on the number of secondary members that
          failed)

        Each member is associated to a specific input section (see method
        :meth:`get_subdirs`) which is also a VORTEX Section object. This method
        uses the optional 'source_conf' footprint attribute of this object to
        descriminate the essential member(s) (with source_conf='4dvarfr'
        whereas for standard PEARP members, source_conf='pearp'
        in the case of the PEARP-S2M operational chain).

        This method returns a list of boolean (with an order matching the one of
        the different members) where a `True` value indicates the essential(s)
        member(s).

        In case there is no 'source_conf' attribute, all members are considered
        as essential members.
        """
        deterministic_member = self.context.sequence.effective_inputs(role=self.role_deterministic_namebuilder())
        avail_members = deterministic_member +\
            self.context.sequence.effective_inputs(role=self.role_members_namebuilder())
        subdirs = list()
        cpl_model = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)
                if hasattr(am.rh.resource, 'source_conf'):
                    cpl_model.append(am.rh.resource.source_conf == '4dvarfr')
                else:
                    # If the origin of the guess is not given the execution is in
                    # 'deterministic' mode (monthly reanalysis)
                    cpl_model.append(True)

        return cpl_model

    def role_deterministic_namebuilder(self):
        """
        Defines the role of the effective inputs to take as reference to define
        the deterministic member.
        """
        return 'Ebauche_Deterministic'

    def role_members_namebuilder(self):
        """
        Defines the role of the effective inputs to take as reference to define
        the different members.
        """
        return 'Ebauche'


class S2MReanalysis(S2MComponent):
    """AlgoComponent that runs several SAFRAN reanalyses in parallel."""

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            execution = dict(
                values   = ['reanalysis'],
                optional = False,
            ),
        ),
    )

    def role_ref_namebuilder(self):
        return 'Observations'

    def get_subdirs(self, rh, opts):
        avail_members = self.context.sequence.effective_inputs(role=self.role_ref_namebuilder())
        subdirs = [am.rh.container.dirname for am in avail_members]
        return list(set(subdirs))

    def get_list_seasons(self, rh, opts):
        list_dates_begin_input = list()
        list_dates_end_input = list()

        datebegin_input = self.datebegin
        if self.datebegin.month >= 8:
            dateend_input = min(Date(self.datebegin.year + 1, 8, 1, 6, 0, 0), self.dateend)
        else:
            dateend_input = min(Date(self.datebegin.year, 8, 1, 6, 0, 0), self.dateend)

        list_dates_begin_input.append(datebegin_input)
        list_dates_end_input.append(dateend_input)

        while dateend_input < self.dateend:
            datebegin_input = dateend_input
            dateend_input = min(datebegin_input.replace(year=datebegin_input.year + 1), self.dateend)
            list_dates_begin_input.append(datebegin_input)
            list_dates_end_input.append(dateend_input)

        list_dates_begin_input.sort()
        list_dates_end_input.sort()

        return list_dates_begin_input, list_dates_end_input

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super(S2MComponent, self)._default_common_instructions(rh, opts)

        for attribute in self.footprint_attributes:
            if attribute not in ['datebegin', 'dateend']:
                ddict[attribute] = getattr(self, attribute)

        return ddict

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        # Note: The number of members and the name of the subdirectories could be
        # auto-detected using the sequence
        subdirs = self.get_subdirs(rh, opts)
        deterministic = [True] * len(subdirs)
        # WARNING : The current method implies that the different seasons directories are sorted
        # One way to ensure that is to use the begin year as directory name.
        subdirs.sort()
        # list_dates_begin, list_dates_end = self.get_list_seasons(rh, opts)
        # subdirs must be named yyyymmddhh_YYYYMMDDHH
        list_dates_begin = [item.split('_')[0] for item in subdirs]
        list_dates_end = [item.split('_')[1] for item in subdirs]
        self._add_instructions(common_i, dict(subdir=subdirs,
                                              datebegin=list_dates_begin,
                                              dateend=list_dates_end,
                                              deterministic=deterministic))
        self._default_post_execute(rh, opts)


class S2MReforecast(S2MComponent):
    """AlgoComponent that runs several SAFRAN reforecasts in parallel."""

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            execution = dict(
                values   = ['reforecast'],
                optional = False,
            ),
            datebegin = dict(
                optional = True,
            ),
            dateend   = dict(
                optional = True,
            ),
        ),
    )

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super(S2MComponent, self)._default_common_instructions(rh, opts)

        for attribute in self.footprint_attributes:
            if attribute not in ['datebegin', 'dateend']:
                ddict[attribute] = getattr(self, attribute)

        return ddict

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        subdirs, list_dates_begin, list_dates_end = self.get_individual_instructions(rh, opts)
        deterministic = [True] * len(subdirs)  # Each simulation day is important
        self._add_instructions(common_i, dict(subdir=subdirs,
                                              datebegin=list_dates_begin,
                                              dateend=list_dates_end,
                                              deterministic=deterministic))
        self._default_post_execute(rh, opts)

    def get_individual_instructions(self, rh, opts):
        avail_members = self.context.sequence.effective_inputs(role=self.role_members_namebuilder())
        subdirs = list()
        list_dates_begin = list()
        list_dates_end = list()
        for am in avail_members:
            # Guess files are now stored in a tar archive
            if self.system.is_tarfile(am.rh.container.basename):
                for fic in self.system.untar(am.rh.container.basename):
                    # fic = YYYYMMDDHH/mbXXX/PYYMMDDhh
                    dirname = self.system.path.dirname(fic)  # YYYYMMDDHH/mbXXX
                    if dirname not in subdirs:
                        subdirs.append(dirname)
                        rundate = Date(fic.split('/')[0])  # YYYYMMDDHH
                        dt = rundate.hour - 6 if rundate.hour in [6, 18] else 6
                        list_dates_begin.append(rundate + Period(hours=dt))
                        list_dates_end.append(rundate + Period(hours=dt) + Period(days=4))
            elif am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)
                # WARNING : The first ech in the corresponding footprint must correspond to 6:00 at day D
                list_dates_begin.append(am.rh.resource.date + Period(hours=am.rh.resource.cumul.hour))
                # The last ech in the corresponding footprint must correspond to 6:00 at day D+4
                # WARNING : There is no check that this resource is effectively here...
                list_dates_end.append(am.rh.resource.date + Period(hours=am.rh.resource.cumul.hour) + Period(days=4))

        return subdirs, list_dates_begin, list_dates_end


@echecker.disabled_if_unavailable
class SurfexComponent(S2MComponent):
    """AlgoComponent that runs several SURFEX executions in parallel."""

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            kind = dict(
                values = ['escroc', 'ensmeteo', 'ensmeteonodet', 'ensmeteo+sytron', 'croco', 'ensmeteo+escroc',
                          'prepareforcing']
            ),
            dateinit = dict(
                info = "The initialization date if different from the starting date.",
                type = Date,
                optional = True,
                default = '[datebegin]',
            ),
            threshold = dict(
                info = "The initialization date if different from the starting date.",
                type = int,
                optional = True,
                default = -999
            ),
            members = dict(
                info = "The members that will be processed",
                type = footprints.stdtypes.FPList,
                optional = True,
            ),
            subensemble = dict(
                info = "Name of the escroc subensemble (define which physical options are used)",
                values = ["E1", "E2", "Crocus", "E1tartes", "E1notartes", "E2open", "E2MIP", "E2tartes", "E2MIPtartes",
                          "E2B21", "E2MIPB21", "E1B21"],
                optional = True,
            ),
            geometry_in=dict(
                info="Area information in case of an execution on a massif geometry",
                type=footprints.stdtypes.FPList,
            ),
            geometry_out=dict(
                info="The resource's massif geometry.",
                type=str,
            ),
            daily = dict(
                info = "If True, split simulations in daily runs",
                type = bool,
                optional = True,
                default = False,
            ),
            dailynamelist = dict(
                info = "If daily is True, possibility to provide a list of namelists to change each day",
                type = list,
                optional = True,
                default = [],
            ),
            multidates = dict(
                info = "If True, several dates allowed",
                type = bool,
                optional = True,
                default = False,
                values = [False]
            ),
            startmbnode = dict(
                info = 'first member rep of the node for example 1,41,81 etc.',
                type = int,
                optional = True,
                default = 1,
            ),
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)

        subdirs = self.get_subdirs(rh, opts)

        if self.subensemble:
            escroc = ESCROC_subensembles(self.subensemble, self.members)
            physical_options = escroc.physical_options
            snow_parameters = escroc.snow_parameters
            self._add_instructions(common_i, dict(subdir=subdirs,
                                                  physical_options=physical_options,
                                                  snow_parameters=snow_parameters))
        else:
            self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        if self.kind == "escroc":
            subdirs = ['mb{:04d}'.format(m) for m in self.members]
        elif self.kind == 'croco':
            subdirs = ['mb{:04d}'.format(m) for m in range(self.startmbnode, self.startmbnode + len(self.members))]
        else:
            subdirs = super().get_subdirs(rh, opts)

            if len(self.geometry_in) > 1:
                # In the case of a postes geometry, there are 3 effective inputs with forcing file role
                # (They are concatenated)
                # Therefore it is necessary to reduce subdirs to 1 single element for each member
                subdirs = list(set(map(self.system.path.dirname, subdirs)))

        return subdirs

    def role_members_namebuilder(self):
        return 'Forcing'

    def role_deterministic_namebuilder(self):
        return 'Forcing_Deterministic'


@echecker.disabled_if_unavailable
class _CENTaylorRun(TaylorRun):
    """
    TaylorRun derived algo components are not (necessarily) associated to an executable and can simply launch
    a piece of python code.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            engine = dict(
                info     = 'The way the executable should be run.',
                values   = ['algo', ],
                default  = 'algo',
            ),
            # TODO : find a more explicit name for role_member ?
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                type     = str,
            ),
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """
        Get the different member's subdirectories.

        One member is associated to each 'effective input' (inputs that where
        actually retrieved during the fetch step) Section with a role matching
        the one defined by the **role_members** footprint.

        WARNING : the use of a footprint attribute instead of a class method to define
        the role to use for members identification is a significant difference with other
        "_CENTaylorRun" derived algo components.
        """
        avail_members = self.context.sequence.effective_inputs(role=self.role_members)

        if len(avail_members) > 0:
            subdirs = list()
            # Retrive the subdirectory asociated to each identified RH
            for am in avail_members:
                if am.rh.container.dirname not in subdirs:
                    subdirs.append(am.rh.container.dirname)
        else:
            subdirs = ['.']

        return subdirs

    def prepare(self, rh, opts):
        """Set some variables according to target definition."""
        super().prepare(rh, opts)
        self.env.DR_HOOK_NOT_MPI = 1

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def postfix(self, rh, opts):
        pass


@echecker.disabled_if_unavailable
class PrepareForcingComponent(_CENTaylorRun):
    """
    AlgoComponent that prepares several forcing files in parallel (changes of geometry).
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            kind = dict(
                values = ['prepareforcing', 'extractforcing', 'shadowsforcing']
            ),
            geometry_in = dict(
                info = "Area information in case of an execution on a massif geometry",
                type = footprints.stdtypes.FPList,
                optional = True,
                default = None
            ),
            geometry_out = dict(
                info = "The resource's massif geometry.",
                type = str,
                optional = True,
                default = None
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        # Note: The number of members and the name of the subdirectories could be
        # auto-detected using the sequence
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs,
                                              datebegin=self.datebegin,
                                              dateend=self.dateend))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):

        return [begin.year for begin in self.datebegin]


@echecker.disabled_if_unavailable
class SurfexComponentMultiDates(SurfexComponent):
    """AlgoComponent that runs several SURFEX in parallel (including several dates for reforecasts)."""
    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            multidates = dict(
                info = "If True, several dates allowed",
                type = bool,
                values = [True]
            )
        )
    )

    def get_dates(self, subdirs):

        # Pour l'instant je fais le porc parce que le passage de dateend ne marche pas du tout
        duration = Period(days=4)

        listdatebegin = []
        listdateend = []
        for datebegin_str in subdirs:
            datebegin = Date(datebegin_str.split('/')[0])
            listdatebegin.append(datebegin)
            listdateend.append(datebegin + duration)

        return listdatebegin, listdateend

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions

        common_i = self._default_common_instructions(rh, opts)

        subdirs = self.get_subdirs(rh, opts)
        listdatebegin, listdateend = self.get_dates(subdirs)
        listdateinit = listdatebegin[:]

        if self.subensemble:
            escroc = ESCROC_subensembles(self.subensemble, self.members)
            physical_options = escroc.physical_options
            snow_parameters = escroc.snow_parameters
            self._add_instructions(common_i, dict(subdir=subdirs,
                                                  datebegin=listdatebegin,
                                                  dateinit=listdateinit,
                                                  dateend=listdateend,
                                                  physical_options=physical_options,
                                                  snow_parameters=snow_parameters))
        else:
            self._add_instructions(common_i, dict(subdir=subdirs,
                                                  datebegin=listdatebegin,
                                                  dateinit=listdateinit,
                                                  dateend=listdateend))
        self._default_post_execute(rh, opts)

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        ddict.pop('datebegin')
        ddict.pop('dateend')
        ddict.pop('dateinit')

        return ddict


@echecker.disabled_if_unavailable
class PrepareForcingComponentForecast(PrepareForcingComponent):
    """
    It adapts forcing files to a ski resort geometry (several members in parallel).

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel',
        attr = dict(
            kind = dict(
                values = ['extractforcing_STforecast', 'extractforcing_LTforecast']
            )
        )
    )

    def _default_common_instructions(self, rh, opts):

        ddict = super(PrepareForcingComponent, self)._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            if attribute in ['datebegin', 'dateend']:
                ddict[attribute] = getattr(self, attribute)[0][0]
            else:
                ddict[attribute] = getattr(self, attribute)

        return ddict

    def execute(self, rh, opts):

        self._default_pre_execute(rh, opts)
        common_i = self._default_common_instructions(rh, opts)
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):

        avail_members = self.context.sequence.effective_inputs(role=self.role_ref_namebuilder())
        subdirs = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)

        return subdirs

    def role_ref_namebuilder(self):
        return 'Forcing'


@echecker.disabled_if_unavailable
class ExtractForcingWorker(PrepareForcingWorker):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member).

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - deterministic case',
        attr = dict(
            kind = dict(
                values = ['extractforcing']
            ),
        )
    )

    def _prepare_forcing_innertask(self, rundir, thisdir, dir_file_1, rdict):
        return super()._prepare_forcing_task(rundir, thisdir, rdict)

    def _prepare_forcing_task(self, rundir, thisdir, rdict):
        datebegin_str = self.datebegin.strftime('%Y%m%d%H')
        dateend_str = self.dateend.strftime('%Y%m%d%H')

        dir_file_1 = self.forcingdir(rundir, thisdir) + '/FORCING_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_2 = self.forcingdir(rundir, thisdir) + '/FORCING_out_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_3 = self.forcingdir(rundir, thisdir) + '/FORCING_in_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_4 = rundir + '/SRU.txt'

        rdict = self._prepare_forcing_innertask(rundir, thisdir, dir_file_1, rdict)

        # Extraction of SRU geometry
        forcinput_extract(dir_file_1, dir_file_2, dir_file_4)
        self.system.mv(dir_file_1, dir_file_3)
        self.system.mv(dir_file_2, dir_file_1)

        # ------------------- #

        return rdict


@echecker.disabled_if_unavailable
class ExtractForcingWorkerEnsembleForecast(ExtractForcingWorker):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member)
    with specific adaptations for short term forecast

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - ensemble forecast',
        attr = dict(
            kind = dict(
                values = ['extractforcing_STforecast']
            )
        )
    )

    def forcingdir(self, rundir, thisdir):
        return thisdir


@echecker.disabled_if_unavailable
class ExtractForcingWorkerLTForecast(ExtractForcingWorkerEnsembleForecast):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member)
    with specific adaptations for seasonal forecasts

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - LT forecast',
        attr = dict(
            kind = dict(
                values = ['extractforcing_LTforecast']
            ),
        )
    )

    def _prepare_forcing_innertask(self, rundir, thisdir, dir_file_1, rdict):
        # Change dates of the climatology to the current season
        forcinput_changedates(dir_file_1, dir_file_1, self.datebegin.nivologyseason_begin)
        return rdict


@echecker.disabled_if_unavailable
class ShadowsForcingWorker(PrepareForcingWorker):
    """
    It only applies shadows to a forcing file without any change of geometry
    (worker for 1 member).
    """

    _footprint = dict(
        info = 'Apply shadows',
        attr = dict(
            kind = dict(
                values = ['shadowsforcing']
            ),
            geometry_out = dict(
                info="The resource's massif geometry.",
                type=str,
                optional = True,
                default = None
            )
        )
    )

    def _prepare_forcing_task(self, rundir, thisdir, rdict):

        need_other_forcing = True
        datebegin_this_run = self.datebegin

        while need_other_forcing:

            forcingdir = self.forcingdir(rundir, thisdir)

            # Get the first file covering part of the whole simulation period
            dateforcbegin, dateforcend = get_file_period("FORCING", forcingdir,
                                                         datebegin_this_run, self.dateend)

            self.system.mv("FORCING.nc", "FORCING_OLD.nc")
            forcinput_applymask(["FORCING_OLD.nc"], "FORCING.nc")

            dateend_this_run = min(self.dateend, dateforcend)

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_forcing = dateend_this_run < self.dateend

            save_file_period(forcingdir, "FORCING", dateforcbegin, dateforcend)

        return rdict
