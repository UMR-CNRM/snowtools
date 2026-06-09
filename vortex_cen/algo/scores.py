"""
Algo components for ESCROC scores.
"""

from bronx.compat import random
from bronx.fancies import loggers
from bronx.stdtypes.date import Date
from bronx.syntax.externalcode import ExternalCodeImportChecker
import footprints

from vortex.algo.components import TaylorRun
from vortex.tools.parallelism import TaylorVortexWorker

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.scores.list_scores import ESCROC_list_scores, scores_file, ensemble_scores_file
    from snowtools.scores.ensemble import ESCROC_EnsembleScores


@echecker.disabled_if_unavailable
class Escroc_Score_Member(TaylorVortexWorker):
    """
    AlgoComponent worker designed to run one member of SURFEX-Crocus experiment
    without MPI parallelization.
    """

    _footprint = dict(
        info = 'AlgoComponent worker designed to run one member of SURFEX-Crocus experiment '
               'without MPI parallelization.',
        attr = dict(
            kind = dict(
                values = ['scores_escroc'],
            ),

            datebegin   = dict(
                info = "The first date of the simulation.",
                type = Date,
                optional = False
            ),

            dateend = dict(
                info = "The final date of the simulation.",
                type = Date,
                optional = False
            ),

            list_scores = dict(
                info = "List of scores to compute",
                type = list,
                optional = False
            ),

            list_var = dict(
                info = "List of variables for which we want to compute the scores",
                type = list,
                optional = False
            ),

            members = dict(
                info = "The members that will be processed",
                type = footprints.FPList,
                optional = False
            )
        )
    )

    def vortex_task(self, **kwargs):

        rdict = dict(rc=True)

        list_pro = ["PRO_" + self.datebegin.ymdh + "_" + self.dateend.ymdh +
                    '_mb{:04d}'.format(member) + ".nc" for member in self.members]
        print(list_pro)
        E = ESCROC_list_scores()
        rdict["scores"] = E.compute_scores_allmembers(list_pro, "obs_insitu.nc",
                                                      self.list_scores, self.list_var)
        rdict["members"] = self.members  # because in the report the members can be in a different order

        return rdict

    def set_env(self, rundir):
        inputs = [x.rh for x in self.context.sequence.effective_inputs()]
        print('DBUG')
        print(self.context.sequence.effective_inputs())
        print(dir(self.context.sequence.effective_inputs()))
        print(inputs)


@echecker.disabled_if_unavailable
class Escroc_Score_Ensemble(TaylorRun):
    """AlgoComponent that compute ESCROC scores for the full ensemble."""

    _footprint = dict(
        info = 'AlgoComponent that compute ESCROC scores for the full ensemble',
        attr = dict(
            engine = dict(
                values = ['blind']
            ),

            kind = dict(
                values = ['scores_escroc'],
            ),

            members = dict(
                info = "The members that will be processed",
                type = footprints.FPList,
                optional = False
            ),
            datebegin = dict(
                info = "The first date of the simulation.",
                type = Date,
                optional = False
            ),

            dateend = dict(
                info = "The final date of the simulation.",
                type = Date,
                optional = False
            ),

            list_scores = dict(
                info = "List of scores to compute",
                type = list,
                optional = False
            ),

            list_var = dict(
                info = "List of variables for which we want to compute the scores",
                type = list,
                optional = False
            ),

        )
    )

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in ["datebegin", "dateend", "list_var", "list_scores"]:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def _default_pre_execute(self, rh, opts):
        super()._default_pre_execute(rh, opts)
        self.local_members = self.split_members_by_task()

    def _default_post_execute(self, rh, opts):
        super()._default_post_execute(rh, opts)
        import numpy as np
        report = self._boss.get_report()

        scores_all = np.empty((len(self.list_scores), len(self.members), len(self.list_var), 1), float)

        for task in range(0, self.ntasks):
            scores_task = report["workers_report"][task]["report"]["scores"]
#             members_task = np.array(self.local_members[task]) - self.local_members[0][0]
            members_task = np.array(report["workers_report"][task]["report"]["members"]) - 1
            print("DEBUG")
            print(members_task)
            print(scores_task[:, :, :].shape)
            print(scores_all[:, members_task, :, :].shape)
            scores_all[:, members_task, :, :] = scores_task[:, :, :, np.newaxis]

        scores_dataset = scores_file("scores.nc", "w")
        for s, score in enumerate(self.list_scores):
            scores_dataset.write(score, scores_all[s, :, :])

        scores_dataset.close()

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)

        print("local members")
        print(self.local_members[:])

        self._add_instructions(common_i, dict(members=self.local_members))
        self._default_post_execute(rh, opts)

    def split_members_by_task(self):
        nmembers = len(self.members)

        # Numbers of members by task
        nmembers_by_task_min = nmembers / self.ntasks
        nmembers_by_task_max = nmembers_by_task_min + 1

        # Numbers of tasks running with the maximum value
        ntasks_with_max = nmembers % self.ntasks

        local_members = []
        firstmember = 1

        for task in range(0, self.ntasks):
            if task < ntasks_with_max:
                nmembers_by_task = nmembers_by_task_max
            else:
                nmembers_by_task = nmembers_by_task_min

            lastmember = firstmember + nmembers_by_task - 1
            local_members.append(range(firstmember, min(lastmember, nmembers) + 1))
            firstmember = lastmember + 1

        return local_members


@echecker.disabled_if_unavailable
class Escroc_Score_Subensemble(TaylorVortexWorker):
    """AlgoComponent worker designed to compute ensemble scores for a given subensemble."""

    _footprint = dict(
        info = 'AlgoComponent worker designed to compute ensemble scores for a given subensemble.',
        attr = dict(
            kind = dict(
                values = ['optim_escroc'],
            ),

            datebegin   = dict(
                info = "The first date of the simulation.",
                type = Date,
                optional = False
            ),

            dateend = dict(
                info = "The final date of the simulation.",
                type = Date,
                optional = False
            ),

            list_scores = dict(
                info = "List of scores to compute",
                type = list,
                optional = False
            ),

            list_var = dict(
                info = "List of variables for which we want to compute the scores",
                type = list,
                optional = False
            ),

            members = dict(
                info = "The members that will be processed",
                type = footprints.FPList,
                optional = False
            )
        )
    )

    def vortex_task(self, **kwargs):

        rdict = dict(rc=True)

        list_pro = ["PRO_" + self.datebegin.ymdh + "_" + self.dateend.ymdh +
                    '_mb{:04d}'.format(member) + ".nc" for member in self.members]
        print(list_pro)
        for var in self.list_var:
            E = ESCROC_EnsembleScores(list_pro, "obs_insitu.nc", var)
            crps = E.CRPS()
            dispersion, rmse, ss = E.dispersionEnsemble()
        rdict["scores"] = [crps, dispersion, rmse, ss]
        rdict["members"] = self.members  # because in the report the members can be in a different order

        return rdict

    def set_env(self, rundir):
        inputs = [x.rh for x in self.context.sequence.effective_inputs()]
        print('DBUG')
        print(self.context.sequence.effective_inputs())
        print(dir(self.context.sequence.effective_inputs()))
        print(inputs)


@echecker.disabled_if_unavailable
class Escroc_Optim_Ensemble(TaylorRun):
    """AlgoComponent that compute ESCROC scores for the full ensemble."""

    _footprint = dict(
        info = 'AlgoComponent that compute ESCROC scores for the full ensemble',
        attr = dict(
            engine = dict(
                values = ['blind']
            ),

            kind = dict(
                values = ['optim_escroc'],
            ),

            members = dict(
                info = "The members that will be processed",
                type = footprints.FPList,
                optional = False
            ),
            datebegin = dict(
                info = "The first date of the simulation.",
                type = Date,
                optional = False
            ),

            dateend = dict(
                info = "The final date of the simulation.",
                type = Date,
                optional = False
            ),

            list_scores = dict(
                info = "List of scores to compute",
                type = list,
                optional = False
            ),

            list_var = dict(
                info = "List of variables for which we want to compute the scores",
                type = list,
                optional = False
            ),

            niter = dict(
                info = "Number of iterations",
                type = int,
                optional = True,
                default = 1000
            ),
        )
    )

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in ["datebegin", "dateend", "list_var", "list_scores"]:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def _default_pre_execute(self, rh, opts):
        super()._default_pre_execute(rh, opts)
        if len(self.members) < 35:
            nmembers = len(self.members)
        else:
            nmembers = 35
        self.local_members = self.select_random_members(nmembers=nmembers, niter=self.niter)

    def _default_post_execute(self, rh, opts):
        super()._default_post_execute(rh, opts)
        import numpy as np
        report = self._boss.get_report()

        ntasks = len(report["workers_report"])
        crps = np.empty(ntasks, float)
        dispersion = np.empty(ntasks, float)
        rmse = np.empty(ntasks, float)
        ss = np.empty(ntasks, float)
        members = np.empty((ntasks, 35), int)

        for task in range(0, ntasks):
            crps[task], dispersion[task], rmse[task], ss[task] = report["workers_report"][task]["report"]["scores"]
            members[task, :] = np.array(report["workers_report"][task]["report"]["members"])

        scores_dataset = ensemble_scores_file("scores.nc", "w")
        scores_dataset.write_members(members)
        scores_dataset.write("crps", crps)
        scores_dataset.write("dispersion", dispersion)
        scores_dataset.write("rmse", rmse)
        scores_dataset.write("ss", ss)

        scores_dataset.close()

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)

        print("local members")
        print(self.local_members[:])

        self._add_instructions(common_i, dict(members=self.local_members))
        self._default_post_execute(rh, opts)

    def select_random_members(self, nmembers=35, niter=1000):

        if niter == 1 and len(self.members) == nmembers:
            return [self.members[:]]
        else:
            # Initialization
            # We want that all sites are tested with the same subensembles,
            # this is why we fix the argument of random.seed()
            rgen = random.Random()
            rgen.seed(0)
            local_members = []

            for iteration in range(0, niter):
                listTest = []
                candidates = self.members[:]
                print(candidates)
                print(type(candidates))
                # Randomly select nmembers members
                for m in range(0, nmembers):
                    member = rgen.choice(candidates)
                    listTest.append(member)
                    candidates.remove(member)
                local_members.append(listTest)

            return local_members
