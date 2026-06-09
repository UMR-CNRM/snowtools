Glossary
========

The full documentation of MF's HPC is available here : http://diagnostix.meteo.fr/Bull/DOC/doc_utilisateur_belenos_taranis_v3.pdf
This section contains only the essential informations to properly work on MF HPC.

HPC architecture
================

Nodes
^^^^^

The MF HPC architecture relies on three different node types, identified as follow in this documentation:

* **login nodes** are the entry point for HPC users, from which they submit jobs. These nodes must only be used for compilation, **they are NOT deigned to make any data transfert or computation.**

* **tranfert nodes** are used to transfert data between the archive and the HPC file system. **These nodes are NOT deigned to make any computation.**

* **compute nodes** are used to do any computation. **These nodes are NOT deigned to make any data transfert.**

In the general case, tranfert and compute nodes should be accessed from a login node through the submission of a *job* (see :ref:`jobs`).

.. note::
   Data transfert between the HPC and any remote server (including the Hendrix archive) use the "ftserv" software, which require your LDAP password.
   When using ftserv for the first time or after a password modification, use the following command to set your password:
   ftmotpasse -h <server-name> -u <username>
   and follow the instructions

File system
^^^^^^^^^^^

There are two different file systems on MF HPC:

* **NFS** refers to users' home directories, which **should only contain source code**. In particular, **it should not host any job's IO**.

.. note::
   A daily backup of the files in the $HOME/SAVE directory is available.

* **Lustre** refers to the file system under /scratch, on which **temporary** data (job's IOs) must be stored (non-temporary data should always be stored on the Hendrix archive).

.. _partitions:

Partitions
^^^^^^^^^^

There are different types of compute node available, which are grouped into clusters called partitions. The choice of a given partition depends on the intended use of the node(s):

* **shared** is the default partition to use for jobs requiring a single node.
* **normal256** is the default partition (128 cores, 256 Go memory) for multi-node jobs.
* **huge512** contains large-memory nodes for jobs dealing with huge IOs (128 cores, 512 Go memory). These nodes are included in the *shared* partition for jobs involving a single node.
* **nmipt** should be used for pre-post processing jobs. These nodes are included in the *shared* partition for jobs involving a single node.
* **ndl** contains the GPUs for machine learning applications.

.. _jobs:

Jobs
^^^^

Jobs are scripts (python, shell,...) used to describe a specific task to be executed on the HPC compute / tranfert nodes.
All submited jobs go through a scheduler responsible to optimise the workload on HPC nodes.
When the workload is significant, a submited job can stay "PENDING" (waiting for a node allocation) for several minutes.
The status of a submited job can be monitored with the "squeue" command.

.. code-block::

   squeue -a -o "%.10i %.25j %.7m %.10M %.9P %.3T %5D %25N %u " | grep <username>
   job ID       jobname               memory       exec_time partition status nnodes  Nodes list
   30639945     safran_reanalysis_alp 247000M      49:14     ibcell0   RUN    1       belenos75   <username>
   30639659     safran_reanalysis_pyr 247000M      50:44     ibcell0   RUN    1       belenos185  <username>
   30647905     safran_reanalysis_cor      2G       0:00     transfert PEN    1                   <username>

You can kill jobs with the "scancel" command. Specify the job's ID to kill a single job:

.. code-block::

   scancel 30639945

or kill all your runing jobs with:

.. code-block::

   scancel -u <username>

.. note::
   Operationnal jobs have a higher priority level than "researche jobs, and can sudenly stop some of your jobs during their execution.
   In this case, you must re-submit your jobs.

At CEN, most job submissions are done through the "mkjob" job launcher.

The most frequent job-specific information to provide to mkjob through the mkjob command line or configuration file include (but are not limited to):

* *name*: The user-defined name, used to identify and monitor the job during the execution
* *nnodes*: The number of jobs to allocate to this particular job
* *ntasks*: The number of tasks to allocate to a given node (the total number of tasks for a job involving a single node)
* *partition*: The name of the partition from which the node(s) will be drawn (see :ref:`partitions` for possible values)
* *time*: a user-defined estimation of the maximum run time ("walltime") of the job. The walltime is used by the scheduler to prioritize "short" jobs.

.. warning::

   A job will automatically be killed once the prescribed "walltime" is reached, so include a magin to anticipate fluctuations in the HPC execution time.

* *mem*: The memory to allocate to this specific job in case it runs on the "shared" partition
* *profile*: The pre-defined job submission configuration (most of the time "rd-belenos-mt"). The recomended profile at CEN is the "rd-belenos-mt" profile.
  This create the jobs under /scratch/mtool/<username>/depot/mstep_XXXXXX_<jobname>, and automatically launches the following sequence:
  
  1. A transfert node to fetch all remote inputs (job: step.01, log: step.01.out)
  
  2. A compute node to do the computing work (job: step.02, log: step.02.out)
  
  3. A transfert node to archive the outputs (job: step.03, log: step.03.out)

  The actual execution of the job is done under /scratch/mtool/<username>/spool/spool_XXXXXX_<jobname>
  If the execution fails, or the argument debug=True is provided, the working directory is saved under /scratch/mtool/<username>/abort/dump_XXXXXX_<jobname>

The Hendrix archive
===================

A more complete documentation of Hendrix is available here (in French) : http://confluence.meteo.fr/spaces/CC/pages/299881305/1.+Pr%C3%A9sentation+du+syst%C3%A8me+de+stockage+Hendrix

Hendrix is a mass storage system.
Hendrix is accessible through a front-end server where only files smaller than 1Go are stored, files larger than 1Go are stored on magnetic tape.
Accessing files on magnetic tape can be very slow, to speed up the process it is recomended to stage all the target files in advance in order to load the files temporary on the front-end server for a quicker acess.
To do this, simply put the list of absolute path to the target files (using shell special characters such as '*' and '?' it can generally fit in a few lines) in a txt file with a ".MIG" extension, and put this file on the /DemandeMig/ChargeEnEspaceRapide repository on Hendrix. It is possible to put your email adress (with a leading '#') on the first line of the pre-staging file in order to be notified by email when your pre-staging request starts and when it has been completed.

For example, to "pre-stage" all SURFEX-ready FORCING files of the S2M reanalysis over the Alps, you can use the following prestaging file:

.. code-block::

   #MAIL=<your_email>
   /home/vernaym/vortex/s2m/reanalysis/release_2026.1/meteo/FORCING*alp27_allslopes*


The full documentation for pre-staging requests is available here (in French): http://confluence.meteo.fr/spaces/~romain.roehrig/pages/263753981/Pr%C3%A9-staging+sur+hendrix?preview=%2F263753981%2F263753983%2Fdoc_pre-staging_hendrix.pdf

You can only connect to Hendrix through a telnet protocol with limited shell commands to explore the the file system.
On HPC, you must first load hpss (module load hpss) before you can connect to Hendrix with the "hshell" command.


Good practices
^^^^^^^^^^^^^^

* Store any file that you do not want to lose under $HOME/SAVE (daily backup)
* **Do not** store IO data under your $HOME
* **Never launch any heavy computation or data transfert from a login node.**
* Always use transfert nodes to transfert data to / from the archive or any remote server.
* For huge jobs, take some time to set the "walltime" properly to avoid having your jobs stoped before the work is done.
* Always use the prestaging when you want to retrieve huge datasets from the Hendrix archive, this will save you a lot of time !
* The optimal size to archive files on Hendrix is between 1Go and 100Go. If / when possible, try to group small files in a single tar file larger than 1 Go.
