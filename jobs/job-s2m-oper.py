#MTOOL set jobname=$name
#MTOOL set jobtag=[this:jobname]
#MTOOL profile target=${target}cn
#SBATCH --cpus-per-task=$openmp
#SBATCH --export=NONE
#SBATCH --job-name=[this:jobname]
#SBATCH --mem=$mem
#SBATCH --nodes=$nnodes
#SBATCH --ntasks-per-node=$ntasks
#SBATCH --partition=$partition
#SBATCH --time=$time
#SBATCH --$exclusive
#SBATCH --$verbose
#MTOOL end

# Build time: $create
# Build user: $mkuser
# Build host: $mkhost
# Build opts: $mkopts

#MTOOL set host=${target}
#MTOOL setconf files=targets.[this:host]
#MTOOL set logtarget=[this:frontend]
#MTOOL set fetch=[this:frontend]
#MTOOL set compute=[this:cpunodes]
#MTOOL set backup=[this:frontend] 

#MTOOL set bangline=${python}_$pyopts
#MTOOL configure submitcmd=$submitcmd

import os, sys
appbase = os.path.abspath('$appbase')
vortexbase = os.path.join(appbase, 'vortex')
sys.path.insert(0, os.path.join(vortexbase, 'site'))
sys.path.insert(0, os.path.join(vortexbase, 'src'))
sys.path.insert(0, appbase)

import bronx.stdtypes.date
from bronx.system.interrupt import SignalInterruptError
import footprints
import vortex
import vortex.layout.jobs

# This temporary shell should work well enough for the autolog step
t = vortex.ticket()
sh = t.sh
e = t.env

#MTOOL common not=autolog

import $package.$task as todo

rd_rootapp  = '$appbase'
rd_vapp     = '$vapp'
rd_vconf    = '$vconf'
rd_member   = $member
rd_xpid     = '$xpid'
rd_jobname  = '$name'
rd_iniconf  = '{0:s}/conf/{1:s}_{2:s}.ini'.format(appbase, 
                                                        rd_vapp, rd_vconf)
rd_rundate   = bronx.stdtypes.date.Date($rundate)
ja = footprints.proxy.jobassistant(kind = 'generic',
                                   modules = footprints.stdtypes.FPSet((
                                       'common', 'gco', 'olive', 'cen', 'iga',
                                       'vortex.tools.lfi', 'vortex.tools.odb',
                                       'common.util.usepygram')),
                                   addons = footprints.stdtypes.FPSet(('lfi', 'iopoll', 'odb')),
                                   special_prefix='rd_',
                                   )
ja.add_plugin('mtool', step='[this:number]', stepid='[this:id]')

try:
    t, e, sh = ja.setup(actual=locals())
    sh.ftraw = True # To activate ftserv

    opts = dict(jobassistant=ja, steps=ja.mtool_steps,
                defaults=dict(gnamespace='gco.multi.fr'))
    driver = todo.setup(t, **opts)
    driver.setup()
    driver.run()

    ja.complete()

except (Exception, SignalInterruptError, KeyboardInterrupt) as trouble:
    ja.fulltraceback(trouble)
    ja.rescue()
    #MTOOL include files=epilog.step
    #MTOOL include files=submit.last

finally:
    #MTOOL include files=epilog.clean.step
    ja.finalise()
    ja.close()
    print 'Bye bye research...'

#MTOOL step id=fetch target=[this:fetch]
#MTOOL step id=compute target=[this:compute]
#MTOOL step id=backup target=[this:backup]

#MTOOL autoclean
#MTOOL autolog
