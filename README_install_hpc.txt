# Pour une installation sur Belenos

module load python/3.7.6nomkl  (Ajouter au .bash_profile)
python -m venv --system-site-packages snowtools
source snowtools/bin/activate
pip install --upgrade pip
cd $SNOWTOOLS_DIR
pip install --upgrade shapely  # Problème de dépendance dans cartopy, fait planter "from snowtools.plots.pearps2m.postprocess import EnsemblePostproc, EnsembleHydro"
pip install . -v

# Dans .bash_profile :
export PYTHONPATH=$HOME/venv/snowtools/lib/python3.7/site-packages:$PYTHONPATH
export VORTEX=/home/mf/dp/marp/verolive/vortex/vortex-cen
export PYTHONPATH=$PYTHONPATH:$VORTEX/src/:$VORTEX/site:$VORTEX/bin
export PATH=$PATH:$VORTEX/bin

