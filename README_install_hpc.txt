# Pour une installation sur Belenos

module load python/3.7.6nomkl  (Ajouter au .bash_profile)
python -m venv --system-site-packages snowtools
source snowtools/bin/activate
pip install --upgrade pip
cd $SNOWTOOLS_DIR
pip install --upgrade shapely  # Problème de dépendance dans cartopy, fait planter "from snowtools.plots.pearps2m.postprocess import EnsemblePostproc, EnsembleHydro"
pip install .

# Pour une installation editable :
module load python/3.10.12

python -m venv test_pip_install_snowtools
source test_pip_install_snowtools/bin/activate
pip install --upgrade pip
pip install meson-python
pip install --upgrade numpy
pip install --upgrade setuptools
pip install --upgrade ninja
pip install pandas==2.0.3
pip install cython
pip install rasterio==1.4.3
module load gcc
cd $SNOWTOOLS_DIR
pip install -e . --no-build-isolation


# Installation de vortex-cen (Vortex2)
module load python/3.10.12
pip install --upgrade pip




# Dans .bash_profile (Vortex1):
export PYTHONPATH=$HOME/venv/snowtools/lib/python3.7/site-packages:$PYTHONPATH
export VORTEX=/home/mf/dp/marp/verolive/vortex/vortex-cen
export PYTHONPATH=$PYTHONPATH:$VORTEX/src/:$VORTEX/site:$VORTEX/bin
export PATH=$PATH:$VORTEX/bin

