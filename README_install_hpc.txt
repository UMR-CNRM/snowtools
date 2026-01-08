# Pour une installation sur Belenos

module load python/3.7.6nomkl
python -m venv --system-site-packages snowtools
source snowtools/bin/activate
pip install --upgrade pip
cd $SNOWTOOLS_DIR
pip install . -v

