# Pour une installation sur Belenos

python -m venv --system-site-packages vortex-cen
source vortex-cen/bin/activate
pip install --upgrade pip
pip install meson-python
pip install fiona --upgrade
pip install cligj --upgrade
cd $SNOWTOOLS_DIR
pip install .


