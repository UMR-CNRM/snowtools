# Pour une installation sur Belenos

python -m venv --system-site-packages vortex_cen_env
source vortex-cen/bin/activate
pip install --upgrade pip
pip install meson-python
pip install fiona --upgrade
pip install cligj --upgrade
cd $SNOWTOOLS_DIR
python -m pip install --no-build-isolation --editable .


# Pour une installation sur SXCEN

python -m venv --system-site-packages vortex_cen_env
source vortex_cen_env/bin/activate
pip install --upgrade pip
pip install meson-python
pip install ninja
pip install fiona --upgrade
pip install cligj --upgrade
cd $SNOWTOOLS_DIR
python -m pip install --no-build-isolation --editable .
