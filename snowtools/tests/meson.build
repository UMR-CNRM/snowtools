pure_subdirs = [
    'namelists',
    'test_soda',
]

this_instdir = inst_dir / 'snowtools/tests'

foreach subdir: pure_subdirs
  install_subdir(subdir, install_dir: this_instdir, install_tag: 'python-runtime')
endforeach

python_sources = [
    '__init__.py',
    'export_output.py',
    's2m_instance.py',
    'tempfolder.py',
    'test_cartopy_plots.py',
    'test_cartopy_plots_index_template.html',
    'test_comp_surfex.py',
    'test_dev_surfex.py',
    'test_Netcdf_add_massif_info.py',
    'test_postprocess.py',
    'test_prosimu.py',
    'test_s2m.py',
    'test_script_compareSimuPostObs.py',
    'test_script_create_forcing.py',
    'future_test_spatial_scores.py',
    'test_hydro.py',
    'test_scd.py'
]

py.install_sources(
  python_sources,
  subdir: 'snowtools/tests'
)