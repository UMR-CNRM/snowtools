
# crps_lib = run_command(py, ['-m', 'numpy.f2py', '-c', '@INPUT@', '-m', 'crps', '--lower'])



python_sources = [
    '__init__.py',
    'crps.f90',
    'clusters.py',
    'CompareSimuPosteObsCsv.py',
    'deterministic.py',
    'ensemble.py',
    'generic.py',
    'list_scores.py',
    'spatial.py',
]

py.install_sources(
  python_sources,
  subdir: 'snowtools/scores',
)
