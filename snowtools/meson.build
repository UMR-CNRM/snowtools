pure_subdirs = [
    'conf',
    'DATA',
    'tools',
    'plots',
    'scripts',
    'tasks',
    'utils',
]

this_instdir = inst_dir / 'snowtools'

foreach subdir: pure_subdirs
  install_subdir(subdir, install_dir: this_instdir, install_tag: 'python-runtime')
endforeach

python_sources = [
    '__init__.py'
]

py.install_sources(
  python_sources,
  subdir: 'snowtools'
)



subdir('interpolation')
subdir('scores')
subdir('tests')

