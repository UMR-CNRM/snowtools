pure_subdirs = [
    'algo',
    'data',
    'layout',
    'task',
    'tools',
]

this_instdir = inst_dir / 'vortex_cen' / 'src' / 'vortex_cen'

foreach subdir: pure_subdirs
  install_subdir(subdir, install_dir: this_instdir, install_tag: 'python-runtime')
endforeach

python_sources = [
    '__init__.py'
]

py.install_sources(
  python_sources,
  subdir: 'vortex_cen/src/vortex_cen'
)

subdir('tasks')

