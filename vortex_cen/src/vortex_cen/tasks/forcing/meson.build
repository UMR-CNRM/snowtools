pure_subdirs = [
    'forcing',
]

this_instdir = inst_dir / 'vortex-cen' / 'src' / 'vortex_cen' / 'tasks' / 'forcing'

foreach subdir: pure_subdirs
  install_subdir(subdir, install_dir: this_instdir, install_tag: 'python-runtime')
endforeach

python_sources = [
    '__init__.py'
]

py.install_sources(
  python_sources,
  subdir: 'vortex-cen/src/vortex_cen/tasks/forcing'
)


