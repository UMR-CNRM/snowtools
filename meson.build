project('snowtools', 'c',
   version: '2.0.2',
   default_options: [
    'c_std=c99'
  ],
  )

py = import('python').find_installation(pure: false)

py_dep = py.dependency()

pure_subdirs = ['cenutils']
inst_dir = py.get_install_dir()

foreach subdir: pure_subdirs
  install_subdir(subdir, install_dir: inst_dir, install_tag: 'python-runtime')
endforeach


add_languages('fortran')

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
 ).stdout().strip()

 incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

crps_source = custom_target('crpsmodule.c',
  input : ['./snowtools/scores/crps.f90'],
  output : 'crpsmodule.c',
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '-m', 'crps', '--lower', '--build-dir', '.'],
)

inc_np = include_directories(incdir_numpy, incdir_f2py)

py.extension_module(
   'crps',
   ['./snowtools/scores/crps.f90', crps_source],
   incdir_f2py / 'fortranobject.c',
    include_directories: inc_np,
    dependencies : py_dep,
    subdir: 'snowtools/scores',
    install: true,
)


subdir('snowtools')