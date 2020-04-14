## BC, Feb. 2020: 
- parallelize the perturbations of forcings (Charrois et al., 2016, cluzet et al., 2020) on beaufix.
- put the result into beaufix cache and on hendrix.
# edit job_gener_pert_forcings to your convenience. In particular, file located paramPath at paramPath holds the parameters of the perturbations 
(except for the impurities, which are hard-written in makeFocingEnsemble.py) 

#Launch:
login on beaufix

$ sbatch bash_forcings.bash
then you can monitor the progress the computation/transfert runs with
$ squeue -u <yourid>