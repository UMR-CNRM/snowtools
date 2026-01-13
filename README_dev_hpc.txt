
# Récupérer les branches de travail
git pull dev_hpc_vortex
git pull dev_pip_install

# Créer une branche perso à partir de la branche de dev commune
git checkout dev_hpc_vortex
git checkout -b dev_hpc_vortex_XX

# Pour tester sur HPC:
# Créer une branche de travail (local uniquement) pour récupérer les modif nécessaires pour pip install snowtools
git checkout -b dev_hpc_vortex_XX_work
git merge dev_pip_install
put snowtools belenos

# Sur belenos
Suivre les instructions de README_install_hpc.txt

# NB : modifications à faire dans dev_hpc_vortex_XX
# Pour tester des modifications :
git checkout dev_hpc_vortex_XX_work
git merge dev_hpc_vortex_XX
put snowtools belenos
