import extract_s2m as todo
import vortex

# TODO :
# - Déplacer l'exécution dans un "workdir" à l'extérieur du dépot
# - Créer un driver [fichier de conf ?] spécifique dans vortex-cen/tests
# - Transformer en script avec la task et la step à tester en arguments

t = vortex.ticket()
steps = ['early-fetch']
steps = ['early-fetch', 'compute', 'late-backup']
#steps = ['early-fetch', 'late-backup']
driver = todo.setup(t, iniconf='../conf/tests_testhpc.ini', steps=steps, xpid='extract_s2m@vernaym', datebegin='2022080106', dateend='2023080106', geometry='cor', vapp='testshpc', vconf='unittest')
driver.setup()
driver.run()
