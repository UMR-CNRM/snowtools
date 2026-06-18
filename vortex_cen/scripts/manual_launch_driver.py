
import vortex
from mkjob.appconf import ConfigSet
import vortex_cen.Crocus.deterministic.drivers.surfex as todo

t = vortex.ticket()
driver = todo.setup(t)
jobconf = driver.read_config(inifile='conf/default_conf.ini')
updconf = jobconf.get("defaults", dict())
updconf.update(jobconf.get('surfex', dict()))
updconf.update({'vapp': 'bidon', 'vconf': 'bidon', 'xpid': 'bidon', 'geometry': 'cor2_flat'})
driver._conf = ConfigSet()
driver.conf.update(updconf)
for node in driver.contents:
    node.setconf(driver.conf, jobconf)
    node.build_context()
    reprod_infos = node.get_reprod_info
    print(reprod_infos)
driver.run()
