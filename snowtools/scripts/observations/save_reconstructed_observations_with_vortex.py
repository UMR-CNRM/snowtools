
import argparse
import os

from vortex import toolbox

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date

from snowtools.data import obs  # noqa

toolbox.active_now = True

description = "Archive reconstructed observation dataset for SAFRAN reanalysis"
parser = argparse.ArgumentParser(description=description)
parser.add_argument("-b", "--begin_year", type=str,
        help="First year of the period covered by the obseravtions (format YYYY)")
parser.add_argument("-e", "--end_year", type=str,
        help="Last of the period covered by the obseravtions (format YYYY)")
parser.add_argument("-x", "--xpid", type=str,
        help="Experiment identifier")
parser.add_argument("-p", "--path", type=str,
        help="Absolute path to the data to archive")

args = parser.parse_args()

datebegin = f'{args.begin_year}080106'
dateend = f'{args.end_year}080106'
list_dates_begin, list_dates_end, _, _ = get_list_dates_files(Date(datebegin), Date(dateend), 'yearly')
dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)

if '@' in args.xpid:
    xpid = args.xpid
else:
    user = os.environ['USER']
    xpid = args.xpid + '@' + user

toolbox.output(
    kind       = 'SurfaceObservation',
    nativefmt  = 'netcdf',
    model      = 'safran',
    datebegin  = list_dates_begin,
    dateend    = dict_dates_end,
    date       = '[dateend]',
    geometry   = 'SinglePoint',
    vapp       = 'safran',
    vconf      = 'france',
    experiment = xpid,
    block      = 'observations',
    filename   = f'{args.path}/RECOBS_[datebegin:subPT24H:ymd]T[datebegin:subPT24H:hh]-[dateend:ymd]T[dateend:hh].nc',
    namespace  = 'vortex.multi.fr',
    namebuild  = 'flat@cen'
)
