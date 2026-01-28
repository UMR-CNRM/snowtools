
import argparse

from vortex import toolbox

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date

from snowtools.data import obs  # noqa

description = "Archive reconstructed observation dataset for SAFRAN reanalysis"
parser = argparse.ArgumentParser(description=description)
parser.add_argument("-b", "--begin_year", type=str,
        help="First year of the period covered by the obseravtions (format YYYY)")
parser.add_argument("-e", "--end_year", type=str,
        help="Last of the period covered by the obseravtions (format YYYY)")
parser.add_argument("-x", "--xpid", type=str,
        help="Experiment identifier, format {xpid}@{user}")
parser.add_argument("-p", "--path", type=str,
        help="Absolute path to the data to archive")

args = parser.parse_args()

datebegin = f'{args.begin_year}080106'
dateend = f'{args.end_year}080106'
list_dates_begin, list_dates_end, _, _ = get_list_dates_files(Date(datebegin), Date(dateend), 'yearly')
dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)

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
    experiment = args.xpid,
    block      = 'observations',
    filename   = f'{args.path}/RECOBS_[datebegin:subPT24H::strftime("%Y%m%dT%H")]-[dateend:stdvortex].nc',
    namespace  = 'vortex.multi.fr',
    namebuild  = 'flat@cen'
)
