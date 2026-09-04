# -*- coding: utf-8 -*-
"""
Extraction tool for resources archived with vortex in a research context.

Usage examples:

With a default configuration file under vortex_cen/conf providing the full resource(s) description.
In this example, the `S2MReanalysis.ini` configuration file allows to extract data from
the S2M reanalysis over the entire period and all geometries covered by the dataset.

Before any actual extraction, the list of files that will be extracted can be checked as follows :

.. code-block:: python

    from vortex_cen.tools.vortex_extractor import get_data
    get_data(configfile='S2MReanalysis.ini',  configsection='SafranFlatReanalysis', verbose=True, checkonly=True)

The actual extraction can be launched by removing the "checkonly=True" keyword argument (WARNING : do not do that
for the entire dataset !)

In order to limit the extraction to one year and one geometry :

.. code-block:: python

    get_data(
        configfile='S2MReanalysis.ini',
        configsection='SafranFlatReanalysis',
        verbose=True,
        datebegin='2024080106',
        dateend='2025080106',
        geometry='cor2_flat',
    )

In order to use a custom configuration file, provide the absolute or relative path to the target file.
If this target file contains only the "DEFAULT" section or a single name section, simply type :

.. code-block:: python

    get_data(configfile='path/to/user/file.ini')

"""

import footprints
import vortex
from bronx.stdtypes.date import Date
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from vortex.util.config import GenericConfigParser

# TODO : Use a Vortex's "temporary_dir_context" to keep current working directory clean


def get_data(configfile=None, configsection=None, **kw):
    """
    Main method to call for the extraction of any resource archived with vortex in a research context.

    The footprint description of the target resource(s) can come from:
    * a configuration file : the absolute path must be provided to the `configfile` keyword argument.
    * footprint-like keyword arguments
    * a mixture of the first two options, with a priority given to keyword arguments

    By default, the files are fetched in the current working directory (or subdirectories in case on an ensemble).
    It is possible to disable the actual file transfer with the `checkonly` keyword argument.
    Similarily, the default vortex verbosity can be overwritten with the `verbose` keyword argument.

    :param configfile: Absolute path to the configuration file or filename of an existing configuration file under
                       vortex_cen/conf containing a (partial) footprint description of the target resource(s).
    :type configfile: str
    :param configsection: Name of the target section in the configuration file. If the configuration file has only
                          one section, `configsection` is automatically set to the existing section name.
                          If the configuration file has no section at all besides 'DEFAULT', a dummy section is
                          created to read the variables provided in 'DEFAULT'.
    :type configsection: str

    Standard keyword arguments for the description of CEN resources include :

    :param kind: The `kind` of resource to extract (ex: FORCING, PRO, PREP,...)
    :type kind: str
    :param vapp: The target resource's application
    :type vapp: str
    :param vconf: The target resource's configuration
    :type vconf: str
    :param experiment: The target resource's experiment identifier.
    :type experiment: str
    :param username: The name of the producer of the target resource(s).
    :type username: str
    :param geometry: The target resource's geometry
    :type geometry: str
    :param block: The target resource's block (generally the name of the producing unit task)
    :type block: str
    :param member: The target resource's member (int) or list of members (format 'first-last-step')
    :type member: str of FPList
    :param datebegin: The begin date of the resources to extract in case they cover a period
    :type datebegin: str or Date
    :param dateend: The end date of the resources to extract in case they cover a period
    :type dateend: str or Date
    :param duration: The length of the period covered by individual files ('yearly', 'monthly' ar 'full')
    :type duration: str
    :param datevalidity: The validity date of the resourcesto extract in case they do not cover a period
    :type datevalidity: str or Date

    The following additional keyword arguments can also be provided :

    :param checkonly: Disable actual data fetch in a dev or test context
    :type checkonly: bool
    :param verbose: Overwrite the default vortex verbosity
    :type verbose: bool

    """

    set_default_footprints()

    description = dict()

    # Read configuration file
    description = read_configuration_file(description, configfile, configsection)

    # Update description with command-line arguments
    for key in ["kind", "vapp", "vconf", "experiment", "username", "geometry", "block", "member", "duration"]:
        if kw.get(key, False):
            description.update({key: kw.get(key)})

    description = set_default_block(description)

    # Set time information depending on the kind of resource to extract
    description = set_time_info(description, **kw)

    # Case of an ensemble simulation
    if description.get('member', False):
        description['member'] = footprints.util.rangex(description['member'])
        # Add "member" sub-directory if necessary
        filename = description['local']
        description['local'] = f"mb[member]/{filename}"

    # Get data now, except if instructed otherwise for dev / test reasons
    if not kw.get('checkonly', False):
        description.update(dict(now=True))

    # a `verbose` keyword argument can be provided (or not) to overwrite the default vortex verbosity
    if 'verbose' in kw.keys():
        description.update(dict(verbose=kw['verbose']))

    # Actual get
    rh = vortex.input(**description)

    # Clean all extration-related files or directories
    sh = vortex.ticket().sh
    if sh.path.isfile("spawn_dump.sh"):
        sh.rm("spawn_dump.sh")

    for tmpdir in sh.glob("dactions_staging_area*"):
        sh.remove(tmpdir)

    return rh


def set_default_footprints():
    """
    Set default footprints for research data
    """
    vortex.defaults(
        nativefmt = 'netcdf',
        namespace = 'vortex.multi.fr',
        namebuild = 'flat@cen',
    )


def set_default_block(description):
    """
    Set the default block value for PRO files (only one possibility in this case)
    """

    if "kind" not in description.keys():
        raise ValueError("Missing the *kind* footprint")
    else:
        if "block" not in description.keys() and description["kind"] in ["PRO", "SnowpackSimulation"]:
            description["block"] = "offline"

        return description


def read_configuration_file(description, configfile, configsection):
    """
    Initialise the footprint description with variables from a configuration file.

    `configfile` can be an absolute or relative path as well as a filename identifying a configuration file
    under vortex_cen/conf.

    If the configuration file contains several sections, the target section must be provided through `configsection`.
    If the configuration file contains less than one section, all variables provided in the configuration file
    are used.

    """

    if configfile is not None:
        if vortex.sh().path.isfile(configfile) or vortex.sh().path.islink(configfile):
            iniparser = GenericConfigParser(inifile=configfile)
        else:
            try:
                iniparser = GenericConfigParser()
                import importlib.resources
                # Look for default configuration files in the vortex_cen package
                with importlib.resources.open_text('vortex_cen.conf', configfile) as ini:
                    iniparser = GenericConfigParser(ini)
            except FileNotFoundError:
                print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
                    f"WARNING : no '{configfile}' configuration file found.\n"
                    "The following extraction will be based on keyword arguments alone.\n"
                    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                return description

        sections = "\n".join([section for section in iniparser.as_dict().keys()])
        if configsection is None:
            # No configuration section provided
            if len(iniparser.as_dict().keys()) == 0:
                # Add "dummy" section to read values in "DEFAULT"
                configsection = 'dummy'
                iniparser.add_section(configsection)
            elif len(iniparser.as_dict().keys()) == 1:
                # Read the only available section
                configsection = list(iniparser.as_dict().keys())[0]
            else:
                raise ValueError(f"Missing a 'configsection' argument.\n"
                        f"Available sections in configuration file {configfile} :\n{sections}")
        else:
            if configsection not in iniparser.as_dict().keys():
                raise ValueError(f"No section {configsection} in configuration file {configfile}.\n"
                        f"Available sections :\n{sections}")

        description.update(iniparser.as_dict()[configsection])

    return description


def set_time_info(description, **kw):
    """
    Update the footprint description with time information depending on the target resource `kind`:
    * PREP files are valid at a given `datevalidity` time
    * FORCING and PRO files are valid over a time period. The list of files covering the extraction
      period is deduced from the prescribed individual files `duration`.
    """
    kind = description.get('kind', None)
    if kind is not None:
        # Deal with time period : PREP --> datevalidity, FORCING / PRO --> datebegin / dateend
        if kind == "PREP":
            datevalidity = kw.get('datevalidity', None) or description.get('datevalidity', None)
            if datevalidity is None:
                raise ValueError("A *datevalidity* attribute must be provided to extract PREP files")
            else:
                filename = '[kind]_[datevalidity:ymdh].nc'
                description.update(dict(
                    datevalidity = kw.datevalidity,
                    local = filename,
                ))
        else:
            datebegin = kw.get('datebegin', None) or description.get('datebegin', None)
            dateend = kw.get('dateend', None) or description.get('dateend', None)
            if datebegin is not None and dateend is not None:
                list_dates_begin, list_dates_end, _, _ = \
                    get_list_dates_files(Date(datebegin), Date(dateend), description.pop('duration', 'yearly'))
                dict_dates_end = get_dic_dateend(list_dates_begin, list_dates_end)
                filename = '[kind]_[datebegin:ymdh]_[dateend:ymdh].nc'
                description.update(dict(
                    datebegin = list_dates_begin,
                    dateend   = dict_dates_end,
                    local = filename,
                ))
            else:
                raise ValueError("*datebegin* and *dateend* attributes must be provided to extract files covering a "
                "time period")

        return description
    else:
        raise ValueError("Missing *kind* value")
