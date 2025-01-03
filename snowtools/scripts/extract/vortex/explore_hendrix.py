import os
import ftplib
from ftplib import FTP
from netrc import netrc
import argparse

"""
Vortex cache : /home/{user}/vortex/{vapp}/{geometry.lower()}/{xpid}/mb{member:04d}/{block}/{subblock}/{filename}

Examples :
----------

1. Search for a specific PRE file :
>>> p explore_hendrix.py -u haddjeria -g gr250ls -x spinup -d 2017080106


2.  List all my experiments IDs that contain SURFEX simulations (PRO files) covering a specific year :

>>> p ~/snowtools/snowtools/scripts/extract/vortex/explore_hendrix.py -u vernaym -a edelweiss
      -b 2017080106 -e 2018080106 -k PRO -o xpid [-s]

"""


def parse_args():
    parser = argparse.ArgumentParser(description=
    """
    Vortex arhive caches explorer.
    """)

    parser.add_argument("-b", "--datebegin", dest="datebegin",
                        default=None, type=str,
                        help="Search for files begining at a given date")

    parser.add_argument("-e", "--dateend", dest="dateend",
                        default=None, type=str,
                        help="Search for files ending at a given date")

    parser.add_argument("-d", "--date", dest="date",
                        default=None, type=str,
                        help="Search for files valid at a given date")

    parser.add_argument("-u", "--users", dest="users",
                        nargs='*', default=None,  # TODO : Explore all CEN users by default
                        help="List of users whose environment you want to explore")

    parser.add_argument("-a", "--vapp", dest="vapp",
                        default=None, type=str, choices=['s2m', 'edelweiss', 'safran', 'Pleiades', 'Sentinel2'],
                        help="Explore a specific application")

    parser.add_argument("-g", "--geometry", dest="geometry",
                        # TODO : map identical geometries with different tags (GrandesRousses250m / gr250ls)
                        default=None, type=str,
                        help="Explore a specific geometry / vconf")

    parser.add_argument("-x", "--xpid", dest="xpid",
                        default=None, type=str,
                        help="Explore a specific xpid. Format xpid@username authorised, in this case,"
                        "the users argument is automatically set to 'username'")

    parser.add_argument("-c", "--block", dest="block",
                        default=None, type=str,
                        help="Explore a specific block (for example 'meteo', 'prep', 'pro',...)")

    parser.add_argument("-k", "--kind", dest="kind",
                        default=None, type=str,
                        help="Explore a specific block (for example 'FORCING', ,forcing', 'PRO', 'PREP',...)."
                             "Both upper and lower case are valid")

    output = parser.add_argument_group("Expected request output")

    output.add_argument("-o", "--output", dest="output",
                        default=None, type=str,
                        choices=['xpid', 'period', 'user', 'vapp', 'vconf', 'geometry', 'block'],
                        help="What specific information are you looking for ?")

    output.add_argument("-s", "--succinct", dest="succinct", action='store_true',
                        help="Display ensembles output informations in one line (unsing mbXXXX syntax to indicate that"
                             "it concerns an ensemble simulation")

    args = parser.parse_args()
    if args.geometry is not None:
        # vconf = geometry.lower
        args.vconf = args.geometry.lower()
    else:
        args.vconf = None

    return args


def ftpconnect(machine):
    username, account, password = netrc().authenticators(machine)
    ftp = FTP(machine)
    ftp.set_debuglevel(1)
    ftp.login(username, password)
    return ftp


def list_subdirectories(ftpObject):
    list_dir       = []  # Subdirectories full info
    subdirectories = []  # Subdirectories names
    ftpObject.dir(list_dir.append)
    for dirinfo in list_dir:
        subdirectories.append(dirinfo.split(' ')[-1])
    return subdirectories


def read_list_user_CEN():
    # TODO définir la liste complète des utilisateurs
    return ['haddjeria', 'fructusm']


def add_entry(dictionary, user, vapp, vconf, xpid, block, value):
    if user in dictionary.keys():
        if vapp in dictionary[user].keys():
            if vconf in dictionary[user][vapp].keys():
                if xpid in dictionary[user][vapp][vconf].keys():
                    dictionary[user][vapp][vconf][xpid][block] = value
                else:
                    dictionary[user][vapp][vconf][xpid] = {block: value}
            else:
                dictionary[user][vapp][vconf] = {xpid: {block: value}}
        else:
            dictionary[user][vapp] = {vconf: {xpid: {block: value}}}
    else:
        dictionary[user] = {vapp: {vconf: {xpid: {block: value}}}}

    return dictionary


def explore_ensemble(ftpObject=None, VortexEnv=None, rootdir=None, user=None, app=None, conf=None, xp=None,
                     datebegin=None, dateend=None, date=None, kind=None, mb=None, **kw):
    try:
        mbdir = os.path.join(rootdir, mb)
        ftpObject.cwd(mbdir)
        block_list = list_subdirectories(ftpObject)
        subdirs = list_subdirectories(ftpObject)
        for bk in subdirs:
            rootdir = mbdir
            VortexEnv = explore_block(**locals())
    except ftplib.error_perm:
        # WARNING : this ignores all files not under a 'block' directory
        pass

    return VortexEnv


def explore_block(ftpObject=None, VortexEnv=None, rootdir=None, user=None, app=None, conf=None,
                  xp=None, bk=None, datebegin=None, dateend=None, date=None, kind=None, **kw):
    try:
        blockdir = os.path.join(rootdir, bk)
        ftpObject.cwd(blockdir)
        # Get list of files
        list_files = list_subdirectories(ftpObject)
        if datebegin is not None:
            list_files = [fic for fic in list_files if datebegin in fic]
        if datebegin is not None:
            list_files = [fic for fic in list_files if dateend in fic]
        if date is not None:
            list_files = [fic for fic in list_files if date in fic and
                    len(fic.split('.')[0].split('_')) == 2]
        if kind is not None:
            list_files = [fic for fic in list_files if kind.upper() in fic]

        if len(list_files) > 0:
            for fic in list_files:
                abspath = os.path.join(blockdir, fic)
                VortexEnv.append(abspath)
            # VortexEnv = add_entry(VortexEnv, user, app, conf, xp, bk, list_files)
    except ftplib.error_perm:
        # WARNING : this ignores all files not under a 'block' directory
        pass

    return VortexEnv


def get_vortex_state(datebegin=None, dateend=None, date=None, users=None, vapp=None, vconf=None,
                     xpid=None, block=None, kind=None, **kw):

    # VortexEnv = dict()  # Main dictionnary
    VortexEnv = list()  # Output initialisation
    ftpObject = ftpconnect("hendrix.meteo.fr")

    # ftpObject.pwd()  # Print (current) Working Directory

    if users is None:
        users = read_list_user_CEN()
    else:
        users = [str(user) for user in users]

    for user in users:

        homedir = f'/home/{user}/vortex/'
        ftpObject.cwd(homedir)  # Change Working Directory

        vapp_list = list_subdirectories(ftpObject)
        if vapp is not None:
            if vapp in vapp_list:
                vapp_list = [vapp]
            else:
                print(f'vapp {vapp} does not exists for user {user}')
                vapp_list = []

        for app in vapp_list:
            vappdir = os.path.join(homedir, app)
            ftpObject.cwd(vappdir)

            vconf_list = list_subdirectories(ftpObject)
            if vconf is not None:
                if vconf in vconf_list:
                    vconf_list = [vconf]
                else:
                    print(f'vconf {vconf} does not exists for user {user} and vapp {vapp}')
                    vconf_list = []

            for conf in vconf_list:
                try:
                    vconfdir = os.path.join(vappdir, conf)
                    ftpObject.cwd(vconfdir)

                    xpid_list = list_subdirectories(ftpObject)
                    if xpid is not None:
                        if xpid in xpid_list:
                            xpid_list = [xpid]
                        else:
                            print(f'xpid {xpid} does not exists for user {user}, vapp {vapp} and vconf {vconf}')
                            xpid_list = []

                    for xp in xpid_list:
                        try:
                            xpiddir = os.path.join(vconfdir, xp)
                            ftpObject.cwd(xpiddir)

                            # TODO : gérer les ressources stockée directement sous "xpid" (sans member/block)

                            block_list = list_subdirectories(ftpObject)
                            subdirs = list_subdirectories(ftpObject)
                            if block is not None:
                                if block in block_list:
                                    block_list = [block]
                                else:
                                    print(f'block {block} does not exists for user {user}, vapp {vapp}, vconf {vconf}'
                                          f'and xpid {xpid}')
                                    block_list = []

                            for sub in subdirs:
                                if 'mb' in sub:  # This is a *member*
                                    mb = sub
                                    rootdir = xpiddir
                                    VortexEnv = explore_ensemble(**locals())
                                    pass  # TODO
                                else:  # this is a *block*
                                    rootdir = xpiddir
                                    bk = sub
                                    VortexEnv = explore_block(**locals())
                        except ftplib.error_perm:
                            # WARNING : this ignores all files not under a 'xpid' directory
                            pass
                except ftplib.error_perm:
                    # WARNING : this ignores all files not under a 'vconf' directory
                    pass

    return VortexEnv


def print_request_info(env, **kw):
    print()
    print('=====================================================================')
    print('                      Environment informations                       ')
    print('=====================================================================')
    print()
    if len(env) == 0:
        print('No known experiment match the request')
        print()
    else:
        if kw['output'] is not None:
            output = kw['output']
            print(f'The following {output}(s) match(es) the request :')
            print()
        else:
            print('The following file(s) match(es) the request :')
            print()

        for abspath in env:
            dirlist = abspath.lstrip('/').split('/')
            if len(dirlist) == 8:
                home, user, vx, vapp, vconf, xpid, block, basename = dirlist
                member = None
            elif len(dirlist) == 9:
                home, user, vx, vapp, vconf, xpid, member, block, basename = dirlist

            split_basename = basename.split('.')[0].split('_')
            kind = split_basename.pop(0)  # Get first element value and remove it from list
            kind = kind.lower()
            if len(split_basename) == 2:
                datebegin, dateend = split_basename
                command = f"io.get_{kind}('{xpid}@{user}', '{vconf}', datebegin='{datebegin}', dateend='{dateend}'"
            elif len(split_basename) == 1:
                date, = split_basename
                command = f"io.get_{kind}('{xpid}@{user}', '{vconf}', date='{date}''"
            else:
                print('Ressource not recognised by vortexIO')
                print()
                command = None

            if kw['output'] is not None:
                if kw['succinct']:
                    if member is None:
                        print(f'{eval(output):30s}  -->  {abspath}')
                    elif member == 'mb0001':
                        # In case of ensemble simulation do not print a line per member, use mbXXXX syntax instead
                        printpath = '/'.join([home, user, vx, vapp, vconf, xpid, 'mbXXXX', block, basename])
                        fmt_output = f'{eval(output)} [ensemble]'
                        print(f'{fmt_output:30s}  -->  /{printpath}')
                else:
                    print(f'{eval(output):20s}  -->  {abspath}')
            else:
                #print("Absolute path :")
                print(f"{abspath}")
                if command is not None:
                    if vapp != 's2m':
                        command = f"--> {command}, vapp='{vapp}'"
                    # TODO find a way to detect default blocks to avoid to add it to the command line
                    command = f"{command}, block='{block}'"
                    command = f"{command})"
                    #print("vortexIO command to retrieve it :")
                    print(command)
                    print()


if __name__ == '__main__':
    args = parse_args()
    # vars(args) convert the args *Namespace* object into a *dictionnary* object
    env = get_vortex_state(**vars(args))
    print_request_info(env, **vars(args))
