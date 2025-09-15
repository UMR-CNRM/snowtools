#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse


# TODO: this geometry should be either shared in vortex or
# not used in the tests but not added on the fly
#
# from vortex.data import geometries
#
# geometry = geometries.UnstructuredGeometry(
#     info   = 'Stations of Grandes Rousses',
#     tag    = 'postes_12_csv',
#     slope  = False,
#     area   = 'postes_12_csv',
#     new    = True
# )
# geometry.to_inifile()

s2m = f'python {os.environ["SNOWTOOLS_CEN"]}/snowtools/tasks/s2m_command.py'

tests_list = [
    'reanalysis', 'escroc', 'stochastic_perturbations', 'croco_openloop', 'croco_real_obs', 'oper_analysis',
    'oper_forecast', 'reforecast_IC_generation', 'reforecast']


def parse_command_line():

    description = "Launch s2m test cases on HPC"

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('-t', '--test_case', type=str, nargs='+', default=None, choices=tests_list,
                        help="Test case(s) to launch")

    args = parser.parse_args()

    if args.test_case is None:
        args.test_case = tests_list

    return args


def reanalysis():
    '''
    Reanalysis test case
    '''
    os.system(f'{s2m} research -r alp_allslopes -b 20220801 -e 20230801 -m safran -f reanalysis2020.2 -o reanalysis_test -n /home/cnrm_other/cen/mrns/lafaysse/PycharmProjects/snowtools_git/snowtools/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis.nam')  # noqa: E501


def escroc():
    '''
    ESCROC test case
    '''
    os.system(f'{s2m} research -r cdp -b 1994100101 -e 2014100100 -x 2014100100 -m ESM-SnowMIP -f obs@lafaysse -o E2_test --task=escroc --escroc=E2')  # noqa: E501


def stochastic_perturbations():
    '''
    Stochastic perturbation test case
    '''
    os.system(f'{s2m} research -r cor_flat -b 20200801 -e 20210801 -m s2m -f reanalysis2020.2@lafaysse -o perturb --task=croco_perturb --nmembers=80')  # noqa: E501


def croco_openloop():
    '''
    Croco openloop test case
    '''
    os.system(f'{s2m} research -r postes_12_csv -b 2013080106 -e 2014063006 -x 20160801 -m safran -f forcing_20132014B_31D_11_t1500_160@fructusm -o testopenloop -n ~lafaysse/croco/OPTIONS_MOTHER_DEP.nam --task=croco --croco=openloop --escroc=E1notartes --nmembers=35 --nforcing=35 --conf=/home/lafaysse/croco/conf.ini -s ~lafaysse/SURFEX/cen/exe_mpi')  # noqa: E501


def croco_real_obs():
    '''
    Croco test case with assim of real observations
    '''
    os.system(f'{s2m} research -r postes_12_csv -b 2013080106 -e 2014063006 -x 20160801 -m safran -f forcing_20132014B_31D_11_t1500_160@fructusm -o test0l -n ~lafaysse/croco/OPTIONS_MOTHER_DEP.nam --task=croco --croco=real --escroc=E1notartes --nmembers=35 --nforcing=35 --conf=/home/lafaysse/croco/conf.ini -s ~lafaysse/SURFEX/cen/exe_mpi --obsxpid=obs@lafaysse --sensor=bdclim')  # noqa: E501


def oper_analysis():
    os.system(f'{s2m} oper -b YYYYMMDD03 -r alp')


def oper_forecast():

    os.system(f'{s2m} oper -b YYYYMMDD03 -r alp --task=forecast')


def reforecast_IC_generation():
    '''Building of reforecast initial conditions test case'''
    os.system(f'{s2m} research -r vog3_allslopes -b 20000801 -e 20010801 -a 400 -m s2m -f reanalysis_era5.2023@lafaysse -p reanalysis_era5.2023@lafaysse -o initialconditions_test -n /home/cnrm_other/cen/mrns/lafaysse/PycharmProjects/snowtools_git/snowtools/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis_forprep.nam')  # noqa: E501


def reforecast():
    '''Reforecast test case'''
    os.system(f'{s2m} research -b 20000302 -e 20000327 -r vog3_allslopes -n /home/cnrm_other/cen/mrns/lafaysse/PycharmProjects/snowtools_git/snowtools/DATA/OPTIONS_reforecast.nam --task=reforecast -m safran -f reforecast_2023 --nmembers=11 -p initdaily_era5.2023@lafaysse -o reforecast_test')  # noqa: E501


if __name__ == '__main__':
    args = parse_command_line()
    for test in args.test_case:
        locals()[test]()
