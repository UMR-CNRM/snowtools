#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 12 March 2026

@author: Matthieu Lafaysse
"""
import os
import argparse
import datetime
from snowtools.utils.dates import check_and_convert_date
from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.tools.execute import printandcallSystemOrDie

def parse_args():
    parser = argparse.ArgumentParser(description="""
    Replay operational runs over a long period
    """)
    parser.add_argument("--geometry",
                        dest="geometry", default=None,
                        help="geometry")

    parser.add_argument("--task",
                        dest="task", default=None,
                        help="task")

    parser.add_argument("-b", "--begin",
                        dest="begin",
                        help="Date of begining of replay (YYYYMMDDHH)")

    parser.add_argument("-e", "--end",
                        dest="end",
                        help="Date of end of replay (YYYYMMDDHH)")

    args = parser.parse_args()
    return args


if __name__ == "__main__":
    args = parse_args()

    datebegin = check_and_convert_date(args.begin)
    dateend = check_and_convert_date(args.end)
    geometry = args.geometry
    task = args.task

    date = datebegin
    while date <= dateend:
        command = (SNOWTOOLS_DIR + "/tasks/s2m_command.py oper " + "-r " + geometry + " -b " + date.ymdh +
                   " --task=" + task + " --dev")

        printandcallSystemOrDie(command)

        date += datetime.timedelta(days=1)