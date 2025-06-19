#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import unittest
import glob

from snowtools.tests.tempfolder import TestWithTempFolderWithChdir
from snowtools.scripts.extract.bdap.Extraction_BDAP import ExtractBDAP
from snowtools.scripts.extract.bdap.Extraction_BDAP import GeometryError, RunTimeError, LeadTimeError
from snowtools.DATA import TESTBASE_DIR

from bronx.stdtypes.date import Date


@unittest.skipIf(not os.path.isdir(TESTBASE_DIR), "input files not available")
class BDAPTest(TestWithTempFolderWithChdir):

    def setUp(self):
        super(BDAPTest, self).setUp()
        refdir = os.path.join(TESTBASE_DIR, "bdap")
        for reference in glob.glob(os.path.join(refdir, '*')):
            os.symlink(reference, os.path.basename(reference))

    def test_antilope(self):
        # Extraction_BDAP.py -b 2025052806 -e 2025052806 -d alp -p PRECIP -m ANTILOPEH -t 1 -g FRANXL1S100 -v SOL
        refname = 'reference_BDAP_request_ANTILOPEH.txt'
        request = ExtractBDAP(
            model       = 'ANTILOPEH',
            date        = Date('2025052806'),
            ech         = '1',
            parameter   = 'PRECIP',
            level_type  = 'SOL',
            levels      = ['SOL'],
            grid        = 'FRANXL1S100',
            coordinates = [46800, 43700, 5000, 7600],
            test        = True,
        )
        request.run()
        reference = open(refname, 'r')
        output = open(request.rqst, 'r')
        assert reference.readlines() == output.readlines()

    def test_wbt_arome(self):
        # Extraction_BDAP.py -b 2025052506 -e 2025052506 -d alp -p WETBT -v HAUTEUR -l 2 10 20 -g EURW1S40 -m PAROME
        refname = 'reference_BDAP_request_WBT_AROME.txt'
        request = ExtractBDAP(
            model       = 'PAROME',
            date        = Date('2025052806'),
            ech         = 1,
            parameter   = 'WETBT',
            level_type  = 'HAUTEUR',
            levels      = ['2', '10', '20'],
            grid        = 'EURW1S100',
            coordinates = [46800, 43700, 5000, 7600],
            test        = True,
        )
        request.run()
        reference = open(refname, 'r')
        output = open(request.rqst, 'r')
        assert reference.readlines() == output.readlines()

    def test_pearome(self):
        # Extraction_BDAP.py -b 2025052803 -e 2025052803 -d alp -p THETAPW -v ISOBARE -l 1000 850 700 500 -g EURW1S40
        # -m PEAROME
        refname = 'reference_BDAP_request_THETAPW_PEAROME.txt'
        request = ExtractBDAP(
            model       = 'PEAROME',
            date        = Date('2025052803'),
            ech         = 1,
            parameter   = 'THETAPW',
            level_type  = 'ISOBARE',
            levels      = ['1000', '850', '700', '500'],
            grid        = 'EURW1S40',
            coordinates = [46800, 43700, 5000, 7600],
            member      = 1,
            test        = True,
        )
        request.run()
        reference = open(refname, 'r')
        output = open(request.rqst, 'r')
        assert reference.readlines() == output.readlines()

    def test_GeometryError(self):
        request = ExtractBDAP(
            model       = 'ANTILOPEH',
            date        = Date('2025052806'),
            ech         = '1',
            parameter   = 'PRECIP',
            level_type  = 'SOL',
            levels      = ['SOL'],
            grid        = 'EURAT01',
            coordinates = [46800, 43700, 5000, 7600],
            test        = True,
        )
        with self.assertRaises(GeometryError):
            request.run()

    def test_RunTimeError(self):
        request = ExtractBDAP(
            model       = 'ANTILOPEH',
            date        = Date('20250528061500'),
            ech         = '1',
            parameter   = 'PRECIP',
            level_type  = 'SOL',
            levels      = ['SOL'],
            grid        = 'FRANXL1S100',
            coordinates = [46800, 43700, 5000, 7600],
            test        = True,
        )
        with self.assertRaises(RunTimeError):
            request.run()

    def test_LeadTimeError(self):
        request = ExtractBDAP(
            model       = 'ANTILOPEH',
            date        = Date('20250528060000'),
            ech         = '15',
            parameter   = 'PRECIP',
            level_type  = 'SOL',
            levels      = ['SOL'],
            grid        = 'FRANXL1S100',
            coordinates = [46800, 43700, 5000, 7600],
            test        = True,
        )
        with self.assertRaises(LeadTimeError):
            request.run()


if __name__ == "__main__":
    unittest.main()
