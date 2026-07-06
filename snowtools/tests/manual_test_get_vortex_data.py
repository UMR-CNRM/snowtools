# -*- coding: utf-8 -*-

import unittest
from snowtools.utils.xarray_snowtools import get_vortex_data
import glob

from snowtools.utils import xarray_snowtools  # noqa: F401


class Test_vortex_extractor(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        pass

    def test_workdir_cleaning(self):
        ls_before = glob.glob('.')
        with get_vortex_data(configfile='S2MReanalysis.ini', configsection='SafranFlatReanalysis',
                datebegin='2022080106', dateend='2023080106', geometry='cor2_flat') as ds:
            print(ds)
        ls_after = glob.glob('.')
        self.assertEqual(ls_before, ls_after)


if __name__ == "__main__":
    unittest.main()
