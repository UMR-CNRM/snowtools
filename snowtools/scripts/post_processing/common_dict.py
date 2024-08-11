#!/usr/bin/env python3
# -*- coding: utf-8 -*-

members_map = dict(
    SAFRAN_perturb           = [mb for mb in range(17)],
    KRIGING_perturb          = [mb for mb in range(17)],
    RS27_pappus              = [mb for mb in range(17)],
    EnKF36_pappus            = [mb for mb in range(17)],
    PF32_pappus              = [mb for mb in range(17)],
    RS27_sorted_pappus       = [mb for mb in range(17)],
    SAFRAN_perturb_assim     = [mb for mb in range(1, 18)],
    KRIGING_perturb_assim    = [mb for mb in range(1, 18)],
    RS27_pappus_assim        = [mb for mb in range(1, 18)],
    EnKF36_pappus_assim      = [mb for mb in range(1, 18)],
    PF32_pappus_assim        = [mb for mb in range(1, 18)],
    RS27_sorted_pappus_assim = [mb for mb in range(1, 18)],
    RS27_spa_erroOBS_025     = [mb for mb in range(1, 18)],
    ANTILOPE_pappus          = None,
    SAFRAN_pappus            = None,
)

product_map = dict(
    RS27_pappus              = 'RS',
    EnKF36_pappus            = 'EnKF',
    PF32_pappus              = 'PF',
    RS27_sorted_pappus       = 'SRS',
    RS27_pappus_assim        = 'RS_assim',
    EnKF36_pappus_assim      = 'EnKF_assim',
    PF32_pappus_assim        = 'PF_assim',
    RS27_sorted_pappus_assim = 'SRS_assim',
    RS27_spa_erroOBS_025     = 'SRS_assim_err025',
    ANTILOPE_pappus          = 'ANTILOPE',
    SAFRAN_pappus            = 'SAFRAN',
    SAFRAN_perturb           = 'SAFRAN_perturb',
    SAFRAN_perturb_assim     = 'SAFRAN_perturb_assim',
    KRIGING                  = 'KRIGING',
    KRIGING_perturb          = 'KRIGING_perturb',
    KRIGING_perturb_assim    = 'KRIGING_perturb_assim',
)

xpid_map = {
    '2018012312': 'CesarDB',
    '2018031612': 'CesarDB_AngeH',
    '2019051312': 'CesarDB_AngeH',
    '2022022612': 'CesarDB',
    '2022050112': 'CesarDB',
}

colors_map = dict(
    obs                   = "snow",
    SAFRAN                = "silver",
    SAFRAN_perturb        = "silver",
    SAFRAN_perturb_assim  = "silver",
    KRIGING               = "indigo",
    KRIGING_perturb       = "indigo",
    KRIGING_perturb_assim = "indigo",
    ANTILOPE              = "#D65F5F",
    RS                    = "#4878D0",
    SRS                   = "#D65F5F",
    SRS28                 = "#6ACC64",
    EnKF                  = "#6ACC64",
    PF                    = "#EE854A",
    RS_assim              = "#4878D0",
    SRS_assim             = "#D65F5F",
    SRS_assim_err025      = 'darkgreen',
    EnKF_assim            = "#6ACC64",
    PF_assim              = "#EE854A",
)
