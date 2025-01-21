#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def members_map(xpid):
    if 'assim' in xpid or 'full_feedback' in xpid:
        return [mb for mb in range(1, 18)]
    else:
        return [mb for mb in range(17)]


def product_map(xpid):

    mapping = dict(
        RS27_pappus              = 'ASANTILOPE',
        EnKF36_pappus            = 'EnKF',
        PF32_pappus              = 'PF',
        RS27_sorted_pappus       = 'SRS',
        RS27_perturb             = 'ASANTILOPE_perturb',
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
        AROME                    = 'AROME',
        AROME_perturb            = 'AROME_perturb',
        AROME_perturb_assim      = 'AROME_perturb_assim',
        RS27_LHR                 = 'ASANTILOPE_LHRM',
        RS27_sorted_feedback     = 'SRS_feedback',
        RS27_sorted_assim_feedback    = 'SRS_assim_feedback',
        RS27_sorted_full_feedback     = 'SRS_full_feedback',
        RS27_sorted_feedback_extended = 'SRS_feedback',
    )
    if xpid in mapping.keys():
        return mapping[xpid]
    else:
        return f'S{xpid}'


xpid_map = {
    '2018012312': 'CesarDB',
    '2018031612': 'CesarDB',
    '2019051312': 'CesarDB_AngeH',
    '2022022612': 'CesarDB',
    '2022050112': 'CesarDB',
}

vmax_map = {
    '2018012312': 3,
    '2018031612': 4,
    '2019051312': 3,
    '2022022612': 3,
    '2022050112': 2,
}

# Colors from "Paired" color palette
colors_map = dict(
    obs = "silver",
    SAFRAN                = "#1f78b4",
    SAFRAN_perturb        = "#1f78b4",
    SAFRAN_perturb_assim  = "#1f78b4",
    KRIGING               = "#e31a1c",
    KRIGING_perturb       = "#e31a1c",
    KRIGING_perturb_assim = "#e31a1c",
    ANTILOPE              = "#b2df8a",
    ASANTILOPE_perturb   = "#b2df8a",
    ASANTILOPE            = "#33a02c",
    SRS                   = "#33a02c",
    SRS_assim             = "#33a02c",
    EnKF                  = "#6a3d9a",
    EnKF_assim            = "#6a3d9a",
    PF                    = "#cab2d6",
    PF_assim              = "#cab2d6",
    SRS37                 = "red",
    SRS27_LPNp200         = "red",
    SRS_feedback          = "red",
    AROME                 = "#6a3d9a",
    AROME_perturb         = "#6a3d9a",
    AROME_perturb_assim   = "#6a3d9a",
    SRS_assim_feedback    = "#6a3d9a",
    ASANTILOPE_LHRM       = "#1f78b4",
    SRS_full_feedback     = "#1f78b4",
)


#colors_map = dict(
#    obs                   = "snow",
#    SAFRAN                = "silver",
#    SAFRAN_perturb        = "silver",
#    SAFRAN_perturb_assim  = "silver",
#    KRIGING               = "indigo",
#    KRIGING_perturb       = "indigo",
#    KRIGING_perturb_assim = "indigo",
#    ANTILOPE              = "#D65F5F",
#    RS                    = "#D65F5F",
#    ASANTILOPE            = "#4878D0",
#    SRS                   = "#4878D0",
#    SRS27_LPNp200         = "#6ACC64",  # Green
#    SRS_perturb           = "#6ACC64",
#    SRS30                 = "#6ACC64",
#    SRS35                 = "#EE854A",
#    SRS32                 = "darkgreen",
#    SRS34                 = "#D65F5F",
#    SRS28                 = "#6ACC64",
#    SRS37                 = "#6ACC64",
#    EnKF                  = "#D65F5F",
#    PF                    = "#EE854A",
#    RS_assim              = "#4878D0",
#    SRS_assim             = "#4878D0",
#    SRS_assim_err025      = 'darkgreen',
#    EnKF_assim            = "#6ACC64",
#    PF_assim              = "#EE854A",
#)
