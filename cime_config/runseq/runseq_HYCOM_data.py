#!/usr/bin/env python

import os, shutil, sys

_CIMEROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..","..","..","..")
sys.path.append(os.path.join(_CIMEROOT, "scripts", "Tools"))

from standard_script_setup import *
#pylint: disable=undefined-variable
logger = logging.getLogger(__name__)

def gen_runseq(case, coupling_times):

    rundir    = case.get_value("RUNDIR")
    caseroot  = case.get_value("CASEROOT")

    atm_cpl_dt = coupling_times["atm_cpl_dt"]
    ocn_cpl_dt = coupling_times["ocn_cpl_dt"]

    outfile   = open(os.path.join(caseroot, "CaseDocs", "nuopc.runseq"), "w")

    if case.get_value("CONTINUE_RUN") or case.get_value("MEDIATOR_READ_RESTART"):
        logger.info("NUOPC run sequence: warm start (concurrent)")

    else:
        logger.info("NUOPC run sequence: cold start (sequential)")
        outfile.write ("runSeq::                                \n")
        outfile.write ("@" + str(ocn_cpl_dt) + "                \n")
        outfile.write ("   @" + str(atm_cpl_dt) + "             \n")
        outfile.write ("     ATM                                \n")
        outfile.write ("   @                                    \n")
        outfile.write ("   OCN                                  \n")
        outfile.write ("@                                       \n")
        outfile.write ("::                                      \n")

    outfile.close()
    shutil.copy(os.path.join(caseroot, "CaseDocs", "nuopc.runseq"), rundir)
