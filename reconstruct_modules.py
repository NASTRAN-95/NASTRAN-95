# Use crackfortran to construct the exact layout of the COMMON block:
# * symbol sizes and offsets from the start of common block
# * all possible alternating / overlapping symbols with their,
#   sizes and offsets, gathered from all objects
# * find the most popular layout accross all users
# * determine the possible datatypes (we can't get data types
#   from object, can we still get them from the assembly?)

# TODO Final target:
# 0) Record the source file names that are using the common block
# 1) Emit one module that incorporates all representations in terms of sizes
# 2) Use equivalence of types (sort of C unions) to express the differences in implementations
#    as shown here: https://stackoverflow.com/a/14734291/4063520
# 3) Choose the datatype from all alternatives based on the actual use of type
#    If more than one type is used (e.g. Icore/Dcore), create a C_LOC/C_FORTRAN_PTR
#    pointer alias.
# 4) Emit '=>' renames in module uses for sources where vars are named differently
# 5) One remaining difficulty: do we need to support any initializations,
#    except moving over the data statements?

import json
import os
import re
import sys
import colorama
from numpy.f2py.crackfortran import main as crackfortran

def run_crackfortran():
    path = '/home/marcusmae/nasa/branches/nastran_f90'

    bd = os.popen(f'find {path}/bd -name "*.f90"').read().splitlines()
    mds = os.popen(f'find {path}/mds -name "*.f90"').read().splitlines()
    mis = os.popen(f'find {path}/mis -name "*.f90"').read().splitlines()

    sources = bd + mds + mis

    # Parse all *.f90 source files with crackfortran.
    parsed = {}
    for source in sources:
        print(source)
        sys.argv = [ 'crackfortran', '-i', f'{path}/include', source ]
        parsed[source] = crackfortran()
        
        # Fixes circular dependency due to 'parent_block' = block in
        # '/home/marcusmae/nasa/branches/nastran_f90/mis/inptt5.f90'
        #if len(parsed[source][0]['body']) != 0:
        #    if 'parent_block' in parsed[source][0]['body'][0]:
        #        del parsed[source][0]['body'][0]['parent_block']
        #
        # '/home/marcusmae/nasa/branches/nastran_f90/mis/pload3.f90'
        # still fails

    with open("crackfortran.json", 'w') as json_file:
        json.dump(parsed, json_file, indent=4)

    print(f'{len(parsed.keys())} source files successfully parsed')

def extract_common_blocks():
    with open('crackfortran.json') as file:
        parsed = json.load(file)

    extracted_commons = {}
    for source, blocks in parsed.items():
        for block in blocks:
            if not 'common' in block:
                continue
            commons = block['common']
            decls = block['vars']
            for common, names in commons.items():
                for i, name in enumerate(names):
                    names[i] = { name : decls[name] }
                if not common in extracted_commons:
                    extracted_commons[common] = []
                if not (names in extracted_commons[common]):
                    extracted_commons[common].append(names)

    with open("commons.json", 'w') as json_file:
        json.dump(extracted_commons, json_file, indent=4)

    print(f'{len(extracted_commons)} common blocks successfully parsed:')
    stats = {}
    for common, representations in extracted_commons.items():
        if not (len(representations) in stats):
            stats[len(representations)] = 0
        stats[len(representations)] += 1

    for representations, count in stats.items():
        print(f'{count} common blocks have {representations} versions')

if __name__ == "__main__":
    representations = 0
    if len(sys.argv) < 2:
        print('Command line argument is required')
        sys.exit(1)
    
    if sys.argv[1] == '-crackfortran':
        run_crackfortran()
    elif sys.argv[1] == '-commons':
        extract_common_blocks()
    else:
        print(f'Unknown command line argument: {sys.argv[1]}')
        sys.exit(1)

