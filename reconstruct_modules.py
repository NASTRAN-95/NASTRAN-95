# Use crackfortran to construct the exact layout of the COMMON block:
# * symbol sizes and offsets from the start of common block
# * all possible alternating / overlapping symbols with their,
#   sizes and offsets, gathered from all objects
# * find the most popular layout accross all users
# * determine the possible datatypes (we can't get data types
#   from object, can we still get them from the assembly?)

# TODO Final target:
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
from functools import reduce

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
    extracted_sources = {}
    for source, blocks in parsed.items():
        for block in blocks:
            if not 'common' in block:
                continue
            commons = block['common']
            decls = block['vars']
            for common, names in commons.items():
                for i, name in enumerate(names):
                    names[i] = ( name, decls[name] )
                if not common in extracted_commons:
                    extracted_commons[common] = []
                if not (names in extracted_commons[common]):
                    extracted_commons[common].append(names)
                if not (common in extracted_sources):
                    extracted_sources[common] = []
                extracted_sources[common].append(source)

    with open("commons.json", 'w') as json_file:
        json.dump(extracted_commons, json_file, indent=4)
    with open("sources.json", 'w') as json_file:
        json.dump(extracted_sources, json_file, indent=4)

    print(f'{len(extracted_commons)} common blocks successfully parsed:')
    stats = {}
    for common, representations in extracted_commons.items():
        if not (len(representations) in stats):
            stats[len(representations)] = 0
        stats[len(representations)] += 1

    for representations, count in stats.items():
        print(f'{count} common blocks have {representations} versions')

def sizeof(typ):
    if typ == 'integer':
        size = 4
    elif typ == 'real':
        size = 4
    elif typ == 'complex':
        size = 8
    elif typ == 'double precision':
        size = 8
    elif typ == 'character':
        size = 1
    elif typ == 'logical':
        size = 1
    else:
        raise Exception(f'Unknown type {typ}')
    return size

def align_common_blocks():
    with open('commons.json') as file:
        commons = json.load(file)

    for common, names_variants in commons.items():
        # Build histogram of size prefix sums.
        prefix_hist = {}
        total_sizes = [0] * len(names_variants)
        for i, names in enumerate(names_variants):
            for j, (name, decl) in enumerate(names):
                # Record the offset of the var.
                names_variants[i][j] = (name, decl, total_sizes[i])

                typ = decl['typespec']
                size = sizeof(typ)
                if 'dimension' in decl:
                    size *= int(reduce(lambda x, y: int(x) * int(y), decl['dimension']))
                total_sizes[i] += size
                if not (total_sizes[i] in prefix_hist):
                    prefix_hist[total_sizes[i]] = 0
                prefix_hist[total_sizes[i]] += 1
                
        # Get histogram keys with values equal to the
        # number of variants (the common alignments).
        alignments = {}
        alignments[0] = len(names_variants)
        for k, v in prefix_hist.items():
            if v == len(names_variants):
                alignments[k] = v
        alignments[max(total_sizes)] = len(names_variants)
        alignments = sorted(alignments.keys())
        
        # Prepare new containers for common variables
        # grouped by alignments.
        new_names_variants = [None] * len(names_variants)
        for i in range(len(names_variants)):
            new_names_variants[i] = [list()] * len(alignments)
        for i, names in enumerate(names_variants):
            for j, (name, decl, offset) in enumerate(names):
                decl['name'] = name
                for j in range(len(alignments)):
                    if offset < alignments[j]:
                        group = j - 1
                new_names_variants[i][group].append(decl)

        commons[common] = new_names_variants

    with open("commons_aligned.json", 'w') as json_file:
        json.dump(commons, json_file, indent=4)

    print(f'{len(commons)} common blocks successfully aligned')


def emit_modules():
    with open('modules.json') as file:
        commons = json.load(file)

    for module, names_variants in module.items():
        print(f'MODULE {module}')
        print(f'END MODULE {module}')
            
if __name__ == "__main__":
    representations = 0
    if len(sys.argv) < 2:
        print('Command line argument is required')
        sys.exit(1)
    
    if sys.argv[1] == '-crackfortran':
        run_crackfortran()
    elif sys.argv[1] == '-commons':
        extract_common_blocks()
    elif sys.argv[1] == '-align':
        align_common_blocks()
    elif sys.argv[1] == '-emit':
        emit_modules()
    else:
        print(f'Unknown command line argument: {sys.argv[1]}')
        sys.exit(1)

