# Use crackfortran to construct the exact layout of the COMMON block:
# * symbol sizes and offsets from the start of common block
# * all possible alternating / overlapping symbols with their,
#   sizes and offsets, gathered from all objects
# * find the most popular layout accross all users
# * determine the possible datatypes

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

    sources = mds # bd + mds + mis

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
        #
        # Note that for now we employ a dirty fix instead: the parent_block
        # assignment is commented out in the crackfortan itself

    with open("crackfortran.json", 'w') as json_file:
        json.dump(parsed, json_file, indent=4)

    print(f'{len(parsed.keys())} source files successfully parsed')

def extract_common_blocks():
    with open('crackfortran.json') as file:
        parsed = json.load(file)

    # The code below creates the following data structure:
    # * A set of possible representations of a COMMON block,
    #   each representation is a tuple of variable declarations
    #   (only few properties such as name, type and dimensions)
    # * A set of routine name and source file name pairs,
    #   where each unique COMMON block representation have been seen.
    # * The COMMON representations are assigned with indexes to make
    #   keys for a separate routine-source index. These indexes are purely
    #   artificial, and are used to avoid having COMMON blocks representations
    #   as keys themselves at a later step.
    commons_seen_in_code = {}
    commons_representations = {}
    nkeys = 0
    for source, blocks in parsed.items():
        for block in blocks:
            if not 'common' in block:
                continue
            commons = block['common']
            decls = block['vars']
            routine = block['name']
            for common, names in commons.items():
                for i, name in enumerate(names):
                    # Keep only declaration properties we are interested in.
                    decl = {
                        'name' : name,
                        'typespec' : decls[name]['typespec']
                    }
                    if 'dimension' in decls[name]:
                        decl['dimension'] = tuple(decls[name]['dimension'])

                    # Tupling for use as keys.
                    names[i] = tuple(decl.items())

                if not common in commons_seen_in_code:
                    commons_seen_in_code[common] = {}

                # Tupling for use as keys.
                common_representation = tuple(names)

                if not (common in commons_representations):
                    commons_representations[common] = {}
                if not (common_representation in commons_representations[common]):
                    commons_representations[common][common_representation] = nkeys
                    commons_seen_in_code[common][nkeys] = []
                    nkeys += 1

                # Refer a routine-source pair, where this COMMON representation
                # have been seen.
                commons_seen_in_code[common][commons_representations[common][common_representation]].append((routine, source))

    print(f'{len(commons_seen_in_code)} common blocks successfully parsed:')
    stats = {}
    for common, representations in commons_seen_in_code.items():
        if not (len(representations) in stats):
            stats[len(representations)] = 0
        stats[len(representations)] += 1

    for representations, count in stats.items():
        print(f'{count} common blocks have {representations} versions')

    # Swap the keys and values before writing out,
    # because in JSON tuple cannot be a dict key.
    # Furthermore, we will need to modify the keys
    # in the next steps.
    for common, common_representations in commons_representations.items():
        commons_representations[common] = dict((v,k) for k,v in common_representations.items())
    commons_seen_in_code['keys'] = commons_representations

    with open("commons.json", 'w') as json_file:
        json.dump(commons_seen_in_code, json_file, indent=4)

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

# Locate the most common alignments between the var offsets of
# different representations of the same COMMON block.
# This way we prepare a waybill of how one unified representation
# of COMMON block module should be sequenced: as unions within the
# aligned segments, and as plain vars -- for fully matched simple
# type vars alignment (of the same type).
def align_common_blocks():
    with open('commons.json') as file:
        commons = json.load(file)
    
    # Since JSON serializes all tuples as lists,
    # we have to manually re-create the tuples from loaded lists.
    keys = {}
    for common, values in commons['keys'].items():
        if not (common in keys):
            keys[common] = {}
        for index, list_key in values.items():
            dict_key = [list()] * len(list_key)
            for i, decl in enumerate(list_key):
                for j, attr in enumerate(decl):
                    if type(attr[1]) is list:
                        attr[1] = tuple(attr[1])
                    dict_key[i].append(tuple(attr))
                dict_key[i] = tuple(dict_key[i])

            # Note: the keys and values remain swapped, as it is
            # much easier to handle them this way in the algo below.            
            keys[common][index] = dict_key
    del commons['keys']

    for common, common_keys in keys.items():
        # Build histogram of size prefix sums.
        prefix_hist = {}
        total_sizes = [0] * len(common_keys)
        for i, (index, key) in enumerate(common_keys.items()):
            for j, decl in enumerate(key):
                # Record the offset of the var.
                keys[common][index][j] = (decl, total_sizes[i])

                decl = dict(decl)
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
        alignments[0] = len(common_keys)
        for k, v in prefix_hist.items():
            if v == len(common_keys):
                alignments[k] = v
        alignments[max(total_sizes)] = len(common_keys)
        alignments = sorted(alignments.keys())
        
        # Prepare new containers for common variables
        # grouped by alignments.
        for (index, key) in common_keys.items():
            new_common_keys = [list()] * len(alignments)
            for (decl, offset) in key:
                for j in range(len(alignments)):
                    if offset < alignments[j]:
                        group = j - 1
                new_common_keys[group].append(decl)

            commons[common][index] = new_common_keys

    with open("commons_aligned.json", 'w') as json_file:
        json.dump(commons, json_file, indent=4)

    print(f'{len(commons)} common blocks successfully aligned')

def optimize_common_blocks():
    with open('commons_aligned.json') as file:
        commons = json.load(file)

    # Transpose to have representations inside groups,
    # instead of groups in representations.
    transposed_commons = {}
    for common, names_variants in commons.items():
        ngroups = len(names_variants[0])
        transposed_common = [list()] * ngroups
        for i, groups in enumerate(names_variants):
            for j, group in enumerate(groups):
                transposed_common[j].append(group)

        transposed_commons[common] = transposed_common

    # TODO Print first common as an example.

    sys.exit(1)

    # For each decl make a dict of all unique names.
    # The most popular name should become a module name,
    # others should be used as aliases.
    for common, groups in transposed_commons.items():
        for group in groups:
            pass

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
    elif sys.argv[1] == '-optimize':
        optimize_common_blocks()
    elif sys.argv[1] == '-emit':
        emit_modules()
    else:
        print(f'Unknown command line argument: {sys.argv[1]}')
        sys.exit(1)

