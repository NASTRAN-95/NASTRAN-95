# Use compiled objects of the reference version to construct
# the exact layout of the COMMON block:
# * symbol sizes and offsets from the start of common block
# * all possible alternating / overlapping symbols with their,
#   sizes and offsets, gathered from all objects
# * find the most popular layout accross all users
# * determine the possible datatypes (we can't get data types
#   from object, can we still get them from the assembly?)

import json
import os
import re
import sys
import colorama

path = '/home/marcusmae/nasa/branches/nastran_f90'

bd = os.popen(f'find {path}/bd -name "*.f90"').read().splitlines()
mds = os.popen(f'find {path}/mds -name "*.f90"').read().splitlines()
mis = os.popen(f'find {path}/mis -name "*.f90"').read().splitlines()

sources = bd + mds + mis

# Parse all *.f90 source files with crackfortran.
parsed = {}
for source in sources:
    crackfortran = os.popen(f'crackfortran {source} -show').read()
    json_str = re.sub('[^\[]+\[', '[', crackfortran, 1, flags=re.MULTILINE | re.DOTALL)
    
    # Thease are to be reported to crackfortran as bugs
    #json_str = re.sub('\'', '\"', json_str)
    #json_str = re.sub('"implicit": None', '"implicit": "None"', json_str)

    
    try:
        parsed[source] = json.loads(json_str)
    except json.JSONDecodeError as e:
        match = re.search(r'\(char\s(\d+)\)', str(e))
        if match:
            pos = int(match.group(1))
            colorama.init()
            highlighted_json = \
                json_str[max(0, pos - 50):max(0, pos - 1)] + \
                colorama.Fore.RED + json_str[pos] + colorama.Fore.RESET + \
                json_str[min(len(json_str), pos + 1):min(len(json_str), pos + 50)]
            print(highlighted_json)
        print(f'In file {source}:\n{e}')
        print(json_str)
        sys.exit(-1)

print(f'{len(parsed.keys())} source files successfully parsed')
