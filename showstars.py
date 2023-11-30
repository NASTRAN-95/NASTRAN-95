#!/usr/bin/env python3
"""
Camfort does not like stars (*) in the subroutine arguments,
so we hide them temporary and reveal again after Camfort processing.
"""
import re
import sys

filename = sys.argv[1]

print(f'Updating file {filename}...')

file_contents=''
with open(filename, 'r') as file:
    for line in file:
        file_contents += line

file_contents = re.sub('\(.*?\n?.*?\!HIDESTARS\s*', '', file_contents, re.S)

with open(filename, 'w') as file:
    file.write(file_contents)

