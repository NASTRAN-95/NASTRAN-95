#!/usr/bin/env python3
"""
Camfort does not like stars (*) in the subroutine arguments,
so we hide them temporary and reveal again after Camfort processing.
"""
import re
import sys

def restore_comma_values(string):
    match = re.match(r'\s*(SUBROUTINE|ENTRY|FUNCTION)\s[^\(]+\(([^\)]*)\)', string)
    if not match:
        return string
    
    # Find comma-separated values
    csv = match.group(2)

    # Restore old comma-separated values from the comment
    string = string.replace(f'{csv}) !HIDESTARS ', '')
    return string

filename = sys.argv[1]

print(f'Updating file {filename}...')

file_contents=''
with open(filename, 'r') as file:
    for line in file:
        file_contents += restore_comma_values(line)

with open(filename, 'w') as file:
    file.write(file_contents)

