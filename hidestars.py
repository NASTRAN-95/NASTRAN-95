#!/usr/bin/env python3
"""
Camfort does not like stars (*) in the subroutine arguments,
so we hide them temporary.
"""
import re
import sys

def remove_comma_values(string):
    match = re.match(r'\s*(SUBROUTINE|ENTRY|FUNCTION)\s[^\(]+\(([^\)]*)\)', string)
    if not match:
        return string

    # Find comma-separated values
    csv = match.group(2)
    pattern = r'([^\,]+)'
    values = re.findall(pattern, csv)

    # Remove values that are equal to *
    has_star = False
    filtered_values = []
    for value in values:
        if value.strip() == '*':
            has_star = True
        else:
            filtered_values.append(value.strip())

    if not has_star:
        return string

    new_csv = ','.join(filtered_values)

    # Write new comma-separated values,
    # hide the original ones after the comment
    string = string.replace(csv, f'{new_csv}) !HIDESTARS ({csv}')
    return string

filename = sys.argv[1]

print(f'Updating file {filename}...')

file_contents=''
with open(filename, 'r') as file:
    for line in file:
        file_contents += remove_comma_values(line)

with open(filename, 'w') as file:
    file.write(file_contents)

