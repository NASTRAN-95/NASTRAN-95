#!/usr/bin/env python3
"""
Camfort does not like Hollerith constants, so we hide them
using integer encoding before Camfort processing,
and decode back afterwards.
"""
import os
import json
import sys

# Load Hollerith-integer dict from a json file
with open('hollerith.json', 'r') as file:
    # Load the JSON data into a dictionary
    mapping = json.load(file)

# Restore all occurences of Hollerith constants
# in the Fortran sources encoded with their corresponding integer value
def restore_in_fortran_file(file_path, mapping):
    print(f'Updating file {file_path}...')

    with open(file_path, 'r') as file:
        file_contents = file.read()

    for num, h in mapping.items():
        old_string = str(num)
        new_string = h
        file_contents = file_contents.replace(old_string, new_string)

    with open(file_path, 'w') as file:
        file.write(file_contents)

file_path = sys.argv[1]
restore_in_fortran_file(file_path, mapping)

