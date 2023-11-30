# Gather together all different representations of the same
# module created from the same COMMON block across multiple source files.

import os
import re

def load_modules(folder):
    modules = {}

    # For each subdirectory of the given folder:
    for root, dirs, files in os.walk(folder):
        for file in files:
            # If folder contains a c_*.f90 file:
            if file.startswith("c_") and file.endswith(".f90"):
                function_name = re.sub('.*\/', '', root)
                module_name = re.sub('^c_', '', re.sub('\.f90', '', file))
                file_path = os.path.join(root, file)
                with open(file_path, "r") as f:
                    content = f.read()
                    if not module_name in modules:
                        modules[module_name] = {}
                    if not content in modules[module_name]:
                        modules[module_name][content] = []
                    modules[module_name][content].append(function_name)
    return modules

folder_path = "./source"
modules = load_modules(folder_path)

for module_name in modules.keys():
    print(f'{module_name} has {len(modules[module_name].keys())} representations:')
    for function_names in modules[module_name].values():
        print('\t' + ', '.join(function_names))
