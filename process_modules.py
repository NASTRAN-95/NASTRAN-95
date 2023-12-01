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
                function_name = re.sub(f'{folder}\/', '', root)
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

single_representation = 0
for module_name in modules.keys():
    print(f'{module_name} has {len(modules[module_name].keys())} representations:')
    if len(modules[module_name].keys()) == 1:
        single_representation += 1
        function_names = list(modules[module_name].values())[0]
        for function_name in function_names:
            library = re.sub('/[^\/]+', '/', function_name)
            os.system(f'mv {folder_path}/{function_name}/c_{module_name}.f90 {library}')
            os.system(f'git rm {folder_path}/{function_name}/c_{module_name}.f90')
    for function_names in modules[module_name].values():
        print('\t' + ', '.join(function_names))
print(f'{single_representation}/{len(modules.keys())} modules with single representation are good to be deployed')
