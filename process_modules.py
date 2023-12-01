# Gather together all different representations of the same
# module created from the same COMMON block across multiple source files.

import os
import re
import sys
from pygments import highlight
from pygments.lexers import FortranLexer
from pygments.formatters import TerminalFormatter

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

def colorize_fortran_code(code):
    lexer = FortranLexer()
    formatter = TerminalFormatter()
    colored_code = highlight(code, lexer, formatter)
    return colored_code

folder_path = "./source"
modules = load_modules(folder_path)

single_representation = 0
two_representations = 0
for module_name in modules.keys():
    print(f'{module_name} has {len(modules[module_name].keys())} representations:')
    if len(modules[module_name].keys()) == 1:
        single_representation += 1
        function_names = list(modules[module_name].values())[0]
        for function_name in function_names:
            library = re.sub('/[^\/]+', '/', function_name)
            # TODO Validate by compiling the source folder with a Makefile
            # TODO Do not remove - copy the module, so that the source could still be compiled!
            # TODO Copy the sources as well
            # TODO Remove the source folder, only if all modules for it are a single representation
            os.system(f'mv {folder_path}/{function_name}/c_{module_name}.f90 {library}')
            os.system(f'git rm {folder_path}/{function_name}/c_{module_name}.f90')
    elif len(modules[module_name].keys()) == 2:
        for module_content in modules[module_name].keys():
            print(colorize_fortran_code(module_content))
        function_names = list(modules[module_name].values())
        
        # We will inspect the differences between the following two modules,
        # with respect to the source files below
        module_file1 = f'{folder_path}/{function_names[0][0]}/c_{module_name}.f90'
        module_file2 = f'{folder_path}/{function_names[1][0]}/c_{module_name}.f90'
        
        # Create a temp folder with symbolic links to all sources,
        # prefixed with the index of representation.
        # Create a compile.sh script to validate the compilation of
        # all sources.
        os.system('rm -rf ./process_modules_temp')
        os.system('mkdir ./process_modules_temp')
        compile_sh = '#!/bin/bash\n'
        compile_sh += 'set +e\n'
        for function_names in modules[module_name].values():
            for function_name in function_names:
                linkname = re.sub('\/', '_', function_name)
                filename = re.sub('.*\/', '', function_name)
                os.system(f'ln -s ../{folder_path}/{function_name}/{filename}.f90 ./process_modules_temp/{linkname}.f90')
                compile_sh += 'gfortran -Wall -J../include '
                compile_sh += f'../{folder_path}/{function_name}/{filename}/c_*.f90 '
                compile_sh += f'../{folder_path}/{function_name}/{filename}.f90 '
                compile_sh += f'-o lib{filename}.so\n'
        with open('./process_modules_temp/compile.sh', "w") as screenrc_file:
            screenrc_file.write(compile_sh)
        os.system('chmod +x ./process_modules_temp/compile.sh')
        
        # Create screen or tmux with two modules split on the left and
        # "mc" browsing the sources links folder on the right.
        screenrc = f"""
            mousetrack on
            defmousetrack on
            screen -t name1 vim {module_file1}
            split -v
            focus
            chdir ./process_modules_temp
            screen -t sources mc
            focus
            split
            focus
            chdir ..
            screen -t name2 vim {module_file2}
            focus
        """
        with open('./process_modules_temp/.screenrc', "w") as screenrc_file:
            screenrc_file.write(screenrc)
        # Emergency stop in case of a bug:
        # killall -9 python3 && killall -9 screen
        os.system(f'screen -c ./process_modules_temp/.screenrc')

        # TODO Can we get a special key shortcut to quickly switch
        # between items?
        # We need the following keys:
        # NEXT problem
        # PREV problem
        # STOP and exit
        # SUBMIT if validation passes for all concerned sources, release the module ONLY COPY (+copy the changed sources) and git-push the change
        two_representations += 1
    for function_names in modules[module_name].values():
        print('\t' + ', '.join(function_names))
print(f'{two_representations}/{len(modules.keys())} modules with two representations can possibly be fixed')
print(f'{single_representation}/{len(modules.keys())} modules with single representation are good to be deployed')
