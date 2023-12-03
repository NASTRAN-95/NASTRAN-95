# Gather together all different representations of the same
# module created from the same COMMON block across multiple source files.

import os
import re
import sys
import subprocess

# TODO Can we get a special key shortcut to quickly switch
# between items?
# We need the following keys:
# NEXT problem
# PREV problem
# STOP and exit
# SUBMIT if validation passes for all concerned sources, release the module ONLY COPY (+copy the changed sources) and git-push the change

# Replacement of system to run the command,
# and optionally capture its output
def system(cmd, capture=False, tokenize=False):
    if capture:
        output = subprocess.check_output(cmd, shell=True)
        output = output.decode("utf-8")
        if tokenize:
            return ' ' + re.sub('\n', ' ', output).strip() + ' '
        return output
    
    err = os.system(cmd)
    if err != 0:
        raise Exception(f'os.system({cmd}) returned error code {err}')

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

def create_playground_for_module(module):
    # Create a temp folder with symbolic links to all sources,
    # prefixed with the index of representation.
    # Create a compile.sh script to validate the compilation of
    # all sources.
    system('rm -rf ./process_modules_temp')
    system('mkdir ./process_modules_temp')
    compile_sh = '#!/bin/bash\n'
    compile_sh += 'set -e\n'
    for function_names in module:
        for function_name in function_names:
            linkname = re.sub('\/', '_', function_name)
            filename = re.sub('.*\/', '', function_name)
            system(f'ln -s ../{folder_path}/{function_name}/{filename}.f90 ./process_modules_temp/{linkname}.f90')
            compile_sh += 'gfortran -Wall -ffree-line-length-none -w -fno-range-check -fPIC '
            compile_sh += '-fno-automatic -fcray-pointer -std=legacy -fallow-invalid-boz -J../include '
            compile_sh += f'`ls ../{folder_path}/{function_name}/c_*.f90 2>/dev/null` '
            compile_sh += f'`ls ../{folder_path}/{function_name}/done_c_*.f90 2>/dev/null` '
            compile_sh += f'`ls ../{folder_path}/{function_name}/{filename}.f90 2>/dev/null` '
            compile_sh += f'-shared -o lib{filename}.so\n'
            compile_sh += f'echo "VERIFIED OK"\n'
    with open('./process_modules_temp/compile.sh', "w") as screenrc_file:
        screenrc_file.write(compile_sh)
    system('chmod +x ./process_modules_temp/compile.sh')

def validate_playground():
    while True:
        output = system('cd ./process_modules_temp && ./compile.sh', capture=True)
        if re.search('VERIFIED\sOK', output):
            break
        
        # TODO Open interactive session for fixing the compile errors.
        screenrc = f"""
            chdir ./process_modules_temp
            screen -t bash -i ./compile.sh | moar
            focus
        """

        with open('./process_modules_temp/.screenrc', "w") as screenrc_file:
            screenrc_file.write(screenrc)

        # Execute interactive session
        # Emergency stop in case of a bug:
        # killall -9 python3 && killall -9 screen
        system(f'screen -c ./process_modules_temp/.screenrc')

if __name__ == "__main__":
    representations = 0
    if len(sys.argv) > 1:
        representations = int(sys.argv[1])

    folder_path = "./source"
    modules = load_modules(folder_path)

    for module_name in modules.keys():
        print(f'{module_name} has {len(modules[module_name].keys())} representations:')
        if (len(modules[module_name].keys()) == 1) and (representations == 1):
            # If module has only one representation, it means that
            # it is used by only one function, and could not have any conflicts
            # with other functions that may define (represent) the same module
            # differently. Or the module is used by multiple functions, but they
            # all agree on the same representation.
            create_playground_for_module(modules[module_name].values())
        
            # Validate the source code readiness by compiling the playground
            validate_playground()

            extra_note = ''
            function_names = list(modules[module_name].values())[0]
            for function_name in function_names:
                library = re.sub('/[^\/]+', '/', function_name)
                system(f'cp {folder_path}/{function_name}/c_{module_name}.f90 {library}')
                system(f'git add {library}/c_{module_name}.f90')
                system(f'cd {folder_path}/{function_name} && git mv c_{module_name}.f90 done_c_{module_name}.f90')

                # If there are no more modules to process for the source,
                # copy the sources as well, and remove the folder.
                if not re.search('\sc_[^\s]+\.f90\s', system(f'ls {folder_path}/{function_name}/', capture=True, tokenize=True)):
                    print("No more modules for function {function_name}, release it")
                    filename = re.sub('.*\/', '', function_name)
                    system(f'cp {folder_path}/{function_name}/{filename} {library}/')
                    system(f'git add {library}/{filename}')
                    system(f'git rm -rf {folder_path}/{function_name}')
                    
                    if extra_note == '':
                        extra_note = ', releasing completed file(s): '
                    else:
                        extra_note += ', '
                    
                    extra_note += function_name
                 
            system(f'git commit -m "Cleaning up COMMON block module {module_name}{extra_note}"')

        elif (len(modules[module_name].keys()) == 2) and (representations == 2):
            # If the module has more than 1 representation, then they all must
            # be merged into one common representation manually. That is, for two
            # representations, we will inspect the differences between them,
            # with respect to the source files below.
            function_names = list(modules[module_name].values())
            module_file1 = f'{folder_path}/{function_names[0][0]}/c_{module_name}.f90'
            module_file2 = f'{folder_path}/{function_names[1][0]}/c_{module_name}.f90'
            
            create_playground_for_module(modules[module_name].values())
            
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

            # Execute interactive session
            # Emergency stop in case of a bug:
            # killall -9 python3 && killall -9 screen
            system(f'screen -c ./process_modules_temp/.screenrc')

        for function_names in modules[module_name].values():
            print('\t' + ', '.join(function_names))

