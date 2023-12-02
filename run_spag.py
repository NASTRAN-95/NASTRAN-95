#!/usr/bin/env python3
import os
import re
import subprocess
import unittest

cmd = "/home/marcusmae/nasa/spag/spag"
cmd761 = "/home/marcusmae/nasa/spag-7.61/spag"

# Run SPAG command, returning error code and True/False
# if it has crashed or not.
def run_spag(cmd):
    try:
        output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        output = output.decode("utf-8")
        if re.search('forrtl\:\ssevere', output):
            return 0, True
        else:
            return 0, False
    except subprocess.CalledProcessError as e:
        output = e.output.decode("utf-8")
        if re.search('forrtl\:\ssevere', output):
            return 0, True
        else:
            return 0, False

def run_spag_for_files(files):
    cmd_errors = 0
    cmd_failures = 0
    cmd761_errors = 0
        
    for file in files:
        name = file.replace('.f90', '')
        cmd_error = 0
        cmd_failure = 0
        errcode, crashed = run_spag([cmd, file])
        if crashed:
            cmd_error = 1
            cmd_errors += 1
        else:
            os.system(f"mkdir -p source/{name} && rm -rf source/{name}")
            os.system(f"mv SPAGged source/{name}")
            has_commons = os.popen(f"cd source/{name} && grep COMMON *").read()
            if "COMMON" in has_commons and "!COMMON" not in has_commons:
                cmd_failure = 1
                cmd_failures += 1
        
        if cmd_error or cmd_failure:
            # Re-run with SPAG 7.61
            errcode, crashed = run_spag([cmd761, file])
            if crashed:
                # Get back to the original SPAG, because 7.61 crashes:
                os.system(f"{cmd} {file}")
                os.system(f"mkdir -p source/{name} && rm -rf source/{name}")
                os.system(f"mv SPAGged source/{name}")
                
                cmd761_errors += 1
            else:
                if file == "mis/t3bgbs.f90":
                    raise Exception(f"{file} should crash SPAG 7.61, but it did not, why?")
            
                os.system(f"mv SPAGged/* source/{name}/")
                os.system("rm -rf SPAGged")

    print("Statistics:")
    print(f"SPAG returned an error or crashed {cmd_errors} times")
    print(f"SPAG failed to remove COMMONs {cmd_failures} times")
    print(f"SPAG 7.61 returned an error or crashed {cmd761_errors} times")

    still_has_commons = os.popen('cd source && grep "^\\s*COMMON /" * -R').read()
    if "COMMON" in still_has_commons:
        filenames = re.findall(r"\b\w+\.f90\b", still_has_commons)
        unique_filenames = list(set(filenames))
        print(f'The following {len(unique_filenames)} files still have COMMON blocks that SPAG and SPAG 7.61 were not able to eliminate:')
        print(still_has_commons)

class TestSPAG(unittest.TestCase):
    def test_spag_crash(self):
        file = "mis/t3bgbs.f90"
        errcode, crashed = run_spag([cmd, file])
        self.assertTrue(not crashed)
    
    def test_spag_761_crash(self):
        file = "mis/t3bgbs.f90"
        errcode, crashed = run_spag([cmd761, file])        
        self.assertTrue(crashed)        

if __name__ == "__main__":
    #unittest.main()

    bd = os.popen('find bd -name "*.f90"').read().splitlines()
    mds = os.popen('find mds -name "*.f90"').read().splitlines()
    mis = os.popen('find mis -name "*.f90"').read().splitlines()

    files = bd + mds + mis

    run_spag_for_files(files)

