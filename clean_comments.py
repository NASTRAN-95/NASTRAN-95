import re
import sys

comment1 = r"""!
!.*argument declarations.*
!
"""

comment2 = r"""!
!.*variable declarations.*
!
"""

comment3 = r"""!
! End of declarations.*
!
"""

# Check if the file path is provided as a command-line argument
if len(sys.argv) < 2:
    print("Please provide the file path as a command-line argument.")
    sys.exit(1)

file_path = sys.argv[1]

with open(file_path, 'r') as file:
    code = ''.join(file.readlines())

code = re.sub(comment1, '', code)
code = re.sub(comment2, '', code)
code = re.sub(comment3, '', code)

with open(file_path, 'w') as file:
    file.write(code)
