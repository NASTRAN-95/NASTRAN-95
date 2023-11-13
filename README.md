# NASTRAN-95

NASTRAN has been released under the
[NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN-95/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN%2095.doc).


NASTRAN is the NASA Structural Analysis System, a finite element
analysis program (FEA) completed in the early 1970's. It was the first
of its kind and opened the door to computer-aided
engineering. Subsections of a design can be modeled and then larger
groupings of these elements can again be modeled. NASTRAN can handle
elastic stability analysis, complex eigenvalues for vibration and
dynamic stability analysis, dynamic response for transient and steady
state loads, and random excitation, and static response to
concentrated and distributed loads, thermal expansion, and enforced
deformations.

NOTE: There is no technical support available for this software.

# Current Tasks

Task                                    | Status
:---------------------------------------|:------------:
Build libnastran                        | Complete
Build executables                       | Complete
Modernize wrapper C shell script        | Complete
Replace wrapper script with namelist    | Complete
Review the contents of the bd directory | Complete
Verify input and output                 | In Progress
Remove machine dependent code           | Queued
Replace all Holerith contants           | Queued
Remove all type coercion                | Queued

# Future Tasks

* Use CMAKE to configure the build
* Improve the organization of the source files
* Modernize to Fortran 95
* Use intrinsic functions to the greatest extent possible
* Use modern data structures
* Improve element extensibility using modern data structures
* Use external high performance linear algebra libraries
