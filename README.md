# NASTRAN-95

NASTRAN is the NASA Structural Analysis System, a finite element analysis program (FEA) completed in the early 1970's. It was the first of its kind and opened the door to computer-aided engineering. Subsections of a design can be modeled and then larger groupings of these elements can again be modeled. NASTRAN can handle elastic stability analysis, complex eigenvalues for vibration and dynamic stability analysis, dynamic response for transient and steady state loads, and random excitation, and static response to concentrated and distributed loads, thermal expansion, and enforced deformations.

NASTRAN has been released under the [NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN-95/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN%2095.doc).

NOTE: There is no technical support available for this software.

## Building

```
./bootstrap
make -j12
```

## Testing

```
make check
```

## Packaging

DEB package can be created using the following commands:

```
sudo apt install debhelper dh-exec quilt
dpkg-buildpackage -rfakeroot -b -uc -us
```

## Release notes

This release is based on the original NASTRAN-95 source code released by NASA, plus fixes and improvements by the following authors:

* Daniel Everhart
* Thomas M. Hermann
* Harry Schaeffer
* Thomas Wuerfl

Furthermore, an automake build system, testing scripts and bugfixes have been contributed by the following authors:

* Luca Dall'Olio
* John Kuras
* Sylvestre Ledru

Among other improvements, this release enables optimized build of Nastran (`-O2`), that passes all tests.

