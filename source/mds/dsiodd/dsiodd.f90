!*==dsiodd.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsiodd
   USE I_GINOX
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'GINOX.COM'
   INCLUDE 'DSIOF.COM'
   lginox = 5*numfcb + numsof + 2
   lhalf = 16
   lendsp = 0
   lenwpb = 0
   maskh1 = 'FFFF0000'x
   maskh2 = '0000FFFF'x
   maske1 = 'FF000000'x
   maske2 = '00FF0000'x
   maske3 = '0000FF00'x
   maske4 = '000000FF'x
   mcbmas = '40000000'x
   maxdsn = numfcb
   maskq1 = maske1
   maskq2 = maske2
   maskq3 = maske3
   maskq4 = maske4
   mulq1 = 2**24
   mulq2 = 2**16
   mulq3 = 2**8
   idsx = '00EE0000'x
   idsp = '000E0000'x
   idsc = '000C0000'x
   idsrh = '11000000'x
   idsrt = '77000000'x
   idssb = '22000000'x
   idsse = '7F000000'x
   idsch = '3B000000'x
   idsct = '3F000000'x
   idssh = '4B000000'x
   idsst = '4F000000'x
   idssd = 'DD000000'x
   idseb = 'EB000000'x
   idsef = 'EF000000'x
   nwrdel(1) = 1
   nwrdel(2) = 2
   nwrdel(3) = 2
   nwrdel(4) = 4
END SUBROUTINE dsiodd
