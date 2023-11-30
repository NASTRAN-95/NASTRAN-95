
SUBROUTINE dsiodd
   IMPLICIT NONE
   INCLUDE 'GINOX.COM'
   INCLUDE 'DSIOF.COM'
   Lginox = 5*NUMFCB + NUMSOF + 2
   Lhalf = 16
   Lendsp = 0
   Lenwpb = 0
   Maskh1 = 'FFFF0000'x
   Maskh2 = '0000FFFF'x
   Maske1 = 'FF000000'x
   Maske2 = '00FF0000'x
   Maske3 = '0000FF00'x
   Maske4 = '000000FF'x
   Mcbmas = '40000000'x
   Maxdsn = NUMFCB
   Maskq1 = Maske1
   Maskq2 = Maske2
   Maskq3 = Maske3
   Maskq4 = Maske4
   Mulq1 = 2**24
   Mulq2 = 2**16
   Mulq3 = 2**8
   Idsx = '00EE0000'x
   Idsp = '000E0000'x
   Idsc = '000C0000'x
   Idsrh = '11000000'x
   Idsrt = '77000000'x
   Idssb = '22000000'x
   Idsse = '7F000000'x
   Idsch = '3B000000'x
   Idsct = '3F000000'x
   Idssh = '4B000000'x
   Idsst = '4F000000'x
   Idssd = 'DD000000'x
   Idseb = 'EB000000'x
   Idsef = 'EF000000'x
   Nwrdel(1) = 1
   Nwrdel(2) = 2
   Nwrdel(3) = 2
   Nwrdel(4) = 4
END SUBROUTINE dsiodd