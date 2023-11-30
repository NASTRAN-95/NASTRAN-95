
SUBROUTINE gridip(Grid,Seqss,Len,Ipset,Cset,No,Z,Lloc)
   IMPLICIT NONE
   INTEGER Ierr , Inam(2)
   COMMON /cmbfnd/ Inam , Ierr
   INTEGER Grid , Len , Lloc , No , Seqss
   INTEGER Cset(6) , Ipset(6) , Z(1)
   INTEGER i , icode , istart , k , kk , loc , mask26 , nent , noapp , posno
   INTEGER maskn , orf , rshift
   EXTERNAL orf , rshift
!
!     THIS SUBROUTINE FINDS SETS OF IP NUMBERS AND DEGREE OF FREEDOM
!     COMPONENT NUMBERS FOR GRID POINTS DEFINED IN A BASIC
!     SUBSTRUCTURE THAT IS A COMPONENT OF A PSEUDO-STRUCTURE.
!
!     ARGUMENTS
!               GRID   - GRID POINT ID NUMBER
!               SEQSS  - THE STARTING ADDRESS IN OPEN CORE OF THE
!                        PSEUDO-STRUCTURE EQSS RECORD
!               LEN    - LENGTH OF THE EQSS
!               IPSET  - THE SET OF IP NUMBERS FOR GRID
!               CSET   - COMPONENTS OF GIVEN IP NUMBER
!               NO     - THE NUMBER OF IP DEFINED BY GRID
!
!
!
   Ierr = 0
   nent = Len/3
!
!     SEARCH FOR THE GRID ID IN THE EQSS
!
!     NOTE --- FOR RAPID LOCATION OF ALL IP FOR A GIVEN GRID,
!              THE COMPONENT WORD OF THE EQSS HAS HAD ITS FIRST
!              SIX BITS PACKED WITH A CODE-  THE FIRST THREE
!              BITS GIVE THE NUMBER OF THE IP AND THE SECOND
!              THREE THE TOTAL NO. OF IP.  E.G. 011101 MEANS
!              THE CURRENT IP IS THE THIRD OF FIVE FOR THIS
!              GRID ID.
!
!
   CALL bisloc(*100,Grid,Z(Seqss),3,nent,loc)
   k = Seqss + loc - 1
   icode = rshift(Z(k+2),26)
!
!     ICODE CONTAINS SIX BIT CODE
!
   posno = icode/8
   noapp = icode - 8*posno
!
!     POSNO IS THE POSITION NUMBER OF THE GRID WE HAVE FOUND,
!     NOAPP IS THE TOTAL NUMBER OF APPEARANCES OF THAT GRID.
!
   IF ( noapp==0 ) posno = 1
   IF ( noapp==0 ) noapp = 1
   istart = k - 3*(posno-1)
   Lloc = istart
!
!     PICK UP RIGHT 26 BITS BY MASK26 FOR CSET(I), INSTEAD OF R/LSHIFT
!
   mask26 = maskn(26,0)
!
   DO i = 1 , noapp
      kk = istart + 3*(i-1)
      Ipset(i) = Z(kk+1)
      Cset(i) = orf(Z(kk+2),mask26)
   ENDDO
!
   No = noapp
   GOTO 99999
 100  Ierr = 1
99999 RETURN
END SUBROUTINE gridip