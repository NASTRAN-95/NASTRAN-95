!*==gfswch.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gfswch(File1,File2)
   USE c_system
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xpfist
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File1
   INTEGER :: File2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifloc , iloc1 , iloc2 , j1 , j2 , k , lastwd , ltu1 , ltu2 , mask , mask1 , mask2 , mask3 , mxe , nacent ,        &
            & nfiles , nuniqe , nwd , psave1 , psave2 , unit , unit1 , unit2 , unt
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , DIMENSION(2) :: name
   EXTERNAL andf , complf , lshift , mesage , orf , rshift
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO INTERCHANGE THE NAMES OF
!     TWO FILES.  THIS IS ACCOMPLISHED BY THE DIRECT UPDATEING
!     OF THE FIAT AND THE FIST
!
   DATA modnam/4HGFSW , 4HCH  /
!
   mask = lshift(1,30) - 1
   mask = lshift(rshift(mask,16),16)
   mask1 = complf(mask)
   mask2 = 32767
   mask3 = complf(mask2)
   nuniqe = ifiat(1)*icfiat + 3
   mxe = ifiat(2)*icfiat + 3
   lastwd = ifiat(3)*icfiat + 3
!
!     LOCATE FILE POINTERS IN THE FIST
!
   nwd = 2*ipfist + 2
   nacent = 2*ifist(2) + 2
   nfiles = nacent - nwd
   psave1 = 0
   psave2 = 0
   DO i = 1 , nfiles , 2
      IF ( ifist(nwd+i)==File1 .OR. ifist(nwd+i)==File2 ) THEN
         IF ( ifist(nwd+i)==File1 ) THEN
            psave1 = ifist(nwd+i+1) + 1
            iloc1 = i + nwd
         ELSEIF ( ifist(nwd+i)==File2 ) THEN
            psave2 = ifist(nwd+i+1) + 1
            iloc2 = i + nwd
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK THAT FILES ARE IN FIST
!
   IF ( psave1==0 ) CALL mesage(-1,File1,modnam)
   IF ( psave2==0 ) CALL mesage(-1,File2,modnam)
!
!     SWITCH THE FIST POINTERS
!
   ifloc = ifist(iloc1+1)
   ifist(iloc1+1) = ifist(iloc2+1)
   ifist(iloc2+1) = ifloc
!
!     SWITCH FILE NAMES IN FIAT
!
   name(1) = ifiat(psave1+1)
   name(2) = ifiat(psave1+2)
   unit1 = andf(mask2,ifiat(psave1))
   unit2 = andf(mask2,ifiat(psave2))
   nwd = icfiat*ifiat(3) - 2
   ltu1 = andf(mask,ifiat(psave1))
   ltu2 = andf(mask,ifiat(psave2))
   ifiat(psave1) = orf(andf(ifiat(psave1),mask2),ltu2)
   ifiat(psave1+1) = ifiat(psave2+1)
   ifiat(psave1+2) = ifiat(psave2+2)
   ifiat(psave2) = orf(andf(ifiat(psave2),mask2),ltu1)
   ifiat(psave2+1) = name(1)
   ifiat(psave2+2) = name(2)
!
!     SWITCH STACKED DATA BLOCKS
!
   DO i = 4 , nwd , icfiat
      IF ( psave1/=i .AND. psave2/=i ) THEN
         IF ( ifiat(i+1)/=0 .OR. ifiat(i+2)/=0 ) THEN
            unit = andf(mask2,ifiat(i))
            IF ( unit==unit1 .OR. unit==unit2 ) THEN
               IF ( unit==unit1 ) unt = unit2
               IF ( unit==unit2 ) unt = unit1
               IF ( i>nuniqe ) THEN
!
!     DATA BLOCK RESIDES IN NON-UNIQUE PORTION OF FIAT
!     SWITCH UNIT NUMBERS
!
                  ifiat(i) = orf(andf(ifiat(i),mask3),unt)
               ELSE
!
!     DATA BLOCK RESIDES IN UNIQUE PART OF FIAT
!     MOVE ENTRY TO BOTTOM
!
                  IF ( lastwd+icfiat>mxe ) THEN
                     WRITE (nout,99001) sfm
99001                FORMAT (A25,' 1021, FIAT OVERFLOW')
                     CALL mesage(-37,0,modnam)
                  ENDIF
                  ifiat(lastwd+1) = orf(andf(ifiat(i),mask3),unt)
                  DO k = 2 , icfiat
                     ifiat(lastwd+k) = ifiat(i+k-1)
                  ENDDO
                  lastwd = lastwd + icfiat
                  ifiat(3) = ifiat(3) + 1
!
!     CLEAR OLD ENTRY IN UNIQUE PART
!
                  ifiat(i) = andf(ifiat(i),mask2)
                  j1 = i + 1
                  j2 = i + icfiat - 1
                  DO k = j1 , j2
                     ifiat(k) = 0
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE gfswch
