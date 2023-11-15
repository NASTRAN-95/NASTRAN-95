
SUBROUTINE switch
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Icfiat , Ifiat(3) , Ifist(2) , Iparam , Ipfist , Nout
   CHARACTER*25 Sfm , Uwm
   REAL Skip(21) , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Iparam
   COMMON /system/ Sysbuf , Nout , Skip , Icfiat
   COMMON /xfiat / Ifiat
   COMMON /xfist / Ifist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xpfist/ Ipfist
!
! Local variable declarations
!
   INTEGER andf , complf , lshift , orf , rshift
   INTEGER file1 , file2 , i , j1 , j2 , k , lastwd , ltu1 , ltu2 , mask , mask1 , mask2 , mask3 , modnam(2) , mxe , nacent ,       &
         & name(2) , nfiles , nuniqe , nwd , psave1 , psave2 , unit , unit1 , unit2 , unt
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
!
!     THE PURPOSE OF THIS MODULE IS TO INTERCHANGE THE NAMES OF THE
!     TWO INPUT FILES.  THIS IS ACCOMPLISHED BY THE DIRECT UPDATING
!     OF THE FIAT
!
   DATA file1/101/ , file2/102/ , modnam/4HSWIT , 4HCH  /
!
   IF ( Iparam>=0 ) RETURN
   mask2 = 32767
   mask3 = complf(mask2)
   mask = lshift(1,30) - 1
   mask = lshift(rshift(mask,16),16)
   mask1 = complf(mask)
   nuniqe = Ifiat(1)*Icfiat + 3
   mxe = Ifiat(2)*Icfiat + 3
   lastwd = Ifiat(3)*Icfiat + 3
!
!     LOCATE FILE POINTERS IN THE FIST
!
   nwd = 2*Ipfist + 2
   nacent = 2*Ifist(2) + 2
   nfiles = nacent - nwd
   psave1 = 0
   psave2 = 0
   DO i = 1 , nfiles , 2
      IF ( Ifist(nwd+i)==file1 .OR. Ifist(nwd+i)==file2 ) THEN
         IF ( Ifist(nwd+i)==file1 ) THEN
            psave1 = Ifist(nwd+i+1) + 1
         ELSEIF ( Ifist(nwd+i)==file2 ) THEN
            psave2 = Ifist(nwd+i+1) + 1
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK THAT FILES ARE IN FIST
!
   IF ( psave1==0 ) CALL mesage(-1,file1,modnam)
   IF ( psave2==0 ) CALL mesage(-1,file2,modnam)
!
!     SWITCH FILE NAMES IN FIAT
!
   name(1) = Ifiat(psave1+1)
   name(2) = Ifiat(psave1+2)
   unit1 = andf(mask2,Ifiat(psave1))
   unit2 = andf(mask2,Ifiat(psave2))
   nwd = Icfiat*Ifiat(3) - 2
   ltu1 = andf(mask,Ifiat(psave1))
   ltu2 = andf(mask,Ifiat(psave2))
   Ifiat(psave1) = orf(andf(Ifiat(psave1),mask2),ltu2)
   Ifiat(psave1+1) = Ifiat(psave2+1)
   Ifiat(psave1+2) = Ifiat(psave2+2)
   Ifiat(psave2) = orf(andf(Ifiat(psave2),mask2),ltu1)
   Ifiat(psave2+1) = name(1)
   Ifiat(psave2+2) = name(2)
!
!     SWITCH STACKED DATA BLOCKS
!
   DO i = 4 , nwd , Icfiat
      IF ( psave1/=i .AND. psave2/=i ) THEN
         unit = andf(mask2,Ifiat(i))
         IF ( unit==unit1 .OR. unit==unit2 ) THEN
            IF ( unit==unit1 ) unt = unit2
            IF ( unit==unit2 ) unt = unit1
            IF ( i>nuniqe ) THEN
!
!     DATA BLOCK RESIDES IN NON-UNIQUE PORTION OF FIAT
!     SWITCH UNIT NUMBERS
!
               Ifiat(i) = orf(andf(Ifiat(i),mask3),unt)
            ELSE
!
!     DATA BLOCK RESIDES IN UNIQUE PART OF FIAT
!     MOVE ENTRY TO BOTTOM
!
               IF ( lastwd+Icfiat>mxe ) THEN
                  WRITE (Nout,99001) Sfm
99001             FORMAT (A25,' 1021, FIAT OVERFLOW')
                  CALL mesage(-37,0,modnam)
               ENDIF
               Ifiat(lastwd+1) = orf(andf(Ifiat(i),mask3),unt)
               DO k = 2 , Icfiat
                  Ifiat(lastwd+k) = Ifiat(i+k-1)
               ENDDO
               lastwd = lastwd + Icfiat
               Ifiat(3) = Ifiat(3) + 1
!
!     CLEAR OLD ENTRY IN UNIQUE PART
!
               Ifiat(i) = andf(Ifiat(i),mask2)
               j1 = i + 1
               j2 = i + Icfiat - 1
               DO k = j1 , j2
                  Ifiat(k) = 0
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE switch
