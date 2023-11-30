
SUBROUTINE casege
   IMPLICIT NONE
   INTEGER Iz(1) , Lmodes , Ndir , Nmodes , Sysbuf
   REAL Z(1)
   COMMON /blank / Lmodes , Ndir , Nmodes
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER buf1 , buf2 , casecc , casedd , i , itot , iwords , lcore , mcb(7) , nam(2)
   INTEGER korsz
!
! GENERATES IDENTICAL SUBCASES LMODES*NDIR TIMES FOR DDAM
!
!     CASEGEN  CASECC/CASEDD/C,Y,LMODES/V,N,NDIR/V,N,NMODES $
!    EQUIV CASEDD,CASECC  $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA casecc , casedd/101 , 201/
   DATA nam/4HCASE , 4HGE  /
!
   lcore = korsz(Z)
   buf1 = lcore - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   lcore = buf2 - 1
   IF ( lcore>0 ) THEN
!
      CALL gopen(casecc,Z(buf1),0)
      CALL gopen(casedd,Z(buf2),1)
      CALL read(*200,*100,casecc,Z,lcore,0,iwords)
   ENDIF
   GOTO 300
 100  IF ( Lmodes>Nmodes ) Lmodes = Nmodes
   itot = Lmodes*Ndir
   DO i = 1 , itot
      Iz(1) = i
      CALL write(casedd,Z,iwords,1)
   ENDDO
   CALL close(casecc,1)
   CALL close(casedd,1)
   mcb(1) = casecc
   CALL rdtrl(mcb)
   mcb(1) = casedd
   mcb(2) = itot
   CALL wrttrl(mcb)
   RETURN
!
 200  CALL mesage(-2,casecc,nam)
 300  CALL mesage(-8,0,nam)
END SUBROUTINE casege