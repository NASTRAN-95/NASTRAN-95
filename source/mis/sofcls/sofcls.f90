!*==sofcls.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofcls
   IMPLICIT NONE
   USE C_ITEMDT
   USE C_SOF
   USE C_SOFCOM
   USE C_SYS
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dit , ditlbn , ditpbn , i , ibl , j , k , last , mdi , mdilbn , mdipbn , nxt , nxtlbn , nxtpbn
   LOGICAL :: ditup , mdiup , nxtrst , nxtup
   INTEGER , SAVE :: iwrt
!
! End of declarations rewritten by SPAG
!
!
!     WRITES OUT AT THE TERMINATION OF A MODULE ALL THE IN CORE BUFFERS
!     AND COMMON BLOCKS.
!
   !>>>>EQUIVALENCE (Dit,A(1)) , (Ditpbn,A(2)) , (Ditlbn,A(3)) , (Mdi,A(15)) , (Mdipbn,A(16)) , (Mdilbn,A(17)) , (Nxt,A(19)) ,           &
!>>>>    & (Nxtpbn,A(20)) , (Nxtlbn,A(21)) , (Ditup,A(34)) , (Mdiup,A(35)) , (Nxtup,A(36)) , (Nxtrst,A(37))
   DATA iwrt/2/
!
   IF ( .NOT.Opnsof ) RETURN
   IF ( ditpbn/=0 ) THEN
      IF ( ditup ) THEN
         CALL sofio(iwrt,ditpbn,Buf(dit-2))
         ditup = .FALSE.
         GOTO 100
      ENDIF
   ENDIF
   IF ( nxtpbn/=0 ) THEN
      IF ( nxtup ) THEN
         CALL sofio(iwrt,nxtpbn,Buf(nxt-2))
         nxtup = .FALSE.
      ENDIF
   ENDIF
 100  IF ( mdipbn/=0 ) THEN
      IF ( mdiup ) THEN
         CALL sofio(iwrt,mdipbn,Buf(mdi-2))
         mdiup = .FALSE.
      ENDIF
   ENDIF
!
!     WRITE OUT COMMON BLOCKS.
!
   last = Nbuff - 4
   DO i = 1 , last
      Buf(dit+i) = 0
   ENDDO
   Buf(dit+1) = Psswrd(1)
   Buf(dit+2) = Psswrd(2)
   Buf(dit+4) = Nfiles
   DO i = 1 , Nfiles
      Buf(dit+4+i) = Filnam(i)
      Buf(dit+14+i) = Filsiz(i)
      Buf(dit+33+i) = A(22+i)
   ENDDO
   DO i = 1 , 4
      Buf(dit+24+i) = B(i)
   ENDDO
   Buf(dit+29) = A(4)
   Buf(dit+30) = A(5)
   Buf(dit+31) = A(6)
   Buf(dit+32) = A(18)
   Buf(dit+33) = A(22)
   Buf(dit+44) = A(33)
   nxtrst = .FALSE.
   Buf(dit+45) = A(37)
   Buf(dit+46) = B(5)
   Buf(dit+47) = B(6)
!
   Buf(dit+100) = Nitem
   k = 100
   DO i = 1 , Nitem
      DO j = 1 , 7
         Buf(dit+k+j) = Item(j,i)
      ENDDO
      k = k + 7
   ENDDO
   ibl = 1
   DO i = 1 , Nfiles
      Buf(dit+3) = i
      CALL sofio(iwrt,ibl,Buf(dit-2))
      ibl = ibl + Filsiz(i)
   ENDDO
   CALL sofio(7,0,0)
   Opnsof = .FALSE.
END SUBROUTINE sofcls
