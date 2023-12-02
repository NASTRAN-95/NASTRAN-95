!*==sofcls.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofcls
   IMPLICIT NONE
   USE c_itemdt
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_system
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
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
   IF ( .NOT.opnsof ) RETURN
   IF ( ditpbn/=0 ) THEN
      IF ( ditup ) THEN
         CALL sofio(iwrt,ditpbn,buf(dit-2))
         ditup = .FALSE.
         GOTO 100
      ENDIF
   ENDIF
   IF ( nxtpbn/=0 ) THEN
      IF ( nxtup ) THEN
         CALL sofio(iwrt,nxtpbn,buf(nxt-2))
         nxtup = .FALSE.
      ENDIF
   ENDIF
 100  IF ( mdipbn/=0 ) THEN
      IF ( mdiup ) THEN
         CALL sofio(iwrt,mdipbn,buf(mdi-2))
         mdiup = .FALSE.
      ENDIF
   ENDIF
!
!     WRITE OUT COMMON BLOCKS.
!
   last = nbuff - 4
   DO i = 1 , last
      buf(dit+i) = 0
   ENDDO
   buf(dit+1) = psswrd(1)
   buf(dit+2) = psswrd(2)
   buf(dit+4) = nfiles
   DO i = 1 , nfiles
      buf(dit+4+i) = filnam(i)
      buf(dit+14+i) = filsiz(i)
      buf(dit+33+i) = a(22+i)
   ENDDO
   DO i = 1 , 4
      buf(dit+24+i) = b(i)
   ENDDO
   buf(dit+29) = a(4)
   buf(dit+30) = a(5)
   buf(dit+31) = a(6)
   buf(dit+32) = a(18)
   buf(dit+33) = a(22)
   buf(dit+44) = a(33)
   nxtrst = .FALSE.
   buf(dit+45) = a(37)
   buf(dit+46) = b(5)
   buf(dit+47) = b(6)
!
   buf(dit+100) = nitem
   k = 100
   DO i = 1 , nitem
      DO j = 1 , 7
         buf(dit+k+j) = item(j,i)
      ENDDO
      k = k + 7
   ENDDO
   ibl = 1
   DO i = 1 , nfiles
      buf(dit+3) = i
      CALL sofio(iwrt,ibl,buf(dit-2))
      ibl = ibl + filsiz(i)
   ENDDO
   CALL sofio(7,0,0)
   opnsof = .FALSE.
END SUBROUTINE sofcls
