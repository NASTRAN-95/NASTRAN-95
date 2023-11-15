
SUBROUTINE sofcls
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER A(37) , B(6) , Buf(1) , Dit , Ditlbn , Ditpbn , Filnam(10) , Filsiz(10) , Item(7,1) , Mdi , Mdilbn , Mdipbn , Nbuff ,    &
         & Nfiles , Nitem , Nxt , Nxtlbn , Nxtpbn , Psswrd(2)
   LOGICAL Ditup , Mdiup , Nxtrst , Nxtup , Opnsof
   REAL First , Status
   COMMON /itemdt/ Nitem , Item
   COMMON /sof   / A
   COMMON /sofcom/ Nfiles , Filnam , Filsiz , Status , Psswrd , First , Opnsof
   COMMON /sys   / B
   COMMON /system/ Nbuff
   COMMON /zzzzzz/ Buf
!
! Local variable declarations
!
   INTEGER i , ibl , iwrt , j , k , last
!
! End of declarations
!
!
!     WRITES OUT AT THE TERMINATION OF A MODULE ALL THE IN CORE BUFFERS
!     AND COMMON BLOCKS.
!
   EQUIVALENCE (Dit,A(1)) , (Ditpbn,A(2)) , (Ditlbn,A(3)) , (Mdi,A(15)) , (Mdipbn,A(16)) , (Mdilbn,A(17)) , (Nxt,A(19)) ,           &
    & (Nxtpbn,A(20)) , (Nxtlbn,A(21)) , (Ditup,A(34)) , (Mdiup,A(35)) , (Nxtup,A(36)) , (Nxtrst,A(37))
   DATA iwrt/2/
!
   IF ( .NOT.Opnsof ) RETURN
   IF ( Ditpbn/=0 ) THEN
      IF ( Ditup ) THEN
         CALL sofio(iwrt,Ditpbn,Buf(Dit-2))
         Ditup = .FALSE.
         GOTO 100
      ENDIF
   ENDIF
   IF ( Nxtpbn/=0 ) THEN
      IF ( Nxtup ) THEN
         CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
         Nxtup = .FALSE.
      ENDIF
   ENDIF
 100  IF ( Mdipbn/=0 ) THEN
      IF ( Mdiup ) THEN
         CALL sofio(iwrt,Mdipbn,Buf(Mdi-2))
         Mdiup = .FALSE.
      ENDIF
   ENDIF
!
!     WRITE OUT COMMON BLOCKS.
!
   last = Nbuff - 4
   DO i = 1 , last
      Buf(Dit+i) = 0
   ENDDO
   Buf(Dit+1) = Psswrd(1)
   Buf(Dit+2) = Psswrd(2)
   Buf(Dit+4) = Nfiles
   DO i = 1 , Nfiles
      Buf(Dit+4+i) = Filnam(i)
      Buf(Dit+14+i) = Filsiz(i)
      Buf(Dit+33+i) = A(22+i)
   ENDDO
   DO i = 1 , 4
      Buf(Dit+24+i) = B(i)
   ENDDO
   Buf(Dit+29) = A(4)
   Buf(Dit+30) = A(5)
   Buf(Dit+31) = A(6)
   Buf(Dit+32) = A(18)
   Buf(Dit+33) = A(22)
   Buf(Dit+44) = A(33)
   Nxtrst = .FALSE.
   Buf(Dit+45) = A(37)
   Buf(Dit+46) = B(5)
   Buf(Dit+47) = B(6)
!
   Buf(Dit+100) = Nitem
   k = 100
   DO i = 1 , Nitem
      DO j = 1 , 7
         Buf(Dit+k+j) = Item(j,i)
      ENDDO
      k = k + 7
   ENDDO
   ibl = 1
   DO i = 1 , Nfiles
      Buf(Dit+3) = i
      CALL sofio(iwrt,ibl,Buf(Dit-2))
      ibl = ibl + Filsiz(i)
   ENDDO
   CALL sofio(7,0,0)
   Opnsof = .FALSE.
END SUBROUTINE sofcls
