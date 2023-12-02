!*==sofopn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofopn(B1,B2,B3)
   IMPLICIT NONE
   USE C_GINOX
   USE C_ITEMDT
   USE C_MACHIN
   USE C_SOF
   USE C_SOFCOM
   USE C_SYS
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: B1
   INTEGER , DIMENSION(1) :: B2
   INTEGER , DIMENSION(1) :: B3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dit , i , ibl1 , isiz , j , k , numb
   INTEGER , DIMENSION(3) :: iptr
   INTEGER , SAVE :: ird
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     READS THE SOF AND SYS COMMON BLOCKS FROM THE DIRECT ACCESS STORAGE
!     DEVICE, AND INITIALIZES THE POINTERS TO THE THREE BUFFERS NEEDED
!     BY THE SOF UTILITY SUBROUTINES
!
   DATA name/4HSOFO , 4HPN  /
   DATA ird/1/
!
   IF ( Opnsof ) THEN
!
!     ERROR MESSAGE
!
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 6222 - ATTEMPT TO CALL SOFOPN MORE THAN ONCE ','WITHOUT CALLING SOFCLS.')
      CALL sofcls
      CALL mesage(-61,0,0)
      GOTO 99999
   ENDIF
!
!     CHECK IF THE OPEN CORE BUFFERS ARE LARGE ENOUGH AND DO NOT OVERLAP
!
   iptr(1) = corwds(Buf,B1) + 2
   iptr(2) = corwds(Buf,B2) + 2
   iptr(3) = corwds(Buf,B3) + 2
   isiz = korsz(Buf)
   DO i = 1 , 3
      IF ( isiz-iptr(i)<Nbuff-3 ) CALL mesage(-8,0,name)
   ENDDO
   DO i = 1 , 2
      k = i + 1
      DO j = k , 3
         isiz = iptr(i) - iptr(j)
         IF ( isiz<0 ) isiz = -isiz
         IF ( isiz<Nbuff ) CALL mesage(-8,0,name)
      ENDDO
   ENDDO
   A(1) = iptr(1)
   A(7) = iptr(2)
   A(15) = iptr(3)
   A(19) = iptr(1)
!
!     SET SOF BUFFER SIZE FROM /GINOX/
!     ON IBM USE /SYSTEM/ BECAUSE /GINOX/ IS IN SUPER LINK
!
   B(1) = Ginobl
   IF ( Mach==2 .OR. Mach>=5 ) B(1) = Nbuff - 4
!WKBD 3/94      IF (MACH .EQ. 12) B(1) =NBUFF -28
   IF ( First ) CALL sofint(iptr(1),iptr(2),numb,ibl1)
!
!     READ AND INITIALIZE THE COMMON BLOCKS SYS AND SOF
!
   dit = iptr(1)
   CALL sofio(ird,1,Buf(dit-2))
   DO i = 1 , 4
      B(i) = Buf(dit+24+i)
   ENDDO
   B(5) = Buf(dit+46)
   B(6) = Buf(dit+47)
   A(1) = iptr(1)
   A(2) = 0
   A(3) = 0
   A(4) = Buf(dit+29)
   A(5) = Buf(dit+30)
   A(6) = Buf(dit+31)
   A(7) = iptr(2)
   DO i = 8 , 14
      A(i) = 0
   ENDDO
   A(15) = iptr(3)
   A(16) = 0
   A(17) = 0
   A(18) = Buf(dit+32)
   A(19) = iptr(1)
   A(20) = 0
   A(21) = 0
   A(22) = Buf(dit+33)
   DO i = 1 , Nfiles
      A(22+i) = Buf(dit+33+i)
   ENDDO
   A(33) = Buf(dit+44)
   A(34) = 0
   A(35) = 0
   A(36) = 0
   A(37) = Buf(dit+45)
!
!     INITILIZE COMMON BLOCK ITEMDT
!
   Nitem = Buf(dit+100)
   k = 100
   DO i = 1 , Nitem
      DO j = 1 , 7
         Item(j,i) = Buf(dit+k+j)
      ENDDO
      k = k + 7
   ENDDO
   Opnsof = .TRUE.
   IF ( .NOT.First ) RETURN
   First = .FALSE.
   IF ( numb==0 ) RETURN
!
!     ADD THE NUMBER NUMB OF BLOCKS TO THE SUPERBLOCK WHOSE SIZE
!     NEEDED TO BE INCREASED
!
   DO i = 1 , numb
      CALL retblk(ibl1+i-1)
   ENDDO
   B(4) = B(4) - numb
   RETURN
99999 END SUBROUTINE sofopn
