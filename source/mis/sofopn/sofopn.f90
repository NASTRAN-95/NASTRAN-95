!*==sofopn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofopn(B1,B2,B3)
   IMPLICIT NONE
   USE c_ginox
   USE c_itemdt
   USE c_machin
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
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
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
   IF ( opnsof ) THEN
!
!     ERROR MESSAGE
!
      WRITE (nout,99001) ufm
99001 FORMAT (A23,' 6222 - ATTEMPT TO CALL SOFOPN MORE THAN ONCE ','WITHOUT CALLING SOFCLS.')
      CALL sofcls
      CALL mesage(-61,0,0)
      GOTO 99999
   ENDIF
!
!     CHECK IF THE OPEN CORE BUFFERS ARE LARGE ENOUGH AND DO NOT OVERLAP
!
   iptr(1) = corwds(buf,B1) + 2
   iptr(2) = corwds(buf,B2) + 2
   iptr(3) = corwds(buf,B3) + 2
   isiz = korsz(buf)
   DO i = 1 , 3
      IF ( isiz-iptr(i)<nbuff-3 ) CALL mesage(-8,0,name)
   ENDDO
   DO i = 1 , 2
      k = i + 1
      DO j = k , 3
         isiz = iptr(i) - iptr(j)
         IF ( isiz<0 ) isiz = -isiz
         IF ( isiz<nbuff ) CALL mesage(-8,0,name)
      ENDDO
   ENDDO
   a(1) = iptr(1)
   a(7) = iptr(2)
   a(15) = iptr(3)
   a(19) = iptr(1)
!
!     SET SOF BUFFER SIZE FROM /GINOX/
!     ON IBM USE /SYSTEM/ BECAUSE /GINOX/ IS IN SUPER LINK
!
   b(1) = ginobl
   IF ( mach==2 .OR. mach>=5 ) b(1) = nbuff - 4
!WKBD 3/94      IF (MACH .EQ. 12) B(1) =NBUFF -28
   IF ( first ) CALL sofint(iptr(1),iptr(2),numb,ibl1)
!
!     READ AND INITIALIZE THE COMMON BLOCKS SYS AND SOF
!
   dit = iptr(1)
   CALL sofio(ird,1,buf(dit-2))
   DO i = 1 , 4
      b(i) = buf(dit+24+i)
   ENDDO
   b(5) = buf(dit+46)
   b(6) = buf(dit+47)
   a(1) = iptr(1)
   a(2) = 0
   a(3) = 0
   a(4) = buf(dit+29)
   a(5) = buf(dit+30)
   a(6) = buf(dit+31)
   a(7) = iptr(2)
   DO i = 8 , 14
      a(i) = 0
   ENDDO
   a(15) = iptr(3)
   a(16) = 0
   a(17) = 0
   a(18) = buf(dit+32)
   a(19) = iptr(1)
   a(20) = 0
   a(21) = 0
   a(22) = buf(dit+33)
   DO i = 1 , nfiles
      a(22+i) = buf(dit+33+i)
   ENDDO
   a(33) = buf(dit+44)
   a(34) = 0
   a(35) = 0
   a(36) = 0
   a(37) = buf(dit+45)
!
!     INITILIZE COMMON BLOCK ITEMDT
!
   nitem = buf(dit+100)
   k = 100
   DO i = 1 , nitem
      DO j = 1 , 7
         item(j,i) = buf(dit+k+j)
      ENDDO
      k = k + 7
   ENDDO
   opnsof = .TRUE.
   IF ( .NOT.first ) RETURN
   first = .FALSE.
   IF ( numb==0 ) RETURN
!
!     ADD THE NUMBER NUMB OF BLOCKS TO THE SUPERBLOCK WHOSE SIZE
!     NEEDED TO BE INCREASED
!
   DO i = 1 , numb
      CALL retblk(ibl1+i-1)
   ENDDO
   b(4) = b(4) - numb
99999 END SUBROUTINE sofopn
