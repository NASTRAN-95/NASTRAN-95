!*==eqsout.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eqsout
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_machin
   USE c_output
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icode , icomp , ii , ioff , isil , isteqs , itest , iw , j , jj , loc , ncomp , ndof , nout , nwrd
   INTEGER , SAVE :: iblank , nheqss
   INTEGER , DIMENSION(64) , SAVE :: ihd
   INTEGER , DIMENSION(2) :: iords
   INTEGER , DIMENSION(7) :: nbot , ntop
   INTEGER , DIMENSION(32) :: string
   INTEGER , DIMENSION(6) , SAVE :: words
   EXTERNAL andf , decode , eqout1 , lshift , orf , page , push , rshift , sfetch , sort , suread
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE WRITES THE CONNECTION TRACE FOR A NEWLY COMBINED
!     PSEUDOSTRUCTURE.
!
   DATA ihd/10*4H     , 4H   S , 4HUMMA , 4HRY O , 4HF PS , 4HEUDO , 4HSTRU , 4HCTUR , 4HE CO , 4HNNEC , 4HTIVI , 4HTIES ,          &
      & 12*4H     , 4HINTE , 4HRNAL , 4H   I , 4HNTER , 4HNAL  , 4H  DE , 4HGREE , 4HS OF , 4H  ** , 5*4H**** , 4H P S , 4H E U ,   &
       &4H D O , 4H S T , 4H R U , 4H C T , 4H U R , 4H E   , 4H N A , 4H M E , 4H S * , 3*4H**** , 3*4H    /
   DATA words/4HPOIN , 4HT NO , 4HFREE , 4HDOM  , 4HDOF  , 4HNO  /
   DATA iblank , nheqss/4H     , 4HEQSS/
!
   IF ( andf(rshift(iprint,11),1)/=1 ) RETURN
   CALL sfetch(cnam,nheqss,1,itest)
   CALL suread(z(score),-1,nout,itest)
   ncomp = z(score+2)
   nwrd = nout - 4
   isteqs = score + nwrd
!
!     MOVE COMPONENT SUBSTRUCTURE NAMES INTO FIRST NWRD OF OPEN CORE.
!
   DO i = 1 , nwrd
      ii = i - 1
      z(score+ii) = z(score+ii+4)
   ENDDO
   DO i = 1 , 32
      string(i) = iblank
   ENDDO
   CALL push(words(1),string,5,8,0)
   CALL push(words(5),string,17,8,0)
   CALL push(words(3),string,29,8,0)
   DO i = 1 , npsub
      iords(1) = combo(i,1)
      iords(2) = combo(i,2)
      loc = 39 + 11*(i-1)
      CALL push(iords(1),string,loc,8,0)
   ENDDO
   DO i = 1 , 64
      ihead(i) = ihd(i)
   ENDDO
   DO i = 65 , 96
      ihead(i) = string(i-64)
   ENDDO
   CALL page
!
!     COMPUTE FIRST AND LAST COMPONENT SUBSTRUCTURE ID NUMBERS
!     FOR EACH PSEUDOSTRUCTURE.
!
   nbot(1) = 1
   DO i = 1 , npsub
      ntop(i) = nbot(i) + combo(i,5) - 1
      ii = i + 1
      IF ( i/=npsub ) nbot(ii) = ntop(i) + 1
   ENDDO
!
!     READ EQSS INTO OPEN CORE STARTING AT LOCATION ISTEQS
!
   jj = 0
   icomp = 0
   SPAG_Loop_1_1: DO
      icomp = icomp + 1
      IF ( icomp>ncomp ) EXIT SPAG_Loop_1_1
      SPAG_Loop_2_2: DO
         CALL suread(z(isteqs+jj+1),3,nout,itest)
         IF ( itest==2 ) EXIT SPAG_Loop_2_2
         IF ( itest==3 ) EXIT SPAG_Loop_1_1
!
!     NORMAL ROUTE - PROCESS ENTRIES
!
         z(isteqs+jj) = icomp
         SPAG_Loop_3_3: DO j = 1 , npsub
            IF ( icomp>=nbot(j) .AND. icomp<=ntop(j) ) EXIT SPAG_Loop_3_3
         ENDDO SPAG_Loop_3_3
         z(isteqs+jj) = orf(lshift(j,ihalf),z(isteqs+jj))
         jj = jj + 4
      ENDDO SPAG_Loop_2_2
   ENDDO SPAG_Loop_1_1
!
!     SORT ON INTERNAL POINT NUMBER
!
   z(isteqs+jj) = 0
   z(isteqs+jj+1) = 0
   z(isteqs+jj+2) = 0
   z(isteqs+jj+3) = 0
   CALL sort(0,0,4,3,z(isteqs),jj)
   ii = 1
   isil = 1
   DO i = 1 , jj , 4
      IF ( z(isteqs+i+1)/=z(isteqs+i+5) ) THEN
         iw = 4*ii
         ioff = i - 1 - 4*(ii-1)
         icode = z(isteqs+ioff+3)
         CALL decode(icode,string,ndof)
         CALL eqout1(z(isteqs+ioff),iw,z(score),nwrd,isil)
         isil = isil + ndof
         ii = 1
      ELSE
         ii = ii + 1
      ENDIF
   ENDDO
END SUBROUTINE eqsout
