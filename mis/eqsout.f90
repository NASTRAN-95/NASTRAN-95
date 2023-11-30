
SUBROUTINE eqsout
   IMPLICIT NONE
   INTEGER Cnam(2) , Combo(7,5) , Iauto , Ihalf , Ihead(96) , Iprint , Isort , Ititl(96) , Junk(5) , Mach , Mcon , Nipnew , Npsub , &
         & Score , Z(1)
   REAL Conect , Conset , Origin(7,3) , Restct(7,7) , Tdat(6) , Toler , Tran
   COMMON /cmb002/ Junk , Score
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /cmb004/ Tdat , Nipnew , Cnam
   COMMON /machin/ Mach , Ihalf
   COMMON /output/ Ititl , Ihead
   COMMON /zzzzzz/ Z
   INTEGER andf , lshift , orf , rshift
   INTEGER i , iblank , icode , icomp , ihd(64) , ii , ioff , iords(2) , isil , isteqs , itest , iw , j , jj , loc , nbot(7) ,      &
         & ncomp , ndof , nheqss , nout , ntop(7) , nwrd , string(32) , words(6)
   EXTERNAL andf , lshift , orf , rshift
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
   IF ( andf(rshift(Iprint,11),1)/=1 ) RETURN
   CALL sfetch(Cnam,nheqss,1,itest)
   CALL suread(Z(Score),-1,nout,itest)
   ncomp = Z(Score+2)
   nwrd = nout - 4
   isteqs = Score + nwrd
!
!     MOVE COMPONENT SUBSTRUCTURE NAMES INTO FIRST NWRD OF OPEN CORE.
!
   DO i = 1 , nwrd
      ii = i - 1
      Z(Score+ii) = Z(Score+ii+4)
   ENDDO
   DO i = 1 , 32
      string(i) = iblank
   ENDDO
   CALL push(words(1),string,5,8,0)
   CALL push(words(5),string,17,8,0)
   CALL push(words(3),string,29,8,0)
   DO i = 1 , Npsub
      iords(1) = Combo(i,1)
      iords(2) = Combo(i,2)
      loc = 39 + 11*(i-1)
      CALL push(iords(1),string,loc,8,0)
   ENDDO
   DO i = 1 , 64
      Ihead(i) = ihd(i)
   ENDDO
   DO i = 65 , 96
      Ihead(i) = string(i-64)
   ENDDO
   CALL page
!
!     COMPUTE FIRST AND LAST COMPONENT SUBSTRUCTURE ID NUMBERS
!     FOR EACH PSEUDOSTRUCTURE.
!
   nbot(1) = 1
   DO i = 1 , Npsub
      ntop(i) = nbot(i) + Combo(i,5) - 1
      ii = i + 1
      IF ( i/=Npsub ) nbot(ii) = ntop(i) + 1
   ENDDO
!
!     READ EQSS INTO OPEN CORE STARTING AT LOCATION ISTEQS
!
   jj = 0
   icomp = 0
   DO
      icomp = icomp + 1
      IF ( icomp>ncomp ) EXIT
      DO
         CALL suread(Z(isteqs+jj+1),3,nout,itest)
         IF ( itest==2 ) EXIT
         IF ( itest==3 ) GOTO 100
!
!     NORMAL ROUTE - PROCESS ENTRIES
!
         Z(isteqs+jj) = icomp
         DO j = 1 , Npsub
            IF ( icomp>=nbot(j) .AND. icomp<=ntop(j) ) EXIT
         ENDDO
         Z(isteqs+jj) = orf(lshift(j,Ihalf),Z(isteqs+jj))
         jj = jj + 4
      ENDDO
   ENDDO
!
!     SORT ON INTERNAL POINT NUMBER
!
 100  Z(isteqs+jj) = 0
   Z(isteqs+jj+1) = 0
   Z(isteqs+jj+2) = 0
   Z(isteqs+jj+3) = 0
   CALL sort(0,0,4,3,Z(isteqs),jj)
   ii = 1
   isil = 1
   DO i = 1 , jj , 4
      IF ( Z(isteqs+i+1)/=Z(isteqs+i+5) ) THEN
         iw = 4*ii
         ioff = i - 1 - 4*(ii-1)
         icode = Z(isteqs+ioff+3)
         CALL decode(icode,string,ndof)
         CALL eqout1(Z(isteqs+ioff),iw,Z(Score),nwrd,isil)
         isil = isil + ndof
         ii = 1
      ELSE
         ii = ii + 1
      ENDIF
   ENDDO
END SUBROUTINE eqsout