
SUBROUTINE cmcase
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Az(1) , Buf1 , Origin(7,3) , Tdat(6) , Toler , Xxx
   INTEGER Buf2 , Casecc , Cnam(2) , Combo(7,5) , Conset , Idat(3) , Idry , Ihead(96) , Iot , Iprint , Isort , Ititl(96) , Junk(8) ,&
         & Junk1(6) , Junk2(2) , Junk3(2) , Line , Mcon , Munk(6) , Nipnew , Nlpp , Npsub , Outt , Pora , Restct(7,7) , Step , Z(1)
   LOGICAL Conect , Iauto , Lonly , Tran
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry , Pora
   COMMON /cmb001/ Junk , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Junk1 , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /cmb004/ Tdat , Nipnew , Cnam , Lonly
   COMMON /output/ Ititl , Ihead
   COMMON /system/ Xxx , Iot , Munk , Nlpp , Junk3 , Line , Junk2 , Idat
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER aaa(2) , auto , comp(7,2) , i , ibits(32) , idir(3) , ierr , ifile , ihd(96) , imsg , isym(15,2) , itest , j , jj , kdh ,&
         & kk , l , lindx , litm , loap , lods , mj , mnem(11) , ncnam(2) , nflg , nheqss , nmnem , nnn , nrec , nwdscc , papp ,    &
         & snam(7,2) , symt(7) , trans(7)
   LOGICAL lf(3) , srch
   INTEGER orf
   EXTERNAL orf
!
! End of declarations
!
!
!     THIS SUBROUTINE PROCESSES THE CASE CONTROL DATA BLOCK
!
   EQUIVALENCE (Z(1),Az(1))
   DATA nmnem/11/ , idir/1HX , 1HY , 1HZ/ , auto/4HAUTO/ , aaa/4HCMCA , 4HSE  /
   DATA mnem/4HOPTS , 4HSORT , 4HNAMC , 4HNAMS , 4HTOLE , 4HCONN , 4HCOMP , 4HTRAN , 4HSYMT , 4HSEAR , 4HOUTP/
   DATA isym/4 , 2 , 1 , 6 , 6 , 5 , 5 , 3 , 3 , 6*7 , 1HX , 1HY , 1HZ , 2HXY , 2HYX , 2HXZ , 2HZX , 2HYZ , 2HZY , 3HXYZ , 3HXZY ,  &
       &3HYXZ , 3HYZX , 3HZXY , 3HZYX/
   DATA ihd/74*4H     , 4H SUM , 4HMARY , 4H OF  , 4HCASE , 4H CON , 4HTROL , 4H FOR , 4H COM , 4HBINE , 4H OPE , 4HRATI , 4HON   , &
      & 10*4H    /
   DATA nheqss/4HEQSS/
   DATA papp , loap , lods/4HPAPP , 4HLOAP , 4HLODS/
!
!     OPEN CASECC DATA BLOCK AND READ INTO OPEN CORE
!
   srch = .FALSE.
   ierr = 0
   DO i = 1 , 96
      Ihead(i) = ihd(i)
   ENDDO
   ifile = Casecc
   CALL open(*1200,Casecc,Z(Buf2),0)
   nrec = Step
   IF ( nrec/=0 ) THEN
      DO i = 1 , nrec
         CALL fwdrec(*1200,Casecc)
      ENDDO
   ENDIF
   CALL read(*1100,*1300,Casecc,Z(1),5,0,nnn)
   i = 2
   nwdscc = Z(i)
   Npsub = Z(i+1)
   CALL read(*1100,*100,Casecc,Z(1),nwdscc,1,nnn)
 100  jj = 0
   kk = 0
   Iprint = 0
!
!     INITIALIZE COMBO AND RESTCT ARRAYS
!
   DO i = 1 , 7
      DO j = 1 , 5
         Combo(i,j) = 0
      ENDDO
      DO j = 1 , 7
         Restct(i,j) = 0
      ENDDO
   ENDDO
!
!     INITIALIZE COMP,TRANS,AND SYMT ARRAYS
!
   Conect = .FALSE.
   Tran = .FALSE.
   DO i = 1 , 7
      symt(i) = 0
      trans(i) = 0
      DO j = 1 , 2
         comp(i,j) = 0
      ENDDO
   ENDDO
   DO i = 1 , 3
      lf(i) = .FALSE.
   ENDDO
   Cnam(1) = 0
   Cnam(2) = 0
!
!     PROCESS CASE CONTROL MNEMONICS
!
   DO i = 1 , nwdscc , 3
      DO j = 1 , nmnem
         IF ( Z(i)==mnem(j) ) THEN
            IF ( j==1 ) GOTO 150
            IF ( j==2 ) GOTO 200
            IF ( j==3 ) GOTO 300
            IF ( j==4 ) GOTO 350
            IF ( j==5 ) GOTO 400
            IF ( j==6 ) GOTO 450
            IF ( j==7 ) GOTO 500
            IF ( j==8 ) GOTO 550
            IF ( j==9 ) GOTO 600
            IF ( j==10 ) GOTO 700
            IF ( j==11 ) GOTO 800
         ENDIF
      ENDDO
      CYCLE
 150  Iauto = .FALSE.
      IF ( Z(i+1)==auto ) Iauto = .TRUE.
      CYCLE
!
 200  DO l = 1 , 3
         IF ( Z(i+1)==idir(l) ) GOTO 250
      ENDDO
      Isort = 1
      CYCLE
 250  Isort = l
      CYCLE
!
 300  IF ( lf(1) ) GOTO 850
      lf(1) = .TRUE.
      Cnam(1) = Z(i+1)
      Cnam(2) = Z(i+2)
      CYCLE
!
 350  jj = jj + 1
      snam(jj,1) = Z(i+1)
      snam(jj,2) = Z(i+2)
      CYCLE
!
 400  IF ( lf(2) ) GOTO 850
      lf(2) = .TRUE.
      Toler = Az(i+2)
      CYCLE
!
 450  IF ( lf(3) ) GOTO 850
      lf(3) = .TRUE.
      Conset = Z(i+2)
      Conect = .TRUE.
      CYCLE
!
 500  kk = kk + 1
      comp(kk,1) = Z(i+1)
      comp(kk,2) = Z(i+2)
      DO lindx = 1 , Npsub
         IF ( Z(i+1)==snam(lindx,1) .AND. Z(i+2)==snam(lindx,2) ) GOTO 900
      ENDDO
      WRITE (Outt,99022) Ufm , Z(i+1) , Z(i+2)
      ierr = 1
      CYCLE
!
 550  trans(kk) = Z(i+2)
      Tran = .TRUE.
      CYCLE
!
 600  DO l = 1 , 15
         IF ( Z(i+1)==isym(l,2) ) GOTO 650
      ENDDO
      ierr = 1
      WRITE (Outt,99001) Ufm , Z(i+1) , comp(kk,1) , comp(kk,2)
!
99001 FORMAT (A23,' 6505, THE SYMMETRY OPTION ',A4,' CONTAINS AN INVALID SYMBOL.')
      CYCLE
 650  symt(kk) = isym(l,1)
      CYCLE
!
 700  DO l = 1 , Npsub
         IF ( Z(i+1)==snam(l,1) .AND. Z(i+2)==snam(l,2) ) GOTO 750
      ENDDO
      WRITE (Outt,99022) Ufm , Z(i+1) , Z(i+2)
      ierr = 1
      CYCLE
 750  srch = .TRUE.
      Restct(lindx,l) = 1
      Restct(l,lindx) = 1
      CYCLE
!
 800  Iprint = orf(Iprint,Z(i+2))
      CYCLE
!
 850  IF ( j==1 .OR. j==2 .OR. j==4 ) CYCLE
      IF ( j==5 ) THEN
         WRITE (Outt,99002) Ufm
99002    FORMAT (A23,' 6520, REDUNDANT VALUES FOR TOLER HAVE BEEN ','SPECIFIED.')
      ELSEIF ( j==6 ) THEN
         WRITE (Outt,99003) Ufm
99003    FORMAT (A23,' 6512, REDUNDANT CONNECTION SET ID S HAVE BEEN ','SPECIFIED.')
      ELSE
         WRITE (Outt,99004) Ufm
99004    FORMAT (A23,' 6519, REDUNDANT NAMES FOR RESULTANT PSEUDOSTRUCTURE',' HAVE BEEN SPECIFIED.')
      ENDIF
      ierr = 1
 900  ENDDO
!
!     IF NO SEARCH OPTIONS SPECIFIED - SEARCH ALL POSSIBLE CONNECTIONS
!
   IF ( .NOT.(srch) ) THEN
      DO i = 1 , 7
         DO j = 1 , 7
            Restct(i,j) = 1
         ENDDO
      ENDDO
   ENDIF
   DO i = 1 , Npsub
      DO j = 1 , Npsub
         IF ( snam(i,1)==comp(j,1) .AND. snam(i,2)==comp(j,2) ) GOTO 950
      ENDDO
      Combo(i,1) = snam(i,1)
      Combo(i,2) = snam(i,2)
      Combo(i,3) = 0
      Combo(i,4) = 0
      CYCLE
 950  Combo(i,1) = snam(i,1)
      Combo(i,2) = snam(i,2)
      Combo(i,3) = trans(j)
      Combo(i,4) = symt(j)
   ENDDO
   CALL close(Casecc,1)
   CALL page
   WRITE (Outt,99005) Npsub
99005 FORMAT (/10X,'THIS JOB STEP WILL COMBINE ',I1,' PSEUDOSTRUCTURES')
   IF ( Iauto ) WRITE (Outt,99006)
99006 FORMAT (/10X,40HCONNECTIONS ARE GENERATED AUTOMATICALLY.)
   IF ( .NOT.Iauto ) WRITE (Outt,99007)
99007 FORMAT (/10X,35HCONNECTIONS ARE SPECIFIED MANUALLY.)
   IF ( .NOT.(Iauto .OR. Conect) ) THEN
      WRITE (Outt,99008) Ufm
99008 FORMAT (A23,' 6501, THE MANUAL COMBINE OPTION HAS BEEN SPECIFIED',', BUT NO CONNECTION SET WAS GIVEN.')
      ierr = 1
   ENDIF
   IF ( Conect ) WRITE (Outt,99009) Conset
99009 FORMAT (/10X,25HTHE CONNECTION SET ID IS ,I8)
   IF ( Cnam(1)==0 .AND. Cnam(2)==0 ) THEN
      WRITE (Outt,99010) Ufm
99010 FORMAT (A23,' 6502, NO NAME HAS BEEN SPECIFIED FOR THE RESULTANT',' COMBINED PSEUDOSTRUCTURE.')
      ierr = 1
   ELSE
      WRITE (Outt,99011) Cnam
99011 FORMAT (/10X,38HTHE RESULTANT PSEUDOSTRUCTURE NAME IS ,2A4)
      CALL fdsub(Cnam,itest)
      IF ( itest/=-1 ) THEN
         litm = lods
         IF ( Pora==papp ) litm = loap
         CALL sfetch(Cnam,litm,3,itest)
         Lonly = .FALSE.
         IF ( itest==3 ) THEN
!
!     NEW LODS ONLY DEFINED
!
            Lonly = .TRUE.
            RETURN
         ELSEIF ( Pora==papp ) THEN
!
!     OPTIONS PA YET LOAP ITEM ALREADY EXISTS
!
            WRITE (Outt,99012) Ufm , Cnam
99012       FORMAT (A23,' 6533, OPTIONS PA HAS BEEN SPECIFIED BUT THE LOAP ','ITEM ALREADY EXISTS FOR SUBSTRUCTURE ',2A4)
            ierr = 1
            GOTO 1000
         ELSE
            WRITE (Outt,99013) Ufm
99013       FORMAT (A23,' 6508, THE NAME SPECIFIED FOR THE RESULTANT ','PSEUDOSTRUCTURE',/32X,'ALREADY EXISTS ON THE SOF.')
            ierr = 1
         ENDIF
      ELSEIF ( Pora==papp ) THEN
!
!     OPTIONS PA YET SUBSTRUCTURE DOES NOT EXIST
!
         WRITE (Outt,99014) Ufm , Cnam
99014    FORMAT (A23,' 6534, OPTIONS PA HAS BEEN SPECIFIED BUT THE ','SUBSTRUCTURE ',2A4,' DOES NOT EXIST.',/30X,                   &
                &'YOU CANNOT APPEND SOMETHING TO NOTHING.')
         ierr = 1
         GOTO 1000
      ENDIF
      IF ( .NOT.lf(2) ) THEN
!
         WRITE (Outt,99015) Ufm
99015    FORMAT (A23,' 6504, A TOLERANCE MUST BE SPECIFIED FOR A COMBINE ','OPERATION.')
         ierr = 1
      ELSE
         WRITE (Outt,99016) Toler
99016    FORMAT (/10X,32HTHE TOLERANCE ON CONNECTIONS IS ,E15.6)
         CALL decode(Iprint,ibits,nflg)
         IF ( nflg==0 ) ibits(1) = 0
         IF ( nflg/=0 ) THEN
            DO i = 1 , nflg
               ibits(i) = ibits(i) + 1
            ENDDO
         ENDIF
         WRITE (Outt,99017) (ibits(kdh),kdh=1,nflg)
99017    FORMAT (/10X,30HTHE PRINT CONTROL OPTIONS ARE ,25I3)
      ENDIF
      DO i = 1 , Npsub
         WRITE (Outt,99018) i , Combo(i,1) , Combo(i,2)
99018    FORMAT (/10X,27HCOMPONENT SUBSTRUCTURE NO. ,I1,8H NAME = ,2A4)
         ncnam(1) = Combo(i,1)
         ncnam(2) = Combo(i,2)
         CALL sfetch(ncnam,nheqss,3,itest)
         IF ( itest==4 ) WRITE (Outt,99019) Ufm , ncnam
99019    FORMAT (A23,' 6507, THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST ON ','THE SOF FILE')
         IF ( itest==4 ) Idry = -2
         IF ( Combo(i,3)/=0 ) WRITE (Outt,99020) Combo(i,3)
99020    FORMAT (/15X,15HTRANS SET ID = ,I8)
         IF ( Combo(i,4)/=0 ) THEN
            DO mj = 1 , 15
               IF ( Combo(i,4)==isym(mj,1) ) EXIT
            ENDDO
            WRITE (Outt,99021) isym(mj,2)
99021       FORMAT (15X,22HSYMMETRY DIRECTIONS = ,A4)
         ENDIF
      ENDDO
   ENDIF
 1000 IF ( ierr==1 ) Idry = -2
   GOTO 1500
 1100 imsg = -2
   GOTO 1400
 1200 imsg = -1
   GOTO 1400
 1300 imsg = -3
 1400 CALL mesage(imsg,ifile,aaa)
 1500 RETURN
99022 FORMAT (A23,' 6506, THE COMPONENT SUBSTRUCTURE ',2A4,' IS NOT ONE OF THOSE ON THE COMBINE CARD.')
END SUBROUTINE cmcase
