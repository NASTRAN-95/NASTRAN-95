!*==cmcase.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmcase
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER , SAVE :: auto , loap , lods , nheqss , nmnem , papp
   REAL , DIMENSION(1) :: az
   INTEGER , DIMENSION(7,2) :: comp , snam
   INTEGER :: i , ierr , ifile , imsg , itest , j , jj , kdh , kk , l , lindx , litm , mj , nflg , nnn , nrec , nwdscc
   INTEGER , DIMENSION(32) :: ibits
   INTEGER , DIMENSION(3) , SAVE :: idir
   INTEGER , DIMENSION(96) , SAVE :: ihd
   INTEGER , DIMENSION(15,2) , SAVE :: isym
   LOGICAL , DIMENSION(3) :: lf
   INTEGER , DIMENSION(11) , SAVE :: mnem
   INTEGER , DIMENSION(2) :: ncnam
   LOGICAL :: srch
   INTEGER , DIMENSION(7) :: symt , trans
   EXTERNAL close , decode , fdsub , fwdrec , mesage , open , orf , page , read , sfetch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS SUBROUTINE PROCESSES THE CASE CONTROL DATA BLOCK
!
   !>>>>EQUIVALENCE (Z(1),Az(1))
   DATA nmnem/11/ , idir/1HX , 1HY , 1HZ/ , auto/4HAUTO/ , aaa/4HCMCA , 4HSE  /
   DATA mnem/4HOPTS , 4HSORT , 4HNAMC , 4HNAMS , 4HTOLE , 4HCONN , 4HCOMP , 4HTRAN , 4HSYMT , 4HSEAR , 4HOUTP/
   DATA isym/4 , 2 , 1 , 6 , 6 , 5 , 5 , 3 , 3 , 6*7 , 1HX , 1HY , 1HZ , 2HXY , 2HYX , 2HXZ , 2HZX , 2HYZ , 2HZY , 3HXYZ , 3HXZY ,  &
       &3HYXZ , 3HYZX , 3HZXY , 3HZYX/
   DATA ihd/74*4H     , 4H SUM , 4HMARY , 4H OF  , 4HCASE , 4H CON , 4HTROL , 4H FOR , 4H COM , 4HBINE , 4H OPE , 4HRATI , 4HON   , &
      & 10*4H    /
   DATA nheqss/4HEQSS/
   DATA papp , loap , lods/4HPAPP , 4HLOAP , 4HLODS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN CASECC DATA BLOCK AND READ INTO OPEN CORE
!
         srch = .FALSE.
         ierr = 0
         DO i = 1 , 96
            Ihead(i) = ihd(i)
         ENDDO
         ifile = Casecc
         CALL open(*60,Casecc,Z(Buf2),0)
         nrec = Step
         IF ( nrec/=0 ) THEN
            DO i = 1 , nrec
               CALL fwdrec(*60,Casecc)
            ENDDO
         ENDIF
         CALL read(*40,*80,Casecc,Z(1),5,0,nnn)
         i = 2
         nwdscc = Z(i)
         Npsub = Z(i+1)
         CALL read(*40,*20,Casecc,Z(1),nwdscc,1,nnn)
 20      jj = 0
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
         SPAG_Loop_1_1: DO i = 1 , nwdscc , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  DO j = 1 , nmnem
                     IF ( Z(i)==mnem(j) ) THEN
                        IF ( j==1 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==2 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==3 ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==4 ) THEN
                           spag_nextblock_2 = 6
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==5 ) THEN
                           spag_nextblock_2 = 7
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==6 ) THEN
                           spag_nextblock_2 = 8
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==7 ) THEN
                           spag_nextblock_2 = 9
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==8 ) THEN
                           spag_nextblock_2 = 10
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==9 ) THEN
                           spag_nextblock_2 = 11
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==10 ) THEN
                           spag_nextblock_2 = 13
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( j==11 ) THEN
                           spag_nextblock_2 = 15
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                  ENDDO
                  CYCLE
               CASE (2)
                  Iauto = .FALSE.
                  IF ( Z(i+1)==auto ) Iauto = .TRUE.
                  CYCLE
               CASE (3)
!
                  DO l = 1 , 3
                     IF ( Z(i+1)==idir(l) ) THEN
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  Isort = 1
                  CYCLE
               CASE (4)
                  Isort = l
                  CYCLE
               CASE (5)
!
                  IF ( lf(1) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(1) = .TRUE.
                  Cnam(1) = Z(i+1)
                  Cnam(2) = Z(i+2)
                  CYCLE
               CASE (6)
!
                  jj = jj + 1
                  snam(jj,1) = Z(i+1)
                  snam(jj,2) = Z(i+2)
                  CYCLE
               CASE (7)
!
                  IF ( lf(2) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(2) = .TRUE.
                  Toler = az(i+2)
                  CYCLE
               CASE (8)
!
                  IF ( lf(3) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(3) = .TRUE.
                  Conset = Z(i+2)
                  Conect = .TRUE.
                  CYCLE
               CASE (9)
!
                  kk = kk + 1
                  comp(kk,1) = Z(i+1)
                  comp(kk,2) = Z(i+2)
                  DO lindx = 1 , Npsub
                     IF ( Z(i+1)==snam(lindx,1) .AND. Z(i+2)==snam(lindx,2) ) CYCLE SPAG_Loop_1_1
                  ENDDO
                  WRITE (Outt,99022) Ufm , Z(i+1) , Z(i+2)
                  ierr = 1
                  CYCLE
               CASE (10)
!
                  trans(kk) = Z(i+2)
                  Tran = .TRUE.
                  CYCLE
               CASE (11)
!
                  DO l = 1 , 15
                     IF ( Z(i+1)==isym(l,2) ) THEN
                        spag_nextblock_2 = 12
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  ierr = 1
                  WRITE (Outt,99001) Ufm , Z(i+1) , comp(kk,1) , comp(kk,2)
!
99001             FORMAT (A23,' 6505, THE SYMMETRY OPTION ',A4,' CONTAINS AN INVALID SYMBOL.')
                  CYCLE
               CASE (12)
                  symt(kk) = isym(l,1)
                  CYCLE
               CASE (13)
!
                  DO l = 1 , Npsub
                     IF ( Z(i+1)==snam(l,1) .AND. Z(i+2)==snam(l,2) ) THEN
                        spag_nextblock_2 = 14
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  WRITE (Outt,99022) Ufm , Z(i+1) , Z(i+2)
                  ierr = 1
                  CYCLE
               CASE (14)
                  srch = .TRUE.
                  Restct(lindx,l) = 1
                  Restct(l,lindx) = 1
                  CYCLE
               CASE (15)
!
                  Iprint = orf(Iprint,Z(i+2))
                  CYCLE
               CASE (16)
!
                  IF ( j==1 .OR. j==2 .OR. j==4 ) CYCLE
                  IF ( j==5 ) THEN
                     WRITE (Outt,99002) Ufm
99002                FORMAT (A23,' 6520, REDUNDANT VALUES FOR TOLER HAVE BEEN ','SPECIFIED.')
                  ELSEIF ( j==6 ) THEN
                     WRITE (Outt,99003) Ufm
99003                FORMAT (A23,' 6512, REDUNDANT CONNECTION SET ID S HAVE BEEN ','SPECIFIED.')
                  ELSE
                     WRITE (Outt,99004) Ufm
99004                FORMAT (A23,' 6519, REDUNDANT NAMES FOR RESULTANT PSEUDOSTRUCTURE',' HAVE BEEN SPECIFIED.')
                  ENDIF
                  ierr = 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_1
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
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  DO j = 1 , Npsub
                     IF ( snam(i,1)==comp(j,1) .AND. snam(i,2)==comp(j,2) ) THEN
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  Combo(i,1) = snam(i,1)
                  Combo(i,2) = snam(i,2)
                  Combo(i,3) = 0
                  Combo(i,4) = 0
                  CYCLE
               CASE (2)
                  Combo(i,1) = snam(i,1)
                  Combo(i,2) = snam(i,2)
                  Combo(i,3) = trans(j)
                  Combo(i,4) = symt(j)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         CALL close(Casecc,1)
         CALL page
         WRITE (Outt,99005) Npsub
99005    FORMAT (/10X,'THIS JOB STEP WILL COMBINE ',I1,' PSEUDOSTRUCTURES')
         IF ( Iauto ) WRITE (Outt,99006)
99006    FORMAT (/10X,40HCONNECTIONS ARE GENERATED AUTOMATICALLY.)
         IF ( .NOT.Iauto ) WRITE (Outt,99007)
99007    FORMAT (/10X,35HCONNECTIONS ARE SPECIFIED MANUALLY.)
         IF ( .NOT.(Iauto .OR. Conect) ) THEN
            WRITE (Outt,99008) Ufm
99008       FORMAT (A23,' 6501, THE MANUAL COMBINE OPTION HAS BEEN SPECIFIED',', BUT NO CONNECTION SET WAS GIVEN.')
            ierr = 1
         ENDIF
         IF ( Conect ) WRITE (Outt,99009) Conset
99009    FORMAT (/10X,25HTHE CONNECTION SET ID IS ,I8)
         IF ( Cnam(1)==0 .AND. Cnam(2)==0 ) THEN
            WRITE (Outt,99010) Ufm
99010       FORMAT (A23,' 6502, NO NAME HAS BEEN SPECIFIED FOR THE RESULTANT',' COMBINED PSEUDOSTRUCTURE.')
            ierr = 1
         ELSE
            WRITE (Outt,99011) Cnam
99011       FORMAT (/10X,38HTHE RESULTANT PSEUDOSTRUCTURE NAME IS ,2A4)
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
99012             FORMAT (A23,' 6533, OPTIONS PA HAS BEEN SPECIFIED BUT THE LOAP ','ITEM ALREADY EXISTS FOR SUBSTRUCTURE ',2A4)
                  ierr = 1
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  WRITE (Outt,99013) Ufm
99013             FORMAT (A23,' 6508, THE NAME SPECIFIED FOR THE RESULTANT ','PSEUDOSTRUCTURE',/32X,'ALREADY EXISTS ON THE SOF.')
                  ierr = 1
               ENDIF
            ELSEIF ( Pora==papp ) THEN
!
!     OPTIONS PA YET SUBSTRUCTURE DOES NOT EXIST
!
               WRITE (Outt,99014) Ufm , Cnam
99014          FORMAT (A23,' 6534, OPTIONS PA HAS BEEN SPECIFIED BUT THE ','SUBSTRUCTURE ',2A4,' DOES NOT EXIST.',/30X,             &
                      &'YOU CANNOT APPEND SOMETHING TO NOTHING.')
               ierr = 1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( .NOT.lf(2) ) THEN
!
               WRITE (Outt,99015) Ufm
99015          FORMAT (A23,' 6504, A TOLERANCE MUST BE SPECIFIED FOR A COMBINE ','OPERATION.')
               ierr = 1
            ELSE
               WRITE (Outt,99016) Toler
99016          FORMAT (/10X,32HTHE TOLERANCE ON CONNECTIONS IS ,E15.6)
               CALL decode(Iprint,ibits,nflg)
               IF ( nflg==0 ) ibits(1) = 0
               IF ( nflg/=0 ) THEN
                  DO i = 1 , nflg
                     ibits(i) = ibits(i) + 1
                  ENDDO
               ENDIF
               WRITE (Outt,99017) (ibits(kdh),kdh=1,nflg)
99017          FORMAT (/10X,30HTHE PRINT CONTROL OPTIONS ARE ,25I3)
            ENDIF
            DO i = 1 , Npsub
               WRITE (Outt,99018) i , Combo(i,1) , Combo(i,2)
99018          FORMAT (/10X,27HCOMPONENT SUBSTRUCTURE NO. ,I1,8H NAME = ,2A4)
               ncnam(1) = Combo(i,1)
               ncnam(2) = Combo(i,2)
               CALL sfetch(ncnam,nheqss,3,itest)
               IF ( itest==4 ) WRITE (Outt,99019) Ufm , ncnam
99019          FORMAT (A23,' 6507, THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST ON ','THE SOF FILE')
               IF ( itest==4 ) Idry = -2
               IF ( Combo(i,3)/=0 ) WRITE (Outt,99020) Combo(i,3)
99020          FORMAT (/15X,15HTRANS SET ID = ,I8)
               IF ( Combo(i,4)/=0 ) THEN
                  SPAG_Loop_2_2: DO mj = 1 , 15
                     IF ( Combo(i,4)==isym(mj,1) ) EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
                  WRITE (Outt,99021) isym(mj,2)
99021             FORMAT (15X,22HSYMMETRY DIRECTIONS = ,A4)
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( ierr==1 ) Idry = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -3
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(imsg,ifile,aaa)
         spag_nextblock_1 = 4
      CASE (4)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99022 FORMAT (A23,' 6506, THE COMPONENT SUBSTRUCTURE ',2A4,' IS NOT ONE OF THOSE ON THE COMBINE CARD.')
END SUBROUTINE cmcase
