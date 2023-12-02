!*==cmcase.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmcase
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_output
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
            ihead(i) = ihd(i)
         ENDDO
         ifile = casecc
         CALL open(*60,casecc,z(buf2),0)
         nrec = step
         IF ( nrec/=0 ) THEN
            DO i = 1 , nrec
               CALL fwdrec(*60,casecc)
            ENDDO
         ENDIF
         CALL read(*40,*80,casecc,z(1),5,0,nnn)
         i = 2
         nwdscc = z(i)
         npsub = z(i+1)
         CALL read(*40,*20,casecc,z(1),nwdscc,1,nnn)
 20      jj = 0
         kk = 0
         iprint = 0
!
!     INITIALIZE COMBO AND RESTCT ARRAYS
!
         DO i = 1 , 7
            DO j = 1 , 5
               combo(i,j) = 0
            ENDDO
            DO j = 1 , 7
               restct(i,j) = 0
            ENDDO
         ENDDO
!
!     INITIALIZE COMP,TRANS,AND SYMT ARRAYS
!
         conect = .FALSE.
         tran = .FALSE.
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
         cnam(1) = 0
         cnam(2) = 0
!
!     PROCESS CASE CONTROL MNEMONICS
!
         SPAG_Loop_1_1: DO i = 1 , nwdscc , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  SPAG_Loop_4_1: DO j = 1 , nmnem
                     IF ( z(i)==mnem(j) ) THEN
                        IF ( j==1 ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==2 ) THEN
                           spag_nextblock_2 = 3
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==3 ) THEN
                           spag_nextblock_2 = 5
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==4 ) THEN
                           spag_nextblock_2 = 6
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==5 ) THEN
                           spag_nextblock_2 = 7
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==6 ) THEN
                           spag_nextblock_2 = 8
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==7 ) THEN
                           spag_nextblock_2 = 9
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==8 ) THEN
                           spag_nextblock_2 = 10
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==9 ) THEN
                           spag_nextblock_2 = 11
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==10 ) THEN
                           spag_nextblock_2 = 13
                           EXIT SPAG_Loop_4_1
                        ENDIF
                        IF ( j==11 ) THEN
                           spag_nextblock_2 = 15
                           EXIT SPAG_Loop_4_1
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_4_1
               CASE (2)
                  iauto = .FALSE.
                  IF ( z(i+1)==auto ) iauto = .TRUE.
               CASE (3)
!
                  DO l = 1 , 3
                     IF ( z(i+1)==idir(l) ) THEN
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  isort = 1
               CASE (4)
                  isort = l
               CASE (5)
!
                  IF ( lf(1) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(1) = .TRUE.
                  cnam(1) = z(i+1)
                  cnam(2) = z(i+2)
               CASE (6)
!
                  jj = jj + 1
                  snam(jj,1) = z(i+1)
                  snam(jj,2) = z(i+2)
               CASE (7)
!
                  IF ( lf(2) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(2) = .TRUE.
                  toler = az(i+2)
               CASE (8)
!
                  IF ( lf(3) ) THEN
                     spag_nextblock_2 = 16
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  lf(3) = .TRUE.
                  conset = z(i+2)
                  conect = .TRUE.
               CASE (9)
!
                  kk = kk + 1
                  comp(kk,1) = z(i+1)
                  comp(kk,2) = z(i+2)
                  DO lindx = 1 , npsub
                     IF ( z(i+1)==snam(lindx,1) .AND. z(i+2)==snam(lindx,2) ) EXIT SPAG_DispatchLoop_2
                  ENDDO
                  WRITE (outt,99022) ufm , z(i+1) , z(i+2)
                  ierr = 1
               CASE (10)
!
                  trans(kk) = z(i+2)
                  tran = .TRUE.
               CASE (11)
!
                  DO l = 1 , 15
                     IF ( z(i+1)==isym(l,2) ) THEN
                        spag_nextblock_2 = 12
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  ierr = 1
                  WRITE (outt,99001) ufm , z(i+1) , comp(kk,1) , comp(kk,2)
!
99001             FORMAT (A23,' 6505, THE SYMMETRY OPTION ',A4,' CONTAINS AN INVALID SYMBOL.')
               CASE (12)
                  symt(kk) = isym(l,1)
               CASE (13)
!
                  DO l = 1 , npsub
                     IF ( z(i+1)==snam(l,1) .AND. z(i+2)==snam(l,2) ) THEN
                        spag_nextblock_2 = 14
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  WRITE (outt,99022) ufm , z(i+1) , z(i+2)
                  ierr = 1
               CASE (14)
                  srch = .TRUE.
                  restct(lindx,l) = 1
                  restct(l,lindx) = 1
               CASE (15)
!
                  iprint = orf(iprint,z(i+2))
               CASE (16)
!
                  IF ( j==1 .OR. j==2 .OR. j==4 ) CYCLE
                  IF ( j==5 ) THEN
                     WRITE (outt,99002) ufm
99002                FORMAT (A23,' 6520, REDUNDANT VALUES FOR TOLER HAVE BEEN ','SPECIFIED.')
                  ELSEIF ( j==6 ) THEN
                     WRITE (outt,99003) ufm
99003                FORMAT (A23,' 6512, REDUNDANT CONNECTION SET ID S HAVE BEEN ','SPECIFIED.')
                  ELSE
                     WRITE (outt,99004) ufm
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
                  restct(i,j) = 1
               ENDDO
            ENDDO
         ENDIF
         DO i = 1 , npsub
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  DO j = 1 , npsub
                     IF ( snam(i,1)==comp(j,1) .AND. snam(i,2)==comp(j,2) ) THEN
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  combo(i,1) = snam(i,1)
                  combo(i,2) = snam(i,2)
                  combo(i,3) = 0
                  combo(i,4) = 0
               CASE (2)
                  combo(i,1) = snam(i,1)
                  combo(i,2) = snam(i,2)
                  combo(i,3) = trans(j)
                  combo(i,4) = symt(j)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         CALL close(casecc,1)
         CALL page
         WRITE (outt,99005) npsub
99005    FORMAT (/10X,'THIS JOB STEP WILL COMBINE ',I1,' PSEUDOSTRUCTURES')
         IF ( iauto ) WRITE (outt,99006)
99006    FORMAT (/10X,40HCONNECTIONS ARE GENERATED AUTOMATICALLY.)
         IF ( .NOT.iauto ) WRITE (outt,99007)
99007    FORMAT (/10X,35HCONNECTIONS ARE SPECIFIED MANUALLY.)
         IF ( .NOT.(iauto .OR. conect) ) THEN
            WRITE (outt,99008) ufm
99008       FORMAT (A23,' 6501, THE MANUAL COMBINE OPTION HAS BEEN SPECIFIED',', BUT NO CONNECTION SET WAS GIVEN.')
            ierr = 1
         ENDIF
         IF ( conect ) WRITE (outt,99009) conset
99009    FORMAT (/10X,25HTHE CONNECTION SET ID IS ,I8)
         IF ( cnam(1)==0 .AND. cnam(2)==0 ) THEN
            WRITE (outt,99010) ufm
99010       FORMAT (A23,' 6502, NO NAME HAS BEEN SPECIFIED FOR THE RESULTANT',' COMBINED PSEUDOSTRUCTURE.')
            ierr = 1
         ELSE
            WRITE (outt,99011) cnam
99011       FORMAT (/10X,38HTHE RESULTANT PSEUDOSTRUCTURE NAME IS ,2A4)
            CALL fdsub(cnam,itest)
            IF ( itest/=-1 ) THEN
               litm = lods
               IF ( pora==papp ) litm = loap
               CALL sfetch(cnam,litm,3,itest)
               lonly = .FALSE.
               IF ( itest==3 ) THEN
!
!     NEW LODS ONLY DEFINED
!
                  lonly = .TRUE.
                  RETURN
               ELSEIF ( pora==papp ) THEN
!
!     OPTIONS PA YET LOAP ITEM ALREADY EXISTS
!
                  WRITE (outt,99012) ufm , cnam
99012             FORMAT (A23,' 6533, OPTIONS PA HAS BEEN SPECIFIED BUT THE LOAP ','ITEM ALREADY EXISTS FOR SUBSTRUCTURE ',2A4)
                  ierr = 1
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  WRITE (outt,99013) ufm
99013             FORMAT (A23,' 6508, THE NAME SPECIFIED FOR THE RESULTANT ','PSEUDOSTRUCTURE',/32X,'ALREADY EXISTS ON THE SOF.')
                  ierr = 1
               ENDIF
            ELSEIF ( pora==papp ) THEN
!
!     OPTIONS PA YET SUBSTRUCTURE DOES NOT EXIST
!
               WRITE (outt,99014) ufm , cnam
99014          FORMAT (A23,' 6534, OPTIONS PA HAS BEEN SPECIFIED BUT THE ','SUBSTRUCTURE ',2A4,' DOES NOT EXIST.',/30X,             &
                      &'YOU CANNOT APPEND SOMETHING TO NOTHING.')
               ierr = 1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( .NOT.lf(2) ) THEN
!
               WRITE (outt,99015) ufm
99015          FORMAT (A23,' 6504, A TOLERANCE MUST BE SPECIFIED FOR A COMBINE ','OPERATION.')
               ierr = 1
            ELSE
               WRITE (outt,99016) toler
99016          FORMAT (/10X,32HTHE TOLERANCE ON CONNECTIONS IS ,E15.6)
               CALL decode(iprint,ibits,nflg)
               IF ( nflg==0 ) ibits(1) = 0
               IF ( nflg/=0 ) THEN
                  DO i = 1 , nflg
                     ibits(i) = ibits(i) + 1
                  ENDDO
               ENDIF
               WRITE (outt,99017) (ibits(kdh),kdh=1,nflg)
99017          FORMAT (/10X,30HTHE PRINT CONTROL OPTIONS ARE ,25I3)
            ENDIF
            DO i = 1 , npsub
               WRITE (outt,99018) i , combo(i,1) , combo(i,2)
99018          FORMAT (/10X,27HCOMPONENT SUBSTRUCTURE NO. ,I1,8H NAME = ,2A4)
               ncnam(1) = combo(i,1)
               ncnam(2) = combo(i,2)
               CALL sfetch(ncnam,nheqss,3,itest)
               IF ( itest==4 ) WRITE (outt,99019) ufm , ncnam
99019          FORMAT (A23,' 6507, THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST ON ','THE SOF FILE')
               IF ( itest==4 ) idry = -2
               IF ( combo(i,3)/=0 ) WRITE (outt,99020) combo(i,3)
99020          FORMAT (/15X,15HTRANS SET ID = ,I8)
               IF ( combo(i,4)/=0 ) THEN
                  SPAG_Loop_2_2: DO mj = 1 , 15
                     IF ( combo(i,4)==isym(mj,1) ) EXIT SPAG_Loop_2_2
                  ENDDO SPAG_Loop_2_2
                  WRITE (outt,99021) isym(mj,2)
99021             FORMAT (15X,22HSYMMETRY DIRECTIONS = ,A4)
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( ierr==1 ) idry = -2
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
