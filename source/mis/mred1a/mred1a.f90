!*==mred1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred1a(Mode)
!
!     THIS SUBROUTINE PROCESSES THE BDYC DATA FOR THE FIXED
!     IDENTIFICATION SET (FIXSET) AND THE BOUNDARY IDENTIFICATION SET
!     (BNDSET) FOR THE MRED1 MODULE.
!
!     INPUT DATA
!     GINO - GEOM4    - BDYC DATA
!     MODE            - PROCESSING OPERATION FLAG
!                     = 1, PROCESS FIXED ID SET
!                     = 2, PROCESS BOUNDARY ID SET
!
!     OUTPUT DATA
!     GINO - USETX    - S,R,B DEGREES OF FREEDOM
!
!     PARAMETERS
!     INPUT  - GBUF1  - GINO BUFFER
!              KORLEN - CORE LENGTH
!              BNDSET - BOUNDARY SET IDENTIFICATION NUMBER
!              FIXSET - FIXED SET IDENTIFICATION NUMBER
!              IO     - OUTPUT OPTION FLAG
!              KORUST - STARTING ADDRESS OF USET ARRAY
!              NCSUBS - NUMBER OF CONTRIBUTING SUBSTRUCTURES
!              NAMEBS - BEGINNING ADDRESS OF BASIC SUBSTRUCTURES NAMES
!              KBDYC  - BEGINNING ADDRESS OF BDYC DATA
!              NBDYCC - NUMBER OF BDYC CARDS
!              USETL  - NUMBER OF WORDS IN USET ARRAY
!     OUTPUT - NOUS   - FIXED POINTS FLAG
!                       .GE.  0, FIXED POINTS DEFINED
!                       .EQ. -1, NO FIXED POINTS DEFINED
!              DRY    - MODULE OPERATION FLAG
!
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: array
   INTEGER , DIMENSION(2) , SAVE :: bdyc , modnam
   INTEGER , SAVE :: geom4
   INTEGER :: i , ibits , ifile , iflag , ii , imsg , ishift , j , k , l , ll , nwds , setid
   EXTERNAL andf , close , locate , mesage , page1 , preloc , read , rshift , sofcls , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA geom4 , bdyc/102 , 910 , 9/
   DATA modnam/4HMRED , 4H1A  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST PROCESSING MODE FLAG
!
         IF ( Mode==2 ) THEN
!
!     SET BOUNDARY INDEX
!
            IF ( Bndset==0 ) THEN
               IF ( Ponly ) RETURN
               WRITE (Iprntr,99001) Ufm
99001          FORMAT (A23,' 6603, A BOUNDARY SET MUST BE SPECIFIED FOR A REDUCE',' OPERATION.')
               Dry = -2
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               setid = Bndset
               ishift = 1
!
!     ALLOCATE USET ARRAY AND TEST OPEN CORE LENGTH
!
               IF ( Nous==1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     TEST FIXED SET ID FLAG AND SET FIXED INDEX
!
         ELSEIF ( Fixset==0 .OR. Skipm==-1 ) THEN
!
!     NO FIXED ID SET DATA
!
            Nous = -1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ELSE
            setid = Fixset
            ishift = 10
         ENDIF
         Kbdyc = Korust + Usetl
         IF ( Kbdyc>=Korlen ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TURN UL, UA, UF, UN, AND UI BITS ON IN USET ARRAY
!
         ibits = Itwo(Ul) + Itwo(Ua) + Itwo(Uf) + Itwo(Un) + Itwo(Ui)
         DO i = 1 , Usetl
            Z(Korust+i-1) = ibits
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ BOUNDARY SET (BDYC) BULK DATA FOR REQUESTED FIXED SET
!     ID (FIXSET) OR BOUNDARY SET ID (BNDSET)
!
         ifile = geom4
         CALL preloc(*20,Z(Gbuf1),geom4)
         CALL locate(*80,Z(Gbuf1),bdyc,iflag)
         SPAG_Loop_1_1: DO
            CALL read(*40,*80,geom4,array,1,0,iflag)
            IF ( array(1)==setid ) THEN
!
!     SET ID FOUND, STORE AT Z(KBDYC+NWDS)
!
               nwds = 0
               EXIT SPAG_Loop_1_1
            ELSE
               SPAG_Loop_2_2: DO
                  CALL read(*40,*60,geom4,array,3,0,iflag)
                  IF ( array(3)==-1 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_3: DO
            CALL read(*40,*60,geom4,Z(Kbdyc+nwds),3,0,iflag)
            IF ( Z(Kbdyc+nwds+2)==-1 ) THEN
!
!     CHECK FOR DUPLICATE BDYC SUBSTRUCTURE NAMES
!
               nwds = nwds/4
               IF ( nwds>1 ) THEN
                  i = nwds - 1
                  DO j = 1 , i
                     k = j + 1
                     ii = 4*(j-1)
                     DO l = k , nwds
                        ll = 4*(l-1)
                        IF ( Z(Kbdyc+ii)==Z(Kbdyc+ll) ) THEN
                           IF ( Z(Kbdyc+ii+1)==Z(Kbdyc+ll+1) ) THEN
                              WRITE (Iprntr,99002) Ufm , Oldnam , array(1)
99002                         FORMAT (A23,' 6623, SUBSTRUCTURE ',2A4,' HAS DUPLICATE NAMES IN BDYC DATA SET ',I8,1H.)
                              Dry = -2
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
               ENDIF
!
!     TEST OUTPUT OPTION
!
               IF ( andf(rshift(Io,ishift),1)/=0 ) THEN
                  IF ( nwds/=0 ) THEN
                     Line = Nlpp + 1
                     DO i = 1 , nwds
                        IF ( Line>Nlpp ) THEN
                           CALL page1
                           IF ( Mode==1 ) WRITE (Iprntr,99003) Fixset
99003                      FORMAT (1H0,43X,'SUMMARY OF COMBINED FIXED SET NUMBER ',I8,//57X,'BASIC      FIXED',/54X,                &
                                  &'SUBSTRUCTURE  SET ID',/58X,'NAME      NUMBER',/)
                           IF ( Mode==2 ) WRITE (Iprntr,99004) Bndset
99004                      FORMAT (1H0,43X,'SUMMARY OF COMBINED BOUNDARY SET NUMBER ',I8,//57X,'BASIC      BOUNDARY',/54X,          &
                                  &'SUBSTRUCTURE   SET ID',/58X,'NAME       NUMBER',/)
                           Line = Line + 7
                        ENDIF
                        j = 4*(i-1)
                        IF ( Mode==1 ) WRITE (Iprntr,99005) Z(Kbdyc+j) , Z(Kbdyc+j+1) , Z(Kbdyc+j+2)
99005                   FORMAT (56X,2A4,3X,I8)
                        IF ( Mode==2 ) WRITE (Iprntr,99006) Z(Kbdyc+j) , Z(Kbdyc+j+1) , Z(Kbdyc+j+2)
99006                   FORMAT (56X,2A4,4X,I8)
                        Line = Line + 1
                     ENDDO
                  ENDIF
               ENDIF
!
!     SORT BDYC DATA ON SET ID
!
               Nbdycc = nwds
               IF ( Nbdycc>1 ) THEN
                  nwds = 4*Nbdycc
                  CALL sort(0,0,4,3,Z(Kbdyc),nwds)
               ENDIF
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     CHECK THAT SUBSTRUCTURE IS A COMPONENT OF STRUCTURE BEING
!     REDUCED
!
               DO i = 1 , Ncsubs
                  j = 2*(i-1)
                  IF ( (Z(Namebs+j)==Z(Kbdyc+nwds)) .AND. (Z(Namebs+j+1)==Z(Kbdyc+nwds+1)) ) EXIT SPAG_Loop_1_3
               ENDDO
!
!     SUBSTRUCTURE IS NOT A COMPONENT
!
               IF ( Mode==1 ) WRITE (Iprntr,99007) Uwm , Z(Kbdyc+nwds) , Z(Kbdyc+nwds+1)
!
99007          FORMAT (A25,' 6622, A FIXED SET HAS BEEN SPECIFIED FOR ',2A4,', BUT IT IS NOT A COMPONENT OF',/32X,                  &
                      &'THE PSEUDOSTRUCTURE',' BEING PROCESSED.  THE FIXED SET WILL BE IGNORED.')
               IF ( Mode==2 ) WRITE (Iprntr,99008) Uwm , Z(Kbdyc+nwds) , Z(Kbdyc+nwds+1)
99008          FORMAT (A25,' 6604, A BOUNDARY SET HAS BEEN SPECIFIED FOR ',2A4,', BUT IT IS NOT A COMPONENT OF',/32X,               &
                      &'THE PSEUDOSTRUCTURE',' BEING PROCESSED.  THE BOUNDARY SET WILL BE IGNORED.')
               Dry = -2
            ENDIF
         ENDDO SPAG_Loop_1_3
!
!     SAVE BASIC SUBSTRUCTURE INDEX
!
         Z(Kbdyc+nwds+3) = i
         nwds = nwds + 4
         IF ( Kbdyc+nwds>=Korlen ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 5
      CASE (5)
         CALL close(geom4,1)
         spag_nextblock_1 = 6
      CASE (6)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 80      IF ( Mode==1 ) WRITE (Iprntr,99009) Uwm , Fixset
99009    FORMAT (A25,' 6621, FIXED SET',I9,' SPECIFIED IN CASE CONTROL ','HAS NOT BEEN DEFINED BY BULK DATA.')
         IF ( Mode==2 ) WRITE (Iprntr,99010) Uwm , Bndset
99010    FORMAT (A25,' 6606, BOUNDARY SET',I9,' SPECIFIED IN CASE CONTROL',' HAS NOT BEEN DEFINED BY BULK DATA.')
         Dry = -1
         spag_nextblock_1 = 7
      CASE (7)
         CALL sofcls
         CALL close(geom4,1)
         RETURN
      CASE (8)
!
!     END OF PROCESSING
!
         CALL close(geom4,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred1a
