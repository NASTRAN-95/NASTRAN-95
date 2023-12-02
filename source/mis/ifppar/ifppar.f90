!*==ifppar.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifppar
   USE c_ifpdta
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: app , ierr , ipm , ipn , msgno
   INTEGER , DIMENSION(4) , SAVE :: appr
   INTEGER , SAVE :: ctyp , hfre , ip1 , ip2 , ip3 , kind , lfre , lmod , nodj , nseg , que
   LOGICAL , SAVE :: ctype , hfreq , kindx , lfreq , lmode , nodje , nsegs , p1 , p2 , p3 , queue
   LOGICAL :: ltest , ptot
   EXTERNAL page2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SUBROUTINE TO TEST FOR PARAM CARD PARAMETERS REQUIRED BY VARIOUS
!     RIGID FORMATS.
!
   DATA appr/4HDMAP , 4HDISP , 4HHEAT , 4HAERO/
   DATA hfre/4HHFRE/ , lfre/4HLFRE/ , lmod/4HLMOD/
   DATA nodj/4HNODJ/ , ip1/4HP1  / , ip2/4HP2  /
   DATA ip3/4HP3  / , que/4HQ   /
   DATA ctyp/4HCTYP/ , kind/4HKIND/ , nseg/4HNSEG/
   DATA hfreq/.FALSE./ , lfreq/.FALSE./ , lmode/.FALSE./
   DATA nodje/.FALSE./ , ctype/.FALSE./ , kindx/.FALSE./
   DATA nsegs/.FALSE./ , p1/.FALSE./ , p2/.FALSE./
   DATA queue/.FALSE./ , p3/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         app = iabs(iapp)
!
!     NO PARAMS REQD FOR HEAT APPROACH,
!     DMAPS DISP 1 THRU 9, DISP 13, AND DISP 16 THRU 19, AND
!     AERO RF 9
!
         IF ( app==1 .OR. app==3 ) RETURN
         IF ( app==2 .AND. (rf<=9 .OR. rf==13 .OR. rf>=16) ) RETURN
         IF ( app==4 .AND. rf==9 ) RETURN
!
!     FATAL ERROR IF NO PARAMS ENTERED AS REQUIRED
!
         IF ( nparam==0 ) THEN
!
!     SET UP ERROR MESSAGE
!
            ASSIGN 20 TO ierr
            msgno = 340
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     LOOP TO TEST PARAMS IN PVT FOR PRESENCE OF REQUIRED ONES.
!
            ipm = 1
         ENDIF
         SPAG_Loop_1_1: DO
            ipn = 2*n1 + ipm
!
            IF ( rf>=14 ) THEN
!
               IF ( ibuff(ipn)==ctyp .AND. ibuff(ipn+2)/=0 ) ctype = .TRUE.
               IF ( ibuff(ipn)==nseg .AND. ibuff(ipn+2)/=0 ) nsegs = .TRUE.
               IF ( ibuff(ipn)==kind .AND. ibuff(ipn+2)/=0 ) kindx = .TRUE.
            ELSE
               IF ( ibuff(ipn)==hfre ) hfreq = .TRUE.
               IF ( ibuff(ipn)==lfre ) lfreq = .TRUE.
               IF ( ibuff(ipn)==lmod .AND. ibuff(ipn+2)/=0 ) lmode = .TRUE.
!
               IF ( app==4 ) THEN
                  IF ( ibuff(ipn)==nodj .AND. ibuff(ipn+2)/=0 ) nodje = .TRUE.
                  IF ( ibuff(ipn)==ip1 ) p1 = .TRUE.
                  IF ( ibuff(ipn)==ip2 ) p2 = .TRUE.
                  IF ( ibuff(ipn)==ip3 ) p3 = .TRUE.
                  IF ( ibuff(ipn)==que .AND. ibuff(ipn+2)/=0 ) queue = .TRUE.
               ENDIF
            ENDIF
!
            ipm = ipm + 4
            IF ( ibuff(ipn+2)>=3 .AND. ibuff(ipn+2)<=5 ) ipm = ipm + 1
            IF ( ibuff(ipn+2)>=6 ) ipm = ipm + 3
            IF ( ipm>=nparam ) THEN
!
!     TEST TO VERIFY THAT ALL REQUIRED PARAMS ARE PRESENT
!
               IF ( rf==14 .OR. rf==15 ) THEN
!
!     TEST FOR CTYPE, NSEGS, OR KINDEX REQD BY DISP RF 14 AND 15.
!
                  ltest = ctype .AND. nsegs
                  IF ( ltest ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ASSIGN 120 TO ierr
                  msgno = 345
                  spag_nextblock_1 = 5
               ELSE
                  IF ( lmode .AND. .NOT.(hfreq .OR. lfreq) ) EXIT SPAG_Loop_1_1
                  IF ( hfreq .AND. lfreq .AND. .NOT.lmode ) EXIT SPAG_Loop_1_1
!
!     SOMETING AMISS - - IS AN LMODES, HFREQ, OR LFREQ MISSING
!
                  IF ( .NOT.(lmode .OR. (hfreq .AND. lfreq)) ) THEN
                     ASSIGN 40 TO ierr
                     msgno = 341
                     spag_nextblock_1 = 5
                  ELSE
!
!     IS LMODES PRESENT WITH HFREQ AND/OR LFREQ
!
                     IF ( .NOT.(lmode .AND. (hfreq .OR. lfreq)) ) EXIT SPAG_Loop_1_1
                     ASSIGN 60 TO ierr
                     msgno = 342
                     spag_nextblock_1 = 6
                  ENDIF
               ENDIF
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( app/=4 ) RETURN
!
!     TEST FOR CORRECT NODJE SETUP FOR AERO RF 10 AND 11
!
         ptot = p1 .AND. p2 .AND. p3
         IF ( .NOT.(nodje .AND. ptot) ) THEN
            IF ( nodje .AND. .NOT.ptot ) THEN
               ASSIGN 80 TO ierr
               msgno = 343
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( (p1 .OR. p2 .OR. p3) .AND. .NOT.nodje ) THEN
               ASSIGN 100 TO ierr
               msgno = 344
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     TEST FOR Q REQUIRED BY AERO RF 11
!
         IF ( rf==10 ) RETURN
         IF ( queue ) RETURN
         ASSIGN 160 TO ierr
         msgno = 347
         spag_nextblock_1 = 5
      CASE (4)
         IF ( rf==14 ) RETURN
!
         IF ( kindx ) RETURN
         ASSIGN 140 TO ierr
         msgno = 346
         spag_nextblock_1 = 5
      CASE (5)
!
         CALL page2(3)
         WRITE (nout,99001) ufm , msgno
99001    FORMAT (A23,I4)
         abort = .TRUE.
         spag_nextblock_1 = 7
      CASE (6)
         CALL page2(3)
         WRITE (nout,99002) uwm , msgno
99002    FORMAT (A25,I4)
         spag_nextblock_1 = 7
      CASE (7)
         GOTO ierr
!
 20      WRITE (nout,99003) appr(app) , rf
99003    FORMAT (' PARAM CARDS REQUIRED BY ',A4,' RIGID FORMAT',I3,' NOT FOUND IN BULK DATA.')
         RETURN
!
 40      WRITE (nout,99004) appr(app) , rf
99004    FORMAT (' LMODES OR HFREQ/LFREQ PARAM REQUIRED BY ',A4,' RIGID FORMAT',I3,' NOT IN BULK DATA OR TURNED OFF.')
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 60      WRITE (nout,99005)
99005    FORMAT (' LMODES PARAM FOUND IN BULK DATA WITH HFREQ OR LFREQ.','  LMODES TAKES PRECEDENCE.')
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 80      WRITE (nout,99006) rf
99006    FORMAT (' NODJE PARAM SPECIFIED FOR AERO RIGID FORMAT',I3,' BUT P1, P2, OR P3 OMITTED.')
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 100     WRITE (nout,99007)
99007    FORMAT (' P1, P2, OR P3 PARAM FOUND IN BULK DATA BUT NODJE ','MISSING OR TURNED OFF.')
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 120     WRITE (nout,99008) rf
99008    FORMAT (' CTYPE OR NSEGS PARAM REQUIRED BY DISPLACEMENT RIGID ','FORMAT',I3,' MISSING OR INCORRECT.')
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
 140     WRITE (nout,99009)
99009    FORMAT (' KINDEX PARAM REQUIRED BY DISPLACEMENT RIGID FORMAT 15',' MISSING OR TURNED OFF.')
         RETURN
!
 160     WRITE (nout,99010)
99010    FORMAT (' DYNAMIC PRESSURE (Q) PARAM REQUIRED BY AERO RIGID FORM','AT 11 NOT IN BULK DATA.')
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE ifppar
