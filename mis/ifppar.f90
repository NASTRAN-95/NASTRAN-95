
SUBROUTINE ifppar
   IMPLICIT NONE
   LOGICAL Abort
   INTEGER Iapp , Ibuff(1) , Idta(509) , N1 , N2(17) , N3(3) , Nout , Nparam , Rf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /ifpdta/ Idta , Nparam
   COMMON /system/ N1 , Nout , Abort , N2 , Iapp , N3 , Rf
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Ibuff
   INTEGER app , appr(4) , ctyp , hfre , ierr , ip1 , ip2 , ip3 , ipm , ipn , kind , lfre , lmod , msgno , nodj , nseg , que
   LOGICAL ctype , hfreq , kindx , lfreq , lmode , ltest , nodje , nsegs , p1 , p2 , p3 , ptot , queue
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
!
   app = iabs(Iapp)
!
!     NO PARAMS REQD FOR HEAT APPROACH,
!     DMAPS DISP 1 THRU 9, DISP 13, AND DISP 16 THRU 19, AND
!     AERO RF 9
!
   IF ( app==1 .OR. app==3 ) GOTO 99999
   IF ( app==2 .AND. (Rf<=9 .OR. Rf==13 .OR. Rf>=16) ) GOTO 99999
   IF ( app==4 .AND. Rf==9 ) GOTO 99999
!
!     FATAL ERROR IF NO PARAMS ENTERED AS REQUIRED
!
   IF ( Nparam==0 ) THEN
!
!     SET UP ERROR MESSAGE
!
      ASSIGN 700 TO ierr
      msgno = 340
      GOTO 400
   ELSE
!
!     LOOP TO TEST PARAMS IN PVT FOR PRESENCE OF REQUIRED ONES.
!
      ipm = 1
   ENDIF
   DO
      ipn = 2*N1 + ipm
!
      IF ( Rf>=14 ) THEN
!
         IF ( Ibuff(ipn)==ctyp .AND. Ibuff(ipn+2)/=0 ) ctype = .TRUE.
         IF ( Ibuff(ipn)==nseg .AND. Ibuff(ipn+2)/=0 ) nsegs = .TRUE.
         IF ( Ibuff(ipn)==kind .AND. Ibuff(ipn+2)/=0 ) kindx = .TRUE.
      ELSE
         IF ( Ibuff(ipn)==hfre ) hfreq = .TRUE.
         IF ( Ibuff(ipn)==lfre ) lfreq = .TRUE.
         IF ( Ibuff(ipn)==lmod .AND. Ibuff(ipn+2)/=0 ) lmode = .TRUE.
!
         IF ( app==4 ) THEN
            IF ( Ibuff(ipn)==nodj .AND. Ibuff(ipn+2)/=0 ) nodje = .TRUE.
            IF ( Ibuff(ipn)==ip1 ) p1 = .TRUE.
            IF ( Ibuff(ipn)==ip2 ) p2 = .TRUE.
            IF ( Ibuff(ipn)==ip3 ) p3 = .TRUE.
            IF ( Ibuff(ipn)==que .AND. Ibuff(ipn+2)/=0 ) queue = .TRUE.
         ENDIF
      ENDIF
!
      ipm = ipm + 4
      IF ( Ibuff(ipn+2)>=3 .AND. Ibuff(ipn+2)<=5 ) ipm = ipm + 1
      IF ( Ibuff(ipn+2)>=6 ) ipm = ipm + 3
      IF ( ipm>=Nparam ) THEN
!
!     TEST TO VERIFY THAT ALL REQUIRED PARAMS ARE PRESENT
!
         IF ( Rf==14 .OR. Rf==15 ) THEN
!
!     TEST FOR CTYPE, NSEGS, OR KINDEX REQD BY DISP RF 14 AND 15.
!
            ltest = ctype .AND. nsegs
            IF ( ltest ) GOTO 300
            ASSIGN 1200 TO ierr
            msgno = 345
            GOTO 400
         ELSE
            IF ( lmode .AND. .NOT.(hfreq .OR. lfreq) ) EXIT
            IF ( hfreq .AND. lfreq .AND. .NOT.lmode ) EXIT
!
!     SOMETING AMISS - - IS AN LMODES, HFREQ, OR LFREQ MISSING
!
            IF ( .NOT.(lmode .OR. (hfreq .AND. lfreq)) ) THEN
               ASSIGN 800 TO ierr
               msgno = 341
               GOTO 400
            ELSE
!
!     IS LMODES PRESENT WITH HFREQ AND/OR LFREQ
!
               IF ( .NOT.(lmode .AND. (hfreq .OR. lfreq)) ) EXIT
               ASSIGN 900 TO ierr
               msgno = 342
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
 100  IF ( app/=4 ) GOTO 99999
!
!     TEST FOR CORRECT NODJE SETUP FOR AERO RF 10 AND 11
!
   ptot = p1 .AND. p2 .AND. p3
   IF ( .NOT.(nodje .AND. ptot) ) THEN
      IF ( nodje .AND. .NOT.ptot ) THEN
         ASSIGN 1000 TO ierr
         msgno = 343
         GOTO 400
      ELSEIF ( (p1 .OR. p2 .OR. p3) .AND. .NOT.nodje ) THEN
         ASSIGN 1100 TO ierr
         msgno = 344
         GOTO 500
      ENDIF
   ENDIF
!
!     TEST FOR Q REQUIRED BY AERO RF 11
!
 200  IF ( Rf==10 ) GOTO 99999
   IF ( queue ) GOTO 99999
   ASSIGN 1400 TO ierr
   msgno = 347
   GOTO 400
 300  IF ( Rf==14 ) GOTO 99999
!
   IF ( kindx ) GOTO 99999
   ASSIGN 1300 TO ierr
   msgno = 346
!
 400  CALL page2(3)
   WRITE (Nout,99001) Ufm , msgno
99001 FORMAT (A23,I4)
   Abort = .TRUE.
   GOTO 600
 500  CALL page2(3)
   WRITE (Nout,99002) Uwm , msgno
99002 FORMAT (A25,I4)
 600  GOTO ierr
!
 700  WRITE (Nout,99003) appr(app) , Rf
99003 FORMAT (' PARAM CARDS REQUIRED BY ',A4,' RIGID FORMAT',I3,' NOT FOUND IN BULK DATA.')
   GOTO 99999
!
 800  WRITE (Nout,99004) appr(app) , Rf
99004 FORMAT (' LMODES OR HFREQ/LFREQ PARAM REQUIRED BY ',A4,' RIGID FORMAT',I3,' NOT IN BULK DATA OR TURNED OFF.')
   GOTO 100
!
 900  WRITE (Nout,99005)
99005 FORMAT (' LMODES PARAM FOUND IN BULK DATA WITH HFREQ OR LFREQ.','  LMODES TAKES PRECEDENCE.')
   GOTO 100
!
 1000 WRITE (Nout,99006) Rf
99006 FORMAT (' NODJE PARAM SPECIFIED FOR AERO RIGID FORMAT',I3,' BUT P1, P2, OR P3 OMITTED.')
   GOTO 200
!
 1100 WRITE (Nout,99007)
99007 FORMAT (' P1, P2, OR P3 PARAM FOUND IN BULK DATA BUT NODJE ','MISSING OR TURNED OFF.')
   GOTO 200
!
 1200 WRITE (Nout,99008) Rf
99008 FORMAT (' CTYPE OR NSEGS PARAM REQUIRED BY DISPLACEMENT RIGID ','FORMAT',I3,' MISSING OR INCORRECT.')
   GOTO 300
!
 1300 WRITE (Nout,99009)
99009 FORMAT (' KINDEX PARAM REQUIRED BY DISPLACEMENT RIGID FORMAT 15',' MISSING OR TURNED OFF.')
   GOTO 99999
!
 1400 WRITE (Nout,99010)
99010 FORMAT (' DYNAMIC PRESSURE (Q) PARAM REQUIRED BY AERO RIGID FORM','AT 11 NOT IN BULK DATA.')
!
99999 RETURN
END SUBROUTINE ifppar
