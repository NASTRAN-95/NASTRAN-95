
SUBROUTINE qparam
   IMPLICIT NONE
   INTEGER In1 , In2 , Iprec , Ksystm(80) , Lsystm , Op(2) , Oscar(16) , Out , Outtap , Switch(2) , Vps(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Op , Out , In1 , In2
   COMMON /oscent/ Oscar
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   COMMON /xvps  / Vps
   INTEGER andf , lshift , orf , xorf
   INTEGER i , mask , off , opcode(30)
   EXTERNAL andf , lshift , orf
!
!     PARAM PERFORMS THE FOLLOWING OPERATIONS ON PARAMETERS--
!      1. OUT = IN1 .AND. IN2
!      2. OUT = IN1 .OR . IN2
!      3. OUT = IN1   +   IN2
!      4. OUT = IN1   -   IN2
!      5. OUT = IN1   *   IN2
!      6. OUT = IN1   /   IN2
!      7. OUT = .NOT. IN1
!      8. OUT = IN1  .IMP. IN2
!      9. STORE VALUE OF OUT IN VPS.
!     10. OUT = VALUE OF PRECISION CELL FROM /SYSTEM/
!     11. OUT = CURRENT TIME
!     12. OUT = TIME TO GO
!     13. OUT = SYSTEM(IN1) = IN2
!     14. OUT = SYSTEM(25) WITH BITS IN1 THRU IN2 TURNED ON OR OFF.
!     15. OUT = SYSTEM CELL IN1.
!     16. SAVE AND RESTORES SENSE SWITCHES
!     17. SETS SENSE SWITCHES
!     18. SAVE AND RESTORES SYSTEM CELLS
!     19. OUT = -1 IF IN1 .EQ. IN2, OUT = +1 OTHERWISE.
!     20. OUT = -1 IF IN1 .GT. IN2, OUT = +1 OTHERWISE.
!     21. OUT = -1 IF IN1 .LT. IN2, OUT = +1 OTHERWISE.
!     22. OUT = -1 IF IN1 .LE. IN2, OUT = +1 OTHERWISE.
!     23. OUT = -1 IF IN1 .GE. IN2, OUT = +1 OTHERWISE.
!     24. OUT = -1 IF IN1 .NE. IN2, OUT = +1 OTHERWISE.
!     25. UNDEFINED.
!     26. UNDEFINED.
!     27. UNDEFINED.
!     28. UNDEFINED.
!     29. UNDEFINED.
!     30. UNDEFINED.
!
   EQUIVALENCE (Ksystm(2),Outtap) , (Ksystm(23),Lsystm) , (Ksystm(55),Iprec) , (Ksystm(79),Switch(1))
   DATA opcode/4HAND  , 4HOR   , 4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOT  , 4HIMPL , 4HNOP  , 4HPREC , 4HKLOC , 4HTMTO , 4HSYST , &
       &4HDIAG , 4HSYSR , 4HSSSR , 4HSSST , 4HSTSR , 4HEQ   , 4HGT   , 4HLT   , 4HLE   , 4HGE   , 4HNE   , 4H**** , 4H**** ,        &
      & 4H**** , 4H**** , 4H**** , 4H****/
   DATA off/4HOFF /
!
!     BRANCH ON OPERATION CODE.
!
   DO i = 1 , 30
      IF ( Op(1)==opcode(i) ) THEN
         IF ( i==1 ) GOTO 100
         IF ( i==2 ) GOTO 200
         IF ( i==3 ) GOTO 300
         IF ( i==4 ) GOTO 400
         IF ( i==5 ) GOTO 500
         IF ( i==6 ) GOTO 600
         IF ( i==7 ) GOTO 700
         IF ( i==8 ) GOTO 800
         IF ( i==9 ) THEN
         ELSEIF ( i==10 ) THEN
            GOTO 900
         ELSEIF ( i==11 ) THEN
            GOTO 1000
         ELSEIF ( i==12 ) THEN
            GOTO 1100
         ELSEIF ( i==13 ) THEN
            GOTO 1200
         ELSEIF ( i==14 ) THEN
            GOTO 1300
         ELSEIF ( i==15 ) THEN
            GOTO 1400
         ELSEIF ( i==16 ) THEN
            GOTO 1500
         ELSEIF ( i==17 ) THEN
            GOTO 1600
         ELSEIF ( i==18 ) THEN
            GOTO 1700
         ELSEIF ( i==19 ) THEN
            GOTO 1800
         ELSEIF ( i==20 ) THEN
            GOTO 1900
         ELSEIF ( i==21 ) THEN
            GOTO 2000
         ELSEIF ( i==22 ) THEN
            GOTO 2100
         ELSEIF ( i==23 ) THEN
            GOTO 2200
         ELSEIF ( i==24 ) THEN
            GOTO 2300
         ELSEIF ( i==25 ) THEN
         ELSEIF ( i==26 ) THEN
         ELSEIF ( i==27 ) THEN
         ELSEIF ( i==28 ) THEN
         ELSEIF ( i==29 ) THEN
         ELSEIF ( i/=30 ) THEN
            CYCLE
         ENDIF
         GOTO 2400
      ENDIF
   ENDDO
!
!     OPERATION CODE NOT DEFINED-- WRITE MESSAGE.
!
   WRITE (Outtap,99001) Ufm , Op(1) , Op(2)
99001 FORMAT (A23,' 2024, OPERATION CODE ',2A4,' NOT DEFINED FOR ','MODULE PARAM.')
   CALL mesage(-61,0,0)
   GOTO 99999
!
!     .AND.
!
 100  Out = -1
   IF ( In1>=0 .OR. In2>=0 ) Out = +1
   GOTO 2400
!
!     .OR.
!
 200  Out = +1
   IF ( In1<0 .OR. In2<0 ) Out = -1
   GOTO 2400
!
!     ADD
!
 300  Out = In1 + In2
   GOTO 2400
!
!     SUB
!
 400  Out = In1 - In2
   GOTO 2400
!
!     MPY
!
 500  Out = In1*In2
   GOTO 2400
!
!     DIV
!
 600  Out = In1/In2
   GOTO 2400
!
!     NOT
!
 700  Out = -In1
   GOTO 2400
!
!     IMPLY
!
 800  Out = +1
   IF ( In1>=0 .OR. In2<0 ) Out = -1
!
!     NOP
!
   GOTO 2400
!
!     PROVIDE PRECISION FROM /SYSTEM/.
!
 900  Out = Iprec
   GOTO 2400
!
!     PROVIDE CURRENT TIME
!
 1000 CALL klock(Out)
   GOTO 2400
!
!     PROVIDE TIME-TO-GO
!
 1100 CALL tmtogo(Out)
   GOTO 2400
!
!     MODIFY SYSTEM CELL.
!
 1200 Out = In2
   Ksystm(In1) = In2
   IF ( In1<=0 .OR. In1>Lsystm ) WRITE (Outtap,99002) Uwm , In1
99002 FORMAT (A25,' 2317, PARAM HAS STORED OUTSIDE DEFINED RANGE OF ','COMMON BLOCK /SYSTEM/.',/32X,'INDEX VALUE =',I20)
   GOTO 2400
!
!     TURN DIAG SWITCH ON OR OFF.
!
 1300 IF ( In2<In1 ) In2 = In1
   DO i = In1 , In2
      IF ( i>31 ) THEN
         Out = i - 31
         Out = lshift(1,Out-1)
         Switch(2) = orf(Switch(2),Out)
         IF ( Op(2)==off ) Switch(2) = Switch(2) - Out
         Out = Out + 31
      ELSE
         Out = lshift(1,i-1)
         Switch(1) = orf(Switch(1),Out)
         IF ( Op(2)==off ) Switch(1) = Switch(1) - Out
      ENDIF
   ENDDO
   Out = Switch(1)
   IF ( i>31 ) Out = Switch(2)
   GOTO 2400
!
!     RETURN VALUE OF IN1-TH WORD OF /SYSTEM/.
!
 1400 Out = Ksystm(In1)
   GOTO 2400
!
!     SAVE OR RESTORE SSWITCH WORD
!
 1500 IF ( In1<0 ) THEN
      IF ( iabs(In1)>31 ) THEN
         Switch(2) = Out
      ELSE
         Switch(1) = Out
      ENDIF
   ELSEIF ( In1>31 ) THEN
      Out = Switch(2)
   ELSE
      Out = Switch(1)
   ENDIF
   GOTO 2400
!
!     TURN SSWITCH ON OR OFF
!
 1600 IF ( Out/=0 ) THEN
      IF ( Out>0 ) THEN
         IF ( Out>31 ) THEN
            Out = Out - 31
            Switch(2) = orf(lshift(1,Out-1),Switch(2))
            Out = Out + 31
         ELSE
            Switch(1) = orf(lshift(1,Out-1),Switch(1))
         ENDIF
      ELSEIF ( iabs(Out)>31 ) THEN
         Out = Out + 31
         mask = lshift(1,iabs(Out)-1)
         Switch(2) = xorf(mask,orf(mask,Switch(2)))
         Out = Out - 31
      ELSE
         mask = lshift(1,iabs(Out)-1)
         Switch(1) = xorf(mask,orf(mask,Switch(1)))
      ENDIF
   ENDIF
   GOTO 2400
!
!     SAVE OR RESTORE A CELL OF SYSTEM
!
!     SAVE
!
 1700 IF ( In1<0 ) THEN
!
!     RESTORE
!
      In1 = iabs(In1)
      Ksystm(In1) = Out
   ELSE
      Out = Ksystm(In1)
   ENDIF
   GOTO 2400
!
!     ARITHMETIC RELATIONAL OPERATORS.
!
 1800 IF ( In1/=In2 ) THEN
      Out = +1
   ELSE
      Out = -1
   ENDIF
   GOTO 2400
 1900 IF ( In1<=In2 ) THEN
      Out = +1
   ELSE
      Out = -1
   ENDIF
   GOTO 2400
 2000 IF ( In1<In2 ) THEN
      Out = -1
   ELSE
      Out = +1
   ENDIF
   GOTO 2400
 2100 IF ( In1<=In2 ) THEN
      Out = -1
   ELSE
      Out = +1
   ENDIF
   GOTO 2400
 2200 IF ( In1<In2 ) THEN
      Out = +1
   ELSE
      Out = -1
   ENDIF
   GOTO 2400
 2300 IF ( In1/=In2 ) THEN
      Out = -1
   ELSE
      Out = +1
!
!     UNDEFINED.
!
!
!     UNDEFINED.
!
!
!     UNDEFINED.
!
!
!     UNDEFINED.
!
!
!     UNDEFINED.
!
!
!     UNDEFINED.
!
   ENDIF
!
!     SAVE OUT IN THE VPS.
!
 2400 i = andf(Oscar(16),65535)
   Vps(i) = Out
   RETURN
99999 RETURN
END SUBROUTINE qparam
