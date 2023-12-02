!*==qparam.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE qparam
   IMPLICIT NONE
   USE c_blank
   USE c_oscent
   USE c_system
   USE c_xmssg
   USE c_xvps
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iprec , lsystm , mask , outtap
   INTEGER , SAVE :: off
   INTEGER , DIMENSION(30) , SAVE :: opcode
   INTEGER , DIMENSION(2) :: switch
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Ksystm(2),Outtap) , (Ksystm(23),Lsystm) , (Ksystm(55),Iprec) , (Ksystm(79),Switch(1))
   DATA opcode/4HAND  , 4HOR   , 4HADD  , 4HSUB  , 4HMPY  , 4HDIV  , 4HNOT  , 4HIMPL , 4HNOP  , 4HPREC , 4HKLOC , 4HTMTO , 4HSYST , &
       &4HDIAG , 4HSYSR , 4HSSSR , 4HSSST , 4HSTSR , 4HEQ   , 4HGT   , 4HLT   , 4HLE   , 4HGE   , 4HNE   , 4H**** , 4H**** ,        &
      & 4H**** , 4H**** , 4H**** , 4H****/
   DATA off/4HOFF /
!
!     BRANCH ON OPERATION CODE.
!
   DO i = 1 , 30
      IF ( op(1)==opcode(i) ) THEN
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
   WRITE (outtap,99001) ufm , op(1) , op(2)
99001 FORMAT (A23,' 2024, OPERATION CODE ',2A4,' NOT DEFINED FOR ','MODULE PARAM.')
   CALL mesage(-61,0,0)
   GOTO 99999
!
!     .AND.
!
 100  out = -1
   IF ( in1>=0 .OR. in2>=0 ) out = +1
   GOTO 2400
!
!     .OR.
!
 200  out = +1
   IF ( in1<0 .OR. in2<0 ) out = -1
   GOTO 2400
!
!     ADD
!
 300  out = in1 + in2
   GOTO 2400
!
!     SUB
!
 400  out = in1 - in2
   GOTO 2400
!
!     MPY
!
 500  out = in1*in2
   GOTO 2400
!
!     DIV
!
 600  out = in1/in2
   GOTO 2400
!
!     NOT
!
 700  out = -in1
   GOTO 2400
!
!     IMPLY
!
 800  out = +1
   IF ( in1>=0 .OR. in2<0 ) out = -1
!
!     NOP
!
   GOTO 2400
!
!     PROVIDE PRECISION FROM /SYSTEM/.
!
 900  out = iprec
   GOTO 2400
!
!     PROVIDE CURRENT TIME
!
 1000 CALL klock(out)
   GOTO 2400
!
!     PROVIDE TIME-TO-GO
!
 1100 CALL tmtogo(out)
   GOTO 2400
!
!     MODIFY SYSTEM CELL.
!
 1200 out = in2
   ksystm(in1) = in2
   IF ( in1<=0 .OR. in1>lsystm ) WRITE (outtap,99002) uwm , in1
99002 FORMAT (A25,' 2317, PARAM HAS STORED OUTSIDE DEFINED RANGE OF ','COMMON BLOCK /SYSTEM/.',/32X,'INDEX VALUE =',I20)
   GOTO 2400
!
!     TURN DIAG SWITCH ON OR OFF.
!
 1300 IF ( in2<in1 ) in2 = in1
   DO i = in1 , in2
      IF ( i>31 ) THEN
         out = i - 31
         out = lshift(1,out-1)
         switch(2) = orf(switch(2),out)
         IF ( op(2)==off ) switch(2) = switch(2) - out
         out = out + 31
      ELSE
         out = lshift(1,i-1)
         switch(1) = orf(switch(1),out)
         IF ( op(2)==off ) switch(1) = switch(1) - out
      ENDIF
   ENDDO
   out = switch(1)
   IF ( i>31 ) out = switch(2)
   GOTO 2400
!
!     RETURN VALUE OF IN1-TH WORD OF /SYSTEM/.
!
 1400 out = ksystm(in1)
   GOTO 2400
!
!     SAVE OR RESTORE SSWITCH WORD
!
 1500 IF ( in1<0 ) THEN
      IF ( iabs(in1)>31 ) THEN
         switch(2) = out
      ELSE
         switch(1) = out
      ENDIF
   ELSEIF ( in1>31 ) THEN
      out = switch(2)
   ELSE
      out = switch(1)
   ENDIF
   GOTO 2400
!
!     TURN SSWITCH ON OR OFF
!
 1600 IF ( out/=0 ) THEN
      IF ( out>0 ) THEN
         IF ( out>31 ) THEN
            out = out - 31
            switch(2) = orf(lshift(1,out-1),switch(2))
            out = out + 31
         ELSE
            switch(1) = orf(lshift(1,out-1),switch(1))
         ENDIF
      ELSEIF ( iabs(out)>31 ) THEN
         out = out + 31
         mask = lshift(1,iabs(out)-1)
         switch(2) = xorf(mask,orf(mask,switch(2)))
         out = out - 31
      ELSE
         mask = lshift(1,iabs(out)-1)
         switch(1) = xorf(mask,orf(mask,switch(1)))
      ENDIF
   ENDIF
   GOTO 2400
!
!     SAVE OR RESTORE A CELL OF SYSTEM
!
!     SAVE
!
 1700 IF ( in1<0 ) THEN
!
!     RESTORE
!
      in1 = iabs(in1)
      ksystm(in1) = out
   ELSE
      out = ksystm(in1)
   ENDIF
   GOTO 2400
!
!     ARITHMETIC RELATIONAL OPERATORS.
!
 1800 IF ( in1/=in2 ) THEN
      out = +1
   ELSE
      out = -1
   ENDIF
   GOTO 2400
 1900 IF ( in1<=in2 ) THEN
      out = +1
   ELSE
      out = -1
   ENDIF
   GOTO 2400
 2000 IF ( in1<in2 ) THEN
      out = -1
   ELSE
      out = +1
   ENDIF
   GOTO 2400
 2100 IF ( in1<=in2 ) THEN
      out = -1
   ELSE
      out = +1
   ENDIF
   GOTO 2400
 2200 IF ( in1<in2 ) THEN
      out = +1
   ELSE
      out = -1
   ENDIF
   GOTO 2400
 2300 IF ( in1/=in2 ) THEN
      out = -1
   ELSE
      out = +1
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
 2400 i = andf(oscar(16),65535)
   vps(i) = out
99999 END SUBROUTINE qparam
