
SUBROUTINE rand6(Xycb,Buffer,Npoint,Iz,Input,Lcore)
   IMPLICIT NONE
   INTEGER Input , Lcore , Npoint , Xycb
   INTEGER Buffer(1) , Iz(1)
   INTEGER auto , file , i , ilist(6) , ip , ip1 , ireq , itype(13,5) , k , l , name(2) , ntype , psdf
!
!     ANALYSIS OF REQUESTS AND BUILDS LIST
!
   DATA name , psdf , auto/4HRAND , 4H6    , 2 , 3/
   DATA itype/13 , 4HDISP , 1 , 4HVELO , 2 , 4HACCE , 3 , 4HDISP , 8 , 4HVELO , 9 , 4HACCE , 10 , 3 , 4HLOAD , 5 , 10*0 , 3 ,       &
      & 4HSPCF , 4 , 10*0 , 3 , 4HSTRE , 6 , 10*0 , 3 , 4HELFO , 7 , 10*0/
! *****
!     XYCB     XY OUTPUT REQUESTS
!     BUFFER   SYSTEM BUFFER
!     NPOINT   NUMBER OF POINTS REQUESTED FOR THIS FILE
!     IZ       LIST OF REQUESTS
!     INPUT    CURRENT FILE
!     ILIST    LIST OF REQUEST FROM XYCB   6  WORDS PER
!     SUBC,FILE,ID,COMP,OPER,DEST
!     PSDF     KEY FOR POWER SPECTRAL DENSITY FUNCTION
!     AUTO     KEY FOR AUTOCORRELATION FUNCTION
!     ITYPE    LIST OF DATA TYPES ON EACH INPUT FILE
!     IREQ     PSDF =1 , AUTO =2  BOTH = 3
!     IP       POINTER INTO  IZ  FOR LAST POINT(SAME POINT MAY OCCUR
!                MANY TIMES IN XYCB
!
!     LIST FORMAT
!     FILE,ID,COMP,IREQ,DEST
!
!
!
!
!
!
!     FIND  ACCEPTABLE MNEUMONICS
!
   k = Input - 103
   ntype = itype(1,k)
   ip = -4
   Npoint = 0
!
!     OPEN XYCB
!
   file = Xycb
   CALL open(*400,Xycb,Buffer(1),0)
   CALL fwdrec(*500,Xycb)
!
!     SKIP PROSE RECORD
!
   CALL fwdrec(*300,Xycb)
 100  DO
!
!     READ DATA RECORD 6 WORDS AT A TIME
!
      CALL read(*300,*300,Xycb,ilist(1),6,0,i)
!
!     IS DATA BLOCK PROPER
!
      DO i = 2 , ntype , 2
         IF ( ilist(2)==itype(i+1,k) ) GOTO 200
!
!     GO TO NEXT REQUEST
!
      ENDDO
   ENDDO
!
!     CHECK FOR RANDOM REQUEST
!
 200  IF ( ilist(5)==psdf ) THEN
!     PSDF REQUEST
!
      ireq = 1
   ELSE
      IF ( ilist(5)/=auto ) GOTO 100
!
!     AUTOCORRELATION REQUEST
!
      ireq = 2
   ENDIF
!
!     STORE  IN LIST
!
   IF ( Npoint/=0 ) THEN
!
!     IS THIS A NEW POINT
!
      IF ( Iz(ip)==itype(i,k) ) THEN
         IF ( Iz(ip+1)==ilist(3) .AND. Iz(ip+2)==ilist(4) ) THEN
!
!     ANOTHER REQUEST FOR SAME POINT
!
            IF ( Iz(ip+3)/=3 .AND. Iz(ip+3)/=ireq ) Iz(ip+3) = Iz(ip+3) + ireq
            IF ( Iz(ip+4)/=3 .AND. Iz(ip+4)/=ilist(6) ) Iz(ip+4) = Iz(ip+4) + ilist(6)
            GOTO 100
         ENDIF
      ENDIF
   ENDIF
!
!     ADD POINT TO LIST
!
   Npoint = Npoint + 1
   ip = ip + 5
   IF ( ip+5>Lcore ) THEN
!
!     FILE ERRORS
!
      Npoint = Npoint + 9
      GOTO 400
   ELSE
      Iz(ip) = itype(i,k)
      Iz(ip+1) = ilist(3)
      Iz(ip+2) = ilist(4)
      Iz(ip+3) = ireq
      Iz(ip+4) = ilist(6)
      GOTO 100
   ENDIF
!
!     GET OUT
!
 300  CALL close(Xycb,1)
!
!     SAVE ORIGINAL COMPONENT IN THE FIFTH LIST LORD
!
   IF ( Npoint/=0 ) THEN
      DO i = 1 , Npoint
         l = (i-1)*5 + 1
         Iz(l+4) = Iz(l+2)
         IF ( k<4 ) Iz(l+2) = Iz(l+2) - 2
      ENDDO
   ENDIF
 400  RETURN
 500  ip1 = -2
   DO
      CALL mesage(ip1,file,name(1))
   ENDDO
END SUBROUTINE rand6