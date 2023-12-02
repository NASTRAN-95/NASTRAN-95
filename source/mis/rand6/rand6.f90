!*==rand6.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rand6(Xycb,Buffer,Npoint,Iz,Input,Lcore)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Xycb
   INTEGER , DIMENSION(1) :: Buffer
   INTEGER :: Npoint
   INTEGER , DIMENSION(1) :: Iz
   INTEGER :: Input
   INTEGER :: Lcore
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: auto , psdf
   INTEGER :: file , i , ip , ip1 , ireq , k , l , ntype , spag_nextblock_1
   INTEGER , DIMENSION(6) :: ilist
   INTEGER , DIMENSION(13,5) , SAVE :: itype
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fwdrec , mesage , open , read
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     ANALYSIS OF REQUESTS AND BUILDS LIST
!
   DATA name , psdf , auto/4HRAND , 4H6    , 2 , 3/
   DATA itype/13 , 4HDISP , 1 , 4HVELO , 2 , 4HACCE , 3 , 4HDISP , 8 , 4HVELO , 9 , 4HACCE , 10 , 3 , 4HLOAD , 5 , 10*0 , 3 ,       &
      & 4HSPCF , 4 , 10*0 , 3 , 4HSTRE , 6 , 10*0 , 3 , 4HELFO , 7 , 10*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*40,Xycb,Buffer(1),0)
         CALL fwdrec(*60,Xycb)
!
!     SKIP PROSE RECORD
!
         CALL fwdrec(*20,Xycb)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     READ DATA RECORD 6 WORDS AT A TIME
!
            CALL read(*20,*20,Xycb,ilist(1),6,0,i)
!
!     IS DATA BLOCK PROPER
!
            DO i = 2 , ntype , 2
               IF ( ilist(2)==itype(i+1,k) ) EXIT SPAG_Loop_1_1
!
!     GO TO NEXT REQUEST
!
            ENDDO
         ENDDO SPAG_Loop_1_1
!
!     CHECK FOR RANDOM REQUEST
!
         IF ( ilist(5)==psdf ) THEN
!     PSDF REQUEST
!
            ireq = 1
         ELSE
            IF ( ilist(5)/=auto ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
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
            GOTO 40
         ELSE
            Iz(ip) = itype(i,k)
            Iz(ip+1) = ilist(3)
            Iz(ip+2) = ilist(4)
            Iz(ip+3) = ireq
            Iz(ip+4) = ilist(6)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     GET OUT
!
 20      CALL close(Xycb,1)
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
 40      RETURN
 60      ip1 = -2
         DO
            CALL mesage(ip1,file,name(1))
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rand6
