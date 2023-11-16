
SUBROUTINE pidck(Pfile,Geom2,Nopid,Z)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Abort
   INTEGER Ibuf , Incr , Kdum(9) , Last , Ne(1) , Nelem , Nout
   REAL Skip(42)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /gpta1 / Nelem , Last , Incr , Ne
   COMMON /system/ Ibuf , Nout , Abort , Skip , Kdum
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Dummy argument declarations
!
   INTEGER Geom2 , Nopid , Pfile
   INTEGER Z(1)
!
! Local variable declarations
!
   INTEGER flag , i , i4 , ii , iz , j , jb , je , jj , jj1 , k , kk , komp , kp1 , name(2) , nc , ng , np , nwds , pcomp(3) ,      &
         & pshell , quad4 , x(3)
!
! End of declarations
!
!
!     THIS ROUTINE CHECKS THE UNIQUNESS OF PROPERTY IDS FOR ALL ELEMENTS
!     THAT HAVE PID FIELDS
!
!     IT SHOULD BE CALLED ONLY ONCE BY IFP
!     IT DOES NOT OPEN NOR CLOSE ANY GINO FILE.
!
!     DESIGN REQUIREMENT -
!
!     IF PID IS REFERENCED BY AN ELEMENT, THE PID MUST RESIDE ON THE
!     THIRD FIELD OF THE ELEMENT INPUT CARD.
!     INPUT FILES - GEOM2 AND PROPERTY FILE (EPT).
!
!     THIS VERSION INCLUDES SPECIAL HANDLING OF THE CQUAD4 AND CTRIA3
!     ELEMENTS WHICH USE AND SHARE MORE THAN ONE STANDARD PROPERTY CARD.
!     THE PROPERTY TYPE IDS OF THE PSHELL, PCOMP, PCOMP1 AND PCOMP2
!     MUST NOT BE INTERRUPTED BY ANOTHER PROPERTY TYPE. (I.E. NO OTHER
!     PROPERTY TYPE SHOULD HAVE AN ID PLACED IN BETWEEN 5502 THRU 5802).
!     NOTICE THAT THE PSHELL CARD HAS FIXED LENGTH WHILE THE 3 PCOMPI
!     CARDS HAVE VARIABLE LENGTH.
!
!     WRITTEN BY G.CHAN/UNISYS, SEPT. 1983
!
   DATA quad4 , pshell , pcomp/5408 , 5802 , 5502 , 5602 , 5702/
   DATA name/4HPIDC , 4HK   /
!
!     UPDATE /GPTA1/ IF DUMMY ELEMENTS ARE PRESENT
!
   DO i = 1 , 9
      IF ( Kdum(i)/=0 ) THEN
         k = Kdum(i)
         ng = k/10000000
         nc = (k-ng*10000000)/10000
         np = (k-ng*10000000-nc*10000)/10
         k = (51+i)*Incr
         Ne(k+6) = 2 + ng + nc
         Ne(k+9) = 2 + np
         Ne(k+10) = ng
      ENDIF
   ENDDO
!
!     CREATE A PROPERTY ID TABLE IN Z FROM /GPTA1/ DATA BLOCK FOR THOSE
!     ELEMENTS THAT HAVE PROPERTY CARDS
!     4 WORDS PER ENTRY
!       WORD 1, PROPERTY TYPE CODE  (EPT-ID)
!       WORD 2, LENGTH OF PROPERTY CARD  (EPTWDS)
!       WORD 3, ELEMENT TYPE CODE   (ECT-ID)
!       WORD 4, LENGTH OF ELEMENT CARD (ECTWDS), PLUS POINTER TO GPTA1
!
   ii = 0
   DO i = 1 , Last , Incr
      IF ( Ne(i+6)/=0 ) THEN
         Z(ii+1) = Ne(i+6)
         Z(ii+2) = -Ne(i+8)
         Z(ii+3) = Ne(i+3)
         Z(ii+4) = Ne(i+5)*10000 + i
         ii = ii + 4
      ENDIF
   ENDDO
!
!     ADD 3 MORE PROPERTY CARDS (PCOMP, PCOMP1, PCOMP2) FOR CQUAD4 (64)
!     AND CTRIA3
!     NOTE - THESE THREE ARE OPEN-ENDED, AND WE SET WORD 2 TO -8888
!          - WE GIVE THEM LOCALLY NEW QUAD4 IDS IN THE 3RD WORD, SO THAT
!            ELEMENT CQUAD4 AND ELEMENT CTRIA3 WILL PICK THEM UP VIA
!            THE PSEHLL DATA LATER.
!
   i = (64-1)*Incr + 1
   IF ( Ne(i+3)/=quad4 ) CALL mesage(-37,0,name)
   DO j = 1 , 3
      Z(ii+1) = pcomp(j)
      Z(ii+2) = -8888
      Z(ii+3) = quad4 - j
      Z(ii+4) = Ne(i+5)*10000 + i
      ii = ii + 4
   ENDDO
!
!     SORT THIS 4-ENTRY Z-TABLE BY THE FIRST WORD.
!     SET WORD 2 TO -9999 IF ELEMENT USES THE SAME PROPERTY CARD AS THE
!     PREVIOUS ELEMENT.
!
   i4 = ii/4
   CALL sort(0,0,4,1,Z,ii)
   DO i = 5 , ii , 4
      IF ( Z(i)==Z(i-4) ) Z(i+1) = -9999
   ENDDO
!
!     READ FROM PFILE ALL PID INTO REMAINING CORE. REPLACE WORD 2 IN THE
!     Z-TABLE BY PID BEGIN-ENDING POINTERS
!
   jj = ii + 1
   IF ( Nopid==1 ) GOTO 500
   CALL rewind(Pfile)
 100  CALL fwdrec(*900,Pfile)
 200  CALL read(*400,*400,Pfile,x,3,0,flag)
!     2147483647  = 2**31-1
   IF ( x(1)==2147483647 ) GOTO 400
   CALL bisloc(*100,x(1),Z,4,i4,k)
   DO
      kp1 = k + 1
      IF ( Z(kp1)/=-9999 ) THEN
         nwds = -Z(kp1)
         IF ( nwds<=0 ) GOTO 100
         komp = 0
         IF ( nwds==8888 ) THEN
            komp = 1
            nwds = 8
         ENDIF
         Z(kp1) = (jj*10000) + (jj-1)
         jb = jj
         EXIT
      ELSE
         k = k - 4
      ENDIF
   ENDDO
 300  CALL read(*900,*200,Pfile,Z(jj),nwds,0,flag)
   IF ( komp/=0 ) THEN
      DO
         CALL read(*900,*200,Pfile,j,1,0,flag)
         IF ( j==-1 ) EXIT
      ENDDO
   ENDIF
   je = mod(Z(kp1),10000)
   IF ( je>=jb ) THEN
      DO j = jb , je
         IF ( Z(jj)==Z(j) ) GOTO 300
      ENDDO
   ENDIF
   Z(kp1) = Z(kp1) + 1
   jj = jj + 1
   GOTO 300
 400  CALL rewind(Pfile)
   jj = jj - 1
   IF ( jj<=ii ) Nopid = -1
!
!     RESET THE PSHELL POINTERS TO INCLUDE THE PCOMP GROUP IDS.
!     MAKE SURE THIS GROUP ARE ALL TOGETHER, NOT SEPERATED BY OTHER
!     PROPERTY CARD
!     THERE ARE 2 PSHELL CARDS, ONE FROM CQUAD4 AND ONE FROM CTRIA3,
!     MAKE SURE THE FIRST PSHELL POINTER IS USED
!
   CALL bisloc(*500,pshell,Z,4,i4,kp1)
   IF ( Z(kp1+1)==-9999 ) kp1 = kp1 - 4
   IF ( Z(kp1-4)/=pcomp(3) .OR. Z(kp1-8)/=pcomp(2) .OR. Z(kp1-12)/=pcomp(1) ) THEN
      WRITE (Nout,99001) Z(kp1) , pshell , Z(kp1-4) , pcomp(3) , Z(kp1-8) , pcomp(2) , Z(kp1-12) , pcomp(1)
99001 FORMAT ('0*** ERROR IN /GPTA1/ PCOMP ARRANGEMENT',(/3X,2I7))
      j = -37
      CALL mesage(j,0,name)
      GOTO 99999
   ELSE
      j = Z(kp1+1)
      IF ( j<=0 ) j = 0
      jb = j/10000
      je = mod(j,10000)
      IF ( jb==0 ) jb = 9999999
      DO i = 1 , 3
         CALL bisloc(*1000,pcomp(i),Z,4,i4,k)
         IF ( Z(k+1)>0 ) THEN
            j = Z(k+1)/10000
            k = mod(Z(k+1),10000)
            IF ( j<jb ) jb = j
            IF ( k>je ) je = k
         ENDIF
      ENDDO
      IF ( jb/=9999999 ) Z(kp1+1) = (jb*10000) + je
   ENDIF
!
!     RESET POINTERS FOR THOSE PROPERTY ID COMMON TO MORE THAN ONE TYPE
!     OF ELEMENTS, AND
!     MOVE THE THIRD ENTRY IN THE Z-TABLE TO FIRST, FOR ELEMENT SORT
!
 500  DO i = 1 , ii , 4
      Z(i) = Z(i+2)
      j = i + 1
      IF ( Z(j)<=0 ) THEN
         IF ( Z(j)==-9999 ) Z(j) = Z(j-4)
      ENDIF
   ENDDO
   CALL sort(0,0,4,1,Z,ii)
!
!     READ IN CONNECTING ELEMENTS, ONE BY ONE, FROM GEOM2 FILE, AND
!     CHECK THE EXISTENCE OF THE PROPERTY ID IF IT IS SPECIFIED.
!
   kk = jj + 1
   CALL rewind(Geom2)
 600  CALL fwdrec(*900,Geom2)
 700  CALL read(*800,*800,Geom2,x,3,0,flag)
   CALL bisloc(*600,x(1),Z,4,i4,k)
   nwds = Z(k+3)/10000
   IF ( nwds<=0 ) GOTO 600
   j = Z(k+1)
   IF ( j<=0 ) THEN
      j = mod(Z(k+3),10000)
      CALL mesage(30,11,Ne(j))
      Abort = .TRUE.
      GOTO 600
   ELSE
      jb = j/10000
      je = mod(j,10000)
      DO
         CALL read(*900,*700,Geom2,Z(kk),nwds,0,flag)
         jj1 = Z(kk+1)
         DO j = jb , je
            iz = iabs(Z(j))
            IF ( jj1==iz ) THEN
               Z(j) = -iz
               GOTO 750
            ENDIF
         ENDDO
         CALL mesage(30,10,Z(kk))
         Abort = .TRUE.
 750  ENDDO
   ENDIF
 800  CALL rewind(Geom2)
   IF ( Abort .OR. Nopid/=0 ) THEN
!
!     SET Z(1) TO ZERO IF NO ACTIVE PROPERTY LIST EXISTS.
!
      Z(1) = 0
      RETURN
   ELSE
!
!     PREPARE AN ACTIVE PROPERTY ID LIST FOR SUBROUTINE MATCK
!
      j = ii + 1
      ii = 1
      DO i = j , jj
         IF ( Z(i)>=0 ) THEN
            Z(kk) = Z(i)
            kk = kk + 1
         ELSE
            ii = ii + 1
            Z(ii) = -Z(i)
         ENDIF
      ENDDO
      Z(1) = ii
!
!     Z(2,...II) CONTAINS A LIST OF ACTIVE PROPERTY IDS, UN-SORTED,
!     REFERENCED BY ELEMENTS IN GEOM2 FILE.  Z(1) = LENGTH OF THIS LIST
!
      jj1 = jj + 1
      kk = kk - 1
      IF ( kk<jj1 ) RETURN
      WRITE (Nout,99002) Uim
99002 FORMAT (A29,', THE FOLLOWING PROPERTY IDS ARE PRESENT BUT NOT ','USED -')
      WRITE (Nout,99003) (Z(j),j=jj1,kk)
99003 FORMAT (/5X,12I9)
      RETURN
   ENDIF
!
 900  j = -2
   CALL mesage(j,0,name)
   GOTO 99999
 1000 WRITE (Nout,99004)
99004 FORMAT ('0*** CAN NOT LOCATE PSHELL OR PCOMP DATA IN /GPTA1/')
   j = -37
   CALL mesage(j,0,name)
99999 RETURN
END SUBROUTINE pidck
