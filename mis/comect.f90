
SUBROUTINE comect(Ele,Max)
   IMPLICIT NONE
   INTEGER Bufsiz , Ect1 , Ect2 , Incr , Ix(1) , Last , Merr , Ne(1) , Nel
   REAL Skp1(12) , Skp2(7) , Skp3(10) , X(1)
   COMMON /blank / Skp1 , Ect1 , Skp2 , Merr , Skp3 , Ect2
   COMMON /gpta1 / Nel , Last , Incr , Ne
   COMMON /system/ Bufsiz
   COMMON /zzzzzz/ X
   INTEGER Max
   INTEGER Ele(1)
   INTEGER b1 , b2 , elid(2) , ept , err(5) , gp(32) , i , idrec(3) , idx , ihx2(20) , ihx3(32) , ilxx , inrew , j , jcomp , k ,    &
         & kcomp , lect , lele , m , m1(18) , n , name(2) , ngpel , nm1 , outrew , pcomp(12) , pid , rew , type
   INTEGER korsz
   REAL offset(1)
!
!     REVISED  10/1990 BY G.CHAN/UNISYS
!              TO INCLUDE OFFSET DATA FOR CBAR, CTRIA3 AND CQUAD4 IN
!              THE ECT2 DATA BLOCK
!              (6 COORDINATE VALUES FOR THE BAR, AND 1 OFFSET VALUE
!              FOR EACH OF THE TWO PLATES, ARE ADDED AFTER THE GRID
!              DATA)
!
   EQUIVALENCE (offset(1),gp(1)) , (Ix(1),X(1))
   DATA name/4H COM , 4HECT / , outrew , rew , inrew/1 , 1 , 0/
   DATA pcomp/5502 , 25 , 2 , 5602 , 14 , 2 , 5702 , 13 , 2 , 5802 , 17 , 17/
!                     PCOMP      PCOMP1     PCOMP2     PSHELL
   DATA nm1/18/ , m1/4H(33X , 4H,2A4 , 4H,18H , 4HIGNO , 4HRING , 4H ELE , 4HMENT , 4H (2A , 4H4,32 , 4HH) W , 4HITH  , 4HMORE ,    &
       &4H THA , 4HN 32 , 4H CON , 4HNECT , 4HIONS , 4H.)  / , ilxx/2HXX/
   DATA ihx2/1 , 1 , 3 , 3 , 5 , 5 , 7 , 7 , 1 , 3 , 5 , 7 , 13 , 13 , 15 , 15 , 17 , 17 , 19 , 19/
   DATA ihx3/1 , 1 , 4 , 4 , 4 , 7 , 7 , 7 , 10 , 10 , 10 , 1 , 1 , 4 , 7 , 10 , 21 , 24 , 27 , 30 , 21 , 21 , 24 , 24 , 24 , 27 ,  &
      & 27 , 27 , 30 , 30 , 30 , 21/
!
   b1 = korsz(X) - (3*Bufsiz+2)
   b2 = b1 + Bufsiz + 3
   err(1) = 4
   err(2) = name(1)
   err(3) = name(2)
!
!     IF EPT FILE IS PRESENT, AND ANY OF THE PSHELL, PCOMP, PCOMP1 AND
!     PCOMP2 CARDS IS ALSO PRESENT, CREATE A TABLE OF PROPERTY ID AND
!     OFFSET DATA, TO BE USE LATER BY CTRIA3 OR CQURD4 ELEMENTS
!
   jcomp = b1
   ept = 104
   CALL open(*200,ept,X(b1),inrew)
   CALL read(*100,*100,ept,Ix,2,1,m)
   CALL close(ept,rew)
   CALL preloc(*200,X(b1),ept)
   n = 1
   DO i = 1 , 12 , 3
      idrec(1) = pcomp(i)
      idrec(2) = idrec(1)/100
      CALL locate(*50,X(b1),idrec,j)
      k = pcomp(i+1)
      j = pcomp(i+2)
      DO
         CALL read(*50,*50,ept,X,k,0,m)
         IF ( X(j)/=0.0 ) THEN
            jcomp = jcomp - 2
            Ix(jcomp) = Ix(1)
            X(jcomp+1) = X(j)
         ENDIF
      ENDDO
 50   n = n + 1
   ENDDO
 100  CALL close(ept,rew)
   kcomp = b1 - 1
!
!     CONSTRUCT A LIST OF INDICES IN THE ECT FOR USE WITH GPECT IN THE
!     PLOT MODULE BY CONTOUR PLOTTING
!
 200  CALL gopen(Ect1,X(b1),inrew)
   DO j = 1 , Max
      Ele(j) = 0
   ENDDO
   i = 1
 300  DO
      CALL read(*800,*400,Ect1,idrec,3,0,m)
      DO j = 1 , Nel
         idx = (j-1)*Incr
         IF ( Ne(idx+4)==idrec(1) ) GOTO 600
      ENDDO
      CALL skprec(Ect1,1)
   ENDDO
 400  CALL mesage(-3,Ect1,name)
 500  CALL mesage(-2,Ect1,name)
 600  lect = Ne(idx+6) - 1
   DO
      CALL read(*500,*300,Ect1,Ele(i),1,0,m)
      CALL fread(Ect1,0,-lect,0)
      i = i + 1
      IF ( i>Max ) CALL mesage(-8,0,name)
   ENDDO
!
 700  CALL mesage(-1,Ect1,name)
!
 800  lele = i - 1
   CALL close(Ect1,rew)
!
   CALL preloc(*700,X(b1),Ect1)
   CALL gopen(Ect2,X(b2),outrew)
   DO n = 1 , Nel
      idx = (n-1)*Incr
!
!     IF SCALAR CONNECTION POSSIBLE FOR ELEMENT THEN SKIP IT
!
      IF ( Ne(idx+11)/=0 ) CYCLE
!
!     SKIP DUMMY ELEMENTS AND POINT ELEMENTS
!
      IF ( Ne(idx+10)<=1 ) CYCLE
      IF ( Ne(idx+16)==ilxx ) CYCLE
      CALL locate(*1000,X(b1),Ne(idx+4),i)
      ngpel = Ne(idx+10)
      IF ( ngpel>32 ) THEN
!
!     ELEMENT TYPE WITH MORE THAN 32 CONNECTIONS
!
         err(4) = Ne(idx+1)
         err(5) = Ne(idx+2)
         CALL wrtprt(Merr,err,m1,nm1)
         CALL skprec(Ect1,1)
         CYCLE
      ELSE
!
         CALL write(Ect2,n,1,0)
         CALL write(Ect2,ngpel,1,0)
      ENDIF
 850  CALL read(*950,*950,Ect1,elid,1,0,i)
!
!     FIND THIS ELEMENTS POINTER IN THE ECT
!
      DO i = 1 , lele
         IF ( Ele(i)==elid(1) ) GOTO 900
      ENDDO
      CALL mesage(-37,0,name)
 900  elid(2) = i
!
!     DETERMINE NUMBER ENTRIES FOR SKIPPING TO GRID ENTRIES
!
      i = Ne(idx+13) - 2
      IF ( n==52 ) THEN
!
!     SPECIAL HANDLING FOR CHBDY
!     IF TYPE IS NEGATIVE, SAVE TYPE FLAG AFTER GRIDS.
!
         CALL fread(Ect1,0,-1,0)
         CALL fread(Ect1,type,1,0)
         CALL fread(Ect1,gp,8,0)
         CALL fread(Ect1,0,-(Ne(idx+6)-ngpel-i-1),0)
         IF ( type<0 ) GOTO 850
         IF ( type==6 ) type = 3
         gp(9) = type
!              CHBDY
!
      ELSEIF ( n==64 .OR. n==83 ) THEN
!
!     (2) CTRIA3 AND CQUAD4 ELEMENTS, ONE OFFSET DATA NORMAL TO PLATE.
!         OFFSET DATA COULD BE ON ELEMENT CARD OR ON PSHELL OR PCOMPI
!         CARDS
!
         CALL fread(Ect1,pid,1,0)
         CALL fread(Ect1,gp,ngpel,0)
         CALL write(Ect2,elid,2,0)
         CALL write(Ect2,gp,ngpel,0)
         j = 5
         IF ( n==64 ) j = 6
         CALL fread(Ect1,0,-j,0)
         CALL fread(Ect1,offset,1,0)
         IF ( offset(1)==0.0 ) THEN
            IF ( jcomp/=b1 ) THEN
               DO i = jcomp , kcomp , 2
                  IF ( Ix(i)==pid ) THEN
                     offset(1) = X(i+1)
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         CALL write(Ect2,offset,1,0)
         GOTO 850
      ELSE
!           CQUAD4        CTRIA3
         IF ( i<0 ) GOTO 700
!
         IF ( i/=0 ) CALL fread(Ect1,0,-i,0)
         CALL fread(Ect1,gp,ngpel,0)
         IF ( n==34 ) THEN
!
!     SPECIAL HANDLING OF THOSE ELEMENTS HAVING GRID OFFSET.
!     ADD THESE OFFSET DATA AFTER THE GRID POINTS
!
!     (1) CBAR ELEMENT, 2 OFFSET VECTORS (6 VALUES)
!
            CALL write(Ect2,elid,2,0)
            CALL write(Ect2,gp,ngpel,0)
            CALL fread(Ect1,0,-6,0)
            CALL fread(Ect1,offset,6,0)
            CALL write(Ect2,offset,6,0)
            GOTO 850
         ELSE
!               CBAR
!
            CALL fread(Ect1,0,-(Ne(idx+6)-ngpel-i-1),0)
!
!     SPCIAL HANDLING OF IHEX2 AND IHEX3 WITH ZERO GRIDS
!
            IF ( n==66 .OR. n==67 ) THEN
               DO j = 1 , ngpel
                  IF ( gp(j)==0 ) THEN
                     k = ihx3(j)
                     IF ( n==66 ) k = ihx2(j)
                     gp(j) = gp(k)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
      CALL write(Ect2,elid,2,0)
      CALL write(Ect2,gp,ngpel,0)
      GOTO 850
!
 950  CALL write(Ect2,0,0,1)
 1000 ENDDO
!
   CALL clstab(Ect2,rew)
   CALL close(Ect1,rew)
END SUBROUTINE comect
