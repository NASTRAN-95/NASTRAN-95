
SUBROUTINE matck(Mfile,Pfile,A,Z)
   IMPLICIT NONE
   LOGICAL Abort
   INTEGER Kdum(9) , N1 , Nout
   REAL Skip(42)
   COMMON /system/ N1 , Nout , Abort , Skip , Kdum
   INTEGER Mfile , Pfile
   INTEGER A(1) , Z(1)
   INTEGER epti(2,40) , group , i , ib , ic , ie , ih(3) , ii , j , jb , je , jx , k , k1 , k4 , kk , komp , ma , mati(2,22) ,      &
         & matj(2,22) , mb , mc , md , me , name(2) , nept , nmat , nn , nomat , nwds
!
!     THIS ROUTINE CHECKS THE UNIQUENESS OF MATERIAL ID'S FOR
!          1. MAT1 (1)     8. MATT1  (MB)  15. MATS1  (MC)
!          2. MAT2         9. MATT2        16. MATPZ1 (MD)
!          3. MAT3        10. MATT3        17. MTTPZ1
!          4. MAT4        11. MATT4        18. MATPZ2
!          5. MAT5        12. MATT5        19. MTTPZ2 (ME)
!          6. MAT6        13. MATT6        20. DUMC
!          7. MAT8 (MA)   14. DUMB         21. DUMD  (NMAT)
!     AND THE MATERIAL ID SPECIFIED ON THE PROPERTY CARDS.
!
!     THIS ROUTINE SHOULD BE CALLED ONLY ONCE BY IFP.
!     THIS ROUTINE DOES NOT OPEN OR CLOSE MATERIAL FILE (MFILE) OR
!     ELEMENT PROPERTY FILE (PFILE)
!
!     WRITTEN BY G.CHAN/UNISYS,  OCT. 1982
!
   DATA matj/103 , -12 , 203 , -17 , 1403 , -16 , 2103 , -3 , 2203 , -8 , 2503 , -31 , 603 , -18 , 703 , -11 , 803 , -16 , 1503 ,   &
      & -16 , 2303 , -2 , 2403 , -7 , 2603 , -31 , -11 , -00 , 503 , -11 , 1603 , -07 , 1803 , -07 , 1703 , -44 , 1903 , -44 , -11 ,&
      & -00 , -11 , -00 , -11 , -00/
   DATA epti/52 , 191 , 2502 , 071 , 7002 , 071 , 0502 , 041 , 2202 , 041 , 5302 , 041 , 0602 , 082 , 0702 , 103 , 0802 , 041 ,     &
      & 0902 , 061 , 1002 , 041 , 2102 , 041 , 7052 , 171 , 1102 , 082 , 1202 , 103 , 1302 , 041 , 7032 , 171 , 1402 , 041 , 1502 , &
      & 082 , 1602 , 051 , 1702 , 041 , 2002 , 031 , 0152 , 243 , 5102 , 241 , 5802 , 174 , 5502 , -49 , 5602 , -06 , 5702 , -06 ,  &
      & 6102 , 001 , 6202 , 001 , 6302 , 001 , 6402 , 001 , 6502 , 001 , 6602 , 001 , 6702 , 001 , 6802 , 001 , 6902 , 001 , 0 , 0 ,&
      & 0 , 0 , 0 , 0/
   DATA nmat/21/ , group/7/
   DATA nept/37/
   DATA name/4HMATC , 4HK   /
!
!     FIRST WORDS ON THE EPTI TABLE ARE PROPERTY CARDS THAT SPECIFY
!     MATERIAL.  THE FIRST 2 DIGITS OF THE SECOND WORD INDICATE THE
!     NUMBER OF WORDS IN EACH PROPERTY INPUT CARD. AND THE 3RD DIGIT
!     INDICATES NUMBER OF MATERIAL ID'S SPECIFIED.
!     IF THIS SECOND WORD IS NEGATIVE, IT MEANS THE PROPERTY CARD IS
!     OPEN-ENDED. THE 3RD DIGIT INDICATES WHERE MID1 BEGINS, AND
!     REPEATING (FOR MID2, MID3,...) EVERY N WORDS WHERE N IS THE
!     ABSOLUTE VALUE OF THE FIRST 2 DIGITS. (NO REPEAT OF N=0)
!
!     ARRAY A CONTAINS A LIST OF ACTIVE PROPERTY IDS - SET UP BY PIDCK
!
   IF ( Abort ) GOTO 900
   nomat = Z(1)
   IF ( nomat==0 ) THEN
      CALL fwdrec(*900,Pfile)
      GOTO 400
   ELSE
!
!     UPDATE EPTI ARRAY IF DUMMY ELEMENT IS PRESENT
!
      DO j = 1 , 9
         IF ( Kdum(j)/=0 ) THEN
            k = mod(Kdum(j),1000)/10
            epti(2,28+j) = k*10 + 1
         ENDIF
      ENDDO
!
!     SET UP POINTERS FOR THE MATI TABLE
!
      ma = group
      mb = ma + 1
      mc = mb + group
      md = mc + 1
      me = mc + 4
!
!     READ MATERIAL ID INTO Z SPACE, AND SAVE APPROP. COUNT IN MATI(2,K)
!
      DO j = 1 , nmat
         mati(1,j) = matj(1,j)
         mati(2,j) = matj(2,j)
      ENDDO
      j = 1
      CALL fwdrec(*300,Mfile)
   ENDIF
 100  DO
      CALL read(*300,*300,Mfile,ih(1),3,0,kk)
      DO k = 1 , nmat
         IF ( ih(1)==mati(1,k) ) GOTO 200
      ENDDO
      CALL fwdrec(*300,Mfile)
   ENDDO
 200  nwds = -mati(2,k)
   IF ( nwds<0 ) CALL mesage(-37,0,name)
   mati(2,k) = 0
   DO
      CALL read(*300,*100,Mfile,Z(j),nwds,0,kk)
      j = j + 1
      mati(2,k) = mati(2,k) + 1
   ENDDO
!
!     INSTALL INITIAL COUNTERS IN MATI(1,K)
!
 300  jx = j
   IF ( jx>1 ) THEN
      mati(1,1) = 0
      DO j = 1 , nmat
         k = j + 1
         IF ( mati(2,j)<0 ) mati(2,j) = 0
         mati(1,k) = mati(1,j) + mati(2,j)
      ENDDO
!
!     NOTE - ORIGINAL DATA IN MATI TABLE IS NOW DESTROYED
!
!     CHECK MATERIAL ID UNIQUENESS AMONG MAT1, MAT2,..., MAT8
!     (MAT4 AND MAT5 ARE UNIQUE ONLY AMONG THEMSELVES)
!
      j = 0
      DO k = 1 , ma
         IF ( mati(2,k)>0 ) j = j + 1
      ENDDO
      IF ( j>1 ) THEN
         kk = mati(1,mb)
         k1 = kk - 1
         k4 = mati(1,4)
         DO k = 1 , k1
            j = Z(k)
            ib = k + 1
            DO i = ib , k1
               IF ( j==Z(i) ) THEN
                  IF ( k>=k4 .OR. i<k4 ) THEN
                     CALL mesage(30,213,j)
                     Abort = .TRUE.
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDIF
!
!     CHECK MATT1, MATT2,..., MATT6 AND MATS1 MATERIAL ID
!     AND THEIR CROSS REFERENCE TO MATI CARDS
!
      DO k = mb , mc
         IF ( mati(2,k)>0 ) THEN
            kk = mod(k,ma)
            ib = mati(1,kk) + 1
            ie = mati(2,kk) + ib - 1
            jb = mati(1,k) + 1
            je = mati(2,k) + jb - 1
            DO j = jb , je
               k1 = Z(j)
               IF ( ie>=ib ) THEN
                  DO i = ib , ie
                     IF ( Z(i)==k1 ) GOTO 310
                  ENDDO
               ENDIF
               ih(1) = k1
               ih(2) = kk
               k1 = 217
               IF ( k==15 ) k1 = 17
               CALL mesage(30,k1,ih)
               Abort = .TRUE.
 310        ENDDO
         ENDIF
      ENDDO
!
!     CHECK MATERIAL ID UNIQUENESS AMONG MATPZI AND MTTPZI
!
      j = 0
      DO k = md , me
         IF ( mati(2,k)>0 ) j = j + 1
      ENDDO
      IF ( j>1 ) THEN
         kk = mati(1,me+1)
         k1 = kk - 1
         nn = mati(1,md)
         DO k = nn , k1
            j = Z(k)
            ib = k + 1
            DO i = ib , kk
               IF ( j==Z(i) ) THEN
                  CALL mesage(30,213,j)
                  Abort = .TRUE.
                  EXIT
               ENDIF
            ENDDO
         ENDDO
      ENDIF
   ENDIF
!
!     NOW, WE CONTINUE TO CHECK MATERIAL ID'S ON MOST PROPERTY CARDS.
!     (MATERIAL ID'S ARE ON THE 2ND, 4TH, AND 6TH POSITIONS OF THE
!     PROPERTY CARDS, EXECPT THE OPEN-ENDED PCOMPI GROUP)
!
   je = mati(1,nmat)
   ii = A(1)
   CALL fwdrec(*900,Pfile)
 400  DO
      CALL read(*900,*900,Pfile,ih(1),3,0,kk)
      DO k = 1 , nept
         IF ( ih(1)==epti(1,k) ) GOTO 500
      ENDDO
      CALL fwdrec(*900,Pfile)
   ENDDO
 500  IF ( nomat==0 ) THEN
!
      CALL mesage(30,16,ih)
      Abort = .TRUE.
      GOTO 99999
   ELSE
      nwds = epti(2,k)/10
      nn = epti(2,k) - nwds*10
      ib = 1
      ie = nn*2
      ic = 2
      komp = 0
!
!     CHANGE NWDS, IB, IE, AND IC IF PROPERTY CARD IS OPEN-ENDED
!     WHERE (IB+JX) POINTS TO THE FIRST MID POSITION
!
      IF ( epti(2,k)<=0 ) THEN
         komp = 1
         ib = -nn - 1
         ic = -nwds
         IF ( nwds==0 ) ic = 9999
         nwds = 10
      ENDIF
   ENDIF
 600  IF ( komp==1 ) ie = jx + nwds - 1
!
!     READ IN PROPERTY CARD. IF ID IS NOT ON ACTIVE LIST, SKIP IT.
!     SKIP IT TOO IF IT HAS NO MATERIAL ID REQUESTED.
!     (NO CORE SIZE CHECK HERE. SHOULD HAVE PLENTY AVAILABLE)
!
   CALL read(*900,*400,Pfile,Z(jx),nwds,0,kk)
   IF ( komp/=0 ) THEN
      DO
         ie = ie + 1
         CALL read(*900,*400,Pfile,Z(ie),1,0,kk)
         IF ( Z(ie)==-1 ) THEN
            ie = ie - 1 - jx
            EXIT
         ENDIF
      ENDDO
   ENDIF
   DO i = 2 , ii
      IF ( Z(jx)==A(i) ) GOTO 700
   ENDDO
   GOTO 600
 700  DO i = ib , ie , ic
      kk = Z(jx+i)
      IF ( ie==8 .AND. i==7 ) kk = Z(jx+i+3)
      IF ( kk/=0 ) THEN
         IF ( jx>1 ) THEN
            DO j = 1 , je
               IF ( kk==Z(j) ) GOTO 800
            ENDDO
         ENDIF
         ih(1) = kk
         ih(2) = Z(jx)
         CALL mesage(30,215,ih)
         Abort = .TRUE.
      ENDIF
 800  ENDDO
   GOTO 600
 900  RETURN
99999 RETURN
END SUBROUTINE matck