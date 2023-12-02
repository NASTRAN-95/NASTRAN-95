!*==matck.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE matck(Mfile,Pfile,A,Z)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mfile
   INTEGER :: Pfile
   INTEGER , DIMENSION(1) :: A
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2,40) , SAVE :: epti
   INTEGER , SAVE :: group , nept , nmat
   INTEGER :: i , ib , ic , ie , ii , j , jb , je , jx , k , k1 , k4 , kk , komp , ma , mb , mc , md , me , nn , nomat , nwds
   INTEGER , DIMENSION(3) :: ih
   INTEGER , DIMENSION(2,22) :: mati
   INTEGER , DIMENSION(2,22) , SAVE :: matj
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL fwdrec , mesage , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( abort ) GOTO 80
         nomat = Z(1)
         IF ( nomat==0 ) THEN
            CALL fwdrec(*80,Pfile)
            GOTO 60
         ELSE
!
!     UPDATE EPTI ARRAY IF DUMMY ELEMENT IS PRESENT
!
            DO j = 1 , 9
               IF ( kdum(j)/=0 ) THEN
                  k = mod(kdum(j),1000)/10
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
            CALL fwdrec(*40,Mfile)
         ENDIF
 20      SPAG_Loop_1_1: DO
            CALL read(*40,*40,Mfile,ih(1),3,0,kk)
            DO k = 1 , nmat
               IF ( ih(1)==mati(1,k) ) EXIT SPAG_Loop_1_1
            ENDDO
            CALL fwdrec(*40,Mfile)
         ENDDO SPAG_Loop_1_1
         nwds = -mati(2,k)
         IF ( nwds<0 ) CALL mesage(-37,0,name)
         mati(2,k) = 0
         DO
            CALL read(*40,*20,Mfile,Z(j),nwds,0,kk)
            j = j + 1
            mati(2,k) = mati(2,k) + 1
         ENDDO
!
!     INSTALL INITIAL COUNTERS IN MATI(1,K)
!
 40      jx = j
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
                  SPAG_Loop_2_2: DO i = ib , k1
                     IF ( j==Z(i) ) THEN
                        IF ( k>=k4 .OR. i<k4 ) THEN
                           CALL mesage(30,213,j)
                           abort = .TRUE.
                           EXIT SPAG_Loop_2_2
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_2
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
                  SPAG_Loop_2_3: DO j = jb , je
                     k1 = Z(j)
                     IF ( ie>=ib ) THEN
                        DO i = ib , ie
                           IF ( Z(i)==k1 ) CYCLE SPAG_Loop_2_3
                        ENDDO
                     ENDIF
                     ih(1) = k1
                     ih(2) = kk
                     k1 = 217
                     IF ( k==15 ) k1 = 17
                     CALL mesage(30,k1,ih)
                     abort = .TRUE.
                  ENDDO SPAG_Loop_2_3
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
                  SPAG_Loop_2_4: DO i = ib , kk
                     IF ( j==Z(i) ) THEN
                        CALL mesage(30,213,j)
                        abort = .TRUE.
                        EXIT SPAG_Loop_2_4
                     ENDIF
                  ENDDO SPAG_Loop_2_4
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
         CALL fwdrec(*80,Pfile)
 60      SPAG_Loop_1_5: DO
            CALL read(*80,*80,Pfile,ih(1),3,0,kk)
            DO k = 1 , nept
               IF ( ih(1)==epti(1,k) ) EXIT SPAG_Loop_1_5
            ENDDO
            CALL fwdrec(*80,Pfile)
         ENDDO SPAG_Loop_1_5
         IF ( nomat==0 ) THEN
!
            CALL mesage(30,16,ih)
            abort = .TRUE.
            RETURN
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
         spag_nextblock_1 = 2
      CASE (2)
         IF ( komp==1 ) ie = jx + nwds - 1
!
!     READ IN PROPERTY CARD. IF ID IS NOT ON ACTIVE LIST, SKIP IT.
!     SKIP IT TOO IF IT HAS NO MATERIAL ID REQUESTED.
!     (NO CORE SIZE CHECK HERE. SHOULD HAVE PLENTY AVAILABLE)
!
         CALL read(*80,*60,Pfile,Z(jx),nwds,0,kk)
         IF ( komp/=0 ) THEN
            SPAG_Loop_1_6: DO
               ie = ie + 1
               CALL read(*80,*60,Pfile,Z(ie),1,0,kk)
               IF ( Z(ie)==-1 ) THEN
                  ie = ie - 1 - jx
                  EXIT SPAG_Loop_1_6
               ENDIF
            ENDDO SPAG_Loop_1_6
         ENDIF
         DO i = 2 , ii
            IF ( Z(jx)==A(i) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (3)
         SPAG_Loop_1_7: DO i = ib , ie , ic
            kk = Z(jx+i)
            IF ( ie==8 .AND. i==7 ) kk = Z(jx+i+3)
            IF ( kk/=0 ) THEN
               IF ( jx>1 ) THEN
                  DO j = 1 , je
                     IF ( kk==Z(j) ) CYCLE SPAG_Loop_1_7
                  ENDDO
               ENDIF
               ih(1) = kk
               ih(2) = Z(jx)
               CALL mesage(30,215,ih)
               abort = .TRUE.
            ENDIF
         ENDDO SPAG_Loop_1_7
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 80      RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE matck
