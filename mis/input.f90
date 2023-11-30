
SUBROUTINE input
   IMPLICIT NONE
   REAL Degra , Pi , Radeg , S4pisq , Twopi , X(1)
   INTEGER Ix(1) , Ksystm(100) , Mach , Modcom(9) , Nbuf , Nin , Nlines , Nout , Parama , Paramb , Paramc , Sperlk , Two(32)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Parama , Paramb , Paramc
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /machin/ Mach
   COMMON /system/ Ksystm
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
   CHARACTER*8 chr , e1 , e2 , e3
   REAL cl , ddy , dx , dy , f , lambda , qk(100) , sl , th , u , x1 , x2 , x3 , xb , xk1 , xk2 , xl , xm , xo , zk , zm
   INTEGER cord2c(2) , eee(3) , fil , file , filin(5) , filout(5) , hfil(3,5) , i , i1t(20) , i1tt , i2t(20) , i2tt , ibuf , ic ,   &
         & icen , ifil , iflag , iflg , ig0 , ii , ij , il , ill , ilmk , iox , ioy , ip , iw , iwmk , iww , ix0 , ixw , iy0 , iyl ,&
         & j , j1t(20) , j1tt , j2t(20) , j2tt , jj , k(100) , k1(100) , k2(100) , k3(70) , kdn(2,20) , kdsort(270) , kk , kkk ,    &
         & kl(20) , kno(20) , knoic , knox , kor , ksrt , ksrtx , kt(20) , ktt , l , m , mnam(2) , n , n1 , ne , nkor , nm1 , nn ,  &
         & nsload , nx , nx1 , ny , ny1 , param1 , paramn , r , r1 , r2 , rdflg , t(7)
   LOGICAL inopen(5)
   INTEGER iunion , korsz , orf
   INTEGER lf
   EXTERNAL orf
!
!     INPUT I1,I2,I3,I4,I5/O1,O2,O3,O4,O5/C,N,-V1-/C,N,-V2-/C,N,-V3- $
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Nbuf) , (Ksystm(2),Nout) , (Ksystm(4),Nin) , (Ksystm(12),Nlines) , (Ksystm(57),Modcom(1)) ,               &
!>>>>    & (Ksystm(95),Sperlk) , (qk(1),k(1)) , (X(1),Ix(1)) , (kdsort(1),k1(1)) , (kdsort(101),k2(1)) , (kdsort(201),k3(1))
!
   DATA cord2c/2001 , 20/
   DATA mnam/4HINPU , 4HT   /
   DATA filin/101 , 102 , 103 , 104 , 105/
   DATA filout/201 , 202 , 203 , 204 , 205/
   DATA eee/3*2147483647/
   DATA param1 , paramn/1 , 8/
   DATA e1 , e2/'ENDDATA ' , 'END DATA'/ , e3/'ENDATA  '/
   DATA inopen/5*.FALSE./
!
!     SORTSEQUENCE (INTERNALSEQUENCEID)
!
!                1    2    3    4    5    6    7    8    9    0
!
   DATA k1/116 , 115 , 2 , 211 , 58 , 57 , 59 , 61 , 60 , 62 , 169 , 215 , 216 , 221 , 144 , 214 , 137 , 104 , 134 , 105 , 135 ,    &
      & 106 , 136 , 165 , 213 , 114 , 233 , 113 , 181 , 189 , 191 , 3 , 185 , 186 , 184 , 188 , 187 , 177 , 178 , 176 , 172 , 182 , &
      & 190 , 170 , 151 , 161 , 56 , 70 , 83 , 85 , 4 , 78 , 79 , 77 , 82 , 81 , 68 , 69 , 67 , 63 , 71 , 84 , 54 , 55 , 49 , 50 ,  &
      & 51 , 52 , 23 , 24 , 25 , 26 , 36 , 37 , 38 , 39 , 122 , 123 , 80 , 76 , 89 , 148 , 138 , 121 , 101 , 98 , 99 , 5 , 1 , 127 ,&
      & 128 , 145 , 227 , 228 , 229 , 230 , 231 , 235 , 6 , 7/
!
!                1    2    3    4    5    6    7    8    9    0
!
   DATA k2/8 , 129 , 9 , 75 , 219 , 10 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 35 , 152 , 153 , 154 , 92 , 94 , 183 , 124 , 91 ,  &
      & 102 , 110 , 109 , 140 , 141 , 142 , 143 , 205 , 206 , 223 , 224 , 210 , 240 , 241 , 242 , 243 , 225 , 226 , 244 , 96 , 13 , &
      & 203 , 22 , 150 , 217 , 139 , 146 , 220 , 171 , 209 , 179 , 234 , 107 , 133 , 100 , 155 , 156 , 157 , 222 , 158 , 159 , 160 ,&
      & 111 , 245 , 247 , 249 , 251 , 253 , 255 , 257 , 259 , 261 , 246 , 248 , 250 , 16 , 21 , 149 , 88 , 90 , 95 , 164 , 252 ,    &
      & 254 , 256 , 130 , 202 , 232 , 258 , 260 , 262 , 199 , 200 , 201 , 166 , 167 , 97/
!
!                1    2    3    4    5    6    7    8    9    0
!
   DATA k3/236 , 237 , 238 , 239 , 117 , 112 , 204 , 180 , 40 , 41 , 42 , 14 , 17 , 108 , 11 , 12 , 74 , 86 , 44 , 45 , 93 , 103 ,  &
      & 15 , 18 , 19 , 20 , 72 , 73 , 118 , 119 , 212 , 43 , 194 , 125 , 126 , 162 , 131 , 132 , 192 , 193 , 195 , 196 , 197 , 198 ,&
      & 207 , 208 , 120 , 147 , 64 , 173 , 46 , 47 , 48 , 163 , 168 , 218 , 87 , 53 , 65 , 174 , 66 , 175 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0/
!
!                1    2    3    4    5    6    7    8    9    0
!
   DATA kl/901 , 1301 , 5501 , 5481 , 4501 , 2408 , 501 , 5301 , 2801 , 3301 , 5401 , 5551 , 3001 , 5001 , 5008 , 5561 , 2001 , 0 , &
      & 0 , 0/
   DATA kt/2 , 2 , 1 , 1 , 1 , 1 , 2 , 1 , 1 , 1 , 1 , 1 , 2 , 1 , 1 , 1 , 1 , 0 , 0 , 0/
!
   DATA i1t/2 , 2 , 5 , 5 , 4 , 3 , 2 , 5 , 3 , 4 , 5 , 5 , 3 , 5 , 5 , 6 , 3 , 0 , 0 , 0/
   DATA j1t/9 , 13 , 7 , 10 , 13 , 8 , 5 , 5 , 12 , 1 , 6 , 1 , 14 , 2 , 2 , 12 , 4 , 0 , 0 , 0/
!
   DATA i2t/7 , 7 , 0 , 0 , 0 , 0 , 7 , 0 , 0 , 0 , 0 , 0 , 7 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
   DATA j2t/8 , 10 , 0 , 0 , 0 , 0 , 9 , 0 , 0 , 0 , 0 , 0 , 16 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
!
   DATA kno/76 , 68 , 16 , 12 , 1 , 180 , 72 , 4 , 57 , 52 , 25 , 105 , 48 , 15 , 258 , 215 , 9 , 0 , 0 , 0/
!
   DATA kdn(1,1) , kdn(2,1)/4HCELA , 4HS4  / , kdn(1,2) , kdn(2,2)/4HCMAS , 4HS4  / , kdn(1,3) , kdn(2,3)/4HSPC  , 4H    / ,        &
      & kdn(1,4) , kdn(2,4)/4HSPC1 , 4H    / , kdn(1,5) , kdn(2,5)/4HGRID , 4H    / , kdn(1,6) , kdn(2,6)/4HCBAR , 4H    / ,        &
      & kdn(1,7) , kdn(2,7)/4HCDAM , 4HP4  / , kdn(1,8) , kdn(2,8)/4HSEQG , 4HP   / , kdn(1,9) , kdn(2,9)/4HCQUA , 4HD1  / ,        &
      & kdn(1,10) , kdn(2,10)/4HCTRI , 4HA1  / , kdn(1,11) , kdn(2,11)/4HSLOA , 4HD   / , kdn(1,12) , kdn(2,12)/4HSPOI , 4HNT  / ,  &
      & kdn(1,13) , kdn(2,13)/4HCROD , 4H    / , kdn(1,14) , kdn(2,14)/4HOMIT , 4H    / , kdn(1,15) , kdn(2,15)/4HCNGR , 4HNT  / ,  &
      & kdn(1,16) , kdn(2,16)/4HASET , 4H    / , kdn(1,17) , kdn(2,17)/4HXXXX , 4H    / , kdn(1,18) , kdn(2,18)/4HXXXX , 4H    / ,  &
      & kdn(1,19) , kdn(2,19)/4HXXXX , 4H    / , kdn(1,20) , kdn(2,20)/4HXXXX , 4H    /
!
!
   lf(i,j,n) = i + n*(j-1)
!
   IF ( param1<=Parama .AND. Parama<=paramn ) THEN
!
      kor = 10*Nbuf + 1
      nkor = korsz(X) - 10*Nbuf
      IF ( nkor<=0 ) CALL mesage(-8,nkor,mnam)
      CALL page1
      Nlines = Nlines + 8
      WRITE (Nout,99001)
99001 FORMAT (//20X,'* U T I L I T Y   M O D U L E   I N P U T *',///,20X,'INPUT DATA ECHO (DATA READ VIA FORTRAN, REMEMBER TO ',   &
             &'RIGHT ADJUST)',///20X,'*   1  **   2  **   3  **   4  ','**   5  **   6  **   7  **   8  **   9  **  10  *',///)
      iox = 0
      ioy = 0
      IF ( Mach<5 .OR. Sperlk/=0 ) GOTO 200
      DO
!
!     ON VAX-11/780 OR UNIX MACHINES, SEARCH FOR END OF BULK DATA DECK.
!
         READ (Nin,99002,END=100) chr
99002    FORMAT (A8)
         IF ( chr==e1 .OR. chr==e2 .OR. chr==e3 ) GOTO 200
      ENDDO
   ELSE
      WRITE (Nout,99003) Ufm , Parama
99003 FORMAT (A23,' 1738, UTILITY MODULE INPUT FIRST PARAMETER VALUE - ',I20,' OUT OF RANGE')
      GOTO 11200
   ENDIF
!
!     ENDDATA CARD NOT FOUND
!
 100  WRITE (Nout,99004) Ufm
99004 FORMAT (A23,' - "ENDDATA" CARD NOT FOUND BY INPUT MODULE')
   CALL mesage(-37,0,mnam)
!
 200  IF ( Parama==2 ) THEN
!
!
!     PARAMA = 2 RECTANGULAR FRAME MADE FROM BAR-S OR ROD-S
!
!     INPUT, ,,,,/G1,G2,,,/C,N,2/C,N,I/C,N,J $
!     INPUT GEOM1,GEOM2,,,/G1,G2,,,/C,N,2/C,N,I/C,N,J $
!            I=1  REGULAR BANDING
!            I=2  DOUBLE BANDING
!            I=3  ACTIVE COLUMN BANDING
!            I=4  REVERSE DOUBLE BANDING
!            J=0  BAR CONFIGURATION
!            J=1  ROD CONFIGURATION 1  (DIAGONALS IN BOTH DIRECTIONS)
!            J=2  ROD CONFIGURATION 2  (DIAGONALS IN LR TO UL DIRECTN)
!            J=3  ROD CONFIGURATION 3  (STATICALLY DETERMINATE)
!
!
      READ (Nin,99005) nx , ny , dx , dy , ip , lambda
99005 FORMAT (2I8,2E8.0,I8,E8.0)
      CALL page2(-1)
      WRITE (Nout,99031) nx , ny , dx , dy , ip , lambda
      nx1 = nx + 1
      ny1 = ny + 1
      ASSIGN 2100 TO r2
      GOTO 1500
   ELSEIF ( Parama==3 ) THEN
      GOTO 3000
   ELSEIF ( Parama==4 ) THEN
      GOTO 3000
   ELSEIF ( Parama==5 ) THEN
!
!
!     PARAMA = 5 N-SEGMENT STRING
!
!     INPUT, ,,,,/,G2,,,/C,N,5 $
!     INPUT, ,GEOM2,,,/,G2,,,/C,N,5 $
!
!
      READ (Nin,99006) n , xk1 , xk2 , xm , xb
99006 FORMAT (I8,4E8.0)
      CALL page2(-1)
      WRITE (Nout,99007) n , xk1 , xk2 , xm , xb
99007 FORMAT (21X,I8,1P,4E8.1)
      n1 = n + 1
      nm1 = n - 1
!
!     G2
!
      ifil = 2
      ASSIGN 4900 TO r
   ELSEIF ( Parama==6 ) THEN
!
!
!     PARAMA = 6 N-CELL BAR
!
!     INPUT, ,,,,/G1,G2,,G4,/C,N,6 $
!     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,6 $
!
!
      READ (Nin,99008) n , xl , ip , iflg , ig0 , m , iox
99008 FORMAT (I8,E8.0,5I8)
      CALL page2(-1)
      WRITE (Nout,99009) n , xl , ip , iflg , ig0 , m , iox
99009 FORMAT (21X,I8,1P,E8.1,5I8)
      n1 = n + 1
!
!     G1
!
      ifil = 1
      ASSIGN 5800 TO r
   ELSEIF ( Parama==7 ) THEN
!
!
!     PARAMA = 7 FULL MATRIX AND OPTIONAL UNIT LOAD
!
!     INPUT, ,,,,/,G2,G3,,G5/C,N,7 $
!     INPUT, ,GEOM2,GEOM3,,/,G2,G3,,G5/C,N,7 $
!
!
      READ (Nin,99010) n , nsload
99010 FORMAT (2I8)
      CALL page2(-1)
      WRITE (Nout,99011) n , nsload
99011 FORMAT (21X,2I8)
      n1 = n + 1
!
!     G2
!
      ifil = 2
      ASSIGN 6900 TO r
   ELSEIF ( Parama==8 ) THEN
!
!
!     PARAMA = 8 N-SPOKE WHEEL
!
!     INPUT, ,,,,/G1,G2,,,/C,N,8 $
!     INPUT GEOM1,GEOM2,,,/G1,G2,,,/C,N,8 $
!
!
      READ (Nin,99012) n , xl , ip , iflg , ig0 , icen
99012 FORMAT (I8,E8.0,4I8)
      CALL page2(-1)
      WRITE (Nout,99013) n , xl , ip , iflg , ig0 , icen
99013 FORMAT (21X,I8,1P,E8.1,4I8)
      n1 = n + 1
!
!     G1
!
      ifil = 1
      ASSIGN 8000 TO r
   ELSE
!
!
!     PARAMA = 1 LAPLACE NETWORK
!
!     INPUT, ,,,,/,G2,,G4,/C,N,1/C,N,1 $         STATICS
!     INPUT, ,GEOM2,,GEOM4,/,G2,,G4,/C,N,1/C,N,1 $         STATICS
!     INPUT, ,,,,/,G2,,,/C,N,1/C,N,2 $          REAL-EIG W/O MASS COUPL
!     INPUT, ,GEOM2,,,/,G2,,,/C,N,1/C,N,2 $     REAL-EIG W/O MASS COUPL
!     INPUT, ,,,,/,G2,,,/C,N,1/C,N,3 $          REAL-EIG WITH MASS COUPL
!     INPUT, ,GEOM2,,,/,G2,,,/C,N,1/C,N,3 $     REAL-EIG WITH MASS COUPL
!
!
      IF ( Paramb==2 ) THEN
!
!
         READ (Nin,99014) n , zk , zm
99014    FORMAT (I8,2E8.0)
         CALL page2(-1)
         WRITE (Nout,99032) n , zk , zm
      ELSEIF ( Paramb==3 ) THEN
!
         READ (Nin,99015) n , zk , zm , f
99015    FORMAT (I8,3E8.0)
         CALL page2(-1)
         WRITE (Nout,99032) n , zk , zm , f
      ELSE
!
         READ (Nin,99016) n , zk , u
99016    FORMAT (I8,2E8.0)
         CALL page2(-1)
         WRITE (Nout,99032) n , zk , u
!
         ASSIGN 300 TO r2
         GOTO 250
      ENDIF
!
      ASSIGN 1400 TO r2
!
 250  n1 = n + 1
      nm1 = n - 1
!
!     G2
!
      ifil = 2
      ASSIGN 700 TO r
   ENDIF
   GOTO 9000
!
!     G4
!
 300  ifil = 4
   ASSIGN 400 TO r
   GOTO 9000
!
!     SPC
!
 400  ic = 3
   ASSIGN 500 TO r
   GOTO 9300
 500  k(1) = 1000 + n
   k(3) = 0
   k(4) = 0
   DO i = 2 , n
      k(2) = i
      CALL write(file,k,4,0)
   ENDDO
   DO i = 2 , n
      k(2) = lf(1,i,n1)
      qk(4) = u
      CALL write(file,k,4,0)
      k(2) = k(2) + n
      k(4) = 0
      CALL write(file,k,4,0)
   ENDDO
   DO i = 2 , n
      k(2) = n*n1 + i
      CALL write(file,k,4,0)
   ENDDO
   ASSIGN 600 TO r1
   GOTO 10300
 600  RETURN
!
!     CELAS4
!
 700  ic = 1
   ASSIGN 800 TO r
   GOTO 9300
 800  qk(2) = zk
   DO j = 2 , n
      DO i = 1 , n
         k(1) = lf(i,j,n1)
         k(3) = k(1)
         k(4) = k(3) + 1
         IF ( Paramb/=1 .AND. i==1 ) k(3) = 0
         IF ( Paramb/=1 .AND. i==n ) k(4) = 0
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   DO j = 1 , n
      DO i = 2 , n
         k(3) = lf(i,j,n1)
         k(4) = k(3) + n1
         k(1) = k(3) + 1000000
         IF ( Paramb/=1 .AND. j==1 ) k(3) = 0
         IF ( Paramb/=1 .AND. j==n ) k(4) = 0
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   ASSIGN 900 TO r1
   GOTO 10600
 900  IF ( Paramb==1 ) GOTO 1100
!
!     CMASS4
!
   ic = 2
   ASSIGN 1000 TO r
   GOTO 9300
 1000 qk(2) = zm
   k(4) = 0
   DO j = 2 , n
      DO i = 2 , n
         k(3) = lf(i,j,n1)
         k(1) = k(3) + 2000000
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   IF ( Paramb==3 ) THEN
!
      qk(2) = -f*zm
      DO j = 2 , n
         DO i = 1 , n
            k(3) = lf(i,j,n1)
            k(1) = k(3) + 3000000
            k(4) = k(3) + 1
            IF ( i==1 ) k(3) = 0
            IF ( i==n ) k(4) = 0
            CALL write(file,k,4,0)
         ENDDO
      ENDDO
      DO j = 1 , n
         DO i = 2 , n
            k(3) = lf(i,j,n1)
            k(4) = k(3) + n1
            k(1) = k(3) + 4000000
            IF ( j==1 ) k(3) = 0
            IF ( j==n ) k(4) = 0
            CALL write(file,k,4,0)
         ENDDO
      ENDDO
      qk(2) = -f*zm/2.0
      DO j = 1 , n
         DO i = 1 , n
            k(3) = lf(i,j,n1)
            k(1) = k(3) + 5000000
            k(4) = k(3) + n1 + 1
            IF ( i==1 .OR. j==1 ) k(3) = 0
            IF ( i==n .OR. j==n ) k(4) = 0
            IF ( k(3)/=0 .OR. k(4)/=0 ) CALL write(file,k,4,0)
         ENDDO
      ENDDO
      DO j = 1 , n
         DO i = 1 , n
            k(3) = lf(i,j,n1)
            k(1) = k(3) + 6000000
            k(4) = k(3) + n1
            k(3) = k(3) + 1
            IF ( i==n .OR. j==1 ) k(3) = 0
            IF ( i==1 .OR. j==n ) k(4) = 0
            IF ( k(3)/=0 .OR. k(4)/=0 ) CALL write(file,k,4,0)
         ENDDO
      ENDDO
   ENDIF
   ASSIGN 1100 TO r1
   GOTO 10600
 1100 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 1300 TO r
      GOTO 9800
!
!     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
!
   ELSEIF ( n<3 ) THEN
      ASSIGN 1300 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT
!
      ic = 15
      ASSIGN 1200 TO r
      GOTO 9300
   ENDIF
 1200 DO j = 2 , n
      DO i = 1 , n
         IF ( .NOT.(Paramb/=1 .AND. (i==1 .OR. i==n)) ) THEN
            k(1) = lf(i,j,n1)
            CALL write(file,k,1,0)
         ENDIF
      ENDDO
   ENDDO
   DO j = 1 , n
      IF ( .NOT.(Paramb/=1 .AND. (j==1 .OR. j==n)) ) THEN
         DO i = 2 , n
            k(1) = lf(i,j,n1) + 1000000
            CALL write(file,k,1,0)
         ENDDO
      ENDIF
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   IF ( Paramb/=1 ) THEN
      DO j = 2 , n
         DO i = 1 , n , nm1
            k(1) = lf(i,j,n1)
            CALL write(file,k,1,0)
         ENDDO
      ENDDO
      DO j = 1 , n , nm1
         DO i = 2 , n
            k(1) = lf(i,j,n1) + 1000000
            CALL write(file,k,1,0)
         ENDDO
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
   ENDIF
   IF ( Paramb/=1 ) THEN
      DO j = 2 , n
         DO i = 2 , n
            k(1) = lf(i,j,n1) + 2000000
            CALL write(file,k,1,0)
         ENDDO
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
      IF ( Paramb/=2 ) THEN
         DO j = 2 , n
            DO i = 2 , nm1
               k(1) = lf(i,j,n1) + 3000000
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         DO j = 2 , nm1
            DO i = 2 , n
               k(1) = lf(i,j,n1) + 4000000
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         DO j = 2 , n
            DO i = 1 , n , nm1
               k(1) = lf(i,j,n1) + 3000000
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         DO j = 1 , n , nm1
            DO i = 2 , n
               k(1) = lf(i,j,n1) + 4000000
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         DO l = 1 , 2
            DO j = 2 , nm1
               DO i = 2 , nm1
                  k(1) = lf(i,j,n1) + 1000000*l + 4000000
                  CALL write(file,k,1,0)
               ENDDO
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         DO j = 1 , n
            DO i = 1 , n
               IF ( i==1 .OR. i==n .OR. j==1 .OR. j==n ) THEN
                  IF ( .NOT.(j==1 .AND. i==n .OR. j==n .AND. i==1) ) THEN
                     k(1) = lf(i,j,n1) + 5000000
                     CALL write(file,k,1,0)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         DO j = 1 , n
            DO i = 1 , n
               IF ( i==1 .OR. i==n .OR. j==1 .OR. j==n ) THEN
                  IF ( .NOT.(j==1 .AND. i==1 .OR. j==n .AND. i==n) ) THEN
                     k(1) = lf(i,j,n1) + 6000000
                     CALL write(file,k,1,0)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
      ENDIF
   ENDIF
   ASSIGN 1300 TO r1
   GOTO 10300
 1300 GOTO r2
 1400 RETURN
!
!     G1
!
 1500 ifil = 1
   ASSIGN 1600 TO r
   GOTO 9000
!
!     GRID
!
 1600 ic = 5
   ASSIGN 1700 TO r
   GOTO 9300
 1700 qk(5) = 0.0
   k(2) = 0
   k(6) = 0
   k(8) = 0
   sl = sin(Degra*lambda)
   cl = cos(Degra*lambda)
   ddy = dy*cl
   jj = -1
   DO j = 1 , ny1
      jj = jj + 1
      IF ( jj>ioy ) jj = 0
      qk(4) = ddy*float(j-1)
      xo = float(j-1)*sl
      ii = -1
      DO i = 1 , nx1
         ii = ii + 1
         IF ( ii>iox ) ii = 0
         k(1) = lf(i,j,nx1)
         qk(3) = dx*float(i-1) + xo
         IF ( ii==0 .OR. jj==0 ) THEN
            k(7) = ip
         ELSE
!
!
            k(7) = 6
         ENDIF
         CALL write(file,k,8,0)
      ENDDO
   ENDDO
   ASSIGN 1800 TO r1
   GOTO 10600
!
 1800 IF ( Paramb==1 ) GOTO 2000
   ic = 8
   ASSIGN 1900 TO r
   GOTO 9300
 1900 IF ( Paramb==1 ) GOTO 2000
   IF ( Paramb==3 ) THEN
!
!     ACTIVE COLUMNS BANDING
!
      ij = nx1*ny1
      IF ( mod(ny,2)==0 ) THEN
         kk = (ny/2+1)*nx1
         kkk = ij - kk
         nn = 2
      ELSE
         kk = ij/2
         kkk = 0
         nn = 1
      ENDIF
      DO j = 1 , ij
         k(1) = j
         IF ( nn==2 ) THEN
            IF ( j<=kkk ) THEN
               k(2) = j + kk
            ELSE
               k(2) = j - kkk
            ENDIF
         ELSEIF ( j<=kk ) THEN
            k(2) = j + kk
         ELSE
            k(2) = j - kk
         ENDIF
         CALL write(file,k,2,0)
      ENDDO
   ELSEIF ( Paramb==4 ) THEN
!
!     REVERSE DOUBLE BANDING
!
      ij = nx1*ny1
      IF ( mod(nx,2)==0 ) THEN
         kk = nx1/2 + 1
         nn = 2
      ELSE
         kk = nx1/2
         nn = 1
      ENDIF
      DO j = 1 , ij
         k(1) = j
         iw = mod(j,nx1)
         IF ( iw==0 ) iw = nx1
         il = (j-1)/nx1 + 1
         iwmk = iw - kk
         IF ( nn==2 ) THEN
            IF ( iwmk<0 ) THEN
               iww = -2*iwmk
            ELSE
               iww = 2*iwmk + 1
            ENDIF
            k(2) = lf(il,iww,ny1)
         ELSE
            IF ( iwmk<=0 ) THEN
               iww = -2*iwmk + 1
            ELSE
               iww = 2*iwmk
            ENDIF
            k(2) = lf(il,iww,ny1)
         ENDIF
         CALL write(file,k,2,0)
      ENDDO
   ELSE
!
!     DOUBLE BANDING
!
      IF ( mod(ny,2)==0 ) THEN
         kk = ny1/2 + 1
         nn = 2
      ELSE
         kk = ny1/2
         nn = 1
      ENDIF
      ij = ny1*nx1
      DO j = 1 , ij
         k(1) = j
         iw = mod(j,nx1)
         IF ( iw==0 ) iw = nx1
         il = (j-1)/nx1 + 1
         ilmk = il - kk
         IF ( nn==2 ) THEN
            IF ( ilmk<0 ) THEN
               ill = -2*ilmk
            ELSE
               ill = 2*ilmk + 1
            ENDIF
            k(2) = lf(iw,ill,nx1)
         ELSE
            IF ( ilmk<=0 ) THEN
               ill = -2*ilmk + 1
            ELSE
               ill = 2*ilmk
            ENDIF
            k(2) = lf(iw,ill,nx1)
         ENDIF
         CALL write(file,k,2,0)
      ENDDO
   ENDIF
!
   ASSIGN 2000 TO r1
   GOTO 10600
 2000 GOTO r2
 2100 ASSIGN 2200 TO r
   GOTO 9800
!
!     G2
!
 2200 ifil = 2
   ASSIGN 2300 TO r
   GOTO 9000
 2300 IF ( Paramc/=0 ) THEN
!
!     CROD
!
      ic = 13
      ASSIGN 2900 TO r
   ELSE
!
!     CBAR
!
      ic = 6
      ASSIGN 2400 TO r
   ENDIF
   GOTO 9300
 2400 k(2) = 101
   qk(5) = 0.0
   qk(6) = 0.0
   qk(7) = 1.0
   k(8) = 1
   DO i = 9 , 16
      k(i) = 0
   ENDDO
   DO j = 1 , ny1
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)
         k(3) = k(1)
         k(4) = k(1) + 1
         CALL write(file,k,16,0)
      ENDDO
   ENDDO
   DO j = 1 , ny
      DO i = 1 , nx1
         k(3) = lf(i,j,nx1)
         k(4) = k(3) + nx1
         k(1) = k(3) + 1000000
         CALL write(file,k,16,0)
      ENDDO
   ENDDO
 2500 ASSIGN 2600 TO r1
   GOTO 10600
 2600 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 2800 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT    (OUT OF SEQUENCE FOR CROD CASES)
!
      ic = 15
      ASSIGN 2700 TO r
      GOTO 9300
   ENDIF
 2700 DO j = 1 , ny1
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)
         CALL write(file,k,1,0)
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   DO j = 1 , ny
      DO i = 1 , nx1
         k(1) = lf(i,j,nx1) + 1000000
         CALL write(file,k,1,0)
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   IF ( Paramc/=0 ) THEN
      DO j = 1 , ny
         DO i = 1 , nx
            k(1) = lf(i,j,nx1)*2 + 1999999
            CALL write(file,k,1,0)
            IF ( Paramc==3 .AND. j>1 ) EXIT
         ENDDO
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
      IF ( Paramc==1 ) THEN
         DO j = 1 , ny
            DO i = 1 , nx
               k(1) = lf(i,j,nx1)*2 + 2000000
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
      ENDIF
   ENDIF
   ASSIGN 2800 TO r1
   GOTO 10300
 2800 RETURN
 2900 k(2) = 101
   DO j = 1 , ny1
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)
         k(3) = k(1)
         k(4) = k(3) + 1
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   DO j = 1 , ny
      DO i = 1 , nx1
         k(3) = lf(i,j,nx1)
         k(4) = k(3) + nx1
         k(1) = k(3) + 1000000
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   DO j = 1 , ny
      DO i = 1 , nx
         k(3) = lf(i,j,nx1) + 1
         k(4) = k(3) + nx
         k(1) = 2*k(3) + 1999997
         CALL write(file,k,4,0)
         IF ( Paramc==3 .AND. j>1 ) EXIT
         IF ( Paramc==1 ) THEN
            k(1) = k(1) + 1
            k(3) = k(3) - 1
            k(4) = k(4) + 1
            CALL write(file,k,4,0)
         ENDIF
      ENDDO
   ENDDO
   GOTO 2500
!
!
!     PARAMA = 3 RECTANGULAR PLATE MADE FROM QUAD1-S
!
!     INPUT, ,,,,/G1,G2,,G4,/C,N,3/C,N,I $
!     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,3/C,N,I $
!              I=1  REGULAR BANDING
!              I=2  DOUBLE BANDING
!              I=3  ACTIVE COLUMN BANDING
!              I=4  REVERSE DOUBLE BANDING
!
!
 3000 READ (Nin,99017) nx , ny , dx , dy , ip , lambda , th
99017 FORMAT (2I8,2E8.0,I8,2E8.0)
   CALL page2(-2)
   WRITE (Nout,99031) nx , ny , dx , dy , ip , lambda , th
   READ (Nin,99018) iy0 , ix0 , iyl , ixw , iox , ioy
99018 FORMAT (6I8)
   WRITE (Nout,99019) iy0 , ix0 , iyl , ixw , iox , ioy
99019 FORMAT (21X,6I8)
   nx1 = nx + 1
   ny1 = ny + 1
!
!     GRID
!
   ASSIGN 3100 TO r2
   GOTO 1500
 3100 ASSIGN 3200 TO r
   GOTO 9800
!
!     G2
!
 3200 ifil = 2
   ASSIGN 3300 TO r
   GOTO 9000
!
!     CQUAD1
!
 3300 IF ( Parama==4 ) THEN
!
!     CTRIA1
!
      ic = 10
      ASSIGN 4600 TO r
   ELSE
      ic = 9
      ASSIGN 3400 TO r
   ENDIF
   GOTO 9300
 3400 k(2) = 101
   qk(7) = th
   DO j = 1 , ny
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)
         k(3) = k(1)
         k(4) = k(3) + 1
         k(6) = k(1) + nx1
         k(5) = k(6) + 1
         CALL write(file,k,7,0)
      ENDDO
   ENDDO
   ASSIGN 3500 TO r1
   GOTO 10600
 3500 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 3800 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT (OUT OF SEQUENCE)
!
      ic = 15
      ASSIGN 3600 TO r
      GOTO 9300
   ENDIF
 3600 DO j = 1 , ny
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)
         CALL write(file,k,1,0)
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
 3700 ASSIGN 3800 TO r1
   GOTO 10300
!
!     SPC-S AND OMIT-S
!
 3800 IF ( ip+iy0+ix0+iyl+ixw+iox+ioy==0 ) GOTO 4500
!
!     G4
!
   ifil = 4
   ASSIGN 4100 TO r
   GOTO 9000
!
!     SPC
!
 3900 ic = 3
   ASSIGN 4000 TO r
   GOTO 9300
 4000 k(1) = 1000*nx + ny
   k(4) = 0
   DO i = 1 , nx1
      k(2) = i
      k(3) = iy0
      IF ( i==1 ) k(3) = iunion(iy0,ix0)
      IF ( i==nx1 ) k(3) = iunion(iy0,ixw)
      IF ( k(3)/=0 ) CALL write(file,k,4,0)
   ENDDO
   DO i = 2 , ny
      k(2) = lf(1,i,nx1)
      k(3) = ix0
      IF ( k(3)/=0 ) CALL write(file,k,4,0)
      k(2) = k(2) + nx
      k(3) = ixw
      IF ( k(3)/=0 ) CALL write(file,k,4,0)
   ENDDO
   k(2) = nx1*ny
   DO i = 1 , nx1
      k(2) = k(2) + 1
      k(3) = iyl
      IF ( i==1 ) k(3) = iunion(iyl,ix0)
      IF ( i==nx1 ) k(3) = iunion(iyl,ixw)
      IF ( k(3)/=0 ) CALL write(file,k,4,0)
   ENDDO
   ASSIGN 4400 TO r1
   GOTO 10600
 4100 IF ( iox+ioy==0 ) GOTO 3900
!
!     OMIT
!
   ic = 14
   ASSIGN 4200 TO r
   GOTO 9300
 4200 DO i = 2 , 12 , 2
      k(i) = i/2
   ENDDO
   jj = 0
   DO j = 2 , ny
      jj = jj + 1
      IF ( jj>ioy ) THEN
         jj = 0
      ELSE
         ii = 0
         DO i = 2 , nx
            ii = ii + 1
            IF ( ii>iox ) THEN
               ii = 0
            ELSE
               k(1) = lf(i,j,nx1)
               DO l = 3 , 11 , 2
                  k(l) = k(1)
               ENDDO
!
!
               CALL write(file,k,10,0)
            ENDIF
         ENDDO
      ENDIF
   ENDDO
   ASSIGN 4300 TO r1
   GOTO 10600
 4300 IF ( ip+iy0+ix0+iyl+ixw>0 ) GOTO 3900
 4400 ASSIGN 4500 TO r
   GOTO 9800
!
!
!     PARAMA = 4 RECTANGULAR PLATE MADE FROM TRIA1-S
!
!     INPUT, ,,,,/G1,G2,,G4,/C,N,4/C,N,I/C,N,J $
!     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,4/C,N,I/C,N,J $
!            I=1  REGULAR BANDING
!            I=2  DOUBLE BANDING
!            I=3  ACTIVE COLUMN BANDING
!            I=4  REVERSE DOUBLE BANDING
!            J=1  TRIANGLE CONFIGURATION OPTION NO. 1    (LL TO UR)
!            J=2  TRIANGLE CONFIGURATION OPTION NO. 2    (LR TO UL)
!
!
 4500 RETURN
 4600 k(2) = 101
   qk(6) = th
   DO j = 1 , ny
      DO i = 1 , nx
         k(3) = lf(i,j,nx1)
         k(4) = k(3) + 1
         k(1) = 2*k(3) - 1
         IF ( Paramc==2 ) THEN
            k(5) = k(3) + nx1
            CALL write(file,k,6,0)
            k(1) = k(1) + 1
            k(3) = k(5) + 1
            k(4) = k(5)
            k(5) = k(3) - nx1
            CALL write(file,k,6,0)
         ELSE
            k(5) = k(4) + nx1
            CALL write(file,k,6,0)
            k(1) = k(1) + 1
            k(4) = k(3) + nx1
            k(3) = k(5)
            k(5) = k(4) - nx1
            CALL write(file,k,6,0)
         ENDIF
      ENDDO
   ENDDO
   ASSIGN 4700 TO r1
   GOTO 10600
 4700 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 3800 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT (OUT OF SEQUENCE)
!
      ic = 15
      ASSIGN 4800 TO r
      GOTO 9300
   ENDIF
 4800 DO j = 1 , ny
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)*2 - 1
         CALL write(file,k,1,0)
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   DO j = 1 , ny
      DO i = 1 , nx
         k(1) = lf(i,j,nx1)*2
         CALL write(file,k,1,0)
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   GOTO 3700
 4900 IF ( xb==0.0 ) GOTO 5100
!
!     CDAMP4
!
   ic = 7
   ASSIGN 5000 TO r
   GOTO 9300
 5000 qk(2) = xb
   k(4) = 0
   DO i = 2 , n
      k(1) = i + 2000000
      k(3) = i
      CALL write(file,k,4,0)
   ENDDO
   ASSIGN 5100 TO r1
   GOTO 10600
!
!     CELAS4
!
 5100 ic = 1
   ASSIGN 5200 TO r
   GOTO 9300
 5200 qk(2) = xk1
   DO i = 1 , n
      k(1) = i
      k(3) = i
      k(4) = i + 1
      IF ( i==1 ) k(3) = 0
      IF ( i==n ) k(4) = 0
      CALL write(file,k,4,0)
   ENDDO
   IF ( xk2/=0.0 ) THEN
!
      qk(2) = xk2
      k(4) = 0
      DO i = 2 , n
         k(1) = i + 3000000
         k(3) = i
         CALL write(file,k,4,0)
      ENDDO
   ENDIF
   ASSIGN 5300 TO r1
   GOTO 10600
!
 5300 IF ( xm==0.0 ) GOTO 5500
!
!     CMASS4
!
   ic = 2
   ASSIGN 5400 TO r
   GOTO 9300
 5400 qk(2) = xm
   k(4) = 0
   DO i = 2 , n
      k(1) = i + 1000000
      k(3) = i
      CALL write(file,k,4,0)
   ENDDO
   ASSIGN 5500 TO r1
   GOTO 10600
 5500 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 5700 TO r
      GOTO 9800
   ELSEIF ( n<=2 ) THEN
      ASSIGN 5700 TO r
      GOTO 9800
   ELSEIF ( n==3 .AND. xm==0.0 .AND. xb==0.0 .AND. xk2==0.0 ) THEN
      ASSIGN 5700 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT
!
      ic = 15
      ASSIGN 5600 TO r
      GOTO 9300
   ENDIF
 5600 IF ( n/=3 ) THEN
      DO i = 2 , nm1
         k(1) = i
         CALL write(file,k,1,0)
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
   ENDIF
   IF ( xm/=0.0 ) THEN
      DO i = 2 , n
         k(1) = i + 1000000
         CALL write(file,k,1,0)
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
   ENDIF
   IF ( xb/=0.0 ) THEN
      DO i = 2 , n
         k(1) = i + 2000000
         CALL write(file,k,1,0)
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
   ENDIF
   IF ( xk2/=0.0 ) THEN
      DO i = 2 , n
         k(1) = i + 3000000
         CALL write(file,k,1,0)
      ENDDO
      k(1) = -1
      CALL write(file,k,1,0)
   ENDIF
   ASSIGN 5700 TO r1
   GOTO 10300
 5700 RETURN
!
!     GRID
!
 5800 ic = 5
   ASSIGN 5900 TO r
   GOTO 9300
 5900 k(2) = 0
   qk(4) = 0.0
   qk(5) = 0.0
   k(6) = 0
   k(8) = 0
   ii = 0
   DO i = 1 , n1
      ii = ii + 1
      k(1) = i
      qk(3) = xl*float(i-1)/float(n)
      IF ( i==1 .OR. ii>iox ) THEN
         k(7) = ip
         ii = 0
      ELSE
         k(7) = 0
      ENDIF
      CALL write(file,k,8,0)
   ENDDO
   ASSIGN 6000 TO r1
   GOTO 10300
!
!     G2
!
 6000 ifil = 2
   ASSIGN 6100 TO r
   GOTO 9000
!
!     CBAR
!
 6100 ic = 6
   ASSIGN 6200 TO r
   GOTO 9300
 6200 k(2) = 101
   k(8) = iflg
   k(5) = ig0
   k(6) = 0
   k(7) = 0
   IF ( iflg==2 ) GOTO 10900
   READ (Nin,99020) x1 , x2 , x3
99020 FORMAT (3E8.0)
   CALL page2(-1)
   WRITE (Nout,99021) x1 , x2 , x3
99021 FORMAT (21X,1P,3E8.1)
   qk(5) = x1
   qk(6) = x2
   qk(7) = x3
   DO i = 9 , 16
      k(i) = 0
   ENDDO
   DO i = 1 , n
      k(1) = i
      k(3) = i
      k(4) = i + 1
      CALL write(file,k,16,0)
   ENDDO
   IF ( m>0 .AND. m<=n-1 ) THEN
      k(2) = 102
      k(3) = 2
      DO i = 1 , m
         k(1) = n + i
         k(4) = n - i + 2
         CALL write(file,k,16,0)
      ENDDO
   ENDIF
   ASSIGN 6300 TO r1
   GOTO 10600
 6300 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 6500 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT
!
      ic = 15
      ASSIGN 6400 TO r
      GOTO 9300
   ENDIF
 6400 DO i = 1 , n
      k(1) = i
      CALL write(file,k,1,0)
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   ASSIGN 6500 TO r1
   GOTO 10300
 6500 IF ( iox==0 ) RETURN
!
!     G4
!
   ifil = 4
   ASSIGN 6600 TO r
   GOTO 9000
!
!     OMIT
!
 6600 ic = 14
   ASSIGN 6700 TO r
   GOTO 9300
 6700 DO i = 2 , 12 , 2
      k(i) = i/2
   ENDDO
   ii = 0
   DO i = 2 , n
      ii = ii + 1
      IF ( ii>iox ) THEN
         ii = 0
      ELSE
         k(1) = i
         DO l = 3 , 11 , 2
            k(l) = k(1)
         ENDDO
         CALL write(file,k,12,0)
      ENDIF
   ENDDO
   ASSIGN 6800 TO r1
   GOTO 10300
 6800 RETURN
!
!     CELAS4
!
 6900 ic = 1
   ASSIGN 7000 TO r
   GOTO 9300
 7000 qk(2) = 1.0
   ii = 0
   DO i = 1 , n
      IF ( i>1 ) ii = ii + n1 - i
      DO j = i , n
         k(1) = ii + j
         k(3) = i
         k(4) = j
         IF ( i==j ) k(4) = 0
         CALL write(file,k,4,0)
      ENDDO
   ENDDO
   ASSIGN 7100 TO r1
   GOTO 10600
 7100 IF ( Modcom(1)/=0 ) THEN
      ASSIGN 7300 TO r
      GOTO 9800
!
!     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
!
   ELSEIF ( n<3 ) THEN
      ASSIGN 7300 TO r
      GOTO 9800
   ELSE
!
!     CNGRNT
!
      ic = 15
      ASSIGN 7200 TO r
      GOTO 9300
   ENDIF
 7200 ii = 0
   DO i = 1 , n
      IF ( i>1 ) ii = ii + n1 - i
      k(1) = ii + i
      CALL write(file,k,1,0)
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   ii = 0
   DO i = 1 , n
      IF ( i>1 ) ii = ii + n1 - i
      DO j = i , n
         IF ( j/=i ) THEN
            k(1) = ii + j
            CALL write(file,k,1,0)
         ENDIF
      ENDDO
   ENDDO
   k(1) = -1
   CALL write(file,k,1,0)
   ASSIGN 7300 TO r1
   GOTO 10300
 7300 IF ( nsload==0 ) GOTO 7600
!
!     G3
!
   ifil = 3
   ASSIGN 7400 TO r
   GOTO 9000
!
!     SLOAD
!
 7400 ic = 11
   ASSIGN 7500 TO r
   GOTO 9300
 7500 k(1) = n
   qk(3) = 1.0
   DO i = 1 , n
      k(2) = i
      CALL write(file,k,3,0)
   ENDDO
   ASSIGN 7600 TO r1
   GOTO 10300
!
!     G5
!
 7600 ifil = 5
   ASSIGN 7700 TO r
   GOTO 9000
!
!     SPOINT
!
 7700 ic = 12
   ASSIGN 7800 TO r
   GOTO 9300
 7800 DO i = 1 , n
      CALL write(file,i,1,0)
   ENDDO
   ASSIGN 7900 TO r1
   GOTO 10300
 7900 ne = n*n1/2
   CALL page2(-2)
   WRITE (Nout,99022) n , ne
99022 FORMAT ('0*INPUT* FULL MATRIX OF ORDER',I9,'   GENERATED WITH',I9,'  ELEMENTS')
   IF ( nsload/=0 ) THEN
      CALL page2(-2)
      WRITE (Nout,99023) n
99023 FORMAT ('0*INPUT* LOAD SET',I9,'  GENERATED')
   ENDIF
   RETURN
!
!     LOCATE AND COPY CORD2C CARD FROM THE FIRST INPUT FILE
!
 8000 ibuf = (ifil+4)*Nbuf + 1
   CALL preloc(*11000,X(ibuf),filin(ifil))
   CALL locate(*11100,X(ibuf),cord2c,qk(3))
   CALL read(*11000,*8100,filin(ifil),qk(4),13,0,iflag)
   CALL close(filin(ifil),1)
   inopen(ifil) = .FALSE.
 8100 ic = 17
   ASSIGN 8200 TO r
   GOTO 9300
 8200 CALL write(file,qk(4),13,0)
   ASSIGN 8300 TO r1
   GOTO 10600
!
!     GRID
!
 8300 ic = 5
   ASSIGN 8400 TO r
   GOTO 9300
 8400 k(2) = 2
!     QK(2) = QK(5)    THIS WILL ASSIGN REFERENCE NUMBER ON CORD2C CARD
!                      TO THE GRID POINTS
!
   IF ( n<=0 .OR. n>=256 ) THEN
      CALL page2(-2)
      WRITE (Nout,99024) Uwm
99024 FORMAT (A25,' 2369, WHEEL MUST HAVE FEWER THAN 256 SPOKES. ','INPUT MODULE RESETTING TO 255')
      n = 255
   ENDIF
   n1 = n + 1
   qk(3) = xl
   qk(5) = 0.0
   k(6) = 2
   k(8) = 0
   k(7) = ip
   DO i = 1 , n
      k(1) = i
      qk(4) = 360.0/float(n)*float(i-1)
      CALL write(file,k,8,0)
   ENDDO
   k(1) = n1
   k(2) = 0
   qk(3) = 0.0
   qk(4) = 0.0
   k(6) = 0
   IF ( icen/=0 ) k(7) = icen
   CALL write(file,k,8,0)
   ASSIGN 8500 TO r1
   GOTO 10300
!
!     G2
!
 8500 ifil = 2
   ASSIGN 8600 TO r
   GOTO 9000
!
!     CBAR
!
 8600 ic = 6
   ASSIGN 8700 TO r
   GOTO 9300
 8700 k(2) = 101
   k(8) = iflg
   k(5) = ig0
   k(6) = 0
   k(7) = 0
   IF ( iflg==2 ) GOTO 10900
   READ (Nin,99025) x1 , x2 , x3
99025 FORMAT (3E8.0)
   CALL page2(-1)
   WRITE (Nout,99026) x1 , x2 , x3
99026 FORMAT (21X,1P,3E8.1)
   qk(5) = x1
   qk(6) = x2
   qk(7) = x3
   DO i = 9 , 16
      k(i) = 0
   ENDDO
   DO i = 1 , n
      k(1) = i
      k(3) = i
      k(4) = i + 1
      IF ( k(4)==n1 ) k(4) = 1
      CALL write(file,k,16,0)
   ENDDO
   k(4) = n1
   DO i = 1 , n
      k(1) = n + i
      k(3) = i
      CALL write(file,k,16,0)
   ENDDO
   ASSIGN 8800 TO r1
   GOTO 10600
 8800 ASSIGN 8900 TO r
   GOTO 9800
 8900 RETURN
!
!
!     UTILITY I/O ROUTINES
!
!
 9000 file = filout(ifil)
   ibuf = (ifil-1)*Nbuf + 1
   CALL gopen(file,X(ibuf),1)
   t(1) = file
   DO j = 2 , 7
      t(j) = 0
   ENDDO
   fil = filin(ifil)
   ibuf = (ifil+4)*Nbuf + 1
   IF ( Parama==8 ) GOTO 9200
   CALL open(*9100,fil,X(ibuf),0)
   inopen(ifil) = .TRUE.
   t(1) = fil
   CALL rdtrl(t)
   t(1) = file
   CALL skprec(fil,1)
   CALL fread(fil,hfil(1,ifil),3,0)
   DO j = 1 , 3
      IF ( hfil(j,ifil)/=eee(j) ) GOTO 9200
   ENDDO
   WRITE (Nout,99027) Sfm
99027 FORMAT (A25,' 1742, NO DATA PRESENT')
   GOTO 11200
 9100 inopen(ifil) = .FALSE.
 9200 GOTO r
!
!
 9300 IF ( inopen(ifil) ) THEN
      knoic = kno(ic)
      ksrt = kdsort(knoic)
      GOTO 9500
   ENDIF
 9400 CALL write(file,kl(ic),1,0)
   CALL write(file,16*(i1t(ic)-2)+j1t(ic),1,0)
   CALL write(file,kno(ic),1,0)
   GOTO r
 9500 knox = hfil(3,ifil)
   ksrtx = kdsort(knox)
   IF ( ksrt<ksrtx ) GOTO 9400
   IF ( ksrt==ksrtx ) THEN
      WRITE (Nout,99028) Ufm , kdn(1,ic) , kdn(2,ic)
99028 FORMAT (A23,' 1744, DATA CARD(S) -',2A4,'- GENERATED BY UTILITY',' MODULE INPUT NOT ALLOWED TO APPEAR IN BULK DATA')
      GOTO 11200
   ELSE
      CALL write(file,hfil(1,ifil),3,0)
      DO
         CALL read(*10800,*9600,fil,X(kor),nkor,0,rdflg)
         CALL write(file,X(kor),nkor,0)
      ENDDO
   ENDIF
 9600 CALL write(file,X(kor),rdflg,1)
   CALL fread(fil,hfil(1,ifil),3,0)
   DO j = 1 , 3
      IF ( hfil(j,ifil)/=eee(j) ) GOTO 9500
   ENDDO
   inopen(ifil) = .FALSE.
   CALL close(fil,1)
   GOTO 9400
!
 9700 ktt = kt(ic)
   i1tt = i1t(ic)
   j1tt = j1t(ic) + 16
   t(i1tt) = orf(t(i1tt),Two(j1tt))
   IF ( ktt/=1 ) THEN
      i2tt = i2t(ic)
      j2tt = j2t(ic) + 16
      t(i2tt) = orf(t(i2tt),Two(j2tt))
   ENDIF
   GOTO r
!
!
 9800 IF ( inopen(ifil) ) THEN
      CALL write(file,hfil(1,ifil),3,0)
      GOTO 10100
   ELSE
      CALL write(file,eee,3,1)
      GOTO 10000
   ENDIF
 9900 CALL close(fil,1)
   inopen(ifil) = .FALSE.
10000 CALL wrttrl(t)
   CALL close(file,1)
   GOTO r
10100 DO
      CALL read(*9900,*10200,fil,X(kor),nkor,0,rdflg)
      CALL write(file,X(kor),nkor,0)
   ENDDO
10200 CALL write(file,X(kor),rdflg,1)
   GOTO 10100
!
10300 CALL write(file,0,0,1)
   ASSIGN 10400 TO r
   GOTO 9700
10400 ASSIGN 10500 TO r
   GOTO 9800
10500 GOTO r1
!
10600 CALL write(file,0,0,1)
   ASSIGN 10700 TO r
   GOTO 9700
10700 GOTO r1
10800 DO
      m = -2
!
!     DIAGNOSTIC PROCESSING
!
      CALL mesage(m,file,mnam)
   ENDDO
10900 WRITE (Nout,99029) Ufm
99029 FORMAT (A23,' 1745, UTILITY MODULE CANNOT HANDLE THE IFLG=2 CASE',' SINCE THERE IS NO WAY TO GENERATE GRID POINT G0')
   GOTO 11200
11000 m = -1
   CALL mesage(m,file,mnam)
   GOTO 10800
11100 WRITE (Nout,99030) Ufm
99030 FORMAT (A23,' 1746, COORDINATE SYSTEM NOT DEFINED ON A CORD2C',' CARD')
!
11200 m = -61
   CALL page2(-2)
   CALL mesage(m,file,mnam)
   GOTO 10800
99031 FORMAT (21X,2I8,1P,2E8.1,I8,1P,2E8.1)
99032 FORMAT (21X,I8,1P,2E8.1,0P,F8.5)
!
END SUBROUTINE input