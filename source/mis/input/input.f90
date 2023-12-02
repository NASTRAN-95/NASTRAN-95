!*==input.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE input
   USE c_blank
   USE c_condas
   USE c_machin
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(8) :: chr
   REAL :: cl , ddy , dx , dy , f , lambda , sl , th , u , x1 , x2 , x3 , xb , xk1 , xk2 , xl , xm , xo , zk , zm
   INTEGER , DIMENSION(2) , SAVE :: cord2c , mnam
   CHARACTER(8) , SAVE :: e1 , e2 , e3
   INTEGER , DIMENSION(3) , SAVE :: eee
   INTEGER :: fil , file , i , i1tt , i2tt , ibuf , ic , icen , ifil , iflag , iflg , ig0 , ii , ij , il , ill , ilmk , iox , ioy , &
            & ip , iw , iwmk , iww , ix0 , ixw , iy0 , iyl , j , j1tt , j2tt , jj , kk , kkk , knoic , knox , kor , ksrt , ksrtx ,  &
            & ktt , l , m , n , n1 , nbuf , ne , nin , nkor , nlines , nm1 , nn , nout , nsload , nx , nx1 , ny , ny1 , r , r1 ,    &
            & r2 , rdflg , sperlk
   INTEGER , DIMENSION(5) , SAVE :: filin , filout
   INTEGER , DIMENSION(3,5) :: hfil
   INTEGER , DIMENSION(20) , SAVE :: i1t , i2t , j1t , j2t , kl , kno , kt
   LOGICAL , DIMENSION(5) , SAVE :: inopen
   INTEGER , DIMENSION(1) :: ix
   INTEGER , DIMENSION(100) :: k
   INTEGER , DIMENSION(100) , SAVE :: k1 , k2
   INTEGER , DIMENSION(70) , SAVE :: k3
   INTEGER , DIMENSION(2,20) , SAVE :: kdn
   INTEGER , DIMENSION(270) :: kdsort
   INTEGER :: lf
   INTEGER , DIMENSION(9) :: modcom
   INTEGER , SAVE :: param1 , paramn
   REAL , DIMENSION(100) :: qk
   INTEGER , DIMENSION(7) :: t
   EXTERNAL close , fread , gopen , iunion , korsz , locate , mesage , open , orf , page1 , page2 , preloc , rdtrl , read , skprec ,&
          & write , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( param1<=parama .AND. parama<=paramn ) THEN
!
            kor = 10*nbuf + 1
            nkor = korsz(x) - 10*nbuf
            IF ( nkor<=0 ) CALL mesage(-8,nkor,mnam)
            CALL page1
            nlines = nlines + 8
            WRITE (nout,99001)
99001       FORMAT (//20X,'* U T I L I T Y   M O D U L E   I N P U T *',///,20X,                                                    &
                   &'INPUT DATA ECHO (DATA READ VIA FORTRAN, REMEMBER TO ','RIGHT ADJUST)',///20X,'*   1  **   2  **   3  **   4  ',&
                   &'**   5  **   6  **   7  **   8  **   9  **  10  *',///)
            iox = 0
            ioy = 0
            IF ( mach<5 .OR. sperlk/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO
!
!     ON VAX-11/780 OR UNIX MACHINES, SEARCH FOR END OF BULK DATA DECK.
!
               READ (nin,99002,END=20) chr
99002          FORMAT (A8)
               IF ( chr==e1 .OR. chr==e2 .OR. chr==e3 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ELSE
            WRITE (nout,99003) ufm , parama
99003       FORMAT (A23,' 1738, UTILITY MODULE INPUT FIRST PARAMETER VALUE - ',I20,' OUT OF RANGE')
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ENDDATA CARD NOT FOUND
!
 20      WRITE (nout,99004) ufm
99004    FORMAT (A23,' - "ENDDATA" CARD NOT FOUND BY INPUT MODULE')
         CALL mesage(-37,0,mnam)
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( parama==2 ) THEN
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
            READ (nin,99005) nx , ny , dx , dy , ip , lambda
99005       FORMAT (2I8,2E8.0,I8,E8.0)
            CALL page2(-1)
            WRITE (nout,99031) nx , ny , dx , dy , ip , lambda
            nx1 = nx + 1
            ny1 = ny + 1
            ASSIGN 380 TO r2
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( parama==3 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( parama==4 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( parama==5 ) THEN
!
!
!     PARAMA = 5 N-SEGMENT STRING
!
!     INPUT, ,,,,/,G2,,,/C,N,5 $
!     INPUT, ,GEOM2,,,/,G2,,,/C,N,5 $
!
!
            READ (nin,99006) n , xk1 , xk2 , xm , xb
99006       FORMAT (I8,4E8.0)
            CALL page2(-1)
            WRITE (nout,99007) n , xk1 , xk2 , xm , xb
99007       FORMAT (21X,I8,1P,4E8.1)
            n1 = n + 1
            nm1 = n - 1
!
!     G2
!
            ifil = 2
            ASSIGN 860 TO r
         ELSEIF ( parama==6 ) THEN
!
!
!     PARAMA = 6 N-CELL BAR
!
!     INPUT, ,,,,/G1,G2,,G4,/C,N,6 $
!     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,6 $
!
!
            READ (nin,99008) n , xl , ip , iflg , ig0 , m , iox
99008       FORMAT (I8,E8.0,5I8)
            CALL page2(-1)
            WRITE (nout,99009) n , xl , ip , iflg , ig0 , m , iox
99009       FORMAT (21X,I8,1P,E8.1,5I8)
            n1 = n + 1
!
!     G1
!
            ifil = 1
            ASSIGN 1040 TO r
         ELSEIF ( parama==7 ) THEN
!
!
!     PARAMA = 7 FULL MATRIX AND OPTIONAL UNIT LOAD
!
!     INPUT, ,,,,/,G2,G3,,G5/C,N,7 $
!     INPUT, ,GEOM2,GEOM3,,/,G2,G3,,G5/C,N,7 $
!
!
            READ (nin,99010) n , nsload
99010       FORMAT (2I8)
            CALL page2(-1)
            WRITE (nout,99011) n , nsload
99011       FORMAT (21X,2I8)
            n1 = n + 1
!
!     G2
!
            ifil = 2
            ASSIGN 1260 TO r
         ELSEIF ( parama==8 ) THEN
!
!
!     PARAMA = 8 N-SPOKE WHEEL
!
!     INPUT, ,,,,/G1,G2,,,/C,N,8 $
!     INPUT GEOM1,GEOM2,,,/G1,G2,,,/C,N,8 $
!
!
            READ (nin,99012) n , xl , ip , iflg , ig0 , icen
99012       FORMAT (I8,E8.0,4I8)
            CALL page2(-1)
            WRITE (nout,99013) n , xl , ip , iflg , ig0 , icen
99013       FORMAT (21X,I8,1P,E8.1,4I8)
            n1 = n + 1
!
!     G1
!
            ifil = 1
            ASSIGN 1480 TO r
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
            IF ( paramb==2 ) THEN
!
!
               READ (nin,99014) n , zk , zm
99014          FORMAT (I8,2E8.0)
               CALL page2(-1)
               WRITE (nout,99032) n , zk , zm
            ELSEIF ( paramb==3 ) THEN
!
               READ (nin,99015) n , zk , zm , f
99015          FORMAT (I8,3E8.0)
               CALL page2(-1)
               WRITE (nout,99032) n , zk , zm , f
            ELSE
!
               READ (nin,99016) n , zk , u
99016          FORMAT (I8,2E8.0)
               CALL page2(-1)
               WRITE (nout,99032) n , zk , u
!
               ASSIGN 40 TO r2
               GOTO 30
            ENDIF
!
            ASSIGN 260 TO r2
!
 30         n1 = n + 1
            nm1 = n - 1
!
!     G2
!
            ifil = 2
            ASSIGN 120 TO r
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     G4
!
 40      ifil = 4
         ASSIGN 60 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     SPC
!
 60      ic = 3
         ASSIGN 80 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 80      k(1) = 1000 + n
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
         ASSIGN 100 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 100     RETURN
!
!     CELAS4
!
 120     ic = 1
         ASSIGN 140 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 140     qk(2) = zk
         DO j = 2 , n
            DO i = 1 , n
               k(1) = lf(i,j,n1)
               k(3) = k(1)
               k(4) = k(3) + 1
               IF ( paramb/=1 .AND. i==1 ) k(3) = 0
               IF ( paramb/=1 .AND. i==n ) k(4) = 0
               CALL write(file,k,4,0)
            ENDDO
         ENDDO
         DO j = 1 , n
            DO i = 2 , n
               k(3) = lf(i,j,n1)
               k(4) = k(3) + n1
               k(1) = k(3) + 1000000
               IF ( paramb/=1 .AND. j==1 ) k(3) = 0
               IF ( paramb/=1 .AND. j==n ) k(4) = 0
               CALL write(file,k,4,0)
            ENDDO
         ENDDO
         ASSIGN 160 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 160     IF ( paramb==1 ) GOTO 200
!
!     CMASS4
!
         ic = 2
         ASSIGN 180 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 180     qk(2) = zm
         k(4) = 0
         DO j = 2 , n
            DO i = 2 , n
               k(3) = lf(i,j,n1)
               k(1) = k(3) + 2000000
               CALL write(file,k,4,0)
            ENDDO
         ENDDO
         IF ( paramb==3 ) THEN
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
         ASSIGN 200 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 200     IF ( modcom(1)/=0 ) THEN
            ASSIGN 240 TO r
!
!     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
!
         ELSEIF ( n<3 ) THEN
            ASSIGN 240 TO r
         ELSE
!
!     CNGRNT
!
            ic = 15
            ASSIGN 220 TO r
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 220     DO j = 2 , n
            DO i = 1 , n
               IF ( .NOT.(paramb/=1 .AND. (i==1 .OR. i==n)) ) THEN
                  k(1) = lf(i,j,n1)
                  CALL write(file,k,1,0)
               ENDIF
            ENDDO
         ENDDO
         DO j = 1 , n
            IF ( .NOT.(paramb/=1 .AND. (j==1 .OR. j==n)) ) THEN
               DO i = 2 , n
                  k(1) = lf(i,j,n1) + 1000000
                  CALL write(file,k,1,0)
               ENDDO
            ENDIF
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         IF ( paramb/=1 ) THEN
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
         IF ( paramb/=1 ) THEN
            DO j = 2 , n
               DO i = 2 , n
                  k(1) = lf(i,j,n1) + 2000000
                  CALL write(file,k,1,0)
               ENDDO
            ENDDO
            k(1) = -1
            CALL write(file,k,1,0)
            IF ( paramb/=2 ) THEN
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
         ASSIGN 240 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 240     GOTO r2
 260     RETURN
      CASE (3)
!
!     G1
!
         ifil = 1
         ASSIGN 280 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     GRID
!
 280     ic = 5
         ASSIGN 300 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 300     qk(5) = 0.0
         k(2) = 0
         k(6) = 0
         k(8) = 0
         sl = sin(degra*lambda)
         cl = cos(degra*lambda)
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
         ASSIGN 320 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
!
 320     IF ( paramb==1 ) GOTO 360
         ic = 8
         ASSIGN 340 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 340     IF ( paramb/=1 ) THEN
            IF ( paramb==3 ) THEN
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
            ELSEIF ( paramb==4 ) THEN
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
            ASSIGN 360 TO r1
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 360     GOTO r2
 380     ASSIGN 400 TO r
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
!
!     G2
!
 400     ifil = 2
         ASSIGN 420 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 420     IF ( paramc/=0 ) THEN
!
!     CROD
!
            ic = 13
            ASSIGN 520 TO r
         ELSE
!
!     CBAR
!
            ic = 6
            ASSIGN 440 TO r
         ENDIF
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 440     k(2) = 101
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
         spag_nextblock_1 = 4
      CASE (4)
         ASSIGN 460 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 460     IF ( modcom(1)/=0 ) THEN
            ASSIGN 500 TO r
            spag_nextblock_1 = 14
         ELSE
!
!     CNGRNT    (OUT OF SEQUENCE FOR CROD CASES)
!
            ic = 15
            ASSIGN 480 TO r
            spag_nextblock_1 = 10
         ENDIF
         CYCLE
 480     DO j = 1 , ny1
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
         IF ( paramc/=0 ) THEN
            DO j = 1 , ny
               SPAG_Loop_2_1: DO i = 1 , nx
                  k(1) = lf(i,j,nx1)*2 + 1999999
                  CALL write(file,k,1,0)
                  IF ( paramc==3 .AND. j>1 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
            ENDDO
            k(1) = -1
            CALL write(file,k,1,0)
            IF ( paramc==1 ) THEN
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
         ASSIGN 500 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 500     RETURN
 520     k(2) = 101
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
            SPAG_Loop_2_2: DO i = 1 , nx
               k(3) = lf(i,j,nx1) + 1
               k(4) = k(3) + nx
               k(1) = 2*k(3) + 1999997
               CALL write(file,k,4,0)
               IF ( paramc==3 .AND. j>1 ) EXIT SPAG_Loop_2_2
               IF ( paramc==1 ) THEN
                  k(1) = k(1) + 1
                  k(3) = k(3) - 1
                  k(4) = k(4) + 1
                  CALL write(file,k,4,0)
               ENDIF
            ENDDO SPAG_Loop_2_2
         ENDDO
         spag_nextblock_1 = 4
      CASE (5)
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
         READ (nin,99017) nx , ny , dx , dy , ip , lambda , th
99017    FORMAT (2I8,2E8.0,I8,2E8.0)
         CALL page2(-2)
         WRITE (nout,99031) nx , ny , dx , dy , ip , lambda , th
         READ (nin,99018) iy0 , ix0 , iyl , ixw , iox , ioy
99018    FORMAT (6I8)
         WRITE (nout,99019) iy0 , ix0 , iyl , ixw , iox , ioy
99019    FORMAT (21X,6I8)
         nx1 = nx + 1
         ny1 = ny + 1
!
!     GRID
!
         ASSIGN 540 TO r2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 540     ASSIGN 560 TO r
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
!
!     G2
!
 560     ifil = 2
         ASSIGN 580 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     CQUAD1
!
 580     IF ( parama==4 ) THEN
!
!     CTRIA1
!
            ic = 10
            ASSIGN 800 TO r
         ELSE
            ic = 9
            ASSIGN 600 TO r
         ENDIF
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 600     k(2) = 101
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
         ASSIGN 620 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 620     IF ( modcom(1)/=0 ) THEN
            ASSIGN 660 TO r
            spag_nextblock_1 = 14
         ELSE
!
!     CNGRNT (OUT OF SEQUENCE)
!
            ic = 15
            ASSIGN 640 TO r
            spag_nextblock_1 = 10
         ENDIF
         CYCLE
 640     DO j = 1 , ny
            DO i = 1 , nx
               k(1) = lf(i,j,nx1)
               CALL write(file,k,1,0)
            ENDDO
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         spag_nextblock_1 = 6
      CASE (6)
         ASSIGN 660 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
!
!     SPC-S AND OMIT-S
!
 660     IF ( ip+iy0+ix0+iyl+ixw+iox+ioy==0 ) GOTO 780
!
!     G4
!
         ifil = 4
         ASSIGN 700 TO r
         spag_nextblock_1 = 8
      CASE (7)
!
!     SPC
!
         ic = 3
         ASSIGN 680 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 680     k(1) = 1000*nx + ny
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
         ASSIGN 760 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 700     IF ( iox+ioy==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OMIT
!
         ic = 14
         ASSIGN 720 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 720     DO i = 2 , 12 , 2
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
         ASSIGN 740 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 740     IF ( ip+iy0+ix0+iyl+ixw>0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 760     ASSIGN 780 TO r
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
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
 780     RETURN
 800     k(2) = 101
         qk(6) = th
         DO j = 1 , ny
            DO i = 1 , nx
               k(3) = lf(i,j,nx1)
               k(4) = k(3) + 1
               k(1) = 2*k(3) - 1
               IF ( paramc==2 ) THEN
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
         ASSIGN 820 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 820     IF ( modcom(1)/=0 ) THEN
            ASSIGN 660 TO r
            spag_nextblock_1 = 14
         ELSE
!
!     CNGRNT (OUT OF SEQUENCE)
!
            ic = 15
            ASSIGN 840 TO r
            spag_nextblock_1 = 10
         ENDIF
         CYCLE
 840     DO j = 1 , ny
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
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 860     IF ( xb==0.0 ) GOTO 900
!
!     CDAMP4
!
         ic = 7
         ASSIGN 880 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 880     qk(2) = xb
         k(4) = 0
         DO i = 2 , n
            k(1) = i + 2000000
            k(3) = i
            CALL write(file,k,4,0)
         ENDDO
         ASSIGN 900 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
!
!     CELAS4
!
 900     ic = 1
         ASSIGN 920 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 920     qk(2) = xk1
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
         ASSIGN 940 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
!
 940     IF ( xm==0.0 ) GOTO 980
!
!     CMASS4
!
         ic = 2
         ASSIGN 960 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 960     qk(2) = xm
         k(4) = 0
         DO i = 2 , n
            k(1) = i + 1000000
            k(3) = i
            CALL write(file,k,4,0)
         ENDDO
         ASSIGN 980 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 980     IF ( modcom(1)/=0 ) THEN
            ASSIGN 1020 TO r
         ELSEIF ( n<=2 ) THEN
            ASSIGN 1020 TO r
         ELSEIF ( n==3 .AND. xm==0.0 .AND. xb==0.0 .AND. xk2==0.0 ) THEN
            ASSIGN 1020 TO r
         ELSE
!
!     CNGRNT
!
            ic = 15
            ASSIGN 1000 TO r
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 1000    IF ( n/=3 ) THEN
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
         ASSIGN 1020 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 1020    RETURN
!
!     GRID
!
 1040    ic = 5
         ASSIGN 1060 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1060    k(2) = 0
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
         ASSIGN 1080 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
!
!     G2
!
 1080    ifil = 2
         ASSIGN 1100 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     CBAR
!
 1100    ic = 6
         ASSIGN 1120 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1120    k(2) = 101
         k(8) = iflg
         k(5) = ig0
         k(6) = 0
         k(7) = 0
         IF ( iflg==2 ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         READ (nin,99020) x1 , x2 , x3
99020    FORMAT (3E8.0)
         CALL page2(-1)
         WRITE (nout,99021) x1 , x2 , x3
99021    FORMAT (21X,1P,3E8.1)
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
         ASSIGN 1140 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 1140    IF ( modcom(1)/=0 ) THEN
            ASSIGN 1180 TO r
            spag_nextblock_1 = 14
         ELSE
!
!     CNGRNT
!
            ic = 15
            ASSIGN 1160 TO r
            spag_nextblock_1 = 10
         ENDIF
         CYCLE
 1160    DO i = 1 , n
            k(1) = i
            CALL write(file,k,1,0)
         ENDDO
         k(1) = -1
         CALL write(file,k,1,0)
         ASSIGN 1180 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 1180    IF ( iox==0 ) RETURN
!
!     G4
!
         ifil = 4
         ASSIGN 1200 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     OMIT
!
 1200    ic = 14
         ASSIGN 1220 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1220    DO i = 2 , 12 , 2
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
         ASSIGN 1240 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 1240    RETURN
!
!     CELAS4
!
 1260    ic = 1
         ASSIGN 1280 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1280    qk(2) = 1.0
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
         ASSIGN 1300 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 1300    IF ( modcom(1)/=0 ) THEN
            ASSIGN 1340 TO r
!
!     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
!
         ELSEIF ( n<3 ) THEN
            ASSIGN 1340 TO r
         ELSE
!
!     CNGRNT
!
            ic = 15
            ASSIGN 1320 TO r
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 1320    ii = 0
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
         ASSIGN 1340 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 1340    IF ( nsload==0 ) GOTO 1400
!
!     G3
!
         ifil = 3
         ASSIGN 1360 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     SLOAD
!
 1360    ic = 11
         ASSIGN 1380 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1380    k(1) = n
         qk(3) = 1.0
         DO i = 1 , n
            k(2) = i
            CALL write(file,k,3,0)
         ENDDO
         ASSIGN 1400 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
!
!     G5
!
 1400    ifil = 5
         ASSIGN 1420 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     SPOINT
!
 1420    ic = 12
         ASSIGN 1440 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1440    DO i = 1 , n
            CALL write(file,i,1,0)
         ENDDO
         ASSIGN 1460 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
 1460    ne = n*n1/2
         CALL page2(-2)
         WRITE (nout,99022) n , ne
99022    FORMAT ('0*INPUT* FULL MATRIX OF ORDER',I9,'   GENERATED WITH',I9,'  ELEMENTS')
         IF ( nsload/=0 ) THEN
            CALL page2(-2)
            WRITE (nout,99023) n
99023       FORMAT ('0*INPUT* LOAD SET',I9,'  GENERATED')
         ENDIF
         RETURN
!
!     LOCATE AND COPY CORD2C CARD FROM THE FIRST INPUT FILE
!
 1480    ibuf = (ifil+4)*nbuf + 1
         CALL preloc(*1840,x(ibuf),filin(ifil))
         CALL locate(*1860,x(ibuf),cord2c,qk(3))
         CALL read(*1840,*1500,filin(ifil),qk(4),13,0,iflag)
         CALL close(filin(ifil),1)
         inopen(ifil) = .FALSE.
 1500    ic = 17
         ASSIGN 1520 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1520    CALL write(file,qk(4),13,0)
         ASSIGN 1540 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
!
!     GRID
!
 1540    ic = 5
         ASSIGN 1560 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1560    k(2) = 2
!     QK(2) = QK(5)    THIS WILL ASSIGN REFERENCE NUMBER ON CORD2C CARD
!                      TO THE GRID POINTS
!
         IF ( n<=0 .OR. n>=256 ) THEN
            CALL page2(-2)
            WRITE (nout,99024) uwm
99024       FORMAT (A25,' 2369, WHEEL MUST HAVE FEWER THAN 256 SPOKES. ','INPUT MODULE RESETTING TO 255')
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
         ASSIGN 1580 TO r1
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
!
!     G2
!
 1580    ifil = 2
         ASSIGN 1600 TO r
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     CBAR
!
 1600    ic = 6
         ASSIGN 1620 TO r
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 1620    k(2) = 101
         k(8) = iflg
         k(5) = ig0
         k(6) = 0
         k(7) = 0
         IF ( iflg==2 ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         READ (nin,99025) x1 , x2 , x3
99025    FORMAT (3E8.0)
         CALL page2(-1)
         WRITE (nout,99026) x1 , x2 , x3
99026    FORMAT (21X,1P,3E8.1)
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
         ASSIGN 1640 TO r1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
 1640    ASSIGN 1660 TO r
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 1660    RETURN
      CASE (8)
!
!
!     UTILITY I/O ROUTINES
!
!
         file = filout(ifil)
         ibuf = (ifil-1)*nbuf + 1
         CALL gopen(file,x(ibuf),1)
         t(1) = file
         DO j = 2 , 7
            t(j) = 0
         ENDDO
         fil = filin(ifil)
         ibuf = (ifil+4)*nbuf + 1
         IF ( parama==8 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*1680,fil,x(ibuf),0)
         inopen(ifil) = .TRUE.
         t(1) = fil
         CALL rdtrl(t)
         t(1) = file
         CALL skprec(fil,1)
         CALL fread(fil,hfil(1,ifil),3,0)
         DO j = 1 , 3
            IF ( hfil(j,ifil)/=eee(j) ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (nout,99027) sfm
99027    FORMAT (A25,' 1742, NO DATA PRESENT')
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 1680    inopen(ifil) = .FALSE.
         spag_nextblock_1 = 9
      CASE (9)
         GOTO r
      CASE (10)
!
!
         IF ( inopen(ifil) ) THEN
            knoic = kno(ic)
            ksrt = kdsort(knoic)
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         CALL write(file,kl(ic),1,0)
         CALL write(file,16*(i1t(ic)-2)+j1t(ic),1,0)
         CALL write(file,kno(ic),1,0)
         GOTO r
      CASE (12)
         knox = hfil(3,ifil)
         ksrtx = kdsort(knox)
         IF ( ksrt<ksrtx ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ksrt==ksrtx ) THEN
            WRITE (nout,99028) ufm , kdn(1,ic) , kdn(2,ic)
99028       FORMAT (A23,' 1744, DATA CARD(S) -',2A4,'- GENERATED BY UTILITY',' MODULE INPUT NOT ALLOWED TO APPEAR IN BULK DATA')
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL write(file,hfil(1,ifil),3,0)
            DO
               CALL read(*1820,*1700,fil,x(kor),nkor,0,rdflg)
               CALL write(file,x(kor),nkor,0)
            ENDDO
         ENDIF
 1700    CALL write(file,x(kor),rdflg,1)
         CALL fread(fil,hfil(1,ifil),3,0)
         DO j = 1 , 3
            IF ( hfil(j,ifil)/=eee(j) ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         inopen(ifil) = .FALSE.
         CALL close(fil,1)
         spag_nextblock_1 = 11
      CASE (13)
!
         ktt = kt(ic)
         i1tt = i1t(ic)
         j1tt = j1t(ic) + 16
         t(i1tt) = orf(t(i1tt),two(j1tt))
         IF ( ktt/=1 ) THEN
            i2tt = i2t(ic)
            j2tt = j2t(ic) + 16
            t(i2tt) = orf(t(i2tt),two(j2tt))
         ENDIF
         GOTO r
      CASE (14)
!
!
         IF ( inopen(ifil) ) THEN
            CALL write(file,hfil(1,ifil),3,0)
            spag_nextblock_1 = 16
         ELSE
            CALL write(file,eee,3,1)
            spag_nextblock_1 = 15
         ENDIF
         CYCLE
 1720    CALL close(fil,1)
         inopen(ifil) = .FALSE.
         spag_nextblock_1 = 15
      CASE (15)
         CALL wrttrl(t)
         CALL close(file,1)
         GOTO r
      CASE (16)
         DO
            CALL read(*1720,*1740,fil,x(kor),nkor,0,rdflg)
            CALL write(file,x(kor),nkor,0)
         ENDDO
 1740    CALL write(file,x(kor),rdflg,1)
         spag_nextblock_1 = 16
      CASE (17)
!
         CALL write(file,0,0,1)
         ASSIGN 1760 TO r
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 1760    ASSIGN 1780 TO r
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
 1780    GOTO r1
      CASE (18)
!
         CALL write(file,0,0,1)
         ASSIGN 1800 TO r
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 1800    GOTO r1
 1820    DO
            m = -2
!
!     DIAGNOSTIC PROCESSING
!
            CALL mesage(m,file,mnam)
         ENDDO
         spag_nextblock_1 = 19
      CASE (19)
         WRITE (nout,99029) ufm
99029    FORMAT (A23,' 1745, UTILITY MODULE CANNOT HANDLE THE IFLG=2 CASE',' SINCE THERE IS NO WAY TO GENERATE GRID POINT G0')
         spag_nextblock_1 = 20
         CYCLE SPAG_DispatchLoop_1
 1840    m = -1
         CALL mesage(m,file,mnam)
         GOTO 1820
 1860    WRITE (nout,99030) ufm
99030    FORMAT (A23,' 1746, COORDINATE SYSTEM NOT DEFINED ON A CORD2C',' CARD')
         spag_nextblock_1 = 20
      CASE (20)
!
         m = -61
         CALL page2(-2)
         CALL mesage(m,file,mnam)
         GOTO 1820
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99031 FORMAT (21X,2I8,1P,2E8.1,I8,1P,2E8.1)
99032 FORMAT (21X,I8,1P,2E8.1,0P,F8.5)
!
END SUBROUTINE input
