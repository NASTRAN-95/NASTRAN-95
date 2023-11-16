
SUBROUTINE gpstgs
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL D(18) , Tolel
   INTEGER Gpst , Ibuf2 , Igpst , Isys(69) , Iz(1) , Npvt , Nsing
   COMMON /gpstgx/ Gpst , Igpst , Npvt , Nsing , Ibuf2
   COMMON /gpstgy/ D
   COMMON /system/ Isys , Tolel
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   REAL b(9) , const , det , dtol , fl(3) , fm , fr , m(3) , r(3) , temp
   INTEGER i , i1 , iarray(8) , iback , igoto , ii , inc1 , inc2 , inc3 , iorder , ip , ipoint , isave , isubnm(2) , itemp , j ,    &
         & j1 , jj , k1 , k2 , kk , kount , ll , nwds , ttlwds
!
! End of declarations
!
!
!     THIS SUBROUTINE GENERATES THE GRID POINT SINGULARITY TABLE
!     BY EXAMINING THE TRANSLATIONAL AND ROTATIONAL 3 X 3
!     SUBMATRICES ALONG THE LEADING DIAGONAL OF THE INPUT
!     STIFFNESS MATRIX
!
!
!
!
!
   EQUIVALENCE (iorder,iarray(1)) , (nwds,iarray(2))
!
   DATA isubnm/4HGPST , 4HG   /
!
   dtol = Tolel
!
! AT THIS POINT, BOTH TRANSLATIONAL AND ROTATIONAL DIAGONAL 3X3 S ARE
! STORED IN THE D ARRAY.  HENCE WE PROCESS THEM.
!
   ip = Npvt - 1
   ASSIGN 900 TO igoto
   ASSIGN 100 TO iback
   DO i = 1 , 9
      b(i) = D(i)
   ENDDO
   GOTO 200
 100  DO i = 1 , 9
      b(i) = D(i+9)
   ENDDO
!
! INSURE THE SYMMETRY OF THE B MATRIX
!
   IF ( b(2)/=0.0 .AND. b(4)/=0.0 ) THEN
      temp = (b(2)+b(4))/2.0
      b(2) = temp
      b(4) = temp
   ELSE
      b(2) = 0.0
      b(4) = 0.0
   ENDIF
   IF ( b(3)/=0.0 .AND. b(7)/=0.0 ) THEN
      temp = (b(3)+b(7))/2.0
      b(3) = temp
      b(7) = temp
   ELSE
      b(3) = 0.0
      b(7) = 0.0
   ENDIF
   IF ( b(6)/=0.0 .AND. b(8)/=0.0 ) THEN
      temp = (b(6)+b(8))/2.0
   ELSE
      b(6) = 0.0
      b(8) = 0.0
   ENDIF
!
! SCALE THE MATRIX BY DIVIDING EACH ELEMENT OF B BY THE LARGEST ELEMENT.
! IF THE LARGEST ELEMENT IS NON-POSITIVE, THE SINGULARITY IS OF ORDER 3.
!
 200  temp = b(1)
   DO i = 2 , 9
      IF ( b(i)>temp ) temp = b(i)
   ENDDO
   IF ( temp<=0.0 ) GOTO 600
   DO i = 1 , 9
      b(i) = b(i)/temp
   ENDDO
!
! FIND THE SQUARES OF THE MAGNITUDES OF THE VECTORS OF THE ROWS OF THE
! B MATRIX.
!
   iorder = 0
   j = 0
   DO i = 1 , 9 , 3
      j = j + 1
      fl(j) = b(i)**2 + b(i+1)**2 + b(i+2)**2
      IF ( fl(j)==0.0 ) iorder = iorder + 1
   ENDDO
   IF ( iorder==2 ) THEN
!
! AT THIS POINT 2 ROWS OF THE B MATRIX ARE IDENTICALLY ZERO.
!
      nwds = 2
      ttlwds = 4
      ipoint = 2
      DO i = 1 , 3
         IF ( fl(i)==0.0 ) THEN
            ipoint = ipoint + 1
            iarray(ipoint) = ip + i
         ENDIF
      ENDDO
      GOTO 700
   ELSE
      IF ( iorder==0 ) THEN
!
! AT STATEMENT NO. 260, WE HAVE THAT ALL THE FL(I) ARE .GT. 0.0, SO
! THAT THE DETERMINANT, DET, OF B MUST BE COMPUTED.
!
         det = b(1)*(b(5)*b(9)-b(6)*b(8)) - b(2)*(b(4)*b(9)-b(6)*b(7)) + b(3)*(b(4)*b(8)-b(5)*b(7))
         const = 0.05*dtol*fl(1)*fl(2)*fl(3)
         IF ( det>const ) GOTO 800
!
! COMPUTE M(I) AND R(I)
!
         m(1) = b(5)*b(9) - b(6)*b(8)
         m(2) = b(1)*b(9) - b(3)*b(7)
         m(3) = b(1)*b(5) - b(2)*b(4)
         r(1) = sqrt(b(5)**2+b(6)**2)*sqrt(b(8)**2+b(9)**2)
         r(2) = sqrt(b(1)**2+b(3)**2)*sqrt(b(7)**2+b(9)**2)
         r(3) = sqrt(b(1)**2+b(2)**2)*sqrt(b(4)**2+b(5)**2)
!
! FIND I1,J1,K1 SUCH THAT M(I1)/R(I1) .GE. M(J1)/R(J1) .GE. M(K1)/R(K1)
!
         i1 = 1
         j1 = 2
         k1 = 3
         IF ( m(1)*r(2)<m(2)*r(1) ) THEN
            i1 = 2
            j1 = 1
         ENDIF
         IF ( m(i1)*r(k1)<m(k1)*r(i1) ) THEN
            itemp = i1
            i1 = k1
            k1 = itemp
         ENDIF
         IF ( m(j1)*r(k1)<m(k1)*r(j1) ) THEN
            itemp = j1
            j1 = k1
            k1 = itemp
         ENDIF
         IF ( m(i1)>=r(i1)*dtol ) THEN
!
! AT THIS POINT THE SINGULARITY IS OF ORDER 1.
!
            iorder = 1
            nwds = 1
            ttlwds = 3
            iarray(3) = ip + i1
            IF ( m(j1)>=r(j1)*dtol ) THEN
               nwds = 2
               ttlwds = 4
               iarray(4) = ip + j1
               IF ( m(k1)>=r(k1)*dtol ) THEN
                  nwds = 3
                  ttlwds = 5
                  iarray(5) = ip + k1
               ENDIF
            ENDIF
            GOTO 700
         ELSE
!
! HERE THE SINGULARITY IS OF ORDER 2.
!
            nwds = 0
            ttlwds = 2
            iorder = 2
!
! FIND II, JJ, KK SUCH THAT B(II) .GE. B(JJ) .GE. B(KK)
!
            ii = 1
            jj = 5
            kk = 9
            IF ( b(1)<b(5) ) THEN
               ii = 5
               jj = 1
            ENDIF
            IF ( b(ii)<b(kk) ) THEN
               itemp = ii
               ii = kk
               kk = itemp
            ENDIF
            IF ( b(jj)<b(kk) ) THEN
               itemp = jj
               jj = kk
               kk = itemp
            ENDIF
            ll = ii
            kount = 0
            ipoint = 3
            DO WHILE ( b(ll)>0.0 )
               nwds = nwds + 2
               ttlwds = ttlwds + 2
               IF ( ll<5 ) THEN
                  inc1 = 2
                  inc2 = 3
               ELSEIF ( ll==5 ) THEN
                  inc1 = 1
                  inc2 = 3
               ELSE
                  inc1 = 1
                  inc2 = 2
               ENDIF
               iarray(ipoint) = ip + inc1
               iarray(ipoint+1) = ip + inc2
               ipoint = ipoint + 2
               kount = kount + 1
               IF ( kount<2 ) THEN
                  ll = jj
               ELSEIF ( kount==2 ) THEN
                  ll = kk
               ELSE
                  EXIT
               ENDIF
            ENDDO
            GOTO 700
         ENDIF
      ELSE
!
! AT THIS POINT ONE AND ONLY ONE FL(I) IS ZERO.
!
         DO i = 1 , 3
            isave = i
            IF ( fl(i)==0.0 ) THEN
               IF ( isave==1 ) GOTO 220
               IF ( isave==2 ) GOTO 250
               IF ( isave==3 ) GOTO 300
            ENDIF
         ENDDO
         CALL mesage(-30,26,isubnm)
 220     fm = b(5)*b(9) - b(6)*b(8)
         fr = sqrt((b(5)**2+b(6)**2)*(b(8)**2+b(9)**2))
         GOTO 400
      ENDIF
 250  fm = b(1)*b(9) - b(3)*b(7)
      fr = sqrt((b(1)**2+b(3)**2)*(b(7)**2+b(9)**2))
      GOTO 400
   ENDIF
 300  fm = b(1)*b(5) - b(2)*b(4)
   fr = sqrt((b(1)**2+b(2)**2)*(b(4)**2+b(5)**2))
 400  IF ( fm/=0.0 ) THEN
      IF ( fr<=0.0 ) GOTO 500
      IF ( fm/fr>=dtol ) GOTO 500
   ENDIF
!
! HERE WE HAVE THAT THE ORDER OF THE SINGULARITY IS 2.
!
   iorder = 2
   nwds = 0
   ttlwds = 2
   IF ( isave==2 ) THEN
      k1 = 1
      k2 = 9
      inc1 = 2
      inc2 = 3
      inc3 = 1
   ELSEIF ( isave==3 ) THEN
      k1 = 1
      k2 = 5
      inc1 = 3
      inc2 = 2
      inc3 = 1
   ELSE
      k1 = 5
      k2 = 9
      inc1 = 1
      inc2 = 3
      inc3 = 2
   ENDIF
   IF ( b(k1)<=0.0 .AND. b(k2)<=0.0 ) GOTO 600
   IF ( b(k1)<=0.0 ) THEN
      ipoint = 3
   ELSE
      nwds = 2
      ttlwds = 4
      iarray(3) = ip + inc1
      iarray(4) = ip + inc2
      ipoint = 5
   ENDIF
   IF ( b(k2)>0.0 ) THEN
      nwds = nwds + 2
      ttlwds = ttlwds + 2
      iarray(ipoint) = ip + inc1
      iarray(ipoint+1) = ip + inc3
   ENDIF
   GOTO 700
!
! AT THIS POINT WE HAVE THAT ONE AND ONLY ONE FL IS ZERO BUT THAT ORDER
! OF THE SINGULARITY IS 1.
!
 500  iorder = 1
   nwds = 1
   ttlwds = 3
   iarray(3) = ip + isave
   GOTO 700
!
! THE SINGULARITY IS OF ORDER 3
!
 600  iorder = 3
   nwds = 3
   ttlwds = 5
   iarray(3) = ip + 1
   iarray(4) = ip + 2
   iarray(5) = ip + 3
!
! WRITE IARRAY ON THE GPST FILE.
!
 700  IF ( Igpst/=1 ) THEN
      Igpst = 1
      CALL gopen(Gpst,Iz(Ibuf2),1)
   ENDIF
   Nsing = Nsing + 1
   CALL write(Gpst,iarray,ttlwds,0)
 800  GOTO igoto
 900  ASSIGN 99999 TO igoto
   ip = ip + 3
   GOTO iback
99999 RETURN
END SUBROUTINE gpstgs
