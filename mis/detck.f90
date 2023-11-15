
SUBROUTINE detck(Jarg,Ifgpst,Npvt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION B(9) , D(18) , Dz(1) , Fl(3) , M(3) , R(3)
   INTEGER Isys(69) , Iz(1)
   REAL Tolel
   COMMON /ma1xx / D , B , Dz , Fl , R , M
   COMMON /system/ Isys , Tolel
!
! Dummy argument declarations
!
   INTEGER Ifgpst , Jarg , Npvt
!
! Local variable declarations
!
   INTEGER back , i , i1 , iarg , iarray(8) , igoto , ii , inc1 , inc2 , inc3 , iorder , ip , ipoint , isave , itemp , j , j1 , jj ,&
         & k1 , k2 , kk , kount , ll , name(2) , neor , nwds , tnwds
   DOUBLE PRECISION const , det , dtol , fm , fr , temp
!
! End of declarations
!
!
!     COMMENTS FROM G.CHAN/UNISYS, 5/1991,
!     THIS ROUTINE WAS NAMED DETCKX BEFORE, WHICH HAD NOT BEEN TESTED.
!     THE ONE THAT USED TO BE DETCK APPEARS TO BE AN OLDER VERSION, AND
!     SHOULD BE REPLACED BY THIS ONE, IF THIS ONE WORKS
!
!     THIS ROUTINE GENERATES THE GRID POINT SINGULARITY TABLE BY
!     EXAMINING THE TRANSLATIONAL AND DIAGONAL 3 X 3 SUBMATRICES OF THE
!     KGG MATRIX.
!     IF JARG = 0, THE PIVOT POINT HAS ELEMENTS ATTACHED TO IT.
!     IF JARG =-1, THE PIVOT IS A SCALAR POINT AND NO ELEMENTS ARE
!                  CONNECTED TO IT.
!     IF JARG = 1, THE PIVOT POINT IS A GRID POINT AND NO ELEMENTS ARE
!                  CONNECTED TO IT.
!
   EQUIVALENCE (Iz(1),Dz(1)) , (iarray(1),iorder) , (iarray(2),nwds)
   DATA name/4HDETC , 4HK   / , neor/0/
!
   dtol = Tolel
   iarg = Jarg
   IF ( iarg<0 ) THEN
      iorder = 1
      nwds = 1
      iarray(3) = Npvt
      CALL write(Ifgpst,iarray(1),3,neor)
      RETURN
   ELSEIF ( iarg==0 ) THEN
   ENDIF
!
!     AT THIS POINT, BOTH TRANSLATIONAL AND ROTATIONAL DIAGONAL 3X3 S
!     ARE STORED IN THE D ARRAY.  HENCE WE PROCESS THEM.
!
   ip = Npvt - 1
   ASSIGN 800 TO igoto
   IF ( iarg/=1 ) THEN
      ASSIGN 100 TO back
      DO i = 1 , 9
         B(i) = D(i)
      ENDDO
      GOTO 200
   ELSE
      ASSIGN 500 TO back
      GOTO 500
   ENDIF
 100  DO i = 1 , 9
      B(i) = D(i+9)
   ENDDO
!
!     INSURE THE SYMMETRY OF THE B MATRIX
!
   IF ( B(2)/=0.0D0 .AND. B(4)/=0.0D0 ) THEN
      temp = (B(2)+B(4))/2.0D0
      B(2) = temp
      B(4) = temp
   ELSE
      B(2) = 0.0D0
      B(4) = 0.0D0
   ENDIF
   IF ( B(3)/=0.0D0 .AND. B(7)/=0.0D0 ) THEN
      temp = (B(3)+B(7))/2.0D0
      B(3) = temp
      B(7) = temp
   ELSE
      B(3) = 0.0D0
      B(7) = 0.0D0
   ENDIF
   IF ( B(6)/=0.0D0 .AND. B(8)/=0.0D0 ) THEN
      temp = (B(6)+B(8))/2.0D0
   ELSE
      B(6) = 0.0D0
      B(8) = 0.0D0
   ENDIF
!
!     SCALE THE MATRIX BY DIVIDING EACH ELEMENT OF B BY THE LARGEST
!     ELEMENT. IF THE LARGEST ELEMENT IS NON-POSITIVE, THE SINGULARITY
!     IS OF ORDER 3.
!
 200  temp = B(1)
   DO i = 2 , 9
      IF ( B(i)>temp ) temp = B(i)
   ENDDO
   IF ( temp<=0.0D0 ) GOTO 500
   DO i = 1 , 9
      B(i) = B(i)/temp
   ENDDO
!
!     FIND THE SQUARES OF THE MAGNITUDES OF THE VECTORS OF THE ROWS OF
!     THE B MATRIX.
!
   iorder = 0
   j = 0
   DO i = 1 , 9 , 3
      j = j + 1
      Fl(j) = B(i)**2 + B(i+1)**2 + B(i+2)**2
      IF ( Fl(j)==0.0D0 ) iorder = iorder + 1
   ENDDO
   IF ( iorder==2 ) THEN
!
!     AT THIS POINT 2 ROWS OF THE B MATRIX ARE IDENTICALLY ZERO.
!
      nwds = 2
      tnwds = 4
      ipoint = 2
      DO i = 1 , 3
         IF ( Fl(i)==0.0D0 ) THEN
            ipoint = ipoint + 1
            iarray(ipoint) = ip + i
         ENDIF
      ENDDO
      GOTO 600
   ELSE
      IF ( iorder==0 ) THEN
!
!     AT STATEMENT NO. 250, WE HAVE THAT ALL THE FL(I) ARE .GT. 0.0D0,
!     SO THAT THE DETERMINANT, DET, OF B MUST BE COMPUTED.
!
         det = B(1)*(B(5)*B(9)-B(6)*B(8)) - B(2)*(B(4)*B(9)-B(6)*B(7)) + B(3)*(B(4)*B(8)-B(5)*B(7))
         const = 0.05D0*dtol*Fl(1)*Fl(2)*Fl(3)
         IF ( det>const ) GOTO 700
!
!     COMPUTE M(I) AND R(I)
!
         M(1) = B(5)*B(9) - B(6)*B(8)
         M(2) = B(1)*B(9) - B(3)*B(7)
         M(3) = B(1)*B(5) - B(2)*B(4)
         R(1) = dsqrt(B(5)**2+B(6)**2)*dsqrt(B(8)**2+B(9)**2)
         R(2) = dsqrt(B(1)**2+B(3)**2)*dsqrt(B(7)**2+B(9)**2)
         R(3) = dsqrt(B(1)**2+B(2)**2)*dsqrt(B(4)**2+B(5)**2)
!
!     FIND I1, J1, K1
!     SUCH THAT M(I1)/R(I1) .GE. M(J1)/R(J1) .GE. M(K1)/R(K1)
!
         i1 = 1
         j1 = 2
         k1 = 3
         IF ( M(1)*R(2)<M(2)*R(1) ) THEN
            i1 = 2
            j1 = 1
         ENDIF
         IF ( M(i1)*R(k1)<M(k1)*R(i1) ) THEN
            itemp = i1
            i1 = k1
            k1 = itemp
         ENDIF
         IF ( M(j1)*R(k1)<M(k1)*R(j1) ) THEN
            itemp = j1
            j1 = k1
            k1 = itemp
         ENDIF
         IF ( M(i1)>=R(i1)*dtol ) THEN
!
!     AT THIS POINT THE SINGULARITY IS OF ORDER 1.
!
            iorder = 1
            nwds = 1
            tnwds = 3
            iarray(3) = ip + i1
            IF ( M(j1)>=R(j1)*dtol ) THEN
               nwds = 2
               tnwds = 4
               iarray(4) = ip + j1
               IF ( M(k1)>=R(k1)*dtol ) THEN
                  nwds = 3
                  tnwds = 5
                  iarray(5) = ip + k1
               ENDIF
            ENDIF
            GOTO 600
         ELSE
!
!     HERE THE SINGULARITY IS OF ORDER 2.
!
            nwds = 0
            tnwds = 2
            iorder = 2
!
!     FIND II, JJ, KK SUCH THAT B(II) .GE. B(JJ) .GE. B(KK)
!
            ii = 1
            jj = 5
            kk = 9
            IF ( B(1)<B(5) ) THEN
               ii = 5
               jj = 1
            ENDIF
            IF ( B(ii)<B(kk) ) THEN
               itemp = ii
               ii = kk
               kk = itemp
            ENDIF
            IF ( B(jj)<B(kk) ) THEN
               itemp = jj
               jj = kk
               kk = itemp
            ENDIF
            ll = ii
            kount = 0
            ipoint = 3
            DO WHILE ( B(ll)>0.0D0 )
               nwds = nwds + 2
               tnwds = tnwds + 2
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
            GOTO 600
         ENDIF
      ELSE
!
!     AT THIS POINT ONE AND ONLY ONE FL(I) IS ZERO.
!
         DO i = 1 , 3
            isave = i
            IF ( Fl(i)==0.0D0 ) THEN
               IF ( isave==1 ) GOTO 220
               IF ( isave==2 ) GOTO 250
               IF ( isave==3 ) GOTO 300
            ENDIF
         ENDDO
         CALL mesage(-30,26,name)
 220     fm = B(5)*B(9) - B(6)*B(8)
         fr = dsqrt((B(5)**2+B(6)**2)*(B(8)**2+B(9)**2))
         GOTO 400
      ENDIF
 250  fm = B(1)*B(9) - B(3)*B(7)
      fr = dsqrt((B(1)**2+B(3)**2)*(B(7)**2+B(9)**2))
      GOTO 400
   ENDIF
 300  fm = B(1)*B(5) - B(2)*B(4)
   fr = dsqrt((B(1)**2+B(2)**2)*(B(4)**2+B(5)**2))
 400  IF ( fm/=0.0D0 ) THEN
      IF ( fr<=0.0D0 .OR. fm/fr>=dtol ) THEN
!
!     AT THIS POINT WE HAVE THAT ONE AND ONLY ONE FL IS ZERO BUT THAT
!     ORDER OF THE SINGULARITY IS 1.
!
         iorder = 1
         nwds = 1
         tnwds = 3
         iarray(3) = ip + isave
         GOTO 600
      ENDIF
   ENDIF
!
!     HERE WE HAVE THAT THE ORDER OF THE SINGULARITY IS 2.
!
   iorder = 2
   nwds = 0
   tnwds = 2
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
   IF ( B(k1)>0.0D0 .OR. B(k2)>0.0D0 ) THEN
      IF ( B(k1)<=0.0D0 ) THEN
         ipoint = 3
      ELSE
         nwds = 2
         tnwds = 4
         iarray(3) = ip + inc1
         iarray(4) = ip + inc2
         ipoint = 5
      ENDIF
      IF ( B(k2)>0.0D0 ) THEN
         nwds = nwds + 2
         tnwds = tnwds + 2
         iarray(ipoint) = ip + inc1
         iarray(ipoint+1) = ip + inc3
      ENDIF
      GOTO 600
   ENDIF
!
!     THE SINGULARITY IS OF ORDER 3
!
 500  iorder = 3
   nwds = 3
   tnwds = 5
   iarray(3) = ip + 1
   iarray(4) = ip + 2
   iarray(5) = ip + 3
!
!     WRITE IARRAY ON THE GPST FILE.
!
 600  CALL write(Ifgpst,iarray(1),tnwds,neor)
 700  GOTO igoto
 800  ASSIGN 99999 TO igoto
   ip = ip + 3
   GOTO back
99999 END SUBROUTINE detck
