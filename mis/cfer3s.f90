
SUBROUTINE cfer3s(V1,V1l,V2,V2l,V3,V3l,V4,V4l,V5,V5l,Zb,Zc)
   IMPLICIT NONE
   REAL Cdp , Dudxx , Dumaa(84) , Eofnrw , Epsdum(2) , Lambda(4) , Rd , Rdp , Rdrew , Regdum(2) , Rew , Rsp , Sqr , Ten2mt ,        &
      & Tenmht , Wrt , Wrtrew , Xcdum(3)
   INTEGER Csp , Idiag , Ii , Iip , Ikmb(7,3) , Ilam(7) , Incr , Incrp , Iphi(7) , Iprc , Iscr(11) , It , Itp1 , Itp2 , Ksystm(65) ,&
         & Mcbvec(7) , Mreduc , Nn , Nnp , Nord , Nord2 , Nord4 , Nordp1 , Norew , Northo , Nout , Nstart , Nswp(2) , Numran , Nzero
   LOGICAL Nob , Qpr , Symmet
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /feeraa/ Ikmb , Ilam , Iphi , Dudxx , Iscr , Dumaa , Mcbvec
   COMMON /feerxc/ Lambda , Symmet , Mreduc , Nord , Idiag , Epsdum , Northo , Nord2 , Nord4 , Nordp1 , Nswp , Nob , It , Ten2mt ,  &
                 & Tenmht , Nstart , Qpr , Regdum , Nzero , Xcdum , Numran
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /system/ Ksystm
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /xmssg / Ufm , Uwm
   REAL V1(1) , V1l(1) , V2(1) , V2l(1) , V3(1) , V3l(1) , V4(1) , V4l(1) , V5(1) , V5l(1) , Zb(1) , Zc(1)
   REAL a(2) , d(4) , dsave(2) , s(8) , ss , temp1(2) , zero
   LOGICAL again , skip , sucess
   INTEGER i , ij , j , j2 , jj , lancos , n3 , name(2) , nord8
   DOUBLE PRECISION temp2
!
!     CFER3S IS A SINGLE PRECISION ROUTINE (CALLED BY CFEER3) WHICH
!     PERFORMS THE TRIDIAGONAL REDUCTION FOR THE COMPLEX FEER METHOD
!
   !>>>>EQUIVALENCE (a(1),d(3)) , (Ksystm(2),Nout) , (d(1),s(1)) , (temp1(1),temp2)
   DATA zero/0./
   DATA name/4HCFER , 4H3S  /
!
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!
!     V1,V2,V3,V4,V5  = AREAS OF OPEN CORE DESIGNATED BY SUBROUTINE
!                       CFEER3 AND USED INTERNALLY AS WORKING VECTORS,
!                       USUALLY RIGHT-HANDED
!     V1L,.......,V5L = SAME AS V1 THRU V5 BUT USUALLY LEFT-HANDED
!     RESTRICTION ..... LEFT-HANDED VECTOR MUST IMMEDIATELY FOLLOW
!                       CORRESPONDING RIGHT-HANDED VECTOR IN CORE
!                       ALSO, V2 SHOULD FOLLOW V1L FOR READ TO WORK
!     ZB,ZC           = REQUIRED GINO BUFFERS
!
!     DEFINITION OF INTERNAL PARAMETERS
!
!     A        = DIAGONAL ELEMENTS OF REDUCED TRIDIAGONAL MATRIX
!     D        = OFF-DIAG ELEMENTS OF REDUCED TRIDIAGONAL MATRIX
!     AGAIN    = LOGICAL INDICATOR FOR CYCLING THRU LOGIC AGAIN WHEN
!                NULL VECTOR TEST (D-BAR) FAILS
!     SKIP     = LOGICAL INDICATOR FOR AVOIDING REDUNDANT OPERATIONS
!     NORTHO   = TOTAL CURRENT NUMBER OF VECTOR PAIRS ON ORTHOGONAL
!                VECTOR FILE
!     NZERO    = NUMBER OF EIGENVECTOR PAIRS ON EIGENVECTOR FILE
!                (RESTART AND PRIOR NEIGHBORHOODS)
!     LANCOS   = LANCZOS ALGORITHM COUNTER
!     NSTART   = NUMBER OF INITIAL REORTHOGONALIZATION ATTEMPTS
!
   IF ( Qpr ) WRITE (Nout,99001)
99001 FORMAT (1H1,50X,6HCFER3S,/1H0)
!
!     SET PACK AND UNPACK CONSTANTS
!
   Iprc = Csp
   Incr = 1
   Itp1 = Iprc
   Itp2 = Itp1
   Incrp = Incr
   Ii = 1
   Iip = 1
!
!     NN AND NNP ARE SET LOCALLY
!
   CALL gopen(Iscr(7),Zb(1),Wrtrew)
   CALL close(Iscr(7),Norew)
   IF ( Northo/=0 ) THEN
!
!     LOAD AND RE-NORMALIZE ALL EXISTING VECTORS ON THE NASTRAN
!     EIGENVECTOR FILE (INCLUDES ANY RESTART VECTORS AND ALL VECTORS
!     OBTAINED IN PRIOR NEIGHBORHOODS). PACK THESE VECTORS ON
!     THE ORTHOGONAL VECTOR SCRATCH FILE.
!
      CALL open(*700,Iphi(1),Zc(1),0)
!
!     LEFT-HAND VECTOR IS STORED IMMEDIATELY AFTER RIGHT-HAND VECTOR
!
      Nnp = Nord2
      nord8 = 2*Nord4
      DO i = 1 , Northo
         IF ( Qpr ) WRITE (Nout,99002) i
99002    FORMAT (1H ,13(10H----------),/,18H ORTHOGONAL VECTOR,I3)
!
!     THIS LOADS VALUES INTO V1, V1L, V2, AND V2L
!
         CALL read(*900,*20,Iphi(1),V1(1),nord8+10,0,n3)
         GOTO 1000
!
!     COMPRESS PHYSICAL EIGENVECTORS TO SINGLE PRECISION
!
 20      DO j = 1 , Nord4
            j2 = j*2
            temp1(1) = V1(j2-1)
            temp1(2) = V1(j2)
            V1(j) = temp2
         ENDDO
         IF ( Idiag/=0 ) THEN
            DO j = 1 , Nord4
               IF ( V1(j)/=zero ) GOTO 40
            ENDDO
            WRITE (Nout,99003) i
99003       FORMAT (18H ORTHOGONAL VECTOR,I4,8H IS NULL)
         ENDIF
 40      IF ( Qpr ) WRITE (Nout,99010) (V1(j),j=1,Nord2)
         IF ( Qpr ) WRITE (Nout,99010) (V1l(j),j=1,Nord2)
         CALL cfnor1(V1(1),V1l(1),Nord2,0,d(1))
         IF ( Idiag/=0 .AND. Nord2<=70 ) WRITE (Nout,99004) i , (V1(j),V1(j+1),V1l(j),V1l(j+1),j=1,Nord2,2)
99004    FORMAT (1H0,17HORTHOGONAL VECTOR,I4,/1H0,23X,5HRIGHT,56X,4HLEFT,//,(1H ,2E25.16,10X,2E25.16))
         CALL gopen(Iscr(7),Zb(1),Wrt)
         CALL pack(V1(1),Iscr(7),Mcbvec(1))
         CALL close(Iscr(7),Norew)
      ENDDO
      CALL close(Iphi(1),Norew)
      IF ( Idiag/=0 ) WRITE (Nout,99005) Northo , Mcbvec
99005 FORMAT (1H0,I10,32H ORTHOGONAL VECTOR PAIRS ON FILE,I5,12X,6I8,/)
   ENDIF
!
!     GENERATE INITIAL PSEUDO-RANDOM VECTORS
!
   n3 = 3*Nord
   ij = 0
   ss = 1.
   Nzero = Northo
   Nstart = 0
   lancos = 0
   again = .FALSE.
   d(1) = zero
   d(2) = zero
 100  Numran = Numran + 1
   DO i = 1 , Nord4
      ij = ij + 1
      ss = -ss
      IF ( i>Nord2 ) THEN
         IF ( i>n3 ) THEN
            jj = 2*(i-n3) + Nord2
         ELSE
            jj = 2*i - 1 - Nord2
         ENDIF
      ELSEIF ( i>Nord ) THEN
         jj = 2*(i-Nord)
      ELSE
         jj = 2*i - 1
      ENDIF
!
!     THIS LOADS VALUES INTO V1 AND V1L
!
      V1(jj) = ss*(mod(ij,3)+1)/(3.*(mod(ij,13)+1)*(1+5*float(i)/Nord))
   ENDDO
   IF ( Qpr ) WRITE (Nout,99011) (V1(i),i=1,Nord4)
   IF ( Qpr ) WRITE (Nout,99012)
!
!     NORMALIZE RIGHT AND LEFT START VECTORS
!
   CALL cfnor1(V1(1),V1l(1),Nord2,0,d(1))
!
!     REORTHOGONALIZE START VECTORS W.R.T. RESTART AND
!     PRIOR-NEIGHBORHOOD VECTORS
!
   CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V1(1),V1l(1),V5(1),V5l(1),V3(1),V3l(1),Zb(1))
   IF ( sucess ) THEN
      IF ( again ) THEN
         CALL cfe1ao(.FALSE.,V1(1),V4(1),V3(1),Zb(1))
         CALL cfe1ao(.TRUE.,V1l(1),V4l(1),V3l(1),Zb(1))
         GOTO 400
      ELSE
!
!     SWEEP START VECTORS CLEAN OF ZERO-ROOT EIGENVECTORS
!
         CALL cfe1ao(.FALSE.,V1(1),V2(1),V3(1),Zb(1))
         CALL cfe1ao(.TRUE.,V1l(1),V2l(1),V3l(1),Zb(1))
!
!     NORMALIZE THE PURIFIED VECTOR AND OBTAIN D(1)
!
         CALL cfnor1(V2(1),V2l(1),Nord2,0,d(1))
         IF ( Nzero==0 .OR. Northo>Nzero ) GOTO 200
!
!     IF RESTART OR BEGINNING OF NEXT NEIGHBORHOOD, PERFORM
!     REORTHOGONALIZATION AND RENORMALIZATION
!
         CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V2(1),V2l(1),V5(1),V5l(1),V3(1),V3l(1),Zb(1))
         IF ( sucess ) THEN
            CALL cfnor1(V2(1),V2l(1),Nord2,0,d(1))
            GOTO 200
         ENDIF
      ENDIF
   ELSEIF ( again ) THEN
      GOTO 600
   ENDIF
   Nstart = Nstart + 1
   IF ( Nstart<=2 ) GOTO 100
   WRITE (Nout,99006) Uwm , Lambda(1) , Lambda(3)
99006 FORMAT (A25,' 3158',//5X,'NO ADDITIONAL MODES CAN BE FOUND BY ','FEER IN THE NEIGHBORHOOD OF ',2E14.6,//)
   GOTO 1100
!
!     LOAD FIRST VECTORS TO ORTHOGONAL VECTOR FILE
!
 200  CALL gopen(Iscr(7),Zb(1),Wrt)
   Nnp = Nord2
   CALL pack(V2(1),Iscr(7),Mcbvec(1))
   CALL close(Iscr(7),Norew)
   Northo = Northo + 1
!
!     COMMENCE LANCZOS ALGORITHM
!
!     INITIALIZE BY CREATING NULL VECTOR
!
   DO i = 1 , Nord2
      V1(i) = zero
      V1l(i) = zero
   ENDDO
   skip = .FALSE.
!
!     ENTER LANCZOS LOOP
!
 300  lancos = lancos + 1
!
!     GENERATE DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL MATRIX
!
   IF ( .NOT.skip ) CALL cfe1ao(.FALSE.,V2(1),V3(1),V5(1),Zb(1))
   skip = .FALSE.
   CALL cfnor1(V3(1),V2l(1),Nord2,1,a(1))
!
!     COMPUTE D-BAR
!
   CALL cfe1ao(.TRUE.,V2l(1),V3l(1),V5(1),Zb(1))
   DO i = 1 , Nord2 , 2
      j = i + 1
      V4(i) = V3(i) - a(1)*V2(i) + a(2)*V2(j) - d(1)*V1(i) + d(2)*V1(j)
      V4(j) = V3(j) - a(1)*V2(j) - a(2)*V2(i) - d(1)*V1(j) - d(2)*V1(i)
      V4l(i) = V3l(i) - a(1)*V2l(i) + a(2)*V2l(j) - d(1)*V1l(i) + d(2)*V1l(j)
      V4l(j) = V3l(j) - a(1)*V2l(j) - a(2)*V2l(i) - d(1)*V1l(j) - d(2)*V1l(i)
   ENDDO
   CALL cfnor1(V4(1),V4l(1),Nord2,2,d(1))
   dsave(1) = d(1)
   dsave(2) = d(2)
!
!     TEST IF LANCZOS ALGORITHM FINISHED
!
   IF ( lancos==Mreduc ) GOTO 500
   IF ( Qpr ) THEN
      WRITE (Nout,99012)
      WRITE (Nout,99007) d
99007 FORMAT (8H D-BAR =,2E16.8,9X,3HA =,2E16.8)
      WRITE (Nout,99011) (V4(i),i=1,Nord2)
      WRITE (Nout,99011) (V4l(i),i=1,Nord2)
      WRITE (Nout,99012)
   ENDIF
!
!     NULL VECTOR TEST
!
   IF ( sqrt(d(1)**2+d(2)**2)<=sqrt(a(1)**2+a(2)**2)*Tenmht ) THEN
      IF ( Idiag/=0 ) WRITE (Nout,99008) d
99008 FORMAT (14H D-BAR IS NULL,10X,4E20.12)
      again = .TRUE.
      GOTO 100
   ENDIF
!
!     PERFORM REORTHOGONALIZATION
!
 400  CALL cfnor1(V4(1),V4l(1),Nord2,0,d(1))
   CALL cf1ort(sucess,10,Ten2mt,Nzero,lancos,V4(1),V4l(1),V3(1),V3l(1),V5(1),V5l(1),Zb(1))
   IF ( .NOT.sucess ) GOTO 600
!
!     NORMALIZE THE REORTHOGONALIZED VECTORS
!
   CALL cfnor1(V4(1),V4l(1),Nord2,0,d(1))
!
!     GENERATE OFF-DIAGONAL ELEMENT OF REDUCED TRIDIAGONAL MATRIX
!
   CALL cfe1ao(.FALSE.,V4(1),V3(1),V5(1),Zb(1))
   skip = .TRUE.
   CALL cfnor1(V3(1),V2l(1),Nord2,1,d(1))
   IF ( again ) THEN
      again = .FALSE.
      d(1) = zero
      d(2) = zero
!
!     NULL VECTOR TEST
!
   ELSEIF ( sqrt(d(1)**2+d(2)**2)<=sqrt(a(1)**2+a(2)**2)*Tenmht ) THEN
      GOTO 600
   ENDIF
!
!     TRANSFER TWO ELEMENTS TO REDUCED TRIDIAGONAL MATRIX FILE
!
   CALL write(Iscr(5),s(1),4,1)
   IF ( Idiag/=0 ) WRITE (Nout,99013) lancos , d
!
!     LOAD CURRENT VECTORS TO ORTHOGONAL VECTOR FILE
!
   CALL gopen(Iscr(7),Zb(1),Wrt)
   Nnp = Nord2
   CALL pack(V4(1),Iscr(7),Mcbvec(1))
   CALL close(Iscr(7),Norew)
   Northo = Northo + 1
!
!     TRANSFER (I+1)-VECTORS TO (I)-VECTORS AND CONTINUE LANCZOS LOOP
!
   DO i = 1 , Nord2
      V1(i) = V2(i)
      V1l(i) = V2l(i)
      V2(i) = V4(i)
      V2l(i) = V4l(i)
   ENDDO
   GOTO 300
!
!     TRANSFER TWO ELEMENTS TO REDUCED TRIDIAGONAL MATRIX FILE
!
 500  IF ( d(1)==zero .AND. d(2)==zero ) THEN
      d(1) = dsave(1)
      d(2) = dsave(2)
   ENDIF
   CALL write(Iscr(5),s(1),4,1)
   IF ( Idiag/=0 ) WRITE (Nout,99013) lancos , d
   GOTO 1100
 600  Mreduc = lancos
   WRITE (Nout,99009) Uwm , Mreduc , Lambda(1) , Lambda(3)
!
99009 FORMAT (A25,' 3157',//5X,'FEER PROCESS MAY HAVE CALCULATED ','FEWER ACCURATE MODES',I5,                                       &
             &' THAN REQUESTED IN THE NEIGHBORHOOD OF ',2E14.6,//)
   IF ( again ) THEN
      d(1) = zero
      d(2) = zero
   ENDIF
   GOTO 500
!
 700  i = -1
 800  CALL mesage(i,Iphi(1),name)
 900  i = -2
   GOTO 800
 1000 i = -8
   GOTO 800
!
 1100 RETURN
99010 FORMAT (1H ,(1H ,4E25.16))
99011 FORMAT (1H0,13(10H----------),/,(1H ,4E25.16))
99012 FORMAT (1H ,13(10H----------))
99013 FORMAT (36H REDUCED TRIDIAGONAL MATRIX ELEMENTS,5X,3HROW,I4,/10X,14HOFF-DIAGONAL =,2E24.16,/14X,10HDIAGONAL =,2E24.16)
END SUBROUTINE cfer3s