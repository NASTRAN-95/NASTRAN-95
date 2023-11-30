
SUBROUTINE t3bgbs(Ng,Nb,Gmat,Bmat,Kmat)
   IMPLICIT NONE
   LOGICAL Bendng , Mbcoup , Membrn , Norpth , Shrflx
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
   INTEGER Nb , Ng
   DOUBLE PRECISION Bmad(1) , Gmad(9,1) , Kmad(1)
   REAL Bmat(1) , Gmat(9,1) , Kmat(1)
   REAL g1(3,3) , gbmat(162)
   DOUBLE PRECISION g2(3,3) , gbmad(162)
   INTEGER i , ii , j , jj , nd3 , nd6
!
!     WITH ENTRY T3BGBD (NG,NB,GMAD,BMAD,KMAD)
!
!     ROUTINE FOR EFFICIENT TRIPLE-MULTPLICATION OF [B] AND [G] MATRICES
!     TO EVALUATE THE CONTRIBUTION TO THE ELEMENT STIFFNESS MATRIX FROM
!     THE CURRENT INTEGRATION POINT
!
!
!     INPUT :
!           NG          - NUMBER OF ROWS AND COLUMNS OF GMAT
!           NB          - NUMBER OF COLUMNS OF BMAT
!           GMAT/GMAD   - [G], FORCE-STRAIN RELATIONSHIP
!           BMAT/BMAD   - [B], STRAIN-DISPLACEMENT RELATIONSHIP
!     OUTPUT:
!           KMAT/KMAD   - CONTRIBUTION TO THE ELEMENT STIFFNESS MATRIX
!                         FROM THE CURRENT INTEGRATION POINT
!
!     ALGORITHM:
!           MATRICES ARE MULTIPLIED IN FULL WHEN MEMBRANE-BENDING
!           COUPLING IN PRESENT, OTHERWISE PARTIAL MULTIPLICATION
!           IS PERFORMED.
!           IN EACH TRIPLE MULTIPLY, THE RESULT IS ADDED TO KMAT.
!
!
   EQUIVALENCE (g1(1,1),g2(1,1)) , (gbmat(1),gbmad(1))
!
!
!     SINGLE PRECISION
!
   nd3 = Nb*3
   nd6 = Nb*6
!
!     IF [G] IS FULLY POPULATED, PERFORM STRAIGHT MULTIPLICATION AND
!     RETURN.
!
   IF ( .NOT.Mbcoup ) THEN
!
!     MULTIPLY MEMBRANE TERMS WHEN PRESENT
!
      IF ( Membrn ) THEN
         DO i = 1 , 3
            DO j = 1 , 3
               g1(i,j) = Gmat(i,j)
            ENDDO
         ENDDO
         CALL gmmats(g1,3,3,0,Bmat(1),3,Nb,0,gbmat)
         CALL gmmats(Bmat(1),3,Nb,-1,gbmat,3,Nb,0,Kmat)
      ENDIF
!
!     MULTIPLY BENDING TERMS WHEN PRESENT
!
      IF ( Bendng ) THEN
         DO i = 1 , 3
            ii = i + 3
            DO j = 1 , 3
               jj = j + 3
               g1(i,j) = Gmat(ii,jj)
            ENDDO
         ENDDO
         CALL gmmats(g1,3,3,0,Bmat(nd3+1),3,Nb,0,gbmat)
         CALL gmmats(Bmat(nd3+1),3,Nb,-1,gbmat,3,Nb,0,Kmat)
!
         DO i = 1 , 3
            ii = i + 6
            DO j = 1 , 3
               jj = j + 6
               g1(i,j) = Gmat(ii,jj)
            ENDDO
         ENDDO
         CALL gmmats(g1,3,3,0,Bmat(nd6+1),3,Nb,0,gbmat)
         CALL gmmats(Bmat(nd6+1),3,Nb,-1,gbmat,3,Nb,0,Kmat)
      ENDIF
   ELSE
      CALL gmmats(Gmat,Ng,Ng,0,Bmat,Ng,Nb,0,gbmat)
      CALL gmmats(Bmat,Ng,Nb,-1,gbmat,Ng,Nb,0,Kmat)
   ENDIF
   RETURN
!
!
   ENTRY t3bgbd(Ng,Nb,Gmad,Bmad,Kmad)
!     ===================================
!
!     DOUBLE PRECISION
!
   nd3 = Nb*3
   nd6 = Nb*6
!
!     IF [G] IS FULLY POPULATED, PERFORM STRAIGHT MULTIPLICATION AND
!     RETURN.
!
   IF ( .NOT.Mbcoup ) THEN
!
!     MULTIPLY MEMBRANE TERMS WHEN PRESENT
!
      IF ( Membrn ) THEN
         DO i = 1 , 3
            DO j = 1 , 3
               g2(i,j) = Gmad(i,j)
            ENDDO
         ENDDO
         CALL gmmatd(g2,3,3,0,Bmad(1),3,Nb,0,gbmad)
         CALL gmmatd(Bmad(1),3,Nb,-1,gbmad,3,Nb,0,Kmad)
      ENDIF
!
!     MULTIPLY BENDING TERMS WHEN PRESENT
!
      IF ( Bendng ) THEN
         DO i = 1 , 3
            ii = i + 3
            DO j = 1 , 3
               jj = j + 3
               g2(i,j) = Gmad(ii,jj)
            ENDDO
         ENDDO
         CALL gmmatd(g2,3,3,0,Bmad(nd3+1),3,Nb,0,gbmad)
         CALL gmmatd(Bmad(nd3+1),3,Nb,-1,gbmad,3,Nb,0,Kmad)
!
         DO i = 1 , 3
            ii = i + 6
            DO j = 1 , 3
               jj = j + 6
               g2(i,j) = Gmad(ii,jj)
            ENDDO
         ENDDO
         CALL gmmatd(g2,3,3,0,Bmad(nd6+1),3,Nb,0,gbmad)
         CALL gmmatd(Bmad(nd6+1),3,Nb,-1,gbmad,3,Nb,0,Kmad)
      ENDIF
   ELSE
      CALL gmmatd(Gmad,Ng,Ng,0,Bmad,Ng,Nb,0,gbmad)
      CALL gmmatd(Bmad,Ng,Nb,-1,gbmad,Ng,Nb,0,Kmad)
   ENDIF
!
!
END SUBROUTINE t3bgbs
