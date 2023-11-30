
SUBROUTINE q2bcd(Est,Planar,Rmat,Et,Ierror)
   IMPLICIT NONE
   INTEGER Ierror
   LOGICAL Planar
   REAL Est(1)
   DOUBLE PRECISION Et(3,3) , Rmat(3,5)
   DOUBLE PRECISION d12(3) , d13(3) , d24(3) , g1(3) , g2(3) , g3(3) , g4(3) , g5(3) , grid(3,5) , iarea , itwoh , mag , vec(3)
   DOUBLE PRECISION dadotb
   INTEGER i
!
!     BASIC CALCULATIONS ARE PERFORMED FOR THE QDMEM2 ELEMENT IN THIS
!     ROUTINE (DOUBLE-PRECISION VERSION)
!
   EQUIVALENCE (grid(1,1),g1(1)) , (grid(1,2),g2(1)) , (grid(1,3),g3(1)) , (grid(1,4),g4(1)) , (grid(1,5),g5(1))
!
!     MOVE GRID COORDINATES AND MAKE DOUBLE-PRECISION IF THIS IS THE
!     DOUBLE-PRECISION VERSION.
!
   DO i = 1 , 3
      g1(i) = Est(i+10)
      g2(i) = Est(i+14)
      g3(i) = Est(i+18)
      g4(i) = Est(i+22)
   ENDDO
!
!     FORM  D   , D   AND  D   VECTORS
!            13    24       12
!
   DO i = 1 , 3
      d12(i) = g2(i) - g1(i)
      d13(i) = g3(i) - g1(i)
      d24(i) = g4(i) - g2(i)
   ENDDO
!
!     NVEC = D13 CROSS D24 = K-VECTOR (UN-NORMALIZED)
!
   CALL daxb(d13,d24,vec)
   mag = dsqrt(dadotb(vec,vec))
   iarea = 0.5D0*mag
!
!     NORMALIZE K-VECTOR
!
   IF ( mag<=0 ) THEN
!
!     ERROR CONDITION, BAD ELEMENT GEOMETRY.
!
      Ierror = 1
   ELSE
      Et(1,3) = vec(1)/mag
      Et(2,3) = vec(2)/mag
      Et(3,3) = vec(3)/mag
!
!     H = .5 * (D   DOT K-VEC)
!                12
!
      itwoh = dadotb(d12,Et(1,3))
!
!     I-VECTOR (UN-NORMALIZED) = (D  ) - 2 H (K-VECTOR)
!                                  12
!
      DO i = 1 , 3
         vec(i) = d12(i) - itwoh*Et(i,3)
      ENDDO
      mag = dsqrt(dadotb(vec,vec))
!
!     NORMALIZE I-VECTOR
!
      IF ( mag<=0 ) THEN
         Ierror = 1
      ELSE
         Et(1,1) = vec(1)/mag
         Et(2,1) = vec(2)/mag
         Et(3,1) = vec(3)/mag
!
!     JVEC = KVEC CROSS IVEC
!
         CALL daxb(Et(1,3),Et(1,1),Et(1,2))
!
!     FILL THE SUB-TRIANGLE ELEMENT COORDINATE MATRIX
!
         DO i = 1 , 3
            g5(i) = 0.25D0*(g1(i)+g2(i)+g3(i)+g4(i))
         ENDDO
         Rmat(1,1) = 0.0D0
         Rmat(2,1) = 0.0D0
         Rmat(3,1) = -itwoh/2.0D0
         DO i = 2 , 5
            vec(1) = grid(1,i) - g1(1)
            vec(2) = grid(2,i) - g1(2)
            vec(3) = grid(3,i) - g1(3)
            CALL gmmatd(Et,3,3,0,vec,3,1,0,Rmat(1,i))
            Rmat(1,i) = Rmat(1,i) + Rmat(1,1)
            Rmat(2,i) = Rmat(2,i) + Rmat(2,1)
            Rmat(3,i) = Rmat(3,i) + Rmat(3,1)
         ENDDO
!
!     SET PLANAR FLAG .TRUE. OR .FALSE.
!
         IF ( (itwoh/2.0D0)**2/iarea<=0.01D0 ) THEN
            Planar = .TRUE.
         ELSE
            Planar = .FALSE.
         ENDIF
!
!     ALL BASIC CALCULATIONS NOW COMPLETE
!
         Ierror = 0
         RETURN
      ENDIF
   ENDIF
END SUBROUTINE q2bcd
