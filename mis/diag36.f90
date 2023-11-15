
SUBROUTINE diag36(Z,Buf,Gpl,Sil,Eqexin)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dummy(6) , Rd , Rdrew , Rew , Skip(2)
   INTEGER Ibuf , L , Nlpp
   COMMON /names / Rd , Rdrew , Skip , Rew
   COMMON /system/ Ibuf , L , Dummy , Nlpp
!
! Dummy argument declarations
!
   INTEGER Buf , Eqexin , Gpl , Sil
   INTEGER Z(2)
!
! Local variable declarations
!
   INTEGER file , i , im1 , j , j1 , j2 , j3 , k , n , n1 , n2 , n3 , nam(2) , nlpx
!
! End of declarations
!
!
!     THIS ROUTINE PRINTS THE INTERNAL-EXTERNAL-SIL NOS. OF THE GRID
!     POINTS AND SCALAR POINTS, AS REQUESTED BY DIAG 36
!
   DATA nam/4HDIAG , 4H34  /
!
   file = Gpl
   Z(1) = Gpl
   CALL rdtrl(Z(1))
   n1 = Z(2)
   n2 = n1 + n1
   n3 = n2 + n1 + 1
   IF ( n1>0 ) THEN
!
      n = 1
      DO i = 1 , 2
         CALL open(*100,file,Z(Buf),Rdrew)
         CALL fwdrec(*200,file)
         CALL read(*100,*300,file,Z(n),n1,1,j)
         CALL close(file,Rew)
         file = Sil
         n = n + n1
      ENDDO
!
!     HERE WE HAVE, IN INTERNAL NUMBER ORDER,
!        Z(   1 THRU N1) = EXTERNAL NOS.
!        Z(N1+1 THRU N2) = SIL NOS.
!
      nlpx = Nlpp - 8
      n = nlpx*3
      DO i = 1 , n1 , n
         CALL page1
         WRITE (L,99001)
99001    FORMAT (/46X,38HTABLE OF INTERNAL-EXTERNAL-SIL NUMBERS,//10X,3(6X,30HINTERNAL  EXTERNAL      SIL   ),/10X,                 &
                &3(6X,3(10H--------  )))
         im1 = i - 1
         DO j = 1 , nlpx
            j1 = im1 + j
            j2 = j1 + nlpx
            j3 = j2 + nlpx
            IF ( j3<=n1 ) WRITE (L,99004) j1 , Z(j1) , Z(j1+n1) , j2 , Z(j2) , Z(j2+n1) , j3 , Z(j3) , Z(j3+n1)
            IF ( j3>n1 .AND. j2<=n1 ) WRITE (L,99004) j1 , Z(j1) , Z(j1+n1) , j2 , Z(j2) , Z(j2+n1)
            IF ( j2>n1 .AND. j1<=n1 ) WRITE (L,99004) j1 , Z(j1) , Z(j1+n1)
         ENDDO
      ENDDO
!
      CALL sswtch(20,j)
      IF ( j==0 ) RETURN
!
      file = Eqexin
      CALL open(*100,file,Z(Buf),Rdrew)
      CALL fwdrec(*200,file)
      CALL read(*100,*300,file,Z(1),n2,1,j)
      CALL read(*100,*300,file,Z(n3),n2,1,j)
      CALL close(file,Rew)
      i = n3 - 1
      j = n2
      k = n3 + n2 - 1
      DO n = 1 , n1
         Z(i) = Z(k)
         Z(i-1) = Z(j)
         Z(i-2) = Z(j-1)
         i = i - 3
         j = j - 2
         k = k - 2
      ENDDO
!
!     HERE WE HAVE AN ARRAY OF EXTERNAL-INTERNAL-CODED SIL. PRINT IT OUT
!
      nlpx = nlpx*3
      n = nlpx*3
      n3 = n3 - 1
      DO i = 1 , n3 , n
         CALL page1
         WRITE (L,99002)
99002    FORMAT (/44X,44HTABLE OF EXTERNAL-INTERNAL-CODED SIL NUMBERS,//10X,3(6X,30HEXTERNAL  INTERNAL CODED SIL  ),/10X,           &
                &3(5X,3(10H--------- ),1X))
         im1 = i - 1
         DO j = 1 , nlpx , 3
            j1 = im1 + j
            j2 = j1 + nlpx
            j3 = j2 + nlpx
            IF ( j3<=n3 ) WRITE (L,99004) Z(j1) , Z(j1+1) , Z(j1+2) , Z(j2) , Z(j2+1) , Z(j2+2) , Z(j3) , Z(j3+1) , Z(j3+2)
            IF ( j3>n3 .AND. j2<=n3 ) WRITE (L,99004) Z(j1) , Z(j1+1) , Z(j1+2) , Z(j2) , Z(j2+1) , Z(j2+2)
            IF ( j2>n3 .AND. j1<=n3 ) WRITE (L,99004) Z(j1) , Z(j1+1) , Z(j1+2)
         ENDDO
      ENDDO
!
      WRITE (L,99003)
99003 FORMAT (//10X,33H*** JOB TERMINATED BY DIAG 20 ***)
      CALL pexit
   ENDIF
!
 100  n = -1
   GOTO 400
 200  n = -2
   GOTO 400
 300  n = -7
 400  CALL mesage(n,file,nam)
99004 FORMAT (10X,3(4X,3I10,2X))
END SUBROUTINE diag36
