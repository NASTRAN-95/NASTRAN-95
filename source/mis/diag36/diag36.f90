!*==diag36.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE diag36(Z,Buf,Gpl,Sil,Eqexin)
   USE c_names
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Z
   INTEGER :: Buf
   INTEGER :: Gpl
   INTEGER :: Sil
   INTEGER :: Eqexin
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , im1 , j , j1 , j2 , j3 , k , n , n1 , n2 , n3 , nlpx
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL close , fwdrec , mesage , open , page1 , pexit , rdtrl , read , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE PRINTS THE INTERNAL-EXTERNAL-SIL NOS. OF THE GRID
!     POINTS AND SCALAR POINTS, AS REQUESTED BY DIAG 36
!
   DATA nam/4HDIAG , 4H34  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
               CALL open(*20,file,Z(Buf),rdrew)
               CALL fwdrec(*40,file)
               CALL read(*20,*60,file,Z(n),n1,1,j)
               CALL close(file,rew)
               file = Sil
               n = n + n1
            ENDDO
!
!     HERE WE HAVE, IN INTERNAL NUMBER ORDER,
!        Z(   1 THRU N1) = EXTERNAL NOS.
!        Z(N1+1 THRU N2) = SIL NOS.
!
            nlpx = nlpp - 8
            n = nlpx*3
            DO i = 1 , n1 , n
               CALL page1
               WRITE (l,99001)
99001          FORMAT (/46X,38HTABLE OF INTERNAL-EXTERNAL-SIL NUMBERS,//10X,3(6X,30HINTERNAL  EXTERNAL      SIL   ),/10X,           &
                      &3(6X,3(10H--------  )))
               im1 = i - 1
               DO j = 1 , nlpx
                  j1 = im1 + j
                  j2 = j1 + nlpx
                  j3 = j2 + nlpx
                  IF ( j3<=n1 ) WRITE (l,99004) j1 , Z(j1) , Z(j1+n1) , j2 , Z(j2) , Z(j2+n1) , j3 , Z(j3) , Z(j3+n1)
                  IF ( j3>n1 .AND. j2<=n1 ) WRITE (l,99004) j1 , Z(j1) , Z(j1+n1) , j2 , Z(j2) , Z(j2+n1)
                  IF ( j2>n1 .AND. j1<=n1 ) WRITE (l,99004) j1 , Z(j1) , Z(j1+n1)
               ENDDO
            ENDDO
!
            CALL sswtch(20,j)
            IF ( j==0 ) RETURN
!
            file = Eqexin
            CALL open(*20,file,Z(Buf),rdrew)
            CALL fwdrec(*40,file)
            CALL read(*20,*60,file,Z(1),n2,1,j)
            CALL read(*20,*60,file,Z(n3),n2,1,j)
            CALL close(file,rew)
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
               WRITE (l,99002)
99002          FORMAT (/44X,44HTABLE OF EXTERNAL-INTERNAL-CODED SIL NUMBERS,//10X,3(6X,30HEXTERNAL  INTERNAL CODED SIL  ),/10X,     &
                     & 3(5X,3(10H--------- ),1X))
               im1 = i - 1
               DO j = 1 , nlpx , 3
                  j1 = im1 + j
                  j2 = j1 + nlpx
                  j3 = j2 + nlpx
                  IF ( j3<=n3 ) WRITE (l,99004) Z(j1) , Z(j1+1) , Z(j1+2) , Z(j2) , Z(j2+1) , Z(j2+2) , Z(j3) , Z(j3+1) , Z(j3+2)
                  IF ( j3>n3 .AND. j2<=n3 ) WRITE (l,99004) Z(j1) , Z(j1+1) , Z(j1+2) , Z(j2) , Z(j2+1) , Z(j2+2)
                  IF ( j2>n3 .AND. j1<=n3 ) WRITE (l,99004) Z(j1) , Z(j1+1) , Z(j1+2)
               ENDDO
            ENDDO
!
            WRITE (l,99003)
99003       FORMAT (//10X,33H*** JOB TERMINATED BY DIAG 20 ***)
            CALL pexit
         ENDIF
!
 20      n = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      n = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      n = -7
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (10X,3(4X,3I10,2X))
END SUBROUTINE diag36
