!*==eqout1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE eqout1(Ia,Len1,Ns,Len2,Isil)
   USE c_cmb002
   USE c_cmb003
   USE c_machin
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ia
   INTEGER :: Len1
   INTEGER , DIMENSION(1) :: Ns
   INTEGER :: Len2
   INTEGER :: Isil
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icode , idbas , ifirst , ips , isub , j , jj , k
   INTEGER , DIMENSION(2) :: ibits
   INTEGER , SAVE :: iblank
   INTEGER , DIMENSION(17) :: n1
   INTEGER , DIMENSION(14) :: n2
   EXTERNAL bitpat , lshift , page , push , rshift , sort
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES OUTPUT ENTRIES FOR CONNECTION TRACE
!
   DATA iblank/4H    /
!
!     SORT ON PSEUDOSTRUCTURE NUMBER
!
   ifirst = 1
   DO k = 1 , 17
      n1(k) = iblank
   ENDDO
   DO k = 1 , 14
      n2(k) = iblank
   ENDDO
   CALL sort(0,0,4,1,Ia(1),Len1)
   j = 1
   n1(1) = Ia(j+2)
   icode = Ia(j+3)
   CALL bitpat(icode,ibits)
   DO i = 1 , 2
      n1(i+1) = ibits(i)
   ENDDO
   SPAG_Loop_1_1: DO
      ips = rshift(Ia(j),ihalf)
      isub = 2*(ips-1) + 4
      idbas = Ia(j) - lshift(ips,ihalf)
      n1(isub) = Ns(2*idbas-1)
      n1(isub+1) = Ns(2*idbas)
      Ia(j) = -Ia(j)
      CALL push(Ia(j+1),n2(2*ips-1),1,8,1)
      jj = j
      DO WHILE ( jj+4<=Len1 )
         IF ( Ia(jj+4)<=0 ) THEN
            jj = jj + 4
         ELSEIF ( rshift(iabs(Ia(j)),ihalf)/=rshift(Ia(jj+4),ihalf) ) THEN
            j = jj + 4
            CYCLE SPAG_Loop_1_1
         ELSE
            jj = jj + 4
         ENDIF
      ENDDO
!
!     WRITE OUTPUT
!
      nline = nline + 3
      IF ( nline>nlpp ) THEN
         CALL page
         nline = nline + 3
      ENDIF
      j = 3 + 2*npsub
      IF ( ifirst==1 ) WRITE (outt,99001) n1(1) , Isil , (n1(k),k=2,j)
99001 FORMAT (8X,I6,6X,I6,8X,A4,A2,7(3X,2A4))
      IF ( ifirst==0 ) WRITE (outt,99002) (n1(k),k=4,j)
99002 FORMAT (/40X,7(3X,2A4))
      WRITE (outt,99003) (n2(k),k=1,14)
99003 FORMAT (40X,7(3X,2A4))
      ifirst = 0
      j = -3
      SPAG_Loop_2_2: DO
         j = j + 4
         IF ( j>Len1 ) THEN
            WRITE (outt,99004)
99004       FORMAT (7X,4H  --,27(4H----),4H-   )
            EXIT SPAG_Loop_2_2
         ELSEIF ( Ia(j)>0 ) THEN
            DO k = 1 , 17
               n1(k) = iblank
            ENDDO
            DO k = 1 , 14
               n2(k) = iblank
            ENDDO
            CYCLE SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_2_2
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE eqout1
