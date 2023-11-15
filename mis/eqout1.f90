
SUBROUTINE eqout1(Ia,Len1,Ns,Len2,Isil)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Conset , Toler
   INTEGER Iauto , Icomb(7,5) , Ihalf , Junk(8) , Junk1(8) , Junk2(2) , Mach , Nline , Nlpp , Npsub , Outt
   COMMON /cmb002/ Junk , Outt
   COMMON /cmb003/ Icomb , Conset , Iauto , Toler , Npsub
   COMMON /machin/ Mach , Ihalf
   COMMON /system/ Junk1 , Nlpp , Junk2 , Nline
!
! Dummy argument declarations
!
   INTEGER Isil , Len1 , Len2
   INTEGER Ia(1) , Ns(1)
!
! Local variable declarations
!
   INTEGER i , ibits(2) , iblank , icode , idbas , ifirst , ips , isub , j , jj , k , n1(17) , n2(14)
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
! End of declarations
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
 100  ips = rshift(Ia(j),Ihalf)
   isub = 2*(ips-1) + 4
   idbas = Ia(j) - lshift(ips,Ihalf)
   n1(isub) = Ns(2*idbas-1)
   n1(isub+1) = Ns(2*idbas)
   Ia(j) = -Ia(j)
   CALL push(Ia(j+1),n2(2*ips-1),1,8,1)
   jj = j
   DO WHILE ( jj+4<=Len1 )
      IF ( Ia(jj+4)<=0 ) THEN
         jj = jj + 4
      ELSEIF ( rshift(iabs(Ia(j)),Ihalf)/=rshift(Ia(jj+4),Ihalf) ) THEN
         j = jj + 4
         GOTO 100
      ELSE
         jj = jj + 4
      ENDIF
   ENDDO
!
!     WRITE OUTPUT
!
   Nline = Nline + 3
   IF ( Nline>Nlpp ) THEN
      CALL page
      Nline = Nline + 3
   ENDIF
   j = 3 + 2*Npsub
   IF ( ifirst==1 ) WRITE (Outt,99001) n1(1) , Isil , (n1(k),k=2,j)
99001 FORMAT (8X,I6,6X,I6,8X,A4,A2,7(3X,2A4))
   IF ( ifirst==0 ) WRITE (Outt,99002) (n1(k),k=4,j)
99002 FORMAT (/40X,7(3X,2A4))
   WRITE (Outt,99003) (n2(k),k=1,14)
99003 FORMAT (40X,7(3X,2A4))
   ifirst = 0
   j = -3
   DO
      j = j + 4
      IF ( j>Len1 ) THEN
         WRITE (Outt,99004)
99004    FORMAT (7X,4H  --,27(4H----),4H-   )
         EXIT
      ELSEIF ( Ia(j)>0 ) THEN
         DO k = 1 , 17
            n1(k) = iblank
         ENDDO
         DO k = 1 , 14
            n2(k) = iblank
         ENDDO
         GOTO 100
      ENDIF
   ENDDO
END SUBROUTINE eqout1
