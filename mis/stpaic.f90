
SUBROUTINE stpaic(Bloc,Dy,Nsize,Gap,Bm,Gm,Pm,Ns,Cla,Ajjl)
   IMPLICIT NONE
   REAL Bb(4) , Beta(4) , Bref , Clam , Dum , Ekr(1) , Fm , Fmach , Refc , Rfk , Tskj(7)
   COMPLEX Ekm(4,4)
   INTEGER Ii , Incr , Isk , Iti , Ito , Mcb(7) , Ncirc , Nd , Ne , Nn , Nncirc , Nns , Nrow , Nsk
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /stripc/ Nns , Bref , Clam , Fm , Ncirc , Nncirc , Ekr , Dum , Bb , Beta , Ekm
   REAL Ajjl
   INTEGER Ns
   REAL Bloc(1) , Bm(4,4,Ns) , Cla(1) , Dy(1) , Gap(1) , Gm(4,3,Ns) , Pm(37,Ns)
   INTEGER Nsize(1)
   REAL bob , ci , const , cr , ekl , tsr
   COMPLEX cdum(4,4) , ch(3,3)
   INTEGER i , im , j , j1 , jm , k , m , n , nopen , nsted
   k = 1
   Ii = Nrow + 1
   Nn = Nrow
   IF ( Ekr(1)<=.00001 ) Ekr(1) = 0.0
   nsted = 0
   IF ( Ekr(k)==0.0 ) nsted = 1
   DO n = 1 , Ns
      bob = Bloc(n)/Bref
      ekl = Ekr(k)*bob
      const = Cla(n)*Dy(n)*Clam
      cr = Fm
      IF ( Ncirc/=0 ) cr = Bb(1)
      ci = 0.
      nopen = 0
      IF ( Nsize(n)==3 .AND. Gap(n)==0.0 ) nopen = 1
      tsr = 0.5*Gap(n)/Bloc(n)
      im = Nsize(n)
      IF ( im<3 ) THEN
         jm = 2
         j1 = 2
      ELSE
         jm = 4
         j1 = 3
      ENDIF
      CALL stpk(ekl,n,Nsize(n),nopen,nsted,tsr,Pm(1,n),cr,ci,im,j1)
      DO i = 1 , im
         DO j = 1 , jm
            cdum(i,j) = cmplx(0.0,0.0)
            DO m = 1 , jm
               cdum(i,j) = cdum(i,j) + Bm(i,m,n)*Ekm(m,j)
            ENDDO
         ENDDO
      ENDDO
      DO i = 1 , im
         DO j = 1 , j1
            ch(i,j) = cmplx(0.0,0.0)
            DO m = 1 , jm
               ch(i,j) = ch(i,j) + cdum(i,m)*Gm(m,j,n)
            ENDDO
            ch(i,j) = const*ch(i,j)
         ENDDO
      ENDDO
      Nn = Nn + im
      DO i = 1 , im
         CALL pack(ch(1,i),Ajjl,Mcb)
      ENDDO
      Ii = Ii + im
   ENDDO
END SUBROUTINE stpaic