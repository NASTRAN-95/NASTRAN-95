
SUBROUTINE pull(Bcd,Out,Icol,Nchar,Flag)
   IMPLICIT NONE
   INTEGER Idum(38) , Nbpc , Nbpw , Ncpw
   COMMON /system/ Idum , Nbpc , Nbpw , Ncpw
   INTEGER Flag , Icol , Nchar
   INTEGER Bcd(1) , Out(1)
   INTEGER blank , cperwd , i , ib1 , ibcd , ibl , itemp , iwd , ixtra , lword , m1 , m2 , nbl , nwds , nx
   LOGICAL first
   INTEGER klshft , krshft , orf
   EXTERNAL orf
!
!     THIS ROUTINE EXTRACTS BCD DATA (OUT) FROM A STRING,(BCD)
!     STARTING AT POSITION ICOL
!
   DATA cperwd/4/ , blank/4H    / , first/.TRUE./
!
   nwds = (Nchar-1)/cperwd + 1
   IF ( first ) THEN
      first = .FALSE.
      nx = Ncpw - cperwd
      ixtra = nx*Nbpc
      ibl = 0
      ib1 = krshft(blank,Ncpw-1)
      IF ( nx/=0 ) THEN
         DO i = 1 , nx
            ibl = orf(ibl,klshft(ib1,i-1))
         ENDDO
      ENDIF
   ENDIF
   DO i = 1 , nwds
      Out(i) = ibl
   ENDDO
!
   iwd = (Icol-1)/cperwd + 1
   m1 = (Icol-(iwd-1)*cperwd-1)*Nbpc
   m2 = cperwd*Nbpc - m1
!
   DO i = 1 , nwds
      ibcd = iwd + i - 1
      itemp = krshft(Bcd(ibcd),ixtra/Nbpc)
      Out(i) = orf(Out(i),klshft(itemp,(m1+ixtra)/Nbpc))
      itemp = krshft(Bcd(ibcd+1),(m2+ixtra)/Nbpc)
      Out(i) = orf(Out(i),klshft(itemp,ixtra/Nbpc))
   ENDDO
   IF ( nwds*cperwd==Nchar ) RETURN
!
!     REMOVE EXTRA CHARACTERS FROM LAST OUT WORD
!
   nbl = (nwds-1)*cperwd + Ncpw - Nchar
   lword = krshft(Out(nwds),nbl)
   Out(nwds) = klshft(lword,nbl)
!
   itemp = 0
   DO i = 1 , nbl
      itemp = orf(itemp,klshft(ib1,i-1))
   ENDDO
   Out(nwds) = orf(Out(nwds),itemp)
!
END SUBROUTINE pull