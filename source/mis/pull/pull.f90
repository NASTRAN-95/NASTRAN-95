!*==pull.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pull(Bcd,Out,Icol,Nchar,Flag)
   IMPLICIT NONE
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Bcd
   INTEGER , DIMENSION(1) :: Out
   INTEGER :: Icol
   INTEGER :: Nchar
   INTEGER :: Flag
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , cperwd
   LOGICAL , SAVE :: first
   INTEGER :: i , ib1 , ibcd , ibl , itemp , iwd , ixtra , lword , m1 , m2 , nbl , nwds , nx
   EXTERNAL klshft , krshft , orf
!
! End of declarations rewritten by SPAG
!
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
