
SUBROUTINE feer2(Iret)
   IMPLICIT NONE
   REAL Critf , Dmpfle , Dumm(12) , Power , Xlmbda , Z(1)
   DOUBLE PRECISION Det , Detc , Mindd
   INTEGER Filea(7) , Filel(7) , Fileu(7) , Ibk , Ichl , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset ,    &
         & Ij(8) , Ik(5) , Isr1fl , Isr2fl , Isr3fl , Ksystm(54) , Lowtri , Mcblt(7) , Mcbsma(7) , Mord , Neig , Nord , Northo ,    &
         & Nz , Prec , Rdp , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle , Uprtri
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerxx/ Dumm , Ifset
   COMMON /names / Ij , Rdp , Ik , Lowtri , Uprtri
   COMMON /opinv / Mcblt , Mcbsma
   COMMON /sfact / Filea , Filel , Fileu , Isr1fl , Isr2fl , Nz , Det , Detc , Power , Isr3fl , Mindd , Ichl
   COMMON /system/ Ksystm , Prec
   COMMON /zzzzzz/ Z
   INTEGER Iret
   INTEGER i
   INTEGER korsz
!
!     FEER2 INITIALIZES THEN CALLS  SDCOMP
!
!
!
   Iret = 0
!
   Filea(1) = Iflelm(1)
   Filel(1) = Iflvec(1)
   Fileu(1) = Sr3fle
   Isr1fl = Sr4fle
   Isr2fl = Sr5fle
   Isr3fl = Sr6fle
   Ichl = 0
   IF ( Ibk==1 .OR. Ifset==1 ) Ichl = 1
   Filea(2) = Ifkaa(2)
   Filea(3) = Ifkaa(3)
   Filea(4) = Ifkaa(4)
   Filea(5) = Prec
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = Prec
!
!     SYMMETRIC DECOMPOSITION
!
   Nz = korsz(Z)
   CALL sdcomp(*200,Z,Z,Z)
 100  Filel(3) = Filel(2)
   Filel(4) = Lowtri
   CALL wrttrl(Filel)
   DO i = 1 , 7
      Mcblt(i) = Filel(i)
   ENDDO
   RETURN
!
 200  Iret = 1
   GOTO 100
END SUBROUTINE feer2