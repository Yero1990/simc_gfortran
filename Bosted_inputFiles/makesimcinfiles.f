      program makeinfile_pi
      implicit none
      integer irun,virun(10000),i,j,k,it,vit(10000),itrig,icoin
      integer index(10000),nrun_tot,jj,nruns
      real*8 ebeam,phms,thhms
      real*8 vebeam(10000),vphms(10000),vthhms(10000)
      real*8 vpshms(10000),vthshms(10000)
      real*8 pshms,thshms,current,curcut,egammax
      real*8 bcm1,bcm2,livetime,tkeffp,tkeffe
      integer elcleane,elcleanp,ratep,ratee
      real*8 bcm1p,pp,thp,chrg,tefe,tefp,dt
      real*8 hmp_x, hmp_y, pmp_x, pmp_y,z,zp
      integer nrun_first,nrun_last,rseed,epelas

      real*8 tgt_A,tgt_z,tgt_amu,tgt_rho,tgt_t

      character*80 fname
      character*8    crun
      character*80 junk

      open(unit=8,file='corrrunlistnew.txt')

      open(unit=50,file='simcsp18.json')
       write(50,198,advance="no")
 198   format('{"name": "simcsp18", "jobs": [')

      read(8,*) junk

      do i=1,10000
c      do i=1,2
      read(8,'(i4,i2,10f7.3,2i8,2i10)',END=1000,err=1000) 
     > virun(i),vit(i),vebeam(i),vphms(i),vthhms(i),
     > vpshms(i),vthshms(i),bcm1,livetime,tkeffe,
     > tkeffp,curcut,ratee,ratep,elcleane,elcleanp

       index(i)=i
       irun=virun(index(i))

c select runs to do
       if(irun.lt.6010 .or. irun.gt.6017) goto 299
c       if(irun.gt.4400) goto 299
c       if(irun.ne.5415) goto 299

       it=vit(index(i))
       ebeam=vebeam(index(i))
       phms=vphms(index(i))
       pshms=vpshms(index(i))
       thhms=vthhms(index(i))
       thshms=vthshms(index(i))
       z = abs(pshms) / (ebeam - abs(phms))
       zp = abs(phms) / (ebeam - abs(pshms))

c if just doing exclusive
c       if(z.lt.0.89) goto 299

c get mis-pointing
       if(thhms.lt.40.) then
        hmp_y = 0.1 * (0.52 - 0.012*thhms + 0.002 * thhms**2)
       else 
        hmp_y = 0.1 * (0.52 - 0.012 * 40. + 0.002 * 40.**2)
       endif
       pmp_y = 0.1 * (-0.6)
       if (thhms < 50) then
        hmp_x = 0.1 * (2.37 - 0.086 * thhms + 0.0012 * thhms**2)
       else
        hmp_x = 0.1 * (2.37 - 0.086 * 50.   + 0.0012 * 50.**2)
       endif
       pmp_x = 0.1 * (-1.26);
           
       write(crun,'(i4)') irun
       fname='scripts/run'//trim(crun)
       open(unit=51,file=fname)
c if  just exclusive!
c       write(51,122) irun
       epelas = 0 
       if(irun.ge.6010 .and. irun.le.6017) then
        epelas = 1
        write(51,1222) irun,irun
 1222   format(
     >  '#!/bin/tcsh'/
     >  'cd /u/group/c-sidis/bosted/simc'/
     >  'setenv SIMCIN simc_',i4,'_pi_rad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_endcap.inp'/
     >  './simc')
       else
        write(51,122) irun,irun,irun,irun,irun,irun,irun
 122    format(
     >  '#!/bin/tcsh'/
     >  'cd /u/group/c-sidis/bosted/simc'/
     >  'setenv SIMCIN simc_',i4,'_pi_rad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_pi_norad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_pi_excl.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_pi_rho.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_endcap.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_k_rad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simc_',i4,'_k_norad.inp'/
     >  './simc')
       endif
c add neg. pol. SHMS runs
       if(pshms.lt.0.) write(51,123) irun,irun,irun,irun
 123   format(
     >  'setenv SIMCIN simcp_',i4,'_pi_rad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simcp_',i4,'_pi_norad.inp'/
     >  './simc'/
     >  'setenv SIMCIN simcp_',i4,'_pi_rho.inp'/
     >  './simc'/
     >  'setenv SIMCIN simcp_',i4,'_endcap.inp'/
     >  './simc'/
     >  'setenv SIMCIN simcp_',i4,'_pi_excl.inp'/
     >  './simc')

      fname='infiles/simc_'//trim(crun)//'_pi_rad.inp'
      open(unit=16,file=fname)
      fname='infiles/simc_'//trim(crun)//'_pi_norad.inp'
      open(unit=17,file=fname)
      fname='infiles/simc_'//trim(crun)//'_k_rad.inp'
      open(unit=18,file=fname)
      fname='infiles/simc_'//trim(crun)//'_k_norad.inp'
      open(unit=19,file=fname)
      fname='infiles/simc_'//trim(crun)//'_pi_excl.inp'
      open(unit=20,file=fname)
      fname='infiles/simc_'//trim(crun)//'_pi_rho.inp'
      open(unit=21,file=fname)
      fname='infiles/simc_'//trim(crun)//'_endcap.inp'
      open(unit=22,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_pi_rad.inp'
      open(unit=26,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_pi_norad.inp'
      open(unit=27,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_k_rad.inp'
      open(unit=28,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_k_norad.inp'
      open(unit=29,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_pi_excl.inp'
      open(unit=30,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_pi_rho.inp'
      open(unit=31,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_endcap.inp'
      open(unit=32,file=fname)
      fname='infiles/simcp_'//trim(crun)//'_pi_rad.inp'

      do jj=1,7
       If(it.eq.1) then
        tgt_A = 1.
        tgt_z = 1.
        tgt_amu = 1.007276
        tgt_rho = 0.07231
        tgt_t = 723.1
       endif
       if(it.eq.2) then
        tgt_A = 2.
        tgt_z = 1.
        tgt_amu = 2.01355
        tgt_rho = 0.167
        tgt_t = 1670.
       endif
c j=7 always does a dummy target pi sidsi with rad.
       if(it.eq.3 .or. jj.eq.7) then
        tgt_A = 27.
        tgt_z = 13.
        tgt_amu = 26.98154
        tgt_rho = 2.700
        tgt_t = 362.000
        tgt_t = 362.000 / 2.
       endif

       write(15+jj,'("; This is a CTP file")')
       write(15+jj,'(" ")')
       write(15+jj,'("begin parm experiment")')
       write(25+jj,'("; This is a CTP file")')
       write(25+jj,'(" ")')
       write(25+jj,'("begin parm experiment")')
c pi rad (or ep elastic rad)
       if(jj.eq.1) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =  90000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  25000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        IF(zp .lt. 0.82) then
         write(25+jj,'("  ngen =  90000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =  25000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c pi norad
       if(jj.eq.2) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =  20000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  5000 	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen = 20000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =  5000 	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c k rad
       if(jj.eq.3) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =  20000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  100    ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen =  20000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =  100    ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c k norad
       if(jj.eq.4) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =  5000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  100    ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen =  5000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =  100    ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c pi excl
       if(jj.eq.5) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =   5000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  20000   ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen =  3000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =  20000   ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c rho
       if(jj.eq.6) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen =  2000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =   100     ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen =  2000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =   100     ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif
c endcap
       if(jj.eq.7) then
        if(z.lt.0.82) then
         write(15+jj,'("  ngen = 10000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(15+jj,'("  ngen =  5000   ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
        if(zp.lt.0.82) then
         write(25+jj,'("  ngen =  5000	        ;  POS: # of successes; 
     >      NEG: # of tries")')
        else
         write(25+jj,'("  ngen =   500   ;  POS: # of successes; 
     >      NEG: # of tries")')
        endif
       endif

       write(15+jj,'("EXPER%charge = 1.0      ;  total charge (mC)")' )
       write(25+jj,'("EXPER%charge = 1.0      ;  total charge (mC)")' )
       write(15+jj,'("doing_phsp = 0		;  (ONE = TRUE)")' )
       write(25+jj,'("doing_phsp = 0		;  (ONE = TRUE)")' )
       if(jj.eq.1.or.jj.eq.2.or.jj.ge.5) then
        write(15+jj,'("doing_kaon = 0		;  (ONE = TRUE) ")' )
        write(25+jj,'("doing_kaon = 0		;  (ONE = TRUE) ")' )
       else
        write(15+jj,'("doing_kaon = 1		;  (ONE = TRUE) ")' )
        write(25+jj,'("doing_kaon = 1		;  (ONE = TRUE) ")' )
       endif

       if(jj.ne.6 .and. epelas.eq.0) then
         write(15+jj,'("doing_pion = 1		;  (ONE = TRUE)" )' )
         write(25+jj,'("doing_pion = 1		;  (ONE = TRUE)" )' )
       else
         write(15+jj,'("doing_pion = 0		;  (ONE = TRUE)" )' )
         write(25+jj,'("doing_pion = 0		;  (ONE = TRUE)" )' )
       endif
      
       if(pshms.gt.0) then
        write(15+jj,'("which_pion = 0          ;  (0=p->pi+,1=n->pi-,10/11 for 
     >   pi+/pi- coherent)")')
        write(25+jj,'("which_pion = 0          ;  (0=p->pi+,1=n->pi-,10/11 for 
     >   pi+/pi- coherent)")')
       else
         write(15+jj,'("which_pion = 1          ;  (0=p->pi+,1=n->pi-,10/11 for 
     >    pi+/pi- coherent)")')
         write(25+jj,'("which_pion = 1          ;  (0=p->pi+,1=n->pi-,10/11 for 
     >    pi+/pi- coherent)")')
       endif


       write(15+jj,'("doing_delta = 0         ; H(e,ep)pi0 ")' ) 
       write(25+jj,'("doing_delta = 0         ; H(e,ep)pi0 ")' ) 
       if(jj.ne.6) then
        write(15+jj,'("doing_rho = 0           ; exclusive rho 
     >   production")')
        write(25+jj,'("doing_rho = 0           ; exclusive rho 
     >   production")')
       else
        write(15+jj,'("doing_rho = 1           ; exclusive rho 
     >   production")')
        write(25+jj,'("doing_rho = 1           ; exclusive rho 
     >   production")')
       endif
       if(jj.ne.5 .and. jj.ne.6.and.epelas.eq.0) then
        write(15+jj,'("doing_semi = 1          ; doing semi-inclusive? Need 
     >  to set doing_pion or doing_kaon ")') 
        write(25+jj,'("doing_semi = 1          ; doing semi-inclusive? Need 
     >  to set doing_pion or doing_kaon ")') 
       else
        write(15+jj,'("doing_semi = 0          ; doing semi-inclusive? Need 
     >  to set doing_pion or doing_kaon ")') 
        write(15+jj,'("doing_semi = 0          ; doing semi-inclusive? Need 
     >  to set doing_pion or doing_kaon ")') 
       endif
       if(pshms.gt.0) then
        write(15+jj,'("doing_hplus = 1	        ; positive hadrons? (only 
     >   for semi or rho)" )' )
       else
        write(15+jj,'("doing_hplus = 0	        ; positive hadrons? (only 
     >   for semi or rho)" )' )
       endif
       if(phms.gt.0) then
        write(25+jj,'("doing_hplus = 1	        ; positive hadrons? (only 
     >   for semi or rho)" )' )
       else
        write(25+jj,'("doing_hplus = 0	        ; positive hadrons? (only 
     >   for semi or rho)" )' )
       endif

       if(epelas.eq.0) then
        write(15+jj,'("doing_decay = 1		;  1=decay ON, 0=decay OFF ")')
        write(25+jj,'("doing_decay = 1		;  1=decay ON, 0=decay OFF ")')
       else
        write(15+jj,'("doing_decay = 0		;  1=decay ON, 0=decay OFF ")')
        write(25+jj,'("doing_decay = 0		;  1=decay ON, 0=decay OFF ")')
       endif
       if(jj.eq.1.or.jj.eq.2.or.jj.ge.5) then
        write(15+jj,'("ctau = 780.4	        ;  decay length (cm) ")' )
        write(25+jj,'("ctau = 780.4	        ;  decay length (cm) ")' )
       else
        write(15+jj,'("ctau = 372.	        ;  decay length (cm) ")' )
        write(25+jj,'("ctau = 372.	        ;  decay length (cm) ")' )
       endif

       write(15+jj,'("end parm experiment")' )
       write(15+jj,'(" ")')
       write(25+jj,'("end parm experiment")' )
       write(25+jj,'(" ")')

       write(15+jj,'("begin parm kinematics_main")' )
       write(15+jj,'("Ebeam = ",f9.2)') ebeam*1000.0
       write(15+jj,'("dEbeam = 0.05	         ;  beam energy variation 
     > (%)")' )
       write(25+jj,'("begin parm kinematics_main")' )
       write(25+jj,'("Ebeam = ",f9.2)') ebeam*1000.0
       write(25+jj,'("dEbeam = 0.05	         ;  beam energy variation 
     > (%)")' )

       write(15+jj,'("electron_arm = 1         ; 1=hms,2=sos,3=hrsr,4=hrsl,
     > 5=shms,7=calo (7=calo HMS side),")')
       write(25+jj,'("electron_arm = 5         ; 1=hms,2=sos,3=hrsr,4=hrsl,
     > 5=shms,7=calo (7=calo HMS side),")')
       write(15+jj,'("hadron_arm = 5           ; 1=hms,2=sos,3=hrsr,4=hrsl,
     > 5=shms,7=calo (8=calo (SOS side)")' )
       write(25+jj,'("hadron_arm = 1           ; 1=hms,2=sos,3=hrsr,4=hrsl,
     > 5=shms,7=calo (8=calo (SOS side)")' )

! Noble gas cer. only in for spring18
       if(irun.lt.4400) then
        write(15+jj,'("use_first_cer = 1        ; Use first cerenkov 
     >  in shms?")') 
        write(25+jj,'("use_first_cer = 1        ; Use first cerenkov 
     >  in shms?")') 
       else
        write(15+jj,'("use_first_cer = 0        ; Use first cerenkov 
     >  in shms?")') 
        write(25+jj,'("use_first_cer = 0        ; Use first cerenkov 
     >  in shms?")') 
       endif
       write(15+jj,'("spec%e%P = ",f9.2)') abs(phms)*1000.0
c		 ;  e arm central momentum (MeV/c)" )' )
       write(15+jj,'("spec%e%theta = ",f6.3)') thhms		
c                ;  e arm angle setting (degrees) ")' )
       write(15+jj,'("spec%p%P = ",f9.2)') abs(pshms)*1000.0		
c                ;  p arm central momentum (MeV/c) ")' )
       write(15+jj,'("spec%p%theta = ",f6.3)') thshms		
c                ;  p arm angle setting (degrees)
       write(25+jj,'("spec%e%P = ",f9.2)') abs(pshms)*1000.0
c		 ;  e arm central momentum (MeV/c)" )' )
       write(25+jj,'("spec%e%theta = ",f6.3)') thshms		
c                ;  e arm angle setting (degrees) ")' )
       write(25+jj,'("spec%p%P = ",f9.2)') abs(phms)*1000.0		
c                ;  p arm central momentum (MeV/c) ")' )
       write(25+jj,'("spec%p%theta = ",f6.3)') thhms		
c                ;  p arm angle setting (degrees)

       write(15+jj,'("end parm kinematics_main")' )
       write(15+jj,'("  ")' )
       write(25+jj,'("end parm kinematics_main")' )
       write(25+jj,'("  ")' )

       write(15+jj,'("begin parm target")' )
       write(15+jj,'("targ%A = ",f4.1)') tgt_A			
c                ;  target A
       write(15+jj,'("targ%Z = ",f4.1)') tgt_z			
c                ;  target Z
       write(15+jj,'("targ%mass_amu = ",f9.6)') tgt_amu
c                ;  target mass in amu
       write(15+jj,'("targ%mrec_amu = 0.0      ;  recoil mass in amu (eep=A-1 
     > system,pion=A-2)")' )
       write(15+jj,'("targ%rho = ",f6.4)' ) tgt_rho 
c                ;  target density (g/cm^3)
       write(15+jj,'("targ%thick = ",f8.3)' ) tgt_t
c		 ;  target thick (mg/cm^2) for 10 cm target
       write(15+jj,'("targ%angle = 0.	         ; target angle (for solid 
     > target) (degrees)")' )
       write(15+jj,'("targ%abundancy = 100.0	 ;  target purity (%)")' )
       write(15+jj,'("targ%can = 1		 ;  1=beer can (fpi), 
     >   2=pudding can (nucpi)" )' )
       write(15+jj,'("end parm target")' )
       write(15+jj,'("  ")' )

       write(25+jj,'("begin parm target")' )
       write(25+jj,'("targ%A = ",f4.1)') tgt_A			
c                ;  target A
       write(25+jj,'("targ%Z = ",f4.1)') tgt_z			
c                ;  target Z
       write(25+jj,'("targ%mass_amu = ",f9.6)') tgt_amu
c                ;  target mass in amu
       write(25+jj,'("targ%mrec_amu = 0.0      ;  recoil mass in amu (eep=A-1 
     > system,pion=A-2)")' )
       write(25+jj,'("targ%rho = ",f6.4)' ) tgt_rho 
c                ;  target density (g/cm^3)
       write(25+jj,'("targ%thick = ",f8.3)' ) tgt_t
c		 ;  target thick (mg/cm^2) for 10 cm target
       write(25+jj,'("targ%angle = 0.	         ; target angle (for solid 
     > target) (degrees)")' )
       write(25+jj,'("targ%abundancy = 100.0	 ;  target purity (%)")' )
       write(25+jj,'("targ%can = 1		 ;  1=beer can (fpi), 
     >   2=pudding can (nucpi)" )' )
       write(15+jj,'("end parm target")' )
       write(15+jj,'("  ")' )

       write(15+jj,'("begin parm debug         ; (ONES give helpful 
     > debug info)")')
       write(15+jj,'("debug(1) = 0	         ; turns on output from 
     > brem.f")')
       write(15+jj,'("debug(2) = 0		 ;  into/outa subs.")' )
       write(15+jj,'("debug(3) = 0		 ;  spit out values (init. 
     > and main loop).")' )
       write(15+jj,'("debug(4) = 0		 ;  mostly comp_ev, gen_rad 
     > diagnostics.")' )
       write(15+jj,'("debug(5) = 0		 ;  a bit of everything.")' )
       write(15+jj,'("end parm debug")' )
       write(15+jj,'(" ")' )
       write(25+jj,'("begin parm debug         ; (ONES give helpful 
     > debug info)")')
       write(25+jj,'("debug(1) = 0	         ; turns on output from 
     > brem.f")')
       write(25+jj,'("debug(2) = 0		 ;  into/outa subs.")' )
       write(25+jj,'("debug(3) = 0		 ;  spit out values (init. 
     > and main loop).")' )
       write(25+jj,'("debug(4) = 0		 ;  mostly comp_ev, gen_rad 
     > diagnostics.")' )
       write(25+jj,'("debug(5) = 0		 ;  a bit of everything.")' )
       write(25+jj,'("end parm debug")' )
       write(15+jj,'(" ")' )

       write(15+jj,'("begin parm e_arm_accept")' )
       write(15+jj,'("SPedge%e%delta%min = -15.0   ;  delta min (SPECTROMETER 
     > ACCEPTANCE !)")' )
       write(15+jj,'("SPedge%e%delta%max =  15.0   ;  delta max")' )
       write(15+jj,'("SPedge%e%yptar%min = -90.0   ; .yptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(15+jj,'("SPedge%e%yptar%max =  90.0   ; .yptar.max = 
     > {TF} / 1000")')
       write(15+jj,'("SPedge%e%xptar%min = -120.0  ; .xptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(15+jj,'("SPedge%e%xptar%max =  120.0  ; .xptar.max = 
     > {TF} / 1000" )' )
       write(15+jj,'("end parm e_arm_accept")' )
       write(15+jj,'(" ")' )

       write(15+jj,'("begin parm p_arm_accept")' )
       write(15+jj,'("SPedge%p%delta%min = -20.    ;  delta min (SPECTROMETER 
     > ACCEPTANCE !)")' )
       write(15+jj,'("SPedge%p%delta%max =  40.    ;  delta max")' )
       write(15+jj,'("SPedge%p%yptar%min = -90.0   ; .yptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(15+jj,'("SPedge%p%yptar%max =  90.0   ; .yptar.max = 
     > {TF} / 1000" )' )
       write(15+jj,'("SPedge%p%xptar%min = -100.0  ; .xptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(15+jj,'("SPedge%p%xptar%max =  100.0  ; .xptar.max = 
     > {TF} / 1000")' )
       write(15+jj,'("end parm p_arm_accept")' )
       write(15+jj,'(" ")')

       write(25+jj,'("begin parm p_arm_accept")' )
       write(25+jj,'("SPedge%p%delta%min = -15.0   ;  delta min (SPECTROMETER 
     > ACCEPTANCE !)")' )
       write(25+jj,'("SPedge%p%delta%max =  15.0   ;  delta max")' )
       write(25+jj,'("SPedge%p%yptar%min = -90.0   ; .yptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(25+jj,'("SPedge%p%yptar%max =  90.0   ; .yptar.max = 
     > {TF} / 1000")')
       write(25+jj,'("SPedge%p%xptar%min = -120.0  ; .xptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(25+jj,'("SPedge%p%xptar%max =  120.0  ; .xptar.max = 
     > {TF} / 1000" )' )
       write(25+jj,'("end parm p_arm_accept")' )
       write(25+jj,'(" ")' )

       write(25+jj,'("begin parm e_arm_accept")' )
       write(25+jj,'("SPedge%e%delta%min = -20.    ;  delta min (SPECTROMETER 
     > ACCEPTANCE !)")' )
       write(25+jj,'("SPedge%e%delta%max =  40.    ;  delta max")' )
       write(25+jj,'("SPedge%e%yptar%min = -90.0   ; .yptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(25+jj,'("SPedge%e%yptar%max =  90.0   ; .yptar.max = 
     > {TF} / 1000" )' )
       write(25+jj,'("SPedge%e%xptar%min = -100.0  ; .xptar.min = 
     > {TF} / 1000 (mrad)" )' )
       write(25+jj,'("SPedge%e%xptar%max =  100.0  ; .xptar.max = 
     > {TF} / 1000")' )
       write(25+jj,'("end parm e_arm_accept")' )
       write(25+jj,'(" ")')

       write(15+jj,'("begin parm beamandtargetinfo")' )
       write(25+jj,'("begin parm beamandtargetinfo")' )
       write(15+jj,'("gen%xwid = 0.008868	     ;  beam width - one 
     > sigma (cm)  (89microns)")' )
       write(25+jj,'("gen%xwid = 0.008868	     ;  beam width - one 
     > sigma (cm)  (89microns)")' )
       write(15+jj,'("gen%ywid = 0.004235	     ;  beam width - one 
     > sigma (cm)  (42microns)")' )
       write(25+jj,'("gen%ywid = 0.004235	     ;  beam width - one 
     > sigma (cm)  (42microns)")' )
c changed from 1 to 3
       write(15+jj,'("targ%fr_pattern = 3.	     ;  raster pattern: 1=square, 
     > 2=circular")' )
       write(25+jj,'("targ%fr_pattern = 3.	     ;  raster pattern: 1=square, 
     > 2=circular")' )
       write(15+jj,'("targ%fr1 = 0.1	             ; horizontal size OR 
     > inner radius(2)" )' )
       write(25+jj,'("targ%fr1 = 0.1	             ; horizontal size OR 
     > inner radius(2)" )' )
       write(15+jj,'("targ%fr2 = 0.1	             ; vertical size OR 
     > outer radius(2)")' )
       write(25+jj,'("targ%fr2 = 0.1	             ; vertical size OR 
     > outer radius(2)")' )
       write(15+jj,'("targ%xoffset = 0.0	     ;  target x-offset (cm): +x 
     > = beam right")' )
       write(25+jj,'("targ%xoffset = 0.0	     ;  target x-offset (cm): +x 
     > = beam right")' )
c set  beam position to zero
       write(15+jj,'("targ%yoffset =  0.0	     ;  target y-offset (cm): +y 
     > = up")' )
       write(25+jj,'("targ%yoffset =  0.0	     ;  target y-offset (cm): +y 
     > = up")' )
       if(it.le.2 .and. jj.ne.7) then
        write(15+jj,'("targ%zoffset = 0.0	     ;  target z-offset (cm): +z 
     >  = downstream")' )
        write(25+jj,'("targ%zoffset = 0.0	     ;  target z-offset (cm): +z 
     >  = downstream")' )
       else
        write(15+jj,'("targ%zoffset = 5.0	     ;  target z-offset (cm): +z 
     >  = downstream")' )
        write(25+jj,'("targ%zoffset = 5.0	     ;  target z-offset (cm): +z 
     >  = downstream")' )
       endif
       write(15+jj,'("end parm beamandtergetinfo")' )
       write(15+jj,'(" ")')
       write(25+jj,'("end parm beamandtergetinfo")' )
       write(25+jj,'(" ")')

       write(15+jj,'(";These are offsets applied before the call to 
     > the single arm montecarlos.")' )
       write(15+jj,'("begin parm spect_offset")' )
c put mispointing back in
c       hmp_x=0.
c       hmp_y=0.
       write(15+jj,
     >  '("spec%e%offset%x = ",f5.2,10x,"; x offset (cm)")') hmp_x
       write(15+jj,
     >  '("spec%e%offset%y = ",f5.2,10x," ; y offset (cm)")') hmp_y
       write(15+jj,'("spec%e%offset%z = 0.	       ; z offset (cm)")')
       write(15+jj,'("spec%e%offset%xptar = 0.       ; xptar 
     > offset (mr) ! x(y)ptar is slope, so")' )
       write(15+jj,'("spec%e%offset%yptar = 0.       ; yptar 
     > offset (mr) ! it is really unitless.")' )
c       write(15+jj,'("spec%p%offset%x = 0.0 	       ; x offset (cm)")' )
c       write(15+jj,'("spec%p%offset%y = 0.0 	       ; y offset (cm)")' )
c put mis-pointing back in
       write(15+jj,
     >  '("spec%p%offset%x = ",f5.2,10x,"; x offset (cm)")') pmp_x
       write(15+jj,
     >  '("spec%p%offset%y = ",f5.2,10x," ; y offset (cm)")') pmp_y
       write(15+jj,'("spec%p%offset%z = 0.	       ; z offset (cm)")' )
       write(15+jj,'("spec%p%offset%xptar = 0.0      ; xptar 
     > offset (mr)")')
       write(15+jj,'("spec%p%offset%yptar = 0.0      ; yptar 
     > offset (mr)")' )
       write(15+jj,'("end parm spect_offset")' )
       write(15+jj,'(" ")' )

       write(15+jj,'("begin parm simulate")' )
       write(15+jj,'("hard_cuts = 0               ;  (ONE = TRUE) SPedge 
     > and Em.max are hard cuts(ntuple)")' )
       if(jj.eq.1 .or. jj.eq.3.or.jj.ge.5) then
        write(15+jj,'("using_rad = 1		    ;  (ONE = TRUE)")' )
       else
        write(15+jj,'("using_rad = 0		    ;  (ONE = TRUE)")' )
       endif
       write(15+jj,'("use_expon = 0		    ;  (LEAVE AT 0)")' )
       write(15+jj,'("one_tail = 0		    ;  0=all, 1=e, 2=epr, 3=p, 
     > -3=all but p")' )
       write(15+jj,'("intcor_mode = 1	            ;  (LEAVE AT 1)")' )
       write(15+jj,'("spect_mode = 0	            ;  0=e+p arms, -1=p arm, 
     > -2=e arm only, 1=none")' )
       write(15+jj,'("cuts%Em%min = 0.	    ;  (Em.min=Em.max=0.0 gives 
     > wide open cuts)")' )
       write(15+jj,'("cuts%Em%max = 0.	    ;  Must be wider than cuts 
     > in analysis(elast. or e,ep)" )' )
       write(15+jj,'("using_Eloss = 1	            ;  (ONE = TRUE)")' )
       write(15+jj,'("correct_Eloss = 1	    ;  ONE = correct reconstructed 
     > events for eloss.")' )
       write(15+jj,'("correct_raster = 1	    ;  ONE=Reconstruct events 
     > using raster matrix elements")' )
       write(15+jj,'("mc_smear = 1.	            ;  ONE = target & 
     > hut mult scatt AND DC smearing.")' )
       write(15+jj,'("deForest_flag = 0	    ;  0=sigcc1, 1=sigcc2, 
     > -1=sigcc1 ONSHELL")' )
       write(15+jj,'("rad_flag = 0		    ;  (radiative option #1...
     > see init.f)")' )
       write(15+jj,'("extrad_flag = 2	            ;  (rad. option 
     > #2...see init.f)")' )
       write(15+jj,'("lambda(1) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(1) = {TF}")' )
       write(15+jj,'("lambda(2) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(2) = {TF}")' )
       write(15+jj,'("lambda(3) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(3) = {TF}")' )
       write(15+jj,'("Nntu = 1		    ;  ONE = generate ntuples")' )
       write(15+jj,'("using_Coulomb = 1	    ;  (ONE = TRUE)")' )
       write(15+jj,'("dE_edge_test = 0.	    ; (move around energy 
     > edges)")')
       write(15+jj,'("use_offshell_rad = 1	    ;  (ONE = TRUE)")')
       egammax = (ebeam - 0.85*phms - 0.8*pshms) * 1000. 
       write(15+jj,'("Egamma_gen_max = ",f7.0,"         ;  Set >0 to hardwire the 
     >   Egamma limits. set big enough!")' ) egammax
       write(15+jj,'("do_fermi = 0")' )
c changed from 4.9 to 4.0 6/29/2019
       write(15+jj,'("pt_b_param = 3.8      ; was 4.9, now 3.8 
     > data for H and D +-")' )
       write(15+jj,'("sigc_flag = 1         ; 0 = bin in z, 1 = bin in pt2, 
     > -1 = bin in xbj" )' )
       write(15+jj,'("sigc_nbin = 100.       ; number of bins for central 
     > cross section calc" )' )
       write(15+jj,'("sigc_kin_min = 0.00   ; minumum z (or pt2) for central 
     > cross section calc")' ) 
       write(15+jj,'("sigc_kin_max = 1.00   ; maximum z (or pt2) for central 
     > cross section calc")' )
       write(15+jj,'("sigc_kin_ind = 0.55   ; value for independent variable 
     > (z or pt2 in GeV2)")' ) 
       rseed = irun + int(10000.*bcm1) + int(10000000.*dt)
       write(15+jj,'("random_seed = ",i10,"   ; randm seed)")' ) rseed
       write(15+jj,'(" ")' )

       write(15+jj,'("end parm simulate")' )

       write(25+jj,'(";These are offsets applied before the call to 
     > the single arm montecarlos.")' )
       write(25+jj,'("begin parm spect_offset")' )
c option to take  out mispointing on Holly's advice
c       hmp_x=0.
c       hmp_y=0.
       write(25+jj,
     >  '("spec%e%offset%x = ",f5.2,10x,"; x offset (cm)")') hmp_x
       write(25+jj,
     >  '("spec%e%offset%y = ",f5.2,10x," ; y offset (cm)")') hmp_y
       write(25+jj,'("spec%e%offset%z = 0.	       ; z offset (cm)")')
       write(25+jj,'("spec%e%offset%xptar = 0.       ; xptar 
     > offset (mr) ! x(y)ptar is slope, so")' )
       write(25+jj,'("spec%e%offset%yptar = 0.       ; yptar 
     > offset (mr) ! it is really unitless.")' )
       write(25+jj,'("spec%p%offset%x = 0.0 	       ; x offset (cm)")' )
       write(25+jj,'("spec%p%offset%y = 0.0 	       ; y offset (cm)")' )
c took out mis-pointing at Holly's advice
c       write(25+jj,
c     >  '("spec%e%offset%x = ",f5.2,10x,"; x offset (cm)")') pmp_x
c       write(25+jj,
c     >  '("spec%e%offset%y = ",f5.2,10x," ; y offset (cm)")') pmp_y
       write(25+jj,'("spec%p%offset%z = 0.	       ; z offset (cm)")' )
       write(25+jj,'("spec%p%offset%xptar = 0.0      ; xptar 
     > offset (mr)")')
       write(25+jj,'("spec%p%offset%yptar = 0.0      ; yptar 
     > offset (mr)")' )
       write(25+jj,'("end parm spect_offset")' )
       write(25+jj,'(" ")' )

       write(25+jj,'("begin parm simulate")' )
       write(25+jj,'("hard_cuts = 0               ;  (ONE = TRUE) SPedge 
     > and Em.max are hard cuts(ntuple)")' )
       if(jj.eq.1 .or. jj.eq.3.or.jj.ge.5) then
        write(25+jj,'("using_rad = 1		    ;  (ONE = TRUE)")' )
       else
        write(25+jj,'("using_rad = 0		    ;  (ONE = TRUE)")' )
       endif
       write(25+jj,'("use_expon = 0		    ;  (LEAVE AT 0)")' )
       write(25+jj,'("one_tail = 0		    ;  0=all, 1=e, 2=epr, 3=p, 
     > -3=all but p")' )
       write(25+jj,'("intcor_mode = 1	            ;  (LEAVE AT 1)")' )
       write(25+jj,'("spect_mode = 0	            ;  0=e+p arms, -1=p arm, 
     > -2=e arm only, 1=none")' )
       write(25+jj,'("cuts%Em%min = 0.	    ;  (Em.min=Em.max=0.0 gives 
     > wide open cuts)")' )
       write(25+jj,'("cuts%Em%max = 0.	    ;  Must be wider than cuts 
     > in analysis(elast. or e,ep)" )' )
       write(25+jj,'("using_Eloss = 1	            ;  (ONE = TRUE)")' )
       write(25+jj,'("correct_Eloss = 1	    ;  ONE = correct reconstructed 
     > events for eloss.")' )
       write(25+jj,'("correct_raster = 1	    ;  ONE=Reconstruct events 
     > using raster matrix elements")' )
       write(25+jj,'("mc_smear = 1.	            ;  ONE = target & 
     > hut mult scatt AND DC smearing.")' )
       write(25+jj,'("deForest_flag = 0	    ;  0=sigcc1, 1=sigcc2, 
     > -1=sigcc1 ONSHELL")' )
       write(25+jj,'("rad_flag = 0		    ;  (radiative option #1...
     > see init.f)")' )
       write(25+jj,'("extrad_flag = 2	            ;  (rad. option 
     > #2...see init.f)")' )
       write(25+jj,'("lambda(1) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(1) = {TF}")' )
       write(25+jj,'("lambda(2) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(2) = {TF}")' )
       write(25+jj,'("lambda(3) = 0.0	            ;  if rad_flag.eq.4 
     > then lambda(3) = {TF}")' )
       write(25+jj,'("Nntu = 1		    ;  ONE = generate ntuples")' )
       write(25+jj,'("using_Coulomb = 1	    ;  (ONE = TRUE)")' )
       write(25+jj,'("dE_edge_test = 0.	    ; (move around energy 
     > edges)")')
       write(25+jj,'("use_offshell_rad = 1	    ;  (ONE = TRUE)")')
       write(25+jj,'("Egamma_gen_max = ",f7.0,"         ;  Set >0 to hardwire the 
     >   Egamma limits. set big enough!")' ) egammax
       write(25+jj,'("do_fermi = 0")' )
c changed from 4.9 to 4.0 6/29/2019
       write(25+jj,'("pt_b_param = 3.8      ; was 4.9, now 3.8 
     > data for H and D +-")' )
       write(25+jj,'("sigc_flag = 1         ; 0 = bin in z, 1 = bin in pt2, 
     > -1 = bin in xbj" )' )
       write(25+jj,'("sigc_nbin = 100.       ; number of bins for central 
     > cross section calc" )' )
       write(25+jj,'("sigc_kin_min = 0.00   ; minumum z (or pt2) for central 
     > cross section calc")' ) 
       write(25+jj,'("sigc_kin_max = 1.00   ; maximum z (or pt2) for central 
     > cross section calc")' )
       write(25+jj,'("sigc_kin_ind = 0.55   ; value for independent variable 
     > (z or pt2 in GeV2)")' ) 
       rseed = irun + int(10000.*bcm1) + int(10000000.*dt)
       write(25+jj,'("random_seed = ",i10,"   ; randm seed)")' ) rseed
       write(25+jj,'(" ")' )

       write(25+jj,'("end parm simulate")' )

       enddo ! j

       nruns = nruns+1
       if(nruns.gt.1) write(50,190,advance="no")
 190   format(',')
       write(6,'(''adding '',i4)') irun
       write(50,199,advance="no") irun,irun
 199   format('{"os": "general", "track": "analysis", ', 
c     > "stdout":', 
c     > '"/work/hallc/c-sidis18/bosted/outfiles/simcsp18',i4,
c     > '.out"', 
     > '"command": "source /u/group/c-sidis/bosted/',
     > 'simc/scripts/run',i4,
     > '", "diskBytes": 2000000000, "ramBytes": 5000000000,', 
c     > '"stderr": "/work/hallc/c-sidis18/bosted/outfiles/',
c     > 'simcsp18',i4,'.err"', 
     >  '"cpuCores": 1, "project":', 
     >  '"c-sidis18", "name": "simc',i4,'", "shell": "/usr/',
cxxx change if want to run a lot of stuff
     >  'bin/tcsh", "timeSecs": 28800}')
c     >  'bin/tcsh", "timeSecs": 7200}')

       close(16)
       close(17)
       close(18)
       close(19)
       close(20)
       close(21)
       close(22)
       close(26)
       close(27)
       close(28)
       close(29)
       close(30)
       close(31)
       close(32)
 299   continue
      enddo

 1000 continue
      write(50,197)
 197  format(']}')

      stop
      end
