Recombination line intensities for hydrogenic ions
==================================================
Recombination line intensities for hydrogenic ions. IV. Total
recombination coefficients and machine-readable tables for Z=1 to 8
Storey P.J., Hummer D.G.
Mon. Not. R. Astron. Soc. 272, 41 (1995)

ADC_Keywords: Atomic physics ;
Keywords: atomic data - atomic processes - line: formation

Description:
    Line emissivities, effective recombination coefficients, opacity
    factors, departure coefficients and total recombination coefficients
    are calculated for hydrogenic ions with Z<=8.

    There are 162 primary files labelled rZCTTTT.d, where
        Z=1,2,..,8 is the ionic charge
        C=a or b   is the Case in the sense of Baker and Menzel
                   (1938ApJ....88...52B), i.e.
                   -- A for a nebula transparent to Lyman line radiation,
                   -- B for an opaque nebula
        TTTT=0.01Te is a reduced temperature variable.

    There are 9 values of electron densities (log(Ne)=2(1)10) for case A,
    and 13 (log(Ne)=2(1)14) for case B. The structure of these files is
    detailed in section 4 of the MNRAS paper.

    There are 16 secondary files labelled eZC.d, where
        Z=1,2,..,8 is the ionic charge
        C=a or b   is the Case in the sense of Baker and Menzel
                   (1938ApJ....88...52B)
    which contain emissivities and total recombination coefficients.

    These files are accompanied by an interactive data server  intrat.f.
    Please refer to section 5 of the MNRAS paper which details the
    usage of the program.

    CGS units are used, meaning that
    => densities    are expressed in cm^-3^
    => emissivities are expressed in erg.s^-1^.cm^-3^   (10^-7^W.cm^-3^)
    => effective recombination coefficients are expressed in cm^3^.s^-1^

Courtesy: Peter J. Storey  <pjs@starlink.ucl.ac.uk> 
