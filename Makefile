FC=gfortran
LD=gfortran
FFLAGS=-ffree-line-length-0 -O3 -fno-backtrace

.PHONY: clean install uninstall

%.o: %.f95
	$(FC) $(FFLAGS) $< -c -o $@

intrat: source/intrat.o
	$(LD) $(FFLAGS) -o $@ $^

clean:
	rm -f intrat source/*.o source/*.mod

install:
	test -e ${DESTDIR}/usr/share/intrat || mkdir -p ${DESTDIR}/usr/share/intrat
	test -e ${DESTDIR}/usr/bin || mkdir -p ${DESTDIR}/usr/bin
	install -m 644 data/*.* ${DESTDIR}/usr/share/intrat
	install intrat ${DESTDIR}/usr/bin

uninstall:
	rm -rf ${DESTDIR}/usr/share/intrat
	rm -f ${DESTDIR}/usr/bin/intrat 
