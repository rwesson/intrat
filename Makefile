FC=gfortran
LD=gfortran
PREFIX=/usr
FFLAGS+=-cpp -DPREFIX=\"${PREFIX}\" -ffree-line-length-0 -fno-backtrace

.PHONY: clean install uninstall new

%.o: %.f95
	$(FC) $(FFLAGS) $< -c -o $@

intrat: source/intrat.o
	$(LD) $(FFLAGS) -o $@ $^

clean:
	rm -f intrat source/*.o source/*.mod

new: clean intrat

install:
	test -e ${DESTDIR}${PREFIX}/share/intrat || mkdir -p ${DESTDIR}${PREFIX}/share/intrat
	test -e ${DESTDIR}${PREFIX}/bin || mkdir -p ${DESTDIR}${PREFIX}/bin
	install -m 644 data/*.* ${DESTDIR}${PREFIX}/share/intrat
	install intrat ${DESTDIR}${PREFIX}/bin

uninstall:
	rm -rf ${DESTDIR}${PREFIX}/share/intrat
	rm -f ${DESTDIR}${PREFIX}/bin/intrat
