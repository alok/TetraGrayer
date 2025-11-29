# Build Metal raytracer library for TetraGrayer
# Run: make -C ffi/metal

LEAN_INCLUDE := $(shell lake env | grep LEAN_SYSROOT | cut -d= -f2)/include

.PHONY: all clean

all: libmetal_raytracer.dylib

# Compile Metal wrapper (Objective-C)
metal_wrapper.o: metal_wrapper.m metal_wrapper.h
	clang -c -o $@ $< \
		-I$(LEAN_INCLUDE) -I. \
		-O3 -ffast-math -fobjc-arc -fPIC

# Compile Lean FFI bridge (C)
lean_metal.o: lean_metal.c metal_wrapper.h
	clang -c -o $@ $< \
		-I$(LEAN_INCLUDE) -I. \
		-O3 -ffast-math -fPIC

# Link as shared library
libmetal_raytracer.dylib: metal_wrapper.o lean_metal.o
	clang -shared -o $@ $^ \
		-framework Metal -framework Foundation \
		-O3 -ffast-math \
		-install_name @rpath/libmetal_raytracer.dylib

clean:
	rm -f *.o *.dylib
