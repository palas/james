ACLOCAL_AMFLAGS = -I m4
EXTRA_DIST = autogen.sh

lib_LTLIBRARIES = libjames.la
libjames_la_SOURCES = james.cpp cache.cpp httpanalysis.cpp package.cpp refs.cpp sock.cpp trap.cpp utils.cpp james.hpp cache.hpp httpanalysis.hpp package.hpp refs.hpp sock.hpp trap.hpp utils.hpp

if DEBUG
AM_CXXFLAGS = -DDEBUG
endif

