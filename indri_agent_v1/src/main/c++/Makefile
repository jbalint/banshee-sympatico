# my local copy is built from SVN on sourceforge
INDRI_HOME=/home/jbalint

#CXXFLAGS += -Wall
CXXFLAGS += -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux
CXXFLAGS += -I$(INDRI_HOME)/include

# not sure how they intend you to get this... indri/atomic.hpp should have it set at installation
CXXFLAGS += -DHAVE_EXT_ATOMICITY_H -DP_NEEDS_GNU_CXX_NAMESPACE

LDFLAGS += -L$(INDRI_HOME)/lib

libbs_indri_v1_Query.so: indri_query.cpp
	$(CXX) $(CXXFLAGS) -c -fpic -ggdb3 -D_GNU_C_SOURCE -o indri_query.o indri_query.cpp
	$(CXX) $(LDFLAGS) -shared -Wl,-soname,libbs_indri_v1_Query.so -o libbs_indri_v1_Query.so indri_query.o -lindri
