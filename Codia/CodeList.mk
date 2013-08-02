CXX = g++ -std=gnu++11
CXXFLAGS = -Wall -g -D CODELIST_UNIT_TEST
INCLUDES = 
LIBS =
OBJS = CodeList.o
PROGRAM = CodeList.out

all:$(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CXX) $(CXXFLAGS) $^ $(INCLUDES) $(LIBS) -o $(PROGRAM)

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(LIBS) -c $<

.PHONY: clean
clean:
	rm -f *o $(PROGRAM)
