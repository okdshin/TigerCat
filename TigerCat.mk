CXX = g++ -std=gnu++11
CXXFLAGS = -Wall -g -D TIGERCAT_UNIT_TEST
INCLUDES = 
LIBS = -lboost_regex
OBJS = TigerCat.o
PROGRAM = TigerCat.out

all:$(PROGRAM)

release:CXXFLAGS += -DRELEASE_MODE
release:$(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CXX) $(CXXFLAGS) $^ $(INCLUDES) $(LIBS) -o $(PROGRAM)

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(LIBS) -c $<

.PHONY: clean
clean:
	rm -f *o $(PROGRAM)
