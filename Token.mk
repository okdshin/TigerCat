CXX = g++ -std=gnu++11
CXXFLAGS = -Wall -g -D TOKEN_UNIT_TEST
INCLUDES = 
LIBS =
OBJS = Token.o
PROGRAM = Token.out

all:$(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CXX) $(CXXFLAGS) $^ $(INCLUDES) $(LIBS) -o $(PROGRAM)

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(LIBS) -c $<

.PHONY: clean
clean:
	rm -f *o $(PROGRAM)
