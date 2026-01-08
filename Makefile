CXX = g++
CXXFLAGS = -std=c++17 -I.
LEX = flex
YACC = bison
YFLAGS = -d

TARGET = objc_compiler
SOURCES = main.cpp classes.cpp utils.cpp output_utils.cpp
OBJ = $(SOURCES:.cpp=.o) objc-lexer.o objc-parser.o

all: $(TARGET)

main.o: main.cpp objc-parser.hpp utils.h output_utils.h
classes.o: classes.cpp
utils.o: utils.cpp utils.h
output_utils.o: output_utils.cpp output_utils.h

objc-parser.o: objc-parser.cpp objc-parser.hpp
objc-lexer.o: objc-lexer.cpp objc-parser.hpp

$(TARGET): $(OBJ)
	$(CXX) -o $@ $^ -lstdc++fs

objc-lexer.cpp: objc-lexer.l objc-parser.hpp
	$(LEX) -o $@ $<

objc-parser.cpp objc-parser.hpp: objc-parser.y
	$(YACC) $(YFLAGS) -o objc-parser.cpp $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET) objc-lexer.cpp objc-parser.cpp objc-parser.hpp *.output

.PHONY: all clean