CXX = g++
CXXFLAGS = -std=c++17 -I. -DDEBUG_ENABLED
LEX = flex
YACC = bison
YFLAGS = -d

TARGET = objc_compiler
SOURCES = main.cpp classes.cpp utils.cpp output_utils.cpp context.cpp node_semantics.cpp
OBJ = $(SOURCES:.cpp=.o) objc-lexer.o objc-parser.o

all: $(TARGET)

main.o: main.cpp objc-parser.hpp utils.h output_utils.h classes.h context.h
classes.o: classes.cpp classes.h types.h
utils.o: utils.cpp utils.h
output_utils.o: output_utils.cpp output_utils.h
context.o: context.cpp context.h classes.h types.h semantic_exceptions.h output_utils.h
node_semantics.o: node_semantics.cpp context.h classes.h types.h output_utils.h

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
	rm -f $(OBJ) $(TARGET) objc-lexer.cpp objc-parser.cpp objc-parser.hpp *.output *.log

rebuild: clean all

.PHONY: all clean rebuild