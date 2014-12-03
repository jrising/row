SYS = Linux

CC = mpic++
DEBUG = -g
OWD = ~/projects/openworld
CFLAGS = -Wall -c $(DEBUG) -I $(OWD) -O3
LFLAGS = -Wall $(DEBUG) -O3

OBJS_MAIN = $(OWD)/dims/Dims.o $(OWD)/dims/Dimensions.o $(OWD)/dims/Dimensionless.o $(OWD)/dims/GlobalDimensions.o $(OWD)/measure/Unit.o $(OWD)/measure/Units.o $(OWD)/memory/Transients.o $(OWD)/sysdyn/TemporalVariable.o $(OWD)/sysdyn/Stock.o $(OWD)/multi/Distribution.o

all: conflict.o $(OBJS_MAIN)
	$(CC) $(LFLAGS) conflict.o $(OBJS_MAIN) -o conflict

c2: conflict2.o $(OBJS_MAIN)
	$(CC) $(LFLAGS) -lgsl -lgslcblas conflict2.o $(OBJS_MAIN) -o conflict2

#Inference rule - states a general rule for compiling .o files
%.o : %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	\rm $(OBJS_MAIN)
	\rm *.o conflict
