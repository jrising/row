SYS = Linux

CC = mpic++
DEBUG = -g
OWD = ~/projects/openworld
CFLAGS = -Wall -c $(DEBUG) -I $(OWD) -O3
LFLAGS = -Wall $(DEBUG) -O3

OBJS_MAIN = $(OWD)/dims/Dims.o $(OWD)/dims/Dimensions.o $(OWD)/dims/Dimensionless.o $(OWD)/dims/GlobalDimensions.o $(OWD)/measure/Unit.o $(OWD)/measure/Units.o $(OWD)/memory/Transients.o $(OWD)/sysdyn/TemporalVariable.o $(OWD)/sysdyn/Stock.o

all: main${MODEL}.o $(OBJS_MAIN)
	$(CC) $(LFLAGS) main${MODEL}.o $(OBJS_MAIN) -o main${MODEL}

#Inference rule - states a general rule for compiling .o files
%.o : %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	\rm $(OBJS_MAIN)
	\rm *.o main${MODEL}
