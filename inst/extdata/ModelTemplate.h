#include <multi/MultiModel.h>
#include <memory/Transients.h>
#include <sysdyn/NamedVariable.h>

using namespace openworld;

class ${MODEL}Model : public SimpleTemporalModel {
 protected:
  // Constants
  ${CONSTANTS}

  // Stocks
  ${STOCKS}

  // Named Variables
  ${NAMEDVARS}

 public:
  ${MODEL}Model() :
  ${VARINITS} {
    ${EQUATIONS}
  }

  ~${MODEL}Model() {
  }
};

