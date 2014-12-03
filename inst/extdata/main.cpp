#include <iostream>
#include <iomanip>
#include <sysdyn/Stock.h>
#include <sysdyn/TemporalVariable.h>
#include <sysdyn/Constant.h>
#include <measure/Units.h>
#include <memory/Transients.h>

#include "${MODEL}Model.h"

using namespace openworld;

int main(int argc, const char* argv[])
{
  cout << "time,${VARLISTCSV}" << endl;

  Stock::deltat = ${SUBSTEP};

  try {
    ${MODEL}Model model;

    for (int tt = 0; tt < ${STEPS}; tt++) {
      map<string, double> vars = model.evaluateVariables(tt);
      cout << tt << "," << ${VARLISTCOUT} << endl;
    }
  } catch (exception& e) {
    cout << e.what() << endl;
    throw e;
  }
}
