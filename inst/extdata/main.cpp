#include <iostream>
#include <iomanip>
#include <sysdyn/Stock.h>
#include <sysdyn/TemporalVariable.h>
#include <sysdyn/Constant.h>
#include <measure/Units.h>
#include <memory/Transients.h>

// For multimodel
#include <multi/Distribution.h>
#include "PopulationModel.h"
#include "FishingModel.h"
#include "MalthusModel.h"
#include "MalthusForestCellModel.h"
#include "MalthusPopulationCellModel.h"
#include "MalthusPopulationCellModel2.h"

using namespace openworld;

void lineout(int model, double time, double pop, double bio, double farm);

int main(int argc, const char* argv[])
{
  cout << "model,time,population,biomass,farm" << endl;

  Stock::deltat = .001;

  try {
    // Simple Population Model
    PopulationModel popmod;

    for (int tt = 0; tt < 100; tt++) {
      map<string, Distribution*> vars = popmod.evaluate(tt);
      lineout(1, tt, vars["Population"]->getMode(), 0, 0);
    }

    // Simple Logistic Model
    FishingModel fishmod;
    
    for (int tt = 0; tt < 100; tt++) {
      map<string, double> vars = fishmod.evaluateVariables(tt);
      lineout(2, tt, vars["Population"], vars["Biomass"], 0);
    }
  } catch (exception& e) {
    cout << e.what() << endl;
    throw e;
  }

  Stock::deltat = .01;

  // Integrated model: exhibits crash
  double matchesPopulation[1000], matchesBiomass[1000], matchesFarmArea[1000];
  try {
    MalthusModel malmod;
  
    unsigned ii = 0;
    for (double tt = 0; tt < 100; tt += .1) {
      map<string, double> vars = malmod.evaluateVariables(tt);
      lineout(3, tt, vars["Population"], vars["Biomass"], vars["FarmArea"]);

      matchesPopulation[ii] = vars["Population"];
      matchesBiomass[ii] = vars["Biomass"];
      matchesFarmArea[ii++] = vars["FarmArea"];      
    }
  } catch (exception& e) {
    cout << e.what() << endl;
    throw e;
  }
  
  // individual forest Ha-- all natural
  try {
    MalthusForestCellModel malmod;
    
    for (int tt = 0; tt < 100; tt++) {
      map<string, double> vars = malmod.evaluateVariables(tt);
      lineout(4, tt, vars["Population"] * 100, vars["Biomass"] * 100, 0);
    }
  } catch (exception& e) {
    cout << e.what() << endl;
    throw e;
  }

 
  // populated cell, with some farmland
  try {
    MalthusPopulationCellModel2 malmod;

    for (int tt = 0; tt < 100; tt++) {
      map<string, double> vars = malmod.evaluateVariables(tt);
      lineout(5, tt, vars["Population"] * 100, 0, vars["FarmArea"] * 100);
    }
  } catch (exception& e) {
    cout << e.what() << endl;
    throw e;
  }

  // A simple coupling: oscillation and explosion

  Stock::deltat = .1;

  MalthusForestCellModel formod;
  MalthusPopulationCellModel popmod;
  for (double tt = 0; tt < 100; tt += .1) {
    map<string, double> forvars = formod.evaluateVariables(tt);
    map<string, double> popvars = popmod.evaluateVariables(tt);

    map<string, double> setfors;
    setfors["Population"] = popvars["Population"];
    setfors["Capacity"] = 1 - popvars["FarmArea"];
    formod.setVariables(setfors);

    map<string, double> setpops;
    setpops["Population"] = forvars["Population"];
    popmod.setVariables(setpops);

    lineout(6, tt, popvars["Population"] * 100, forvars["Biomass"] * 100, popvars["FarmArea"] * 100);
  }
  
  Stock::deltat = .001;

  MalthusForestCellModel formod2;
  MalthusPopulationCellModel2 popmod2;
  for (double tt = 0; tt < 100; tt += .1) {
    map<string, double> forvars = formod2.evaluateVariables(tt);
    map<string, double> popvars = popmod2.evaluateVariables(tt);

    double population = popvars["Population"];
    double targetPopulationFactor = forvars["Gather"] / (population * forvars["Requirement"]);
    double natureArea = 1 - popvars["FarmArea"];

    double fixesbug = forvars["Gather"];

    map<string, double> setfors;
    setfors["Population"] = population;
    setfors["Capacity"] = natureArea;
    formod2.setVariables(setfors);

    map<string, double> setpops;
    setpops["TargetPopulationFactor"] = targetPopulationFactor;
    popmod2.setVariables(setpops);

    lineout(7, tt, popvars["Population"] * 100, forvars["Biomass"] * 100, popvars["FarmArea"] * 100);
  }

  srand(time(NULL));
  double bestfc = 4.93186431153443e-05, bestfs = 0.000434850163614111, bestpc = 0.0669757624786906, bestps = 0.635878109579844, besterror = 0; // 85238.3
  while (1) {
    double fc, fs, pc, ps;
    if (besterror == 0) {
      fc = bestfc;
      fs = bestfs;
      pc = bestpc;
      ps = bestps;
    } else {
      fc = rand() % 10 ? (bestfc * (1 + (rand() / (double) RAND_MAX - .5) * (rand() / (double) RAND_MAX))) : (rand() / (double) RAND_MAX);
      fs = rand() % 10 ? (bestfs * (1 + (rand() / (double) RAND_MAX - .5) * (rand() / (double) RAND_MAX))) : (rand() / (double) RAND_MAX);
      pc = rand() % 10 ? (bestpc * (1 + (rand() / (double) RAND_MAX - .5) * (rand() / (double) RAND_MAX))) : (rand() / (double) RAND_MAX);
      ps = rand() % 10 ? (bestps * (1 + (rand() / (double) RAND_MAX - .5) * (rand() / (double) RAND_MAX))) : (rand() / (double) RAND_MAX);
    }

    GaussianNoiseModel formod_mc(new MalthusForestCellModel(), fc);
    GaussianNoiseModel formod_mc_solo(new MalthusForestCellModel(), fs);
    GaussianNoiseModel popmod_gn(new MalthusPopulationCellModel2(), pc);
    GaussianNoiseModel popmod_gn_solo(new MalthusPopulationCellModel2(), ps);

    double checkPopulation[1000], checkBiomass[1000], checkFarmArea[1000];
  
    unsigned ii = 0;
    for (double tt = 0; tt < 100; tt += .1) {
      map<string, Distribution*> forvars = formod_mc.evaluate(tt);
      map<string, Distribution*> popvars = popmod_gn.evaluate(tt);
      map<string, Distribution*> forvarsSolo = formod_mc_solo.evaluate(tt);
      map<string, Distribution*> popvarsSolo = popmod_gn_solo.evaluate(tt);
    
      map<string, Distribution*> merged;
    
      for (map<string, Distribution*>::iterator it = forvars.begin(); it != forvars.end(); ++it)
        merged[it->first] = it->second;

      for (map<string, Distribution*>::iterator it = forvarsSolo.begin(); it != forvarsSolo.end(); ++it) {
        map<string, Distribution*>::iterator found = merged.find(it->first);
        if (found != merged.end()) {
          merged[it->first] = &(found->second->merge(*it->second));
          merged[it->first]->mergePurge();
        } else
          merged[it->first] = it->second;
      }
      merged.erase("Population");

      for (map<string, Distribution*>::iterator it = popvars.begin(); it != popvars.end(); ++it) {
        map<string, Distribution*>::iterator found = merged.find(it->first);
        if (found != merged.end()) {
          merged[it->first] = &(found->second->merge(*it->second));
          merged[it->first]->mergePurge();
        } else
          merged[it->first] = it->second;
      }

      for (map<string, Distribution*>::iterator it = popvarsSolo.begin(); it != popvarsSolo.end(); ++it) {
        map<string, Distribution*>::iterator found = merged.find(it->first);
        if (found != merged.end()) {
          merged[it->first] = &(found->second->merge(*it->second));
          merged[it->first]->mergePurge();
        } else
          merged[it->first] = it->second;
      }

      if (tt > 0) {
        merged["TargetPopulationFactor"] = &(*merged["Gather"] / (*merged["Population"] * *merged["Requirement"]));
        merged["Capacity"] = &(1 - *merged["FarmArea"]);
      }

      formod_mc.setVariables(merged);
      popmod_gn.setVariables(merged);

      lineout(8, tt, merged["Population"]->getMode() * 100, merged["Biomass"]->getMode() * 100, merged["FarmArea"]->getMode() * 100);

      checkPopulation[ii] = merged["Population"]->getMode() * 100;
      checkBiomass[ii] = merged["Biomass"]->getMode() * 100;
      checkFarmArea[ii++] = merged["FarmArea"]->getMode() * 100;
    }

    exit(0);

    double error = 0;
    for (unsigned ii = 0; ii < 1000; ii++) {
      error += (checkPopulation[ii] - matchesPopulation[ii])*(checkPopulation[ii] - matchesPopulation[ii])
        + (checkBiomass[ii] - matchesBiomass[ii])*(checkBiomass[ii] - matchesBiomass[ii])
        + (checkFarmArea[ii] - matchesFarmArea[ii])*(checkFarmArea[ii] - matchesFarmArea[ii]);
    }

    cout << "Error: " << error << " for " << fc << ", " << fs << ", " << pc << ", " << ps << endl;
    if (besterror == 0) {
      besterror = error;
      cout << "Initialized." << endl;
    } else if (error < besterror) {
      bestfc = fc;
      bestfs = fs;
      bestpc = pc;
      bestps = ps;
      besterror = error;
      
      cout << "Improved: " << setprecision(15) << fc << ", " << fs << ", " << pc << ", " << ps << endl;
    }
  }
}

void lineout(int model, double time, double pop, double bio, double farm) {
  cout << model << "," << time << "," << pop << "," << bio << "," << farm << endl;
}


/*  } catch (exception& e) {
    cout << e.what() << endl;
  }
*/
