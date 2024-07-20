#ifndef SEMANT_H_
#define SEMANT_H_

#include <deque>
#include <vector>
#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

using std::deque;
using std::vector;

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  unordered_map<Symbol, Class_> class_map;
  unordered_map<Symbol, deque<Class_> *> chain_map;
  vector<Symbol> base_class_vec;
  vector<Symbol> no_inhe_class_vec;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Class_ c, tree_node *t);
  ostream& semant_error(Symbol filename, tree_node *t);

  void check();
  void checkAddFeatures();
  bool checkAddMethods(unordered_map<Symbol, Feature>&, Class_&, Feature&);
  bool checkAddAttrs(unordered_map<Symbol, Feature>&, Class_&, Feature&);
  void buildInheChains();
  void buildScopeTabs(deque<Class_>&);
  void checkInheritance();
  void checkInheRedefinitions(Class_&);

  vector<Symbol>& getBaseClasses() { return base_class_vec; }
  unordered_map<Symbol, Class_>& getClasses() { return class_map; }
  vector<Symbol>& getNoInheClasses() { return no_inhe_class_vec; }
  unordered_map<Symbol, deque<Class_> *>& getChains() { return chain_map; }
};


#endif
