#include <assert.h>
#include <stdio.h>
#include <utility>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <string>
#include <queue>
#include <set>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

using std::pair;
using std::vector;
using std::reverse;
using std::unordered_map;
using std::queue;
using std::find;
using std::string;
using std::set;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();

   vector<CgenNodeP> collectAllClasses();
   vector<pair<Symbol, method_class *>> getAllClassMethods();
   void buildMaps();
   void buildTabs();
   void buildNameTabs();
   void buildObjTabs();
   void buildDispatchTabs();
   void buildProtoObjs();
   void codeObjs();
   void codeMethods();
};


class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   vector<CgenNodeP> getChildrenVector();
   vector<CgenNodeP> getInhePath();
   vector<pair<Symbol, method_class *>> getMethods();
   vector<pair<Symbol, method_class *>> getAllMethods();
   vector<pair<Symbol, attr_class *>> getAttrs();
   vector<pair<Symbol, attr_class *>> getAllAttrs();
   void buildNameTabs(ostream&);
   void buildObjTabs(ostream&);
   void buildDispatchTabs(ostream&);
   void buildProtoObjs(ostream&);
   void codeObj(ostream&);
   void codeMethods(ostream&);
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};




class Context {
 private:
  CgenNodeP current_class;
  std::vector<Symbol> params_table;
  std::vector<Symbol> let_vars_table;

 public:
  Context(CgenNodeP c) {
    current_class = c;
  }

  CgenNodeP getCurrClass() {
    return current_class;
  }

  int lookupAttr(Symbol attr_name) {
    auto all_attr_pairs = current_class->getAllAttrs();

    auto found_iter = find_if (all_attr_pairs.begin(), all_attr_pairs.end(), [attr_name] (auto this_pair) {
       return (this_pair.second)->get_name() == attr_name;
    });
    return (found_iter != all_attr_pairs.end()) ? found_iter - all_attr_pairs.begin() : -1;
  }

  int lookupParam(Symbol param_name) {
    auto found_iter = find (params_table.begin(), params_table.end(), param_name);
    return (found_iter != params_table.end()) ? params_table.end() - found_iter - 1 : -1;
  }

  int addParam(Symbol param_name) {
    params_table.emplace_back(param_name);
    return (params_table.size() - 1);
  }

  // Let_var offset in reverse order
  int lookupLetVar(Symbol let_var_name) {
    auto found_iter = find (let_vars_table.rbegin(), let_vars_table.rend(), let_var_name);
    return (found_iter != let_vars_table.rend()) ? let_vars_table.rend() - found_iter - 1 : -1;
  }

  int addLetVar(Symbol let_var_name) {
    let_vars_table.emplace_back(let_var_name);
    return let_vars_table.size() - 1;
  }

  int popLetVar() {
    let_vars_table.pop_back();
    return let_vars_table.size() - 1;
  }

};
