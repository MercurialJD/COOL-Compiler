

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <algorithm>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

SymbolTable<Symbol, Symbol> attr_scope_tab;
SymbolTable<Symbol, method_class *> method_scope_tab;

Class_ global_class { nullptr };
ClassTable *classtable { nullptr };

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

template <typename T, typename V>
bool existWithin(T& container, V& value) {
    auto first = begin(container);
    auto last = end(container);
    if (find(first, last, value) != last)
        return true;
    else
        return false;
}

bool hasDefined(Symbol sym_check) {
    bool ret = true;
    auto classes = classtable->getClasses();
    if ((sym_check != SELF_TYPE) && (classes.find(sym_check) == classes.end()))
        ret = false;
    return ret;
}

Symbol getLowerType(Symbol e1, Symbol e2) {
    auto ret = Object;

    if (e1 == e2)
        ret = e1;
    else {
        if (e1 == SELF_TYPE)
            e1 = global_class->getName();
        if (e2 == SELF_TYPE)
            e2 = global_class->getName();

        auto e1_chain = classtable->getChains()[e1];
        auto e2_chain = classtable->getChains()[e2];
        auto i = e1_chain->begin();
        auto j = e2_chain->begin();
        while (i != e1_chain->end() && j != e2_chain->end()) {
            if ((*i)->getName() == (*j)->getName()) {
                ret = (*i)->getName();
                ++i; ++j;
            } else {
                break;
            }
        }
    }

    return ret;
}

bool checkInheRelation(Symbol sub, Symbol base) {
    if (sub == base)
        return true;

    if (base == SELF_TYPE && sub != SELF_TYPE)
        return false;

    sub = (sub == SELF_TYPE) ? global_class->getName() : sub;
    base = (base == SELF_TYPE) ? global_class->getName() : base;

    auto sub_classes = classtable->getClasses()[sub];
    while (sub != No_class) {
        if (sub == base)
            return true;
        else {
            sub_classes = classtable->getClasses()[sub];
            sub = sub_classes->getParent();
        }
    }

    return false;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    install_basic_classes();

    // Install classes and check definition errors
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto this_class = classes->nth(i);
        auto this_class_name = this_class->getName();
        auto parent_class = this_class->getParent();
        // Check if redefinition base classes
        if(existWithin(base_class_vec, this_class_name))
            semant_error(this_class) << "Redefinition of basic class " << this_class_name << "." << endl;

        // Check if class has been defined before
        else if (class_map.find(this_class_name) != class_map.end())
            semant_error(this_class) << "Class " << this_class_name << " was previously defined." << endl;

        // Check if class cannot inherit from base calss
        else if (existWithin(no_inhe_class_vec, parent_class))
            semant_error(this_class) << "Class " << this_class_name << " cannot inherit class " << parent_class << "." << endl;

        // Install class
        else
            class_map[this_class_name] = this_class;
    }

    // Check if class inherits from undefined class
    for_each (class_map.begin(), class_map.end(), [&](auto& entry) {
        auto parent = entry.second->getParent();
        if (parent != No_class && class_map.find(parent) == class_map.end())
            semant_error(entry.second) << "Class " << entry.second->getName() << " inherits from an undefined class " << parent << "." << endl;
    });

    // Check if contain inheritance cycle
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ this_class = classes->nth(i);
        Symbol this_class_name = this_class->getName();
        Symbol parent_class_name = this_class->getParent();

        while (parent_class_name != Object && parent_class_name != this_class_name) {
            this_class = class_map[parent_class_name];
            if (this_class == nullptr)
                return;
            parent_class_name = this_class->getParent();
        }

        // If the while-loop stops because parent_name == this_name, then CYCLE!
        if (parent_class_name != Object) {
            semant_error(this_class) << "Class "<< this_class_name <<", or an ancestor of "<< this_class_name <<", is involved in an inheritance cycle." << endl;
            return;
        }
    }

    // Check if Main class exists
    if (class_map.find(Main) == class_map.end()) {
        semant_error() << "Class Main is not defined." << endl;
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat,
								      single_Formals(formal(arg, Str)),
								      Str,
								      no_expr()))),
			       single_Features(method(substr,
						      append_Formals(single_Formals(formal(arg, Int)),
								     single_Formals(formal(arg2, Int))),
						      Str,
						      no_expr()))),
	       filename);

    // Put basic classes into class_map
    class_map[Object_class->getName()] = Object_class;
    class_map[IO_class->getName()] = IO_class;
    class_map[Int_class->getName()] = Int_class;
    class_map[Bool_class->getName()] = Bool_class;
    class_map[Str_class->getName()] = Str_class;

    // Set base classes that connot be redefined
    base_class_vec = {Object, SELF_TYPE, IO, Int, Bool, Str};

    // Set no inheritance classes that connot be inherited
    no_inhe_class_vec = {SELF_TYPE, Int, Bool, Str};
}

bool ClassTable::checkAddMethods(unordered_map<Symbol, Feature>& methods, Class_& this_class, Feature& feature) {
    if (methods.find(feature->getName()) != methods.end())
        semant_error(this_class, feature) << "Method " << feature->getName() << " is multiply defined." << endl;

    methods[feature->getName()] = feature;

    if (this_class->getName() == Main && feature->getName() == main_meth)
        return true;
    else
        return false;
}

bool ClassTable::checkAddAttrs(unordered_map<Symbol, Feature>& attrs, Class_& this_class, Feature& feature) {
    if (attrs.find(feature->getName()) != attrs.end())
        semant_error(this_class, feature) << "Attribute " << feature->getName() << " is multiply defined in class." << endl;

    attrs[feature->getName()] = feature;

    return true;
}

void ClassTable::checkAddFeatures() {
    bool main_method_exist = false;

    for (auto entry : class_map) {
        auto& this_class = entry.second;
        auto this_class_features = this_class->getFeatures();

        // Init
        this_class->getMethods() = *(new unordered_map<Symbol, Feature>);
        auto &methods = this_class->getMethods();
        this_class->getAttrs() = *(new unordered_map<Symbol, Feature>);
        auto &attrs = this_class->getAttrs();

        // Check and add methods/attrs
        for (int i = this_class_features->first(); this_class_features->more(i); i = this_class_features->next(i)) {
            auto feature = this_class_features->nth(i);

            if (feature->isMethod()) {
                bool feature_is_main_method = checkAddMethods(methods, this_class, feature);
                main_method_exist = (main_method_exist || feature_is_main_method) ? true : false;
            }
            else
                checkAddAttrs(attrs, this_class, feature);
        }
    }

    // Check main method in Main class
    if (main_method_exist == false)
        semant_error(class_map[Main]) << "No 'main' method in class Main." << endl;
}

void ClassTable::buildInheChains() {
    for (auto entry : class_map) {
        auto this_class_name = entry.first;
        auto this_class = entry.second;
        chain_map[this_class_name] = new deque<Class_>;
        auto& chain = *(chain_map[this_class_name]);

        for (auto i = this_class_name; i != Object; i = class_map[i]->getParent())
            chain.push_front(class_map[i]);
        chain.push_front(class_map[Object]);
    }
}

void ClassTable::buildScopeTabs(deque<Class_>& this_class_chain) {
    for (auto iter = this_class_chain.begin(); iter != prev(this_class_chain.end(), 1); ++iter) {
        auto intermidiate_class = *iter;

        method_scope_tab.enterscope();
        attr_scope_tab.enterscope();

        Features intermidiate_class_features = intermidiate_class->getFeatures();
        for (int i = intermidiate_class_features->first(); intermidiate_class_features->more(i); i = intermidiate_class_features->next(i)) {
            Feature feature = intermidiate_class_features->nth(i);
            if (feature->isMethod()) {
                if (method_scope_tab.lookup(feature->getName()) == NULL)
                    method_scope_tab.addid(feature->getName(), new (method_class *)(dynamic_cast<method_class *>(feature)));
            } else {
                if (feature->getName() != self && attr_scope_tab.lookup(feature->getName()) == NULL)
                    attr_scope_tab.addid(feature->getName(), new Symbol(feature->getType()));
            }
        }
    }
}

void ClassTable::checkInheRedefinitions(Class_& this_class) {
    Features this_class_features = this_class->getFeatures();
    for (auto i = this_class_features->first(); this_class_features->more(i); i = this_class_features->next(i)) {
        Feature feature = this_class_features->nth(i);

        if (feature->isMethod()) {      // Check methods
            auto old = method_scope_tab.lookup(feature->getName());
            if (old != NULL) {
                auto old_method = *old;
                auto new_method = (method_class *)feature;

                // Check return type
                if (old_method->getType() != new_method->getType()) {
                    semant_error(this_class, new_method) << "In redefined method " << new_method->getName() << ", return type " << new_method->getType() << " is different from original return type " << old_method->getType() << "." << endl;
                }

                // Check formals
                Formals old_formals = old_method->getFormals();
                Formals new_formals = new_method->getFormals();
                if (old_formals->len() != new_formals->len()) {
                    semant_error(this_class, new_method) << "Incompatible number of formal parameters in redefined method " << new_method->getName() << "." << endl;
                }
                int i = old_formals->first();
                int j = new_formals->first();
                while (old_formals->more(i) && new_formals->more(j)) {
                    auto old_formal = old_formals->nth(i);
                    auto new_formal = new_formals->nth(j);
                    if (old_formal->getType() != new_formal->getType())
                        semant_error(this_class, new_method) << "In redefined method " << new_method->getName() << ", parameter type " << new_formal->getType() << " is different from original type " << old_formal->getType() << endl;
                    i = old_formals->next(i);
                    j = new_formals->next(j);
                }
            } else
                method_scope_tab.addid(feature->getName(), new (method_class *)(dynamic_cast<method_class *>(feature)));
        } else {        // Check attributes
            auto old = attr_scope_tab.lookup(feature->getName());
            if (feature->getName() == self) {
                semant_error(this_class, feature) << "'self' cannot be the name of an attribute." << endl;
            } else if (old != NULL)
                semant_error(this_class, feature) << "Attribute " << feature->getName() << " is an attribute of an inherited class." << endl;
            else
                attr_scope_tab.addid(feature->getName(), new Symbol(feature->getType()));
        }
    }
}

void ClassTable::checkInheritance() {
    vector<Symbol> skip_vec = { Object, IO, Int, Bool, Str };
    for (auto& entry : class_map) {
        auto this_class_name = entry.first;
        if (existWithin(skip_vec, this_class_name))
            continue;

        auto this_class = entry.second;
        global_class = this_class;
        auto& this_class_chain = *(chain_map[this_class_name]);

        // Build scope tab for each class, and enter scopes until meeting this class itself
        buildScopeTabs(this_class_chain);

        // Check redefinition in the sub class
        method_scope_tab.enterscope();
        attr_scope_tab.enterscope();
        checkInheRedefinitions(this_class);

        // Set and check types
        auto this_class_features = this_class->getFeatures();
        for (auto i = this_class_features->first(); this_class_features->more(i); i = this_class_features->next(i)) {
            auto feature = this_class_features->nth(i);
            feature->getDynType();
        }

        // Exit scopes
        for (auto yclass : this_class_chain) {
            method_scope_tab.exitscope();
            attr_scope_tab.exitscope();
        }
    }
}

void ClassTable::check() {

    // Check and add features, which further check and add methods/attrs
    checkAddFeatures();

    // Build inheritance chains for each class
    buildInheChains();

    // Check inheritance
    checkInheritance();

    return;
}


Symbol method_class::getDynType() {
    attr_scope_tab.enterscope();

    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        auto this_formal = formals->nth(i);
        auto this_formal_name = this_formal->getName();
        auto this_formal_type = this_formal->getName();
        int formal_err_cnt = 0;

        if (attr_scope_tab.probe(this_formal->getName())) {
            ++formal_err_cnt;
            classtable->semant_error(global_class, this_formal) << "Formal parameter " << this_formal->getName() << " is multiply defined." << endl;
        }

        if (this_formal->getType() == SELF_TYPE) {
            ++formal_err_cnt;
            classtable->semant_error(global_class, this_formal) << "Formal parameter " << this_formal->getName() << " cannot have type SELF_TYPE." << endl;
        }

        if (this_formal->getName() == self) {
            ++formal_err_cnt;
            classtable->semant_error(global_class, this_formal) << "'self' cannot be the name of a formal parameter." << endl;
        }

        if (formal_err_cnt == 0)
            attr_scope_tab.addid(formals->nth(i)->getName(), new Symbol(formals->nth(i)->getType()));
    }

    if (classtable->getClasses().find(return_type) == classtable->getClasses().end() && return_type != SELF_TYPE)
        classtable->semant_error(global_class->get_filename(), this) << "Undefined return type " << return_type << " in method "<< name <<"." << endl;

    auto expr_type = expr->getDynType();
    if (checkInheRelation(expr_type, return_type) == false) {
        classtable->semant_error(global_class->get_filename(), this) << "Inferred return type "<< expr_type <<" of method "<<name<<" does not conform to declared return type "<< return_type <<"." << endl;
    }

    attr_scope_tab.exitscope();
    return Object;
}

Symbol attr_class::getDynType() {
    auto init_type = init->getDynType();
    if (init_type != No_type && checkInheRelation(init_type, type_decl) == false)
        classtable->semant_error(global_class, this) << "Inferred type " << init_type << " of initialization of attribute " << name << " does not conform to declared type " << type_decl << "." << endl;
    return init_type;
}

Symbol branch_class::getDynType() {
    attr_scope_tab.enterscope();

    auto attr_add_type = hasDefined(type_decl) ? type_decl : Object;
    attr_scope_tab.addid(name, new Symbol(attr_add_type));

    auto expr_type = expr->getDynType();
    attr_scope_tab.exitscope();
    return expr_type;
}

Symbol assign_class::getDynType() {
    Symbol* left = attr_scope_tab.lookup(name);
    Symbol right = expr->getDynType();
    auto filename = global_class->get_filename();

    if (name == self) {
        classtable->semant_error(filename, this) << "Cannot assign to 'self'." << endl;
        type = right;
    } else if (left == NULL) {
        classtable->semant_error(filename, this) << "Assignment to undeclared variable "<< name <<"." << endl;
        type = right;
    } else if (checkInheRelation(right, *left) == false) {
        classtable->semant_error(filename, this) << "Type "<< right <<" of assigned expression does not conform to declared type "<< *left <<" of identifier "<< name <<"." << endl;
        type = *left;
    } else {
        type = right;
    }

    return type;
}

Symbol static_dispatch_class::getDynType() {
    type = Object;
    auto expr_type = expr->getDynType();

    if (checkInheRelation(expr_type, type_name) == false)
        classtable->semant_error(global_class, this) << "Expression type " << expr_type << " does not conform to declared static dispatch type " << type_name << "." << endl;

    method_class *target = nullptr;
    for (auto a_class : *(classtable->getChains()[type_name])) {
        auto a_class_methods = a_class->getMethods();
        if (a_class_methods.count(name) > 0)
            target = (method_class *)(a_class_methods[name]);
    }

    auto target_formals = target->getFormals();
    int i = target_formals->first();
    int j = actual->first();
    while (target_formals->more(i) && actual->more(j)) {
        auto actual_type = actual->nth(j)->getDynType();
        auto target_type = target_formals->nth(i)->getType();
        auto target_name = target_formals->nth(i)->getName();
        if (checkInheRelation(actual_type, target_type) == false)
            classtable->semant_error(global_class, this) << "In call of method " << name << ", type " << actual_type << " of parameter " << target_name << " does not conform to declared type " << target_type << "." << endl;
        i = target_formals->next(i);
        j = actual->next(j);
    }

    auto target_type = target->getType();
    if (hasDefined(target_type) == false)
        type = Object;
    else
        type = target->getType();
    return type;
}

Symbol dispatch_class::getDynType() {
    type = Object;
    auto expr_type = expr->getDynType();

    if (expr_type == SELF_TYPE)
        expr_type = global_class->getName();

    method_class *target = nullptr;
    auto expr_chain = *(classtable->getChains()[expr_type]);
    for (auto a_class : expr_chain) {
        auto a_class_methods = a_class->getMethods();
        if (a_class_methods.count(name) > 0)
            target = dynamic_cast<method_class *>(a_class_methods[name]);
    }

    if (target == nullptr) {
        classtable->semant_error(global_class, this) << "Dispatch to undefined method " << name << "." << endl;
        type = Object;
        return type;
    }

    auto target_formals = target->getFormals();
    if (target_formals->len() != actual->len())
        classtable->semant_error(global_class, this) << "Method " << name << " called with wrong number of arguments." << endl;

    int i = target_formals->first();
    int j = actual->first();
    while (target_formals->more(i) && actual->more(j)) {
        auto actual_type = actual->nth(j)->getDynType();
        auto target_type = target_formals->nth(i)->getType();
        auto target_name = target_formals->nth(i)->getName();
        if (checkInheRelation(actual_type, target_type) == false)
            classtable->semant_error(global_class, this) << "In call of method " << name << ", type " << actual_type << " of parameter " << target_name << " does not conform to declared type " << target_type << "." << endl;
        i = target_formals->next(i);
        j = actual->next(j);
    }

    auto target_type = target->getType();
    if (hasDefined(target_type)) {
        if (target_type == SELF_TYPE) {
            if (expr_type == global_class->getName())
                type = SELF_TYPE;
            else
                type = expr_type;
        } else {
            type = target_type;
        }
    } else {
        type = Object;
    }

    return type;
}

Symbol cond_class::getDynType() {
    auto cond = pred->getDynType();
    if (cond != Bool)
        classtable->semant_error(global_class, this) << "Predicate of 'if' does not have type Bool." << endl;
    auto e1_type = then_exp->getDynType();
    auto e2_type = else_exp->getDynType();
    type = getLowerType(e1_type, e2_type);
    return type;
}

Symbol loop_class::getDynType() {
    auto cond = pred->getDynType();
    if (cond != Bool)
        classtable->semant_error(global_class, this) << "Loop condition does not have type Bool." << endl;
    body->getDynType();
    type = Object;
    return type;
}

Symbol typcase_class::getDynType() {
    expr->getDynType();
    vector<Symbol> exist_branch_types;
    type = No_type;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        auto branch = (branch_class *)(cases->nth(i));
        auto branch_type = branch->getType();
        bool err_flag = false;
        if (existWithin(exist_branch_types, branch_type)) {
            classtable->semant_error(global_class, branch) << "Duplicate branch " << branch->getType() << " in case statement." << endl;
            err_flag = true;
        }

        if (err_flag == false)
            exist_branch_types.push_back(branch_type);

        type = (type == No_type) ? branch->getDynType() : getLowerType(type, branch->getDynType());
    }
    return type;
}

Symbol block_class::getDynType() {
    for (int i = body->first(); body->more(i); i = body->next(i))
        type = body->nth(i)->getDynType();
    return type;
}

Symbol let_class::getDynType() {
    attr_scope_tab.enterscope();
    auto init_type = init->getDynType();

    if (identifier == self)
        classtable->semant_error(global_class, this) << "'self' cannot be bound in a 'let' expression." << endl;

    if (hasDefined(type_decl) == false)
        classtable->semant_error(global_class, this) << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined." << endl;

    if (init_type != No_type && checkInheRelation(init_type, type_decl) == false)
        classtable->semant_error(global_class, this) << "Inferred type " << init_type << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << "." << endl;

    attr_scope_tab.addid(identifier, new Symbol(type_decl));

    type = body->getDynType();
    attr_scope_tab.exitscope();
    return type;
}

Symbol plus_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " + " << e2_type << endl;
    type = Int;
    return type;
}

Symbol sub_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " - " << e2_type << endl;
    type = Int;
    return type;
}

Symbol mul_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " * " << e2_type << endl;
    type = Int;
    return type;
}

Symbol divide_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " / " << e2_type << endl;
    type = Int;
    return type;
}

Symbol neg_class::getDynType() {
    auto e1_type = e1->getDynType();
    if (e1_type != Int)
        classtable->semant_error(global_class, this) << "Argument of '~' has type " << e1_type << " instead of Int." << endl;
    type = Int;
    return type;
}

Symbol lt_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " < " << e2_type << endl;
    type = Bool;
    return type;
}

Symbol eq_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (e1_type != e2_type) {
        vector<Symbol> illegal_comp_vec = {Int, Bool, Str};
        if (existWithin(illegal_comp_vec, e1_type) || existWithin(illegal_comp_vec, e2_type))   // If found, ILLEGAL!
            classtable->semant_error(global_class, this) << "Illegal comparison with a basic type." << endl;
    }
    type = Bool;
    return type;
}

Symbol leq_class::getDynType() {
    auto e1_type = e1->getDynType();
    auto e2_type = e2->getDynType();
    if (!(e1_type == Int && e2_type == Int))
        classtable->semant_error(global_class, this) << "non-Int arguments: " << e1_type << " <= " << e2_type << endl;
    type = Bool;
    return type;
}

Symbol comp_class::getDynType() {
    auto e1_type = e1->getDynType();
    if (e1_type != Bool)
        classtable->semant_error(global_class, this) << "Argument of 'not' has type " << e1_type << " instead of Bool." << endl;
    type = Bool;
    return type;
}

Symbol int_const_class::getDynType() {
    type = Int;
    return type;
}

Symbol bool_const_class::getDynType() {
    type = Bool;
    return type;
}

Symbol string_const_class::getDynType() {
    type = Str;
    return type;
}

Symbol new__class::getDynType() {
    type = Object;
    if (hasDefined(type_name))
        type = type_name;
    else
        classtable->semant_error(global_class, this) << "'new' used with undefined class " << type_name << "." << endl;
    return type;
}

Symbol isvoid_class::getDynType() {
    e1->getDynType();
    type = Bool;
    return type;
}

Symbol no_expr_class::getDynType() {
    type = No_type;
    return type;
}

Symbol object_class::getDynType() {
    type = Object;
    if (name == self)
        type = SELF_TYPE;
    else if (attr_scope_tab.lookup(name))
        type = *attr_scope_tab.lookup(name);
    else
        classtable->semant_error(global_class, this) << "Undeclared identifier " << name << "." << endl;
    return type;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////


ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}
ostream &ClassTable::semant_error(Class_ c, tree_node *t)
{
    return semant_error(c->get_filename(), t);
}
ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->check();

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

