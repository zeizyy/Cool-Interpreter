/*----------------------| Imports |-------------------------*/
const fs = require("fs")
const path = require("path")

/*----------------------| Flags   |-------------------------*/
var DEBUG = false


/*----------------------| Global Consts |--------------------------*/
const NATIVE_CLASSES = new Set(['Object', 'IO', 'String', 'Int', 'Bool'])


/*----------------------| Utility Functions |------------------------*/
function debug_log(...messages) {
    if (DEBUG) {
        console.log(` | DEBUG : ${messages.join(" ")}`)
    }
}

function* range(n) {
    for (let i = 0; i < n; ++i) {
        yield i
    }
}

function is_valid_class_and_selftype(class_name) {
    return class_name in name2map || class_name === "SELF_TYPE"
}


function list_conform(list1, list2, compare_func = is_sub_type) {
    if (list1.length !== list2.length) {
        return false
    } else {
        for (let n of range(list1.length)) {
            if (!compare_func(list1[n], list2[n])) {
                return false;
            }
        }
    }
    return true;
}


function replace_self_type(type, default_type) {
    if (type === 'SELF_TYPE') {
        return default_type
    } else {
        return type
    }
}

function first_duplicate(arr, key = 0) {
    l = arr
    if (key != 0) {
        l = l.map(key)
    }

    for (let i = 0; i < l.length; ++i) {
        if (l.indexOf(l[i]) < i) {
            return arr[i]
        }
    }
    return false
}

function first_diff(a, b, c_func = x => x) {
    for (let i of range(a.length)) {
        if (c_func(a[i]) !== c_func(a[i]))
            return a[i]
    }
    return false;
}


/*------------------| Type Checking |------------------*/
function is_sub_type(child, parent, c) {
    if (child === 'SELF_TYPE') {
        child = c;
    }

    if (parent === 'SELF_TYPE') {
        parent = c;
    }

    if (child === parent) {
        return true;
    } else if (child === "Object") {
        return false;
    } else {
        return is_sub_type(name2map[child].parent.name.ident, parent, c);
    }
}

function parent_of(type) {
    if (type === 'Object') {
        return 'Object'
    }
    return name2map[type].parent.name.ident
}

function list_of_inheritance(type) {
    if (type === 'Object') {
        return ['Object']
    } else {
        k = list_of_inheritance(parent_of(type))
        k.push(type)
        return k
    }
}

function lca(type_1, type_2, c) {
    r_type_1 = replace_self_type(type_1, c);
    r_type_2 = replace_self_type(type_2, c);

    parent_1 = list_of_inheritance(r_type_1)
    parent_2 = list_of_inheritance(r_type_2)
    const set = new Set(parent_1)
    for (let i of range(parent_2.length)) {
        if (!set.has(parent_2[i])) {
            return parent_2[i - 1];
        }
    }

    lca_type = parent_2[parent_2.length - 1];

    if (type_1 === "SELF_TYPE" && r_type_1 === lca_type ||
        type_2 === "SELF_TYPE" && r_type_2 === lca_type) {
        return "SELF_TYPE";
    }

    return lca_type;
}

function type_check_program(ast) {
    const m = {}
    for (let cls of Object.keys(name2map).map(key => name2map[key])) {
        c = cls.name.ident;
        m[c] = {};
        for (let method of cls.features.filter(f => f.kind === 'method')) {
            m[c][method.name.ident] = {
                'ret_type': method.ret_type.ident,
                'formal_types': method.formals.map(formal => formal.f_type.ident)
            }
        }
    }

    for (let cls of ast.classes.filter(cls => !NATIVE_CLASSES.has(cls.name.ident))) {
        type_check_class(cls, cls.name.ident, m, {})
    }
}

function type_check_class(cls, c, m, o) {
    const attrs = cls.features.filter(f => f.kind !== 'method')
    for (attr of attrs) {
        let name = attr.name.ident
        let type = attr.f_type.ident
        push_in_o(name, type, o)
    }
    push_in_o('self', 'SELF_TYPE', o);

    // Check all init attributes
    cls.features.filter(f => f.kind !== 'method' && f.init).map(attr => {
        type = type_check(attr.init, c, m, o);
        if (!is_sub_type(type, attr.f_type.ident, c)) {
            output_error(attr.name.line, `Attribute initializer for ${attr.name.ident} does not conform to ${attr.f_type.ident}`);
        }
    })

    // Check method
    cls.features.filter(f => f.kind === 'method').map(method => {
        for (formal of method.formals) {
            push_in_o(formal.name.ident, formal.f_type.ident, o)
        }
        type = type_check(method.body, c, m, o);
        method.body.t = type;
        for (formal of method.formals) {
            pop_from_o(formal.name.ident, o)
        }
        if (method.ret_type.ident === 'SELF_TYPE' && type !== 'SELF_TYPE') {
            output_error(method.name.line, `${type} does not conform to SELF_TYPE(${cls.name.ident}) in method ${method.name.ident}`)
        }
        if (!is_sub_type(
                replace_self_type(type, c),
                method.ret_type.ident, c)) {
            output_error(method.name.line, `Return type for ${method.name.ident} does not conform to ${method.ret_type.ident}`);
        }


    })
    pop_from_o('self', o)
}

function push_in_o(name, type, o) {
    if (typeof o[name] === 'undefined') {
        o[name] = [type]
        return
    }
    o[name].unshift(type)
    return o
}

function pop_from_o(name, o) {
    if (name in o) {
        o[name].shift()
    } else {
        output_error(0, "POP from O: error")
    }
}

function type_in_o(identifier, o) {
    if (typeof o[identifier.ident] === 'undefined' || o[identifier.ident].length === 0) {
        output_error(identifier.line, `unbound identifier ${identifier.ident}`)
    }
    return o[identifier.ident][0];
}

function type_check(expr, c, m, o) {
    let t = '';
    debug_log('expr', expr.kind, ':', expr.line)

    switch (expr.kind) {
        case 'identifier':
            {
                t = type_in_o(expr.identifier, o);
                break;
            }
        case 'assign':
            {
                if (expr.var.ident === 'self') {
                    output_error(expr.var.line, `cannot assign to self`)
                }
                const t_var = type_in_o(expr.var, o);
                const t_rhs = type_check(expr.rhs, c, m, o);
                if (is_sub_type(t_rhs, t_var, c)) {
                    t = t_rhs
                } else {
                    output_error(expr.var.line, `Assignment does not conform`)
                }
                break;
            }
        case 'true':
        case 'false':
            {
                t = 'Bool'
                break
            }
        case 'integer':
            {
                t = 'Int'
                break
            }
        case 'string':
            {
                t = 'String'
                break
            }
        case 'new':
            {
                t = expr.class.ident;
                break;
            }
        case 'self_dispatch':
            {
                const t0 = 'SELF_TYPE';
                const args_types = expr.args.map(arg => type_check(arg, c, m, o))

                const method = expr.method.ident
                const t0p = replace_self_type(t0, c);

                if (!(method in m[t0p])) {
                    output_error(expr.line, `unknown method ${method} in dispatch on ${t0p}`);
                }

                const func_formal_types = m[t0p][method].formal_types
                const func_ret_type = m[t0p][method].ret_type

                if (func_formal_types.length !== args_types.length) {
                    output_error(expr.line,
                        `wrong number of actual arguments (${args_types.length} vs. ${func_formal_types.length})`)
                }

                for (let n of range(func_formal_types.length)) {
                    if (!is_sub_type(args_types[n], func_formal_types[n], c)) {
                        output_error(expr.method.line, `argument #${n + 1} type ${args_types[n]} does not conform to formal type ${func_formal_types[n]}.`)
                    }
                }

                const ret_type = replace_self_type(func_ret_type, t0);

                t = ret_type
                break;
            }
        case 'dynamic_dispatch':
            {
                const t0 = type_check(expr.e, c, m, o)
                const args_types = expr.args.map(arg => type_check(arg, c, m, o))

                const method = expr.method.ident
                const t0p = replace_self_type(t0, c)



                if (!(method in m[t0p])) {
                    output_error(expr.line, `unknown method ${method} in dispatch on ${t0p}`);
                }

                const func_formal_types = m[t0p][method].formal_types
                const func_ret_type = m[t0p][method].ret_type

                if (func_formal_types.length !== args_types.length) {
                    output_error(expr.line,
                        `wrong number of actual arguments (${args_types.length} vs. ${func_formal_types.length})`)
                }

                for (let n of range(func_formal_types.length)) {
                    if (!is_sub_type(args_types[n], func_formal_types[n], c)) {
                        output_error(expr.method.line, `argument #${n + 1} type ${args_types[n]} does not conform to formal type ${func_formal_types[n]}.`)
                    }
                }

                const ret_type = replace_self_type(func_ret_type, t0)

                t = ret_type
                break;
            }
        case 'static_dispatch':
            {
                const t0 = type_check(expr.e, c, m, o)
                const args_types = expr.args.map(arg => type_check(arg, c, m, o))
                const T = expr.d_type.ident
                if (T === 'SELF_TYPE') {
                    output_error(expr.line, `SELF_TYPE(${c}) does not conform to SELF_TYPE in static dispatch`)
                }


                if (!is_sub_type(t0, T, c)) {
                    output_error(expr.line, `${t0} does not conform to ${T} in static dispatch`)
                }

                const method = expr.method.ident

                if (!(method in m[T])) {
                    output_error(expr.line, `unknown method ${method} in dispatch on ${T}`);
                }

                const func_formal_types = m[T][method].formal_types
                const func_ret_type = m[T][method].ret_type

                if (func_formal_types.length !== args_types.length) {
                    output_error(expr.line,
                        `wrong number of actual arguments (${args_types.length} vs. ${func_formal_types.length})`)
                }


                for (let n of range(func_formal_types.length)) {
                    if (!is_sub_type(args_types[n], func_formal_types[n], c)) {
                        output_error(expr.method.line, `argument #${n + 1} type ${args_types[n]} does not conform to formal type ${func_formal_types[n]}.`)
                    }
                }

                const ret_type = replace_self_type(func_ret_type, t0)

                t = ret_type
                break;
            }
        case 'if':
            {
                const predicate_t = type_check(expr.predicate, c, m, o)
                if (predicate_t !== 'Bool') {
                    output_error(expr.line, `conditional has type ${predicate_t} instead of Bool`)
                }
                const then_t = type_check(expr.then, c, m, o)

                const else_t = type_check(expr.else, c, m, o)
                debug_log('IF:', 'then', then_t, 'else', else_t)
                t = lca(then_t, else_t, c)
                break;
            }
        case 'block':
            {
                t = type_check_sequence(expr.body, c, m, o);
                break;
            }
        case 'let':
            {
                t = type_check_let(expr, expr.exp_list, expr.body, c, m, o)
                break;
            }
        case 'case':
            {
                const e0 = expr.value
                const case_list = expr.case_list

                // TODO: figure out if need to check this.
                const bound_twice = first_duplicate(case_list, each_case => each_case.m_type.ident)
                if (bound_twice !== false) {
                    output_error(bound_twice.m_type.line, `case branch type ${bound_twice.m_type.ident} is bound twice`)
                }


                type_check(e0, c, m, o);

                t = case_list
                .map(each_case => {
                    push_in_o(each_case.m_ident.ident, each_case.m_type.ident, o);
                    const type = type_check(each_case.action, c, m, o);

                    if (each_case.m_type.ident === 'SELF_TYPE') {
                        output_error(
                            each_case.m_type.line, `using SELF_TYPE as a case branch type is not allowed`
                        )
                    }

                    if (each_case.m_ident.ident === 'self') {
                        output_error(
                            each_case.m_type.line, `binding self in a case expression is not allowed`
                        )
                    }
                    pop_from_o(each_case.m_ident.ident, o);
                    return type;
                })
                .reduce(
                    (acc, val) => lca(acc, val, c)
                );
                break;
            }
        case 'while':
            {
                const predicate_t = type_check(expr.predicate, c, m, o)

                if (predicate_t !== 'Bool') {
                    output_error(expr.line, `predicate has type ${predicate_t} Instead of Bool`)
                }

                const body_t = type_check(expr.body, c, m, o)
                t = 'Object'
                break;
            }
        case 'isvoid':
            {
                const e_type = type_check(expr.e, c, m, o)
                t = 'Bool'
                break;
            }
        case 'not':
            {
                const x_type = type_check(expr.x, c, m, o)
                if (!x_type === 'Bool') {
                    output_error(expr.line, `not applied to type ${x_type} instead of Bool`)
                }
                t = 'Bool'
                break;
            }
        case 'negate':
            {
                const x_type = type_check(expr.x, c, m, o)
                if (!x_type === 'Int') {
                    output_error(expr.line, `negate applied to type ${x_type} instead of Int`)
                }
                t = 'Int'
                break;
            }
        case 'plus':
        case 'minus':
        case 'times':
        case 'divide':
            {
                const x_type = type_check(expr.x, c, m, o)
                const y_type = type_check(expr.y, c, m, o)
                debug_log('ARITHMETIC', expr.line, x_type, y_type)
                if (x_type !== 'Int') {
                    output_error(expr.line, `${expr.kind} applied to type ${x_type} instead of Ints`)
                }
                if (y_type !== 'Int') {
                    output_error(expr.line, `${expr.kind} applied to type ${y_type} instead of Ints`)
                }

                t = 'Int'
                break;
            }
        case 'eq':
        case 'lt':
        case 'le':
            {
                const x_type = type_check(expr.x, c, m, o)
                const y_type = type_check(expr.y, c, m, o)

                const NATIVE = ['Int', 'String', 'Bool']

                if (NATIVE.includes(x_type) || NATIVE.includes(y_type)) {
                    if (x_type != y_type) {
                        output_error(expr.line, `Comparison between ${x_type} and ${y_type}`)
                    }
                }

                t = 'Bool'
                break;
            }
        default:
            output_error(-1, `Type Checking expression ${expr.kind}`)
            break;
    }
    debug_log('expr', expr.kind, ':', expr.line, ':', t);
    expr.t = t;
    return t;

}

function type_check_sequence(sequence, c, m, o) {
    const types = sequence.map(expr => type_check(expr, c, m, o))
    return types[types.length - 1]
}

function type_check_let(expr, let_list, let_body, c, m, o) {
    if (let_list.length === 0) {
        return type_check(let_body, c, m, o);
    } else {
        const single_let = let_list[0]
        const name = single_let.name.ident
        const kind = single_let.kind
        const type = single_let.type.ident;
        if (kind === 'let_binding_init') {
            const init = single_let.init
            const t1 = type_check(init, c, m, o)
            if (!is_sub_type(t1, type, c)) {
                output_error(expr.line, `Initializer type ${t1} does not conform to type ${type}`)
            }
        }
        push_in_o(name, type, o)
        const t = type_check_let(expr, let_list.slice(1), let_body, c, m, o)
        pop_from_o(name, o)
        return t
    }
}

// ---------------------------| Non-type Checking Processes |------------------

function construct_class_tree(classes) {
    for (let i = 0; i < classes.length; i++) {
        class_obj = classes[i];
        if (class_obj.name.ident === 'Object') {
            class_obj.parent = 'Root'
        }
        if (!(class_obj.parent.ident in name2map)) {
            // Check to see if a class inherits from an undeclared class.
            output_error(class_obj.parent.line, `class ${class_obj.name.ident} inherits from unknown class ${class_obj.parent.ident}`)
        }
        class_obj.parent = name2map[class_obj.parent.ident];
    }
}

function has_cycle(cls) {
    if (cls.name.ident === "Object") {
        return false;
    }

    cls.visited = true;
    if (cls.parent.visited === true || has_cycle(cls.parent)) {
        output_error(0, `inheritance cycle: ${cls.parent.name.ident} ${cls.name.ident}`)
    }
    cls.visited = false;
    return false;
}

function have_cycle(ast) {
    // Check for cycles in the class hierarchy.
    ast.classes.map(has_cycle);
    return;
}

// Haven't looked into for refactoring
function copy_parent_features(cls) {
    if (cls.name.ident === "Object") {
        return cls.features;
    }

    if (!cls.visited) {
        parent_features = copy_parent_features(cls.parent);
        // Check for a child class that redefines a parent method but changes the parameters.
        // Check if a child class redefines a parent attribute
        const cls_name = cls.name.ident;

        for (const cur_feature of cls.features) {
            const name = cur_feature.name.ident;
            const kind = (cur_feature.kind === "method") ? "method" : "attribute";
            const lineno = cur_feature.name.line;

            for (const cur_parent_feature of parent_features) {
                const parent_name = cur_parent_feature.name.ident;
                const parent_kind = (cur_parent_feature.kind === "method") ? "method" : "attribute";
                if (name === parent_name && kind === parent_kind) {
                    if (kind === "method") {
                        const cur_formals = cur_feature.formals;
                        const cur_parent_formals = cur_parent_feature.formals;
                        if (cur_formals.length !== cur_parent_formals.length) {
                            output_error(lineno, `class ${cls_name} redefines method ${name} and changes number of formals`)
                        } else {
                            const diff = first_diff(cur_formals, cur_parent_formals, formal => formal.f_type.ident);
                            if (diff) {
                                output_error(lineno, `class ${cls_name} redefines method ${name} and changes type of formal ${diff.name.ident}`)
                            }
                            const cur_ret = cur_feature.ret_type.ident;
                            const cur_parent_ret = cur_parent_feature.ret_type.ident;
                            if (cur_ret !== cur_parent_ret) {
                                output_error(lineno, `class ${cls_name} redefines method ${name} and changes return type (from ${cur_parent_ret} to ${cur_ret})`)
                            }
                        }
                    } else {
                        output_error(lineno, `class ${cls_name} redefines attribute ${name}`)
                    }
                }
            }
        }

        cls.features = parent_features.concat(cls.features);
        cls.visited = true;
    }
    return cls.features;
}

/**
 * Helper function to read in cl-ast formatted items
 */
function read_list(lines, f) {
    const list = [];
    const num_items = lines.shift();
    for (let i = 0; i < num_items; i++) {
        list.push(f(lines))
    }
    return list;
}

function list2stringClass(l) {
    let str = "";
    str += l.length;
    if (l.length > 0) {
        str += "\n";
        str += l.map(e => e.toStringClass()).join("");
    } else {
        str += "\n"
    }
    return str;
}

function list2stringImp(l) {
    let str = "";
    str += l.length;
    if (l.length > 0) {
        str += "\n";
        str += l.map(e => {
            return e.toStringImp()
        }).join("");
    } else {
        str += "\n"
    }
    return str;
}

function list2stringAnnotated(l) {
    let str = "";
    str += l.length;
    if (l.length > 0) {
        str += "\n";
        str += l.map(e => {
            return e.toStringAnnotated();
        }).join("");
    } else {
        str += "\n"
    }
    return str;
}

function read_program(lines) {
    const program = {};

    program.classes = read_list(lines, read_class);

    Object.keys(name2map).map(key => name2map[key])
        .forEach(
            cls => cls.features
            .forEach(method => {
                method.inherited = cls.name.ident;
            }));
    // ------------------| Main / main checks |---------------------
    if (!("Main" in name2map)) {
        // Check for a missing method main in class Main.
        output_error(0, "class Main not found");
    }
    construct_class_tree(program.classes);

    have_cycle(program);
    Object.keys(name2map)
        .map(key => name2map[key])
        .map(copy_parent_features);

    let has_main = false;
    name2map["Main"]
        .features
        .filter(feature => feature.kind === "method")
        .forEach(method => {
            if (method.name.ident === "main") {
                has_main = true;
                if (method.formals.length !== 0) {
                    output_error(0, "class Main method main with 0 parameters not found")
                }
            }
        })

    if (!has_main) {
        output_error(0, "class Main method main not found");
    }

    Object.keys(name2map)
        .forEach(key => {
            const cls = name2map[key];
            if (!cls.features) {
                return
            }
            cls.features.forEach(feature => {
                if (feature.kind === "method") {
                    if (!is_valid_class_and_selftype(feature.ret_type.ident)) {
                        output_error(feature.name.line, `class ${cls.name.ident} has method ${feature.name.ident} with unknown return type ${feature.ret_type.ident}`)
                    }
                    feature.formals.forEach(formal => {
                        if (!(formal.f_type.ident in name2map)) {
                            output_error(formal.f_type.line, `class ${cls.name.ident} has method ${feature.name.ident} with formal parameter of unknown type ${formal.f_type.ident}`)
                        }
                    })
                } else {
                    if (!is_valid_class_and_selftype(feature.f_type.ident)) {
                        output_error(feature.name.line, `class ${cls.name.ident} has attrubute ${feature.name.ident} with unknown type ${feature.f_type.ident}`)
                    }
                }
            })
        })

    program.toStringClass = function() {
        return list2stringClass(this.classes);
    };

    program.toStringAnnotated = function() {
        return list2stringAnnotated(this.classes);
    };

    return program;
}

function read_class(lines) {
    const class_obj = {};
    class_obj.name = read_ident(lines);

    class_obj.inherit_type = lines.shift();
    if (class_obj.inherit_type === "no_inherits") {
        class_obj.parent = { "ident": "Object" }; // changed from false
    } else {
        class_obj.parent = read_ident(lines);
        class_obj.parent_ident = class_obj.parent;
    }

    if (class_obj.name.ident === "SELF_TYPE") {
        output_error(class_obj.name.line, "class named SELF_TYPE");
    }

    // Check to see if a class redefines another class
    if (class_obj.name.ident in name2map || ["Bool", "Int", "String", "IO", "Object"].includes(class_obj.name.ident)) {
        output_error(class_obj.name.line, `class ${class_obj.name.ident} redefined`);
    }

    // Check to see if a class inherits from Int (etc.).
    if (["Bool", "Int", "String"].includes(class_obj.parent.ident)) {
        output_error(class_obj.name.line, `class ${class_obj.name.ident} inherits from ${class_obj.parent.ident}`);
    }

    // Check to see if a class inherits from SELF_TYPE
    if (class_obj.parent.ident === "SELF_TYPE") {
        output_error(class_obj.name.line, `class ${class_obj.name.ident} inherits from unknown class SELF_TYPE`)
    }

    name2map[class_obj.name.ident] = class_obj;

    class_obj.features = read_list(lines, read_feature);
    class_obj.features
        .filter(feat => feat.kind === "method")
        .forEach(method => {
            method.formals.forEach(formal => {
                if (formal.name.ident === "self") {
                    output_error(formal.name.line, `class ${class_obj.name.ident} has method ${method.name.ident} with formal parameter named self`)
                }
            })
            const formal = first_duplicate(method.formals, formal => formal.name.ident);
            if (formal != false) {
                output_error(formal.name.line, `class ${class_obj.name.ident} has method ${method.name.ident} with duplicate formal parameter named ${formal.name.ident}`)
            }

        });

    class_obj.features
        .filter(feature => feature.kind !== "method")
        .forEach(attr => {
            if (attr.name.ident === "self") {
                output_error(attr.name.line, `class ${class_obj.name.ident} has an attribute named self`)
            }
        });

    // Check for duplicate method or attribute definitions in the same class.
    const attributes = class_obj.features
        .filter(feature => feature.kind !== "method");
    const dup_attribute = first_duplicate(attributes, attribute => attribute.name.ident);
    if (dup_attribute) {
        output_error(dup_attribute.name.line, `class ${class_obj.name.ident} redefines attribute ${dup_attribute.name.ident}`);
    }

    const methods = class_obj.features
        .filter(feature => feature.kind == "method");
    const dup_method = first_duplicate(methods, method => method.name.ident);
    if (dup_attribute) {
        output_error(dup_method.name.line, `class ${class_obj.name.ident} redefines attribute ${dup_method.name.ident}`);
    }

    class_obj.toStringClass = function() {
        let str = "";
        str += `${this.name.ident}\n`;
        const attribute_list = this.features.filter(item => item.kind !== "method");
        str += `${attribute_list.length}\n`;
        for (let i = 0; i < attribute_list.length; i++) {
            cur_attribute = attribute_list[i];
            str += `${(cur_attribute.kind == "attribute_no_init") ? "no_initializer" : "initializer"}\n`;
            str += `${cur_attribute.name.ident}\n`;
            str += `${cur_attribute.f_type.ident}\n`;
            if (cur_attribute.kind === "attribute_init") {
                str += cur_attribute.init.toStringClass();
            }
        }
        return str;
    };

    class_obj.toStringAnnotated = function() {
        if (NATIVE_CLASSES.has(this.name.ident)) {
            return "";
        }

        let str = "";
        str += this.name.toStringAnnotated();
        str += `${this.inherit_type}\n`;
        if (this.inherit_type === "inherits") {
            str += this.parent_ident.toStringAnnotated();
        }
        const self_feature_list = this.features.filter(feature => feature.inherited === this.name.ident);
        str += list2stringAnnotated(self_feature_list);
        return str;
    };

    return class_obj;
}

function read_ident(lines) {
    const ident = {};
    ident.line = lines.shift();
    ident.ident = lines.shift();
    ident.toStringClass = function() {
        let str = "";
        str += `${this.line}\n`;
        str += `${this.ident}\n`;
        return str;
    };
    ident.toStringImp = ident.toStringClass;
    ident.toStringAnnotated = ident.toStringClass;
    return ident;
}

function read_case_element(lines) {
    const case_element = {};
    case_element.m_ident = read_ident(lines);
    case_element.m_type = read_ident(lines);
    case_element.action = read_exp(lines);
    case_element.toStringClass = function() {
        let str = "";
        str += this.m_ident.toStringClass();
        str += this.m_type.toStringClass();
        str += this.action.toStringClass();
        return str;
    };

    case_element.toStringImp = function() {
        let str = "";
        str += this.m_ident.toStringClass();
        str += this.m_type.toStringClass();
        str += this.action.toStringImp();
        return str;
    };

    case_element.toStringAnnotated = function() {
        let str = "";
        str += this.m_ident.toStringAnnotated();
        str += this.m_type.toStringAnnotated();
        str += this.action.toStringAnnotated();
        return str;
    };

    return case_element;
}

function read_each_let(lines) {
    const each_let = {};
    each_let.kind = lines.shift();
    each_let.name = read_ident(lines);
    each_let.type = read_ident(lines);
    if (each_let.kind === "let_binding_init") {
        each_let.init = read_exp(lines);
    }
    if (each_let.name.ident === 'self') {
        output_error(each_let.name.line, 'binding self in a let is not allowed')
    }
    each_let.toStringClass = function() {
        let str = "";
        str += `${this.kind}\n`;
        str += this.name.toStringClass();
        str += this.type.toStringClass();
        if (this.kind == "let_binding_init") {
            str += this.init.toStringClass();
        }
        return str;
    }

    each_let.toStringImp = function() {
        let str = "";
        str += `${this.kind}\n`;
        str += this.name.toStringClass();
        str += this.type.toStringClass();
        if (this.kind == "let_binding_init") {
            str += this.init.toStringImp();
        }
        return str;
    }

    each_let.toStringAnnotated = function() {
        let str = "";
        str += `${this.kind}\n`;
        str += this.name.toStringAnnotated();
        str += this.type.toStringAnnotated();
        if (this.kind == "let_binding_init") {
            str += this.init.toStringAnnotated();
        }
        return str;
    }
    return each_let;
}

function read_feature(lines) {
    const feature_obj = {};
    feature_obj.kind = lines.shift();
    switch (feature_obj.kind) {
        case "attribute_no_init":
            feature_obj.name = read_ident(lines);
            feature_obj.f_type = read_ident(lines);
            feature_obj.toStringAnnotated = function() {
                let str = "";
                str += `${this.kind}\n`;
                str += this.name.toStringAnnotated();
                str += this.f_type.toStringAnnotated();
                return str;
            };
            break;
        case "attribute_init":
            feature_obj.name = read_ident(lines);
            feature_obj.f_type = read_ident(lines);
            feature_obj.init = read_exp(lines);
            feature_obj.toStringAnnotated = function() {
                let str = "";
                str += `${this.kind}\n`;
                str += this.name.toStringAnnotated();
                str += this.f_type.toStringAnnotated();
                str += this.init.toStringAnnotated();
                return str;
            }
            break;
        case "method":
            feature_obj.name = read_ident(lines);
            feature_obj.formals = read_list(lines, read_formal);
            feature_obj.ret_type = read_ident(lines);
            feature_obj.body = read_exp(lines);
            feature_obj.toStringAnnotated = function() {
                let str = "";
                str += `${this.kind}\n`;
                str += this.name.toStringAnnotated();
                str += list2stringAnnotated(this.formals);
                str += this.ret_type.toStringAnnotated();
                str += this.body.toStringAnnotated();
                return str;
            }
            break;
        default:
            break; // may want more checking here
    }
    return feature_obj;
}

function read_formal(lines) {
    const formal_obj = {};
    formal_obj.name = read_ident(lines);
    formal_obj.f_type = read_ident(lines);
    formal_obj.toStringAnnotated = function() {
        let str = "";
        str += this.name.toStringAnnotated();
        str += this.f_type.toStringAnnotated();
        return str;
    }
    return formal_obj;
}

function read_exp(lines) {
    const exp_obj = {};

    exp_obj.line = lines.shift();
    exp_obj.kind = lines.shift();

    switch (exp_obj.kind) {
        case "assign":
            exp_obj.var = read_ident(lines);
            exp_obj.rhs = read_exp(lines);
            exp_obj.fields = ["var", "rhs"];
            break;
        case "dynamic_dispatch":
            exp_obj.e = read_exp(lines);
            exp_obj.method = read_ident(lines);
            exp_obj.args = read_list(lines, read_exp);
            exp_obj.fields = ["e", "method", "args"];
            break;
        case "static_dispatch":
            exp_obj.e = read_exp(lines);
            exp_obj.d_type = read_ident(lines);
            exp_obj.method = read_ident(lines);
            exp_obj.args = read_list(lines, read_exp);
            exp_obj.fields = ["e", "d_type", "method", "args"];
            break;
        case "self_dispatch":
            exp_obj.method = read_ident(lines);
            exp_obj.args = read_list(lines, read_exp);
            exp_obj.fields = ["method", "args"];
            break;
        case "if":
            exp_obj.predicate = read_exp(lines);
            exp_obj.then = read_exp(lines);
            exp_obj.else = read_exp(lines);
            exp_obj.fields = ["predicate", "then", "else"];
            break;
        case "while":
            exp_obj.predicate = read_exp(lines);
            exp_obj.body = read_exp(lines);
            exp_obj.fields = ["predicate", "body"];
            break;
        case "block":
            exp_obj.body = read_list(lines, read_exp);
            exp_obj.fields = ["body"];
            break;
        case "new":
            exp_obj.class = read_ident(lines);
            exp_obj.fields = ["class"];
            break;
        case "isvoid":
            exp_obj.e = read_exp(lines);
            exp_obj.fields = ["e"];
            break;
        case "plus":
        case "minus":
        case "times":
        case "divide":
        case "lt":
        case "le":
        case "eq":
            exp_obj.x = read_exp(lines);
            exp_obj.y = read_exp(lines);
            exp_obj.fields = ["x", "y"];
            break;
        case "not":
        case "negate":
            exp_obj.x = read_exp(lines);
            exp_obj.fields = ["x"];
            break;
        case "string":
        case "integer":
            exp_obj.const = lines.shift();
            exp_obj.fields = []; // special case to deal with
            break;
        case "true":
        case "false":
            exp_obj.const = exp_obj.kind;
            exp_obj.fields = []; // special case to deal with
            break;
        case "identifier":
            exp_obj.identifier = read_ident(lines);
            exp_obj.fields = ["identifier"];
            break;
        case "case":
            exp_obj.value = read_exp(lines);
            exp_obj.case_list = read_list(lines, read_case_element);
            exp_obj.fields = ["value", "case_list"];
            break;
        case "let":
            exp_obj.exp_list = read_list(lines, read_each_let);
            exp_obj.body = read_exp(lines);
            exp_obj.fields = ["exp_list", "body"];
            break;
        default:
            console.log(exp_obj);
            console.log("should not reach this line?");
            break;
    }

    exp_obj.toStringClass = function() {
        let str = "";
        str += `${this.line}\n`;
        str += `${this.t}\n`;
        str += `${this.kind}\n`;
        if (this.kind == "string" || this.kind == "integer") {
            str += `${this.const}\n`;
        } else {
            for (let i = 0; i < this.fields.length; i++) {
                let cur_field = this[this.fields[i]];
                if (Array.isArray(cur_field)) {
                    str += list2stringClass(cur_field);
                } else {
                    str += cur_field.toStringClass();
                }
            }
        }
        return str;
    }

    exp_obj.toStringImp = function() {
        let str = "";
        str += `${this.line}\n`;
        str += `${this.t}\n`;
        str += `${this.kind}\n`;
        if (this.kind == "string" || this.kind == "integer") {
            str += `${this.const}\n`;
        } else {
            for (let i = 0; i < this.fields.length; i++) {
                let cur_field = this[this.fields[i]];

                if (Array.isArray(cur_field)) {

                    str += list2stringImp(cur_field);
                } else {
                    str += cur_field.toStringImp();

                }
            }
        }
        return str;
    }

    exp_obj.toStringAnnotated = function() {
        let str = "";
        str += `${this.line}\n`;
        str += `${this.t}\n`;
        str += `${this.kind}\n`;
        if (this.kind == "string" || this.kind == "integer") {
            str += `${this.const}\n`;
        } else {
            for (let i = 0; i < this.fields.length; i++) {
                let cur_field = this[this.fields[i]];
                if (Array.isArray(cur_field)) {
                    str += list2stringAnnotated(cur_field);
                } else {
                    str += cur_field.toStringAnnotated();
                }
            }
        }
        return str;
    }

    return exp_obj;
}

// ---------------------| Printer Methods |-----------------------

function generate_class_map(ast) {
    let str = "";
    str += "class_map\n";
    const sorted_extended_classes = Object.keys(name2map).sort().map(key => name2map[key]);
    str += list2stringClass(sorted_extended_classes);
    return str;
}

function generate_parent_map(ast) {
    let str = `parent_map\n`
    let class_names = Object.keys(name2map)
    str += `${class_names.length - 1}\n`
    str += class_names
        .filter(name => name != 'Object')
        .sort()
        .map(name => `${name}\n${name2map[name].parent.name.ident}\n`)
        .join("")
    return str
}

function generate_implementation_map(ast) {
    let str = `implementation_map\n`;
    let class_names = Object.keys(name2map);
    str += `${class_names.length}\n`;
    for (let cls_name of class_names.sort()) {
        str += `${cls_name}\n`;
        let methods = name2map[cls_name].features.filter(f => f.kind === 'method');
        let inherited_methods = [];
        let overridden_methods = [];
        let self_methods = [];
        let methods_set = new Set();
        let copied_methods = methods
            .filter(method => method.inherited !== cls_name);
        let copied_method_names = copied_methods.map(method => method.name.ident);
        let internal_methods = methods
            .filter(method => method.inherited === cls_name);
        internal_methods.forEach(method => {
            if (copied_method_names.includes(method.name.ident)) {
                overridden_methods.push(method);
            } else {
                self_methods.push(method);
            }
        });

        let overridden_methods_dict = {};
        overridden_methods.forEach(method => {
            overridden_methods_dict[method.name.ident] = method;
        });

        let copied_methods_dict = {};
        copied_methods.forEach(method => {
            copied_methods_dict[method.name.ident] = method;
        });

        copied_methods.forEach(method => {
            let method_name = method.name.ident;
            if (!methods_set.has(method_name)) {
                if (method_name in overridden_methods_dict) {
                    inherited_methods.push(overridden_methods_dict[method_name]);
                } else {
                    inherited_methods.push(copied_methods_dict[method_name]);
                }
                methods_set.add(method_name);
            }
        });

        let combined_methods = inherited_methods.concat(self_methods);

        str += `${combined_methods.length}\n`;
        for (let method of combined_methods) {

            str += `${method.name.ident}\n`;
            let formals = method.formals;
            str += `${formals.length}\n`;
            for (let formal of formals) {
                str += `${formal.name.ident}\n`;
            }
            str += `${method.inherited}\n`;
            str += `${method.body.toStringImp()}`;
        }
    }
    return str;
}

function generate_annotated_ast(ast) {
    return ast.toStringAnnotated();
}

function output_error(line, msg) {
    console.log(`ERROR: ${line}: Type-Check: ${msg}`);
    process.exit();
}



if (process.argv.length < 3) {
    // node.js main.js [cool-ast file]
    console.log("usage: node main.js [cl-ast file]");
    process.exit(1);
}

// read in the file
const data = fs.readFileSync(process.argv[2], "utf8", (err, data) => {
    // this function is called when the file data is returned
    if (err) {
        console.log("Couldn't read in file.");
        process.exit(1);
    }
});

OBJ = {
    "name": {
        "line": "1",
        "ident": "Object"
    },
    "features": [{
            "kind": "method",
            "name": {
                "line": "2",
                "ident": "abort"
            },
            "formals": [

            ],
            "ret_type": {
                "line": "2",
                "ident": "Object"
            },
            "body": {
                "line": "0",
                "kind": "integer",
                "const": "0",
            }
        },
        {
            "kind": "method",
            "name": {
                "line": "4",
                "ident": "copy"
            },
            "formals": [

            ],
            "ret_type": {
                "line": "4",
                "ident": "SELF_TYPE"
            },
            "body": {
                "line": "5",
                "kind": "identifier",
                "identifier": {
                    "line": "5",
                    "ident": "self"
                },
                "fields": [
                    "identifier"
                ]
            }
        },
        {
            "kind": "method",
            "name": {
                "line": "3",
                "ident": "type_name"
            },
            "formals": [

            ],
            "ret_type": {
                "line": "3",
                "ident": "String"
            },
            "body": {
                "line": "3",
                "kind": "string",
                "const": "Object",
                "fields": [

                ]
            }
        }
    ]
}

// create dummy objects for the built-in classes
name2map = {
    "Object": OBJ,
    "IO": {
        "name": {
            "line": "9",
            "ident": "IO"
        },
        "parent": OBJ,
        "features": [{
                "kind": "method",
                "name": {
                    "line": "13",
                    "ident": "in_int"
                },
                "formals": [

                ],
                "ret_type": {
                    "line": "13",
                    "ident": "Int"
                },
                "body": {
                    "line": "13",
                    "kind": "integer",
                    "const": "0",
                    "fields": [

                    ]
                }
            },
            {
                "kind": "method",
                "name": {
                    "line": "12",
                    "ident": "in_string"
                },
                "formals": [

                ],
                "ret_type": {
                    "line": "12",
                    "ident": "String"
                },
                "body": {
                    "line": "12",
                    "kind": "string",
                    "const": "io",
                    "fields": [

                    ]
                }
            },
            {
                "kind": "method",
                "name": {
                    "line": "11",
                    "ident": "out_int"
                },
                "formals": [{
                    "name": {
                        "line": "11",
                        "ident": "x"
                    },
                    "f_type": {
                        "line": "11",
                        "ident": "Int"
                    }
                }],
                "ret_type": {
                    "line": "11",
                    "ident": "SELF_TYPE"
                },
                "body": {
                    "line": "11",
                    "kind": "identifier",
                    "identifier": {
                        "line": "11",
                        "ident": "self"
                    },
                    "fields": [
                        "identifier"
                    ]
                }
            },
            {
                "kind": "method",
                "name": {
                    "line": "10",
                    "ident": "out_string"
                },
                "formals": [{
                    "name": {
                        "line": "10",
                        "ident": "x"
                    },
                    "f_type": {
                        "line": "10",
                        "ident": "String"
                    }
                }],
                "ret_type": {
                    "line": "10",
                    "ident": "SELF_TYPE"
                },
                "body": {
                    "line": "10",
                    "kind": "identifier",
                    "identifier": {
                        "line": "10",
                        "ident": "self"
                    },
                    "fields": [
                        "identifier"
                    ]
                }
            }
        ]
    },
    "Int": {
        "name": {
            "line": "16",
            "ident": "Int"
        },
        "parent": OBJ,
        "features": [

        ]
    },
    "Bool": {
        "name": {
            "line": "20",
            "ident": "Bool"
        },
        "parent": OBJ,
        "features": [

        ]
    },
    "String": {
        "name": {
            "line": "24",
            "ident": "String"
        },
        "parent": OBJ,
        "features": [{
                "kind": "method",
                "name": {
                    "line": "29",
                    "ident": "concat"
                },
                "formals": [{
                    "name": {
                        "line": "29",
                        "ident": "s"
                    },
                    "f_type": {
                        "line": "29",
                        "ident": "String"
                    }
                }],
                "ret_type": {
                    "line": "29",
                    "ident": "String"
                },
                "body": {
                    "line": "30",
                    "kind": "string",
                    "const": "-",
                    "fields": [

                    ]
                }
            },
            {
                "kind": "method",
                "name": {
                    "line": "25",
                    "ident": "length"
                },
                "formals": [

                ],
                "ret_type": {
                    "line": "25",
                    "ident": "Int"
                },
                "body": {
                    "line": "26",
                    "kind": "integer",
                    "const": "0",
                    "fields": [

                    ]
                }
            },
            {
                "kind": "method",
                "name": {
                    "line": "33",
                    "ident": "substr"
                },
                "formals": [{
                        "name": {
                            "line": "33",
                            "ident": "i"
                        },
                        "f_type": {
                            "line": "33",
                            "ident": "Int"
                        }
                    },
                    {
                        "name": {
                            "line": "33",
                            "ident": "l"
                        },
                        "f_type": {
                            "line": "33",
                            "ident": "Int"
                        }
                    }
                ],
                "ret_type": {
                    "line": "33",
                    "ident": "String"
                },
                "body": {
                    "line": "34",
                    "kind": "string",
                    "const": "string",
                    "fields": [

                    ]
                }
            }
        ]
    }
}

Object.keys(name2map).forEach(key => {
    // define toStringClass method for the dummy objects
    name2map[key].toStringClass = () => `${ key }\n0\n`;
    name2map[key].features.
    forEach(method => {
        method.body.toStringImp = function() {
            return `0\n${ method.ret_type.ident }\ninternal\n${ key }.${ method.name.ident }\n`
        };
    });
});


// Cross-Platform Compatibility
let lines = data.split("\n");
lines = lines.map(line => {
    if (line[line.length - 1] === '\n') {
        line = line.substring(0, line.length - 1);
    }
    if (line[line.length - 1] === '\r') {
        line = line.substring(0, line.length - 1);
    }
    return line;
});

var js, script, filename, options;
[js, script, filename, ...options] = process.argv;

if (options.includes('debug')) {
    DEBUG = true;
};

const ast = read_program(lines);
type_check_program(ast);
const class_map = generate_class_map(ast);
const parent_map = generate_parent_map(ast);
const implementation_map = generate_implementation_map(ast);
const annotated_ast = generate_annotated_ast(ast);

const file_basename = filename.substring(0, filename.length - 3);

// fs.writeFileSync(`${file_basename}class-map`, class_map);
// fs.writeFileSync(`${file_basename}parent-map`, parent_map);
// fs.writeFileSync(`${file_basename}imp-map`, implementation_map);
fs.writeFileSync(`${file_basename}type`, class_map + implementation_map + parent_map + annotated_ast);