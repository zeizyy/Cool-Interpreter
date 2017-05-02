include Printf;

/* ---------------------------
   Type definitions
                   ----------------------------- */
type cool_address = int;

type cool_value =
  | Cool_Int int32
  | Cool_Bool bool
  | Cool_String string
  | Cool_Object string (list (string, cool_address))
  | Void;

type exp =
  | New int string
  | Dispatch int exp string (list exp)
  | StaticDispatch int exp string string (list exp)
  | If int exp exp exp
  | Sequence int (list exp)
  | Let int string string exp exp
  | Case int exp (list (string, string, exp))
  | While int exp exp
  | IsVoid int exp
  | Not int exp
  | Neg int exp
  /* Arith */
  | Plus int exp exp
  | Times int exp exp
  | Minus int exp exp
  | Divide int exp exp
  /* Comparisons */
  | LessEq int exp exp
  | Less int exp exp
  | Equal int exp exp
  | Variable int string
  | Assign int string exp
  | Integer int int32
  | StringLiteral int string
  | Self int
  | Internal int string
  | True int
  | False int
  | Default cool_value
  /* self-defined type to simulate the None type in other programming language */
  | None;

/* Environment type */
type environment = list (string, cool_address);

/* Store type */
type store = list (cool_address, cool_value);

type class_map = list (string, list (string, string, exp));

type imp_map = list ((string, string), (list string, exp));

/* ---------------------------
   Utility Functions
                   ----------------------------- */
/* generates the default_value given a string of type */
let default_value a_type =>
  switch a_type {
  | "Int" => Cool_Int 0l
  | "Bool" => Cool_Bool false
  | "String" => Cool_String ""
  | _ => Void
  };

/* zip two lists together to form a list of tuple */
let rec zip l1 l2 =>
  switch (l1, l2) {
  | ([hd1, ...tl1], [hd2, ...tl2]) => [(hd1, hd2), ...zip tl1 tl2]
  | _ => []
  };

let join_string delimiter str_list =>
  List.fold_left (fun acc elt => acc ^ delimiter ^ elt) "" str_list;

/* split a string into its first char and the rest of the string */
let first_char (s: string) =>
  switch s {
  | "" => ("", "")
  | _ => (Bytes.sub s 0 1, Bytes.sub s 1 (Bytes.length s - 1))
  };

/* exists in Array */
let arr_exists pred arr => Array.fold_left (fun acc i => acc || pred i) false arr;

/* ----------------------------
   Debug Facilities
                 -------------------------- */
let do_debug = ref false;
let do_break = ref false;
let stack_limit = ref false;
let debug fmt => {
  let handle result_string =>
    if !do_debug {
      printf "%s" result_string
    };
  kprintf handle fmt
};

let debug_break fmt => {
  let handle result_string =>
    if !do_break {
      printf "%s%!" result_string;
      let _ = input_line stdin;
    };
  kprintf handle fmt
};


let indent_count = ref 0;

let debug_indent () :unit => debug "%s" (String.make !indent_count ' ');

let stack_height = ref 0;
let stack = ref (Stack.create ());

let strace = ref false;


let pop_stack () :unit =>
  if (not (Stack.is_empty !stack)) {
    let _ = Stack.pop !stack;
    stack_height := !stack_height - 1;
    ()
  };

let rec print_stack () :unit => {
  printf "\n\n\nStack Frame\n====================\n";
  while (not (Stack.is_empty !stack)) {
    printf "%s\n-----------------------------------------\n" (Stack.pop !stack)
  }
};

/* Error Reporting during execution to provide formated output */
let eprint fmt => {
  if !strace {
    print_stack ()
  };
  let handle result_string => {
    printf "%s%!" result_string;
    exit 1;
  };
  kprintf handle fmt
};

let push_stack str :unit =>
{
  if (!stack_limit && !stack_height > 20) {
    print_stack ();
    printf "Stack height exceeds 20, continue?%!";
    let _ = input_line stdin;
  };
  stack_height := !stack_height + 1;
  Stack.push str !stack;
};
/* ----------------------------
   ToString Methods
                 -------------------------- */
let value_to_str (v: cool_value) :string =>
  switch v {
  | Cool_Int i => sprintf "Int(%ld)" i
  | Cool_Bool b => sprintf "Bool(%b)" b
  | Cool_String s => sprintf "String(\"%s\")" s
  | Void => sprintf "Void"
  | Cool_Object cname attrs =>
    let attr_str =
      join_string ", " (List.map (fun (aname, aaddr) => sprintf "%s=%d" aname aaddr) attrs);
    sprintf "%s([%s])" cname attr_str
  };

let rec exp_to_str (e: exp) :string =>
  switch e {
  | New lineno s => sprintf "New(%s)" s
  | Dispatch lineno ro fname args =>
    let arg_str = List.fold_left (fun acc elt => acc ^ ", " ^ exp_to_str elt) "" args;
    sprintf "Dispatch(%s, %s, [%s])" (exp_to_str ro) fname arg_str
  | StaticDispatch lineno caller_exp static_type fname args =>
    let arg_str = List.fold_left (fun acc elt => acc ^ ", " ^ exp_to_str elt) "" args;
    sprintf "Static Dispatch(%s, %s, %s, [%s])" (exp_to_str caller_exp) static_type fname arg_str
  | If lineno e1 e2 e3 =>
    sprintf "If (pred=%s, then=%s, else=%s)" (exp_to_str e1) (exp_to_str e2) (exp_to_str e3)
  | Sequence lineno exp_list =>
    sprintf
      "Sequence (number=%d, expr_list=[%s]"
      (List.length exp_list)
      (List.fold_left (fun acc s => acc ^ "," ^ s) "" (List.map exp_to_str exp_list))
  | Let lineno id t1 e1 e2 =>
    sprintf "Let (id=%s, type=%s, init=%s, in=%s)" id t1 (exp_to_str e1) (exp_to_str e2)
  | Case lineno e0 branch_list =>
    sprintf
      "Case(%s, [%s])"
      (exp_to_str e0)
      (
        List.fold_left
          (fun acc s => acc ^ "," ^ s)
          ""
          (List.map (fun (id, t, e) => sprintf "(%s : %s => %s)" id t (exp_to_str e)) branch_list)
      )
  | While lineno e1 e2 => sprintf "While(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | IsVoid lineno e1 => sprintf "IsVoid()"
  | Not lineno e => sprintf "Not(%s)" (exp_to_str e)
  | Neg lineno e => sprintf "Neg(%s)" (exp_to_str e)
  /* Arith */
  | Plus lineno e1 e2 => sprintf "Plus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Times lineno e1 e2 => sprintf "Times(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Minus lineno e1 e2 => sprintf "Minus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Divide lineno e1 e2 => sprintf "Divide(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  /* Comparisons */
  | LessEq lineno e1 e2 => sprintf "LessEq(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Less lineno e1 e2 => sprintf "Less(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Equal lineno e1 e2 => sprintf "Equal(%s, %s)" (exp_to_str e1) (exp_to_str e2)
  | Variable lineno v => sprintf "Variable(%s)" v
  | Assign lineno id body => sprintf "Assign(%s, %s)" id (exp_to_str body)
  | Integer lineno i => sprintf "Integer(%ld)" i
  | StringLiteral lineno s => sprintf "String(%s)" s
  | Self lineno => sprintf "Self"
  | Internal lineno mname => sprintf "Internal(%s)" mname
  | True lineno => sprintf "Bool(%s)" "True"
  | False lineno => sprintf "Bool(%s)" "False"
  | Default v => sprintf "%s" (value_to_str v)
  | None => sprintf "None"
  };

let store_to_str (s: store) :string => {
  let binding_str =
    join_string
      ", " (List.map (fun (addr, cvalue) => sprintf "%d=%s" addr (value_to_str cvalue)) s);
  sprintf "[%s]" binding_str
};

let env_to_str (e: environment) :string => {
  let binding_str =
    join_string ", " (List.map (fun (aname, aaddr) => sprintf "%s=%d" aname aaddr) e);
  sprintf "[%s]" binding_str
};

/*  +----------------------
    |                           Read                              |
    +                                    -------------------------+  */
/* turn the input into an integer */
let input_int (file: in_channel) :int => {
  let line = input_line file;
  try (int_of_string line) {
  | Failure _ => eprint "int_of_string failed at %s" line
  }
};

let read_valid_int (trimed_str: string): int32 => {
  let r = Str.regexp "^[+|-]?[0-9]+";
  let found = Str.string_match r trimed_str 0;
  switch found {
    | true => {
      let raw_str = Str.matched_string trimed_str;
      let no_plus_str = switch (first_char raw_str) {
        | ("+", rest) => rest
        | _ => raw_str
      };
      /* print_string no_plus_str; */
      try (Int32.of_string (no_plus_str)) {
        | Failure "int_of_string" => Int32.zero
      }
    }
    | false => Int32.zero
  }
};

/* turn the input into an integer 32 */
let input_int32 (file: in_channel) :int32 => {
  let input_str = try (input_line file) {
    | End_of_file => "0"
  };
  read_valid_int (String.trim input_str)
};

let validate_string (input_str: string): string => {
  let pos = try (String.index input_str (Char.chr 0)) {
    | Not_found => -1
  };

  switch pos {
    | -1 => input_str
    | _ => ""
  }
};

let input_string (file: in_channel) :string => {
  let input_str = try (input_line file) {
    | End_of_file => ""
  };
  validate_string input_str
};

let rec read_list (num: int) (file: in_channel) read_func => {
  let l = ref [];
  for i in 1 to num {
    l := !l @ [read_func file]
  };
  !l
};

let read_identifier (file: in_channel) :string => {
  debug_indent ();
  let _ = int_of_string (input_line file);
  let ident = input_line file;
  debug "Read ident [%s]" ident;
  ident
};

let rec read_expr (file: in_channel) :exp => {
  debug_indent ();
  debug "Read Expr:\n";
  indent_count := !indent_count + 2;
  let lineno = input_int file; /* line no */
  let e_type = input_line file; /* type annotation */
  let e_kind = input_line file;
  debug_indent ();
  debug "line number = %d" lineno;
  debug_indent ();
  debug "type = %s" e_type;
  debug_indent ();
  debug "kind = %s" e_kind;
  let exp =
    switch e_kind {
    | "assign" =>
      let id = read_identifier file;
      let body = read_expr file;
      Assign lineno id body
    | "dynamic_dispatch" =>
      let caller = read_expr file;
      let fname = read_identifier file;
      let num_args = input_int file;
      let args = read_list num_args file read_expr;
      Dispatch lineno caller fname args
    | "static_dispatch" =>
      let caller = read_expr file;
      let dtype = read_identifier file;
      let fname = read_identifier file;
      let num_args = input_int file;
      let args = read_list num_args file read_expr;
      StaticDispatch lineno caller dtype fname args
    | "self_dispatch" =>
      let fname = read_identifier file;
      let num_args = input_int file;
      let args = read_list num_args file read_expr;
      Dispatch lineno (Self lineno) fname args
    | "if" =>
      let pre = read_expr file;
      let the = read_expr file;
      let els = read_expr file;
      If lineno pre the els
    | "while" =>
      let predicate = read_expr file;
      let body = read_expr file;
      While lineno predicate body
    | "block" =>
      let num_expr = int_of_string (input_line file);
      let body = read_list num_expr file read_expr;
      Sequence lineno body
    | "new" =>
      let cls = read_identifier file;
      New lineno  cls
    | "isvoid" =>
      let e = read_expr file;
      IsVoid lineno e
    | "plus"
    | "minus"
    | "times"
    | "divide"
    | "lt"
    | "le"
    | "eq" =>
      let e1 = read_expr file;
      let e2 = read_expr file;
      switch e_kind {
      | "plus" => Plus lineno e1 e2
      | "minus" => Minus lineno e1 e2
      | "times" => Times lineno e1 e2
      | "divide" => Divide lineno e1 e2
      | "lt" => Less lineno e1 e2
      | "le" => LessEq lineno e1 e2
      | "eq" => Equal lineno e1 e2
      | _ => failwith "impossible to reach"
      }
    | "not" =>
      let x = read_expr file;
      Not lineno x
    | "negate" =>
      let x = read_expr file;
      Neg lineno x
    | "integer" =>
      let i = Int32.of_string (input_line file);
      Integer lineno i
    | "string" =>
      let s = input_line file;
      StringLiteral lineno s
    | "identifier" =>
      let v = read_identifier file;
      switch v {
      | "self" => Self lineno
      | _ => Variable lineno v
      }
    | "internal" =>
      let method_name = input_line file;
      Internal lineno method_name
    | "true" => True lineno
    | "false" => False lineno
    | "let" =>
      let each_let_num: int = input_int file;
      let rec read_let (each_let_num: int) =>
        switch each_let_num {
        | 0 => read_expr file
        | _ =>
          let let_type: string = input_line file;
          let let_exp: exp =
            switch let_type {
            | "let_binding_no_init" =>
              let ident = read_identifier file;
              let t = read_identifier file;
              Let lineno ident t (Default (default_value t)) (read_let (each_let_num - 1))
            | "let_binding_init" =>
              let ident = read_identifier file;
              let t = read_identifier file;
              let exp = read_expr file;
              Let lineno ident t exp (read_let (each_let_num - 1))
            | _ => failwith "each let has another type"
            };
          let_exp
        };
      read_let each_let_num
    | "case" =>
      let case_exp = read_expr file;
      let each_case_num = int_of_string (input_line file);
      let read_case f => {
        let v = read_identifier f;
        let t = read_identifier f;
        let case_body = read_expr f;
        (v, t, case_body)
      };
      let case_list = read_list each_case_num file read_case;
      Case lineno case_exp case_list
    | _ => failwith (sprintf "Read Expr Error: %s" e_kind)
    };
  debug_indent ();
  debug "exp = [%s]\n" (exp_to_str exp);
  indent_count := !indent_count - 2;
  exp
};

let read_class_map (file: in_channel) :class_map => {
  debug_indent ();
  debug "Read class map: \n";
  indent_count := !indent_count + 2;
  let _ = input_line file;
  let num_of_class = input_int file;
  let classes = ref [];
  for i in 1 to num_of_class {
    debug_indent ();
    debug "Read class: \n";
    indent_count := !indent_count + 2;
    let name = input_line file;
    debug_indent ();
    debug "name = %s\n" name;
    let num_attr = int_of_string (input_line file);
    debug_indent ();
    debug "num of attr = %d\n" num_attr;
    let attrs = ref [];
    for j in 1 to num_attr {
      debug_indent ();
      debug "Read attribute: \n";
      indent_count := !indent_count + 2;
      let has_init = input_line file == "initializer";
      let attr_name = input_line file;
      let attr_type = input_line file;
      debug_indent ();
      debug "name = %s\n" attr_name;
      debug_indent ();
      debug "type = %s\n" attr_type;
      debug_indent ();
      debug "init = %b\n" has_init;
      let attr_init = has_init ? read_expr file : Default (default_value attr_type);
      indent_count := !indent_count - 2;
      attrs := !attrs @ [(attr_name, attr_type, attr_init)]
    };
    classes := !classes @ [(name, !attrs)];
    indent_count := !indent_count - 2
  };
  indent_count := !indent_count - 2;
  !classes
};

let read_imp_map file :imp_map => {
  debug_indent ();
  debug "Read implementation map: \n";
  indent_count := !indent_count + 2;
  let _ = input_line file; /* iml_map title */
  let num_of_class = input_int file;
  let impm = ref [];
  for i in 1 to num_of_class {
    debug_indent ();
    debug "Read class: \n";
    indent_count := !indent_count + 2;
    let cname = input_line file;
    debug_indent ();
    debug "name = %s\n" cname;
    let num_method = int_of_string (input_line file);
    for j in 1 to num_method {
      let mname = input_line file;
      debug_indent ();
      debug "method: %s\n" mname;
      indent_count := !indent_count + 2;
      let num_formal = int_of_string (input_line file);
      let formals = read_list num_formal file input_line;
      let _ = input_line file; /* parent_class */
      let m_exp = read_expr file;
      impm := !impm @ [((cname, mname), (formals, m_exp))];
      indent_count := !indent_count - 2
    };
    indent_count := !indent_count - 2
  };
  indent_count := !indent_count - 2;
  !impm
};


type parent_map = list (string, string);

let read_parent_map file : parent_map => {
  let _ = input_line file;
  let num = input_int file;
  let p = ref [];
  for i in 1 to num {
    let child = input_line file;
    let parent = input_line file;
    p := !p @ [(child, parent)]
  };
  !p
};

/*  +---------------------------
    Eval Expression
                  ------------------------- */
/* The class map and implementation map instance variable
   Used ref type, so that it can be used as a global variable */
let g_class_map = ref ([]: class_map);

let g_imp_map = ref ([]: imp_map);

let g_parent_map = ref ([] : parent_map);

let largestloc = ref 0;

let stack_counter = ref 0;


/* Generates a new location by globally increment a counter to ensure uniqueness */
let newloc store :cool_address => {
  largestloc := !largestloc + 1;
  !largestloc
};

let addkey key item dict => [(key, item), ...dict];


/* Returns the type's name and a list of attributes */
let type_of_cool value =>
  switch value {
  | Cool_Int _ => ("Int", [])
  | Cool_String _ => ("String", [])
  | Cool_Bool _ => ("Bool", [])
  | Cool_Object cname attr_list => (cname, attr_list)
  | Void => ("Void", [])
  };

let find key (dict: list 'a) => List.assoc key dict;

let get_val_from_env (e: environment) (s: store) (name: string) :cool_value => {
  let loc =
    try (find name e) {
    | Not_found => eprint "Not_found Error: name = %s is not in env = %s" name (env_to_str e)
    | _ => eprint "Unkonwon error"
    };
  let v =
    try (find loc s) {
    | Not_found => eprint "Not_found Error: loc = %d is not in store = %s" loc (store_to_str s)
    | _ => eprint "Unknown error"
    };
  v
};

let rec copy_attr attr_list => {
  switch attr_list {
  | [] => []
  | [hd, ...tl] => [hd] @ copy_attr tl
  }
};


let visualize_stack_frame so s e exp =>
  sprintf
    "\n  o Expression = %s\n    + self = %s\n    + environment \n\t%s\n    + store \n\t%s\n  "
    (exp_to_str exp)
    (value_to_str so)
    (env_to_str e)
    (store_to_str s);

/* general eval function */
let rec eval (so: cool_value) (s: store) (e: environment) (exp: exp) :(cool_value, store) => {
  /* debug "\n";
  debug_indent ();
  debug "+ eval: %s\n" (exp_to_str exp);
  indent_count := !indent_count + 2;
  debug_indent ();
  debug "- self = %s\n" (value_to_str so);
  debug_indent ();
  debug "- sto = %s\n" (store_to_str s);
  debug_indent ();
  debug "- env = %s\n" (env_to_str e);
  debug_indent (); */
  /* if !strace {
    push_stack (visualize_stack_frame so s e exp)
  };
  debug_break "press enter to step";
  debug "\n"; */

  let increment_stack_counter lineno => {
    if (!stack_counter >= 1000) {
      eprint "ERROR: %d: Exception: stack overflow" lineno;
    };
    stack_counter := !stack_counter + 1;
  };

  let decrement_stack_counter () => {
    stack_counter := !stack_counter - 1;
  };

  /*let (new_val, new_store) =*/
    switch exp {
    | New lineno t =>
      switch t {
        | "Int" => (Cool_Int (Int32.zero), s)
        | "String" => (Cool_String "", s)
        | "Bool" => (Cool_Bool false, s)
        | _ => {
          increment_stack_counter lineno;
          let (x, _) = type_of_cool so;
          let t0: string = t == "SELF_TYPE" ? x : t;
          let attr_list = find t0 !g_class_map;
          let a_names = List.map (fun (a, _, _) => a) attr_list;
          let a_types = List.map (fun (_, t, _) => t) attr_list;
          /*let a_inits = List.map (fun (_, _, e) => e) attr_list;*/
          let locs: list cool_address = List.map (fun _ => newloc s) attr_list;
          let e1: environment = zip a_names locs;
          let v1: cool_value = Cool_Object t0 e1;
          let def_val: list cool_value = List.map (fun t => default_value t) a_types;
          let s2: store = zip locs def_val @ s;
          let assign_list = List.map (fun attr => {
            let (name, types, init) = attr;
            Assign lineno name init
          }) attr_list;
          let (v2, s3) = eval_sequence v1 s2 e1 assign_list;
          decrement_stack_counter ();
          (v1, s3)
        }
      }
    | Dispatch lineno caller m_name args_list =>
      increment_stack_counter lineno;
      let (v_list, sn1) = eval_sequence so s e args_list;
      switch (eval so sn1 e caller) {
      | (Void, _) =>  eprint "ERROR: %d: Exception: %s\n" lineno "dispatch on void"
      | (caller_val, sn2) =>
        let (c_name, attr_list) = type_of_cool caller_val;
        let (param_list, e_body) = find (c_name, m_name) !g_imp_map;
        let locations = List.map newloc v_list;
        let sn3 = (zip locations v_list) @ sn2;
        let e1 = zip param_list locations @ attr_list;
        let (v, s4) = eval caller_val sn3 e1 e_body;
        decrement_stack_counter ();
        (v, s4)
      }
    | StaticDispatch lineno e0 t fname args_list =>
      increment_stack_counter lineno;
      let (v_list, sn1) = eval_sequence so s e args_list;
      switch (eval so sn1 e e0) {
      | (Void, _) =>  eprint "ERROR: %d: Exception: %s" lineno "dispatch on Void"
      | (v0, sn2) =>
        let (c_name, attr_list) = type_of_cool v0;
        let (param_list, e_body) = find (t, fname) !g_imp_map;
        let locations = List.map newloc v_list;
        let sn3 = zip locations v_list;
        let sn3 = sn3 @ sn2;
        let e1 = zip param_list locations @ attr_list;
        let (v, s4) = eval v0 sn3 e1 e_body;
        decrement_stack_counter ();
        (v, s4)
      }
    | Assign lineno id e1 =>
      let (v1, s2) = eval so s e e1;
      let l1 = find id e;
      let s3 = addkey l1 v1 s2;
      (v1, s3)
    | Self lineno => (so, s)
    | True lineno => (Cool_Bool true, s)
    | False lineno => (Cool_Bool false, s)
    | Variable lineno id =>
      let v = get_val_from_env e s id;
      (v, s)
    | StringLiteral lineno str => (Cool_String str, s)
    | Integer lineno i => (Cool_Int i, s)
    | Internal lineno mname => syscall mname so s e
    | If lineno pre the els =>
      let (b, s2) = eval so s e pre;
      let (v2, s3) =
        switch b {
        | Cool_Bool true => eval so s e the
        | Cool_Bool false => eval so s e els
        | _ => eprint "TypeError: %s\n" "If predicate evals to non-Bool vals"
        };
      (v2, s3)
    | Sequence lineno body => List.fold_left (fun (v_i, s_i) expr => eval so s_i e expr) (Void, s) body
    | Let lineno ident t1 e1 e2 =>
      /* Let string string exp exp */
      let (v1, s2) = eval so s e e1;
      let l1 = newloc s2;
      let e_p = addkey ident l1 e;
      let s3 = addkey l1 v1 s2;
      eval so s3 e_p e2
    | Case lineno e0 case_list =>
      /* exp (list (string, string, exp)) */
      let (v0, s2) = eval so s e e0;
      /* TODO: what kind of equal sign? */
      if (v0 == Void) {
        eprint "ERROR: %d: Exception: case on void\n" lineno
      };
      let (typ, _) = type_of_cool v0;

      let rec find_branch cname =>
        try(List.find (fun (_, t, _) => t == cname) case_list) {
        | Not_found =>
          find_branch (try (find cname !g_parent_map) {
          | Not_found => eprint "ERROR: %d: Exception: case without matching branch: %s(...)" lineno typ
          })
        };
      let (idi, ti, ei) = find_branch typ;

      let l0 = newloc s2;
      let s3 = addkey l0 v0 s2;
      let e_p = addkey idi l0 e;
      eval so s3 e_p ei
    | While lineno e1 e2 =>
      let (b, s2) = eval so s e e1;
      switch b {
      | Cool_Bool false => (Void, s2)
      | Cool_Bool true =>
        let (v2, s3) = eval so s2 e e2;
        eval so s3 e (While lineno e1 e2)
      | _ => failwith "predicate is something other than bool value"
      }
    /* | While predicate body */
    | IsVoid lineno e1 =>
      let (cool_val, s2) = eval so s e e1;
      switch cool_val {
      | Void => (Cool_Bool true, s2)
      | Cool_Object _ _ => (Cool_Bool false, s2)
      | _ => failwith "can only call isvoid on void and cool object"
      }
    | Not lineno e1 =>
      let (b, s2) = eval so s e e1;
      switch b {
      | Cool_Bool tf => (Cool_Bool (not tf), s2)
      | _ => failwith "can only call not on bool"
      }
    | Neg lineno e1 =>
      let (i, s2) = eval so s e e1;
      switch i {
      | Cool_Int i_val => (Cool_Int (Int32.neg i_val), s2)
      | _ => failwith "can only call not on int"
      }
    | Plus lineno e1 e2
    | Minus lineno  e1 e2
    | Times lineno  e1 e2
    | Divide lineno  e1 e2 =>
      let (v1, s1) = eval so s e e1;
      let i1 =
        switch v1 {
        | Cool_Int i1 => i1
        | _ => failwith "impossible to reach"
        };
      let (v2, s2) = eval so s1 e e2;
      let i2 =
        switch v2 {
        | Cool_Int i2 => i2
        | _ => failwith "impossible to reach"
        };
      let res =
        switch exp {
        | Plus _ _ _ => Int32.add i1 i2
        | Minus _ _ _ => Int32.sub i1 i2
        | Times _ _ _ => Int32.mul i1 i2
        | Divide _ _ _ =>
          if (i2 == 0l) {
            eprint "ERROR: %d: Exception: division by zero" lineno
          };
          Int32.div i1 i2
        | _ => failwith "impossible to reach"
        };
      (Cool_Int res, s2)
    | Equal lineno e1 e2
    | Less lineno e1 e2
    | LessEq lineno e1 e2 =>
      let (v1, s1) = eval so s e e1;
      let (v2, s2) = eval so s1 e e2;
      let comparator v1 v2 =>
        switch (v1, v2) {
        | (Cool_Int i1, Cool_Int i2) => Int32.compare i1 i2
        | (Cool_String s1, Cool_String s2) => String.compare s1 s2
        | (Cool_Bool b1, Cool_Bool b2) =>
          if (b1 == b2) {
            0
          } else if (b1 < b2) {
            (-1)
          } else {
            1
          }
        | (o1, o2) => o1 === o2 ? 0 : 2
        };
      let comp_val = comparator v1 v2;
      let true_ret = (Cool_Bool true, s2);
      let false_ret = (Cool_Bool false, s2);
      switch exp {
      | Equal _ _ _  =>
        switch comp_val {
        | 0 => true_ret
        | _ => false_ret
        }
      | Less _ _ _  =>
        switch comp_val {
        | (-1) => true_ret
        | _ => false_ret
        }
      | LessEq _ _  _ =>
        switch comp_val {
        | (-1)
        | 0 => true_ret
        | _ => false_ret
        }
      | _ => failwith "impossible to reach"
      }
    | Default v => (v, s)
    | _ =>
      debug_indent ();
      debug "Expr not implemented: %s" (exp_to_str exp);
      (Void, s)
    }
  /*debug "\n";
  debug_indent ();
  debug "=> val = %s\n" (value_to_str new_val);
  debug_indent ();
  debug "=> store = %s\n" (store_to_str new_store);
  indent_count := !indent_count - 2;
  pop_stack ();
  (new_val, new_store)*/
}
and eval_sequence so s e exp_list => {
  let eval_one (v_list, sto) exp => {
    let (v, snew) = eval so sto e exp;
    (v_list @ [v], snew)
  };
  List.fold_left eval_one ([], s) exp_list
}
and syscall mname so s e => {
  debug_indent ();
  debug "syscall %s\n" mname;
  let out_string (x: cool_value) => {
    switch x {
    | Cool_String x => {
      let rt = Str.regexp "\\\\t";
      let rn = Str.regexp "\\\\n";
      let x = Str.global_replace rt "\t" x;
      let x = Str.global_replace rn "\n" x;
      printf "%s" x
    }
    | _ => eprint "TypeError: string expected, got %s instead" (value_to_str x)
    };
    (so, s)
  };
  let out_int (x: cool_value) => {
    switch x {
    | Cool_Int x => printf "%ld" x
    | _ => eprint "TypeError: int expected, got %s instead" (value_to_str x)
    };
    (so, s)
  };
  let in_string () => {
    let v = input_string stdin;
    let v = Cool_String v;
    (v, s)
  };
  let in_int () => {
    let v = input_int32 stdin;
    let v = Cool_Int v;
    (v, s)
  };
  let string_length () =>
    switch so {
    | Cool_String str => (Cool_Int (Int32.of_int (String.length str)), s)
    | _ => eprint "Error"
    };
  let string_concat (s2: cool_value) =>
    switch (so, s2) {
    | (Cool_String s1, Cool_String s2) => (Cool_String (s1 ^ s2), s)
    | _ => eprint "Error"
    };
  let string_substr (start: cool_value) (length: cool_value) =>
    switch (so, start, length) {
    | (Cool_String str, Cool_Int start, Cool_Int sub_len) =>
      let start = Int32.to_int start;
      let sub_len = Int32.to_int sub_len;
      let sub_str = try (String.sub str start sub_len) {
      | Invalid_argument _ => eprint "ERROR: 0: Exception: String.substr out of range"
      };
      (Cool_String sub_str, s)
    | _ => eprint "String.substr Receives wrong types of argument"
    };
  let object_abort () => eprint "abort\n";
  let object_type_name () => {
    let (cname, _) = type_of_cool so;
    (Cool_String cname, s)
  };
  let copy_shallow v => {
    switch (v) {
      | Cool_Int i => Cool_Int i
      | Cool_Bool b => Cool_Bool b
      | Cool_String s => Cool_String s
      | Void => Void
      | Cool_Object _ _ => v
    }
  };
  let copy () => {
    switch so {
    | Cool_Object cname attrs =>
        let t0 = cname;
        let attr_list = find t0 !g_class_map;
        let a_names = List.map (fun (a, _, _) => a) attr_list;
        let locs: list cool_address = List.map (fun _ => newloc s) attr_list;
        let e1: environment = zip a_names locs;
        let v1: cool_value = Cool_Object t0 e1;
        let def_val: list cool_value = List.map (fun (_, loc) => find loc s) attrs;
        let s2: store = zip locs def_val @ s;
        (v1, s2)
    | _ => (copy_shallow so, s)
    }
  };
  let get_val = get_val_from_env e s;
  switch mname {
  | "IO.out_string" => out_string (get_val "x")
  | "IO.out_int" => out_int (get_val "x")
  | "IO.in_string" => in_string ()
  | "IO.in_int" => in_int ()
  | "String.length" => string_length ()
  | "String.concat" => string_concat (get_val "s")
  | "String.substr" => string_substr (get_val "i") (get_val "l")
  | "Object.abort" => object_abort ()
  | "Object.copy" => copy ()
  | "Object.type_name" => object_type_name ()
  | _ => eprint "syscall \"%s\" not implemented" mname
  }
};

let equal_to key item => item == key;

let main () => {
  do_debug := arr_exists (equal_to "-d") Sys.argv;
  strace := arr_exists (equal_to "-s") Sys.argv;
  do_break := arr_exists (equal_to "-b") Sys.argv;
  stack_limit := arr_exists (equal_to "--limit") Sys.argv;
  let file_name = Sys.argv.(1);
  let file = open_in file_name;
  debug "File: %s\n" file_name;
  debug "\n";
  debug "Reading File\n";
  debug "============\n";
  g_class_map := read_class_map file;
  g_imp_map := read_imp_map file;
  g_parent_map := read_parent_map file;

  debug_break "enter something to continue...";
  debug "%s\n" "";
  debug "%s\n" "";
  debug "%s\n" "";
  debug "%s\n" "Start evaluating!";
  debug "%s\n" "=================";
  indent_count := 0;
  let main_exp = Dispatch 0 (New 0 "Main") "main" [];
  eval Void [] [] main_exp
};

main ();
