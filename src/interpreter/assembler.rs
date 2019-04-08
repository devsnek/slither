use crate::interpreter::{Op, REGISTER_COUNT};
use crate::parser::{FunctionKind, Node, Operator, Scope, ScopeKind};
use byteorder::{LittleEndian, WriteBytesExt};

struct Register {
    id: u32,
}

impl Register {
    fn new(id: u32) -> Register {
        Register { id }
    }
}

struct RegisterScope {
    index: u32,
    assembler: *mut Assembler,
}

impl RegisterScope {
    fn new(assembler: &mut Assembler) -> RegisterScope {
        RegisterScope {
            index: assembler.register_index,
            assembler,
        }
    }

    fn register(&self) -> Register {
        let a = unsafe { &mut *self.assembler };
        if a.register_index == a.register_max {
            panic!("unable to allocate register");
        }
        let r = Register::new(a.register_index);
        a.register_index += 1;
        r
    }
}

impl Drop for RegisterScope {
    fn drop(&mut self) {
        unsafe {
            (*self.assembler).register_index = self.index;
        }
    }
}

struct Label {
    index: Option<usize>,
    targets: Vec<usize>,
}

impl Drop for Label {
    fn drop(&mut self) {
        if !self.targets.is_empty() && self.index == None {
            panic!("jump was not marked");
        }
    }
}

pub struct AssemblerFunctionInfo {
    pub kind: FunctionKind,
    pub name: Option<String>,
    pub parameters: Vec<String>,
    pub position: usize,
}

pub struct Assembler {
    pub code: Vec<u8>,
    pub string_table: Vec<String>,
    pub function_info: Vec<AssemblerFunctionInfo>,
    register_index: u32,
    register_max: u32,
    break_label: Option<*mut Label>,
    continue_label: Option<*mut Label>,
    throw_label: Option<*mut Label>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            code: Vec::new(),
            string_table: Vec::new(),
            function_info: Vec::new(),
            register_index: 0,
            register_max: REGISTER_COUNT as u32,
            break_label: None,
            continue_label: None,
            throw_label: None,
        }
    }

    pub fn assemble(&mut self, ast: &Node) -> usize {
        let start = self.code.len();
        self.visit(ast);
        self.push_op(Op::End);
        start
    }

    fn visit(&mut self, node: &Node) {
        match node {
            Node::NullLiteral => self.visit_null(),
            Node::TrueLiteral => self.visit_true(),
            Node::FalseLiteral => self.visit_false(),
            Node::NumberLiteral(n) => self.visit_number(*n),
            Node::StringLiteral(s) => self.visit_string(s),
            Node::SymbolLiteral(s) => self.visit_symbol(s),
            Node::RegexLiteral(r) => self.visit_regex(r),
            Node::ObjectLiteral(inits) => self.visit_object(inits),
            Node::ArrayLiteral(exprs) => self.visit_array(exprs),
            Node::TupleLiteral(exprs) => self.visit_tuple(exprs),
            Node::TemplateLiteral(quasis, exprs) => self.visit_template(quasis, exprs),
            Node::Identifier(var) => self.visit_identifier(var),
            Node::Block(scope, stmts) => self.visit_block(scope, stmts),
            Node::IfStatement(test, consequent, alternative) => {
                self.visit_if(test, consequent, alternative)
            }
            Node::ConditionalExpression(test, consequent, alternative) => {
                self.visit_conditional(test, consequent, alternative)
            }
            Node::WhileLoop(test, body) => self.visit_while(test, body),
            Node::ForLoop(r#async, binding, target, body) => {
                self.visit_for(*r#async, binding, target, body)
            }
            Node::ExpressionStatement(expr) => self.visit_expression_statement(expr),
            Node::UnaryExpression(op, expr) => self.visit_unary(*op, expr),
            Node::BinaryExpression(op, lhs, rhs) => self.visit_binary(*op, lhs, rhs),
            Node::ParenthesizedExpression(expr) => self.visit_parenthesized_expression(expr),
            Node::YieldExpression(expr) => self.visit_yield(expr),
            Node::AwaitExpression(expr) => self.visit_await(expr),
            Node::ThisExpression => self.visit_this(),
            Node::NewExpression(target) => self.visit_new(target),
            Node::MemberExpression(target, key) => self.visit_member_expression(target, key),
            Node::ComputedMemberExpression(target, expr) => {
                self.visit_computed_member_expression(target, expr)
            }
            Node::CallExpression(callee, args) => self.visit_call(callee, args, false),
            Node::TailCallExpression(callee, args) => self.visit_call(callee, args, true),
            Node::FunctionExpression(kind, name, args, body) => {
                self.visit_function_expression(*kind, name, args, body)
            }
            Node::FunctionDeclaration(kind, name, args, body) => {
                self.visit_function_declaration(*kind, name, args, body)
            }
            Node::ArrowFunctionExpression(kind, args, body) => {
                self.visit_arrow_function(*kind, args, body)
            }
            Node::ClassExpression(name, extends, body) => {
                self.visit_class_expression(name, extends, body)
            }
            Node::ClassDeclaration(name, extends, body) => {
                self.visit_class_declaration(name, extends, body)
            }
            Node::LexicalInitialization(var, expr) => self.visit_lexical_initialization(var, expr),
            Node::ReturnStatement(expr) => self.visit_return(expr),
            Node::ThrowStatement(expr) => self.visit_throw(expr),
            Node::BreakStatement => self.visit_break(),
            Node::ContinueStatement => self.visit_continue(),
            Node::TryStatement(tryc, binding, catch, finally) => {
                self.visit_try(tryc, binding, catch, finally)
            }
            Node::ImportDeclaration(..)
            | Node::ImportNamedDeclaration(..)
            | Node::ImportDefaultDeclaration(..)
            | Node::ImportStandardDeclaration(..) => {}
            Node::ExportDeclaration(decl) => self.visit_export(decl),
            Node::Initializer(..) => unreachable!(),
        }
    }

    fn visit_null(&mut self) {
        self.load_null();
    }

    fn visit_true(&mut self) {
        self.load_true();
    }

    fn visit_false(&mut self) {
        self.load_false();
    }

    fn visit_number(&mut self, n: f64) {
        self.load_f64(n);
    }

    fn visit_string(&mut self, s: &str) {
        self.load_string(s);
    }

    fn visit_symbol(&mut self, s: &str) {
        self.load_symbol(s);
    }

    fn visit_regex(&mut self, p: &str) {
        let id = self.string_id(p);
        self.push_op(Op::BuildRegex);
        self.push_u32(id);
    }

    fn visit_array(&mut self, exprs: &[Node]) {
        let rscope = RegisterScope::new(self);
        let array = rscope.register();
        self.push_op(Op::CreateEmptyArray);
        self.store_accumulator_in_register(&array);
        for (idx, expr) in exprs.iter().enumerate() {
            self.visit(expr);
            self.push_op(Op::StoreInArrayLiteral);
            self.push_u32(array.id);
            self.push_u32(idx as u32);
        }
        self.load_accumulator_with_register(&array);
    }

    fn visit_tuple(&mut self, exprs: &[Node]) {
        let rscope = RegisterScope::new(self);
        let tuple = rscope.register();
        self.push_op(Op::CreateEmptyTuple);
        self.store_accumulator_in_register(&tuple);
        for expr in exprs {
            self.visit(expr);
            self.push_op(Op::StoreInTuple);
            self.push_u32(tuple.id);
        }
        self.load_accumulator_with_register(&tuple);
    }

    fn visit_object(&mut self, inits: &[Node]) {
        let rscope = RegisterScope::new(self);
        let obj = rscope.register();
        let key = rscope.register();
        self.push_op(Op::CreateEmptyObject);
        self.store_accumulator_in_register(&obj);
        for init in inits {
            if let Node::Initializer(name, value) = init {
                self.visit(name);
                self.store_accumulator_in_register(&key);
                self.visit(value);
                self.push_op(Op::StoreInObjectLiteral);
                self.push_u32(obj.id);
                self.push_u32(key.id);
            } else {
                unreachable!();
            }
        }
        self.load_accumulator_with_register(&obj);
    }

    fn visit_template(&mut self, quasis: &[String], exprs: &[Node]) {
        if exprs.is_empty() {
            debug_assert_eq!(quasis.len(), 1);
            self.load_string(quasis[0].as_str());
            return;
        }

        debug_assert_eq!(quasis.len(), exprs.len() + 1);

        let rscope = RegisterScope::new(self);
        let last = rscope.register();

        let mut last_valid = false;
        for (i, expr) in exprs.iter().enumerate() {
            if i != 0 {
                self.store_accumulator_in_register(&last);
                last_valid = true;
            }

            if let Some(quasi) = quasis.get(i) {
                self.load_string(quasi);
                if last_valid {
                    self.push_op(Op::Add);
                    self.push_u32(last.id);
                }
                self.store_accumulator_in_register(&last);
                last_valid = true;
            }

            self.visit(expr);
            self.push_op(Op::ToString);
            if last_valid {
                self.push_op(Op::Add);
                self.push_u32(last.id);
            }
            last_valid = false;
        }

        if let Some(quasi) = quasis.last() {
            self.store_accumulator_in_register(&last);
            self.load_string(quasi);
            self.push_op(Op::Add);
            self.push_u32(last.id);
        }
    }

    fn visit_identifier(&mut self, name: &str) {
        self.push_op(Op::ResolveIdentifier);
        let id = self.string_id(name);
        self.push_u32(id);
    }

    fn visit_block(&mut self, scope: &Scope, stmts: &[Node]) {
        if !scope.bindings.is_empty() && scope.kind != ScopeKind::TopLevel {
            self.push_op(Op::EnterScope);
        }
        for (name, mutable) in &scope.bindings {
            self.lexical_declaration(name, *mutable);
        }
        for stmt in stmts {
            self.visit(stmt);
        }
        if !scope.bindings.is_empty() && scope.kind != ScopeKind::TopLevel {
            self.push_op(Op::ExitScope);
        }
    }

    fn visit_if(&mut self, test: &Node, consequent: &Node, alternative: &Option<Box<Node>>) {
        let mut alt = self.label();
        self.visit(test);
        self.jump_if_false(&mut alt);
        self.visit(consequent);
        self.mark(&mut alt);
        if let Some(alternative) = alternative {
            self.visit(alternative);
        }
    }

    fn visit_conditional(&mut self, test: &Node, consequent: &Node, alternative: &Node) {
        let mut alt = self.label();
        self.visit(test);
        self.jump_if_false(&mut alt);
        self.visit(consequent);
        self.mark(&mut alt);
        self.visit(alternative);
    }

    fn visit_while(&mut self, test: &Node, body: &Node) {
        let mut head = self.label();
        let mut end = self.label();
        self.mark(&mut head);
        self.visit(test);
        self.jump_if_false(&mut end);
        let pbl = self.break_label;
        self.break_label = Some(&mut end as *mut Label);
        let pcl = self.continue_label;
        self.continue_label = Some(&mut head as *mut Label);
        self.visit(body);
        self.break_label = pbl;
        self.continue_label = pcl;
        self.jump(&mut head);
        self.mark(&mut end);
        self.load_null();
    }

    fn visit_for(&mut self, r#async: bool, binding: &str, target: &Node, body: &Node) {
        /*
        for (await) BINDING in TARGET { BODY }

        @=>

        iterator = GetIterator(TARGET)
        head:
        result = (await) IteratorNext(iterator)
        if result.done {
          jump end
        }
        BINDING = result.value
        BODY
        jump head
        end:
        */

        let mut head = self.label();
        let mut end = self.label();

        let rscope = RegisterScope::new(self);

        let iterator = rscope.register();
        self.visit(target);
        self.push_op(if r#async {
            Op::GetAsyncIterator
        } else {
            Op::GetIterator
        });
        self.store_accumulator_in_register(&iterator);

        self.mark(&mut head);
        self.push_op(if r#async {
            Op::AsyncIteratorNext
        } else {
            Op::IteratorNext
        });
        self.push_u32(iterator.id);
        let result = rscope.register();
        self.store_accumulator_in_register(&result);
        self.load_named_property("done");
        self.jump_if_true(&mut end);

        self.load_accumulator_with_register(&result);
        self.load_named_property("value");

        self.push_op(Op::EnterScope);
        self.lexical_declaration(binding, false);
        self.lexical_initialization(binding);

        if let Node::Block(scope, stmts) = body {
            for (name, mutable) in &scope.bindings {
                self.lexical_declaration(name, *mutable);
            }

            let pbl = self.break_label;
            self.break_label = Some(&mut end as *mut Label);
            let pcl = self.continue_label;
            self.continue_label = Some(&mut head as *mut Label);
            for stmt in stmts {
                self.visit(stmt);
            }
            self.break_label = pbl;
            self.continue_label = pcl;
        } else {
            unreachable!();
        }
        self.push_op(Op::ExitScope);

        self.jump(&mut head);

        self.mark(&mut end);
        self.load_null();
    }

    fn visit_expression_statement(&mut self, expr: &Node) {
        self.visit(expr);
        self.load_null();
    }

    fn visit_unary(&mut self, op: Operator, expr: &Node) {
        self.visit(expr);
        match op {
            Operator::Not => self.push_op(Op::LNOT),
            Operator::BitwiseNOT => self.push_op(Op::BitNOT),
            Operator::Typeof => self.push_op(Op::Typeof),
            Operator::Void => self.push_op(Op::Void),
            Operator::Sub => self.push_op(Op::UnSub),
            _ => unreachable!(),
        }
    }

    fn visit_binary(&mut self, op: Operator, lhs: &Node, rhs: &Node) {
        if op == Operator::LogicalAND {
            let mut end = self.label();
            self.visit(lhs);
            self.jump_if_false(&mut end);
            self.visit(rhs);
            self.mark(&mut end);
            return;
        }

        if op == Operator::LogicalOR {
            let mut end = self.label();
            self.visit(lhs);
            self.jump_if_true(&mut end);
            self.visit(rhs);
            self.mark(&mut end);
            return;
        }

        let rscope = RegisterScope::new(self);

        if op == Operator::Assign {
            match lhs {
                Node::Identifier(s) => {
                    self.visit(rhs);
                    self.push_op(Op::AssignIdentifier);
                    let id = self.string_id(s);
                    self.push_u32(id);
                }
                Node::MemberExpression(base, name) => {
                    let obj = rscope.register();
                    self.visit(base);
                    self.store_accumulator_in_register(&obj);
                    self.visit(rhs);
                    self.store_named_property(&obj, name);
                }
                Node::ComputedMemberExpression(base, key) => {
                    let obj = rscope.register();
                    let keyr = rscope.register();
                    self.visit(base);
                    self.store_accumulator_in_register(&obj);
                    self.visit(key);
                    self.store_accumulator_in_register(&keyr);
                    self.visit(rhs);
                    self.store_computed_property(&obj, &keyr);
                }
                _ => unreachable!(),
            }
            return;
        }

        let lhsr = rscope.register();
        self.visit(lhs);
        self.store_accumulator_in_register(&lhsr);
        self.visit(rhs);
        // accumulator = lhs @ rhs
        match op {
            Operator::Add | Operator::AddAssign => self.push_op(Op::Add),
            Operator::Sub | Operator::SubAssign => self.push_op(Op::Sub),
            Operator::Mul | Operator::MulAssign => self.push_op(Op::Mul),
            Operator::Div | Operator::DivAssign => self.push_op(Op::Div),
            Operator::Mod | Operator::ModAssign => self.push_op(Op::Mod),
            Operator::Pow | Operator::PowAssign => self.push_op(Op::Pow),
            Operator::BitwiseOR => self.push_op(Op::BitOR),
            Operator::BitwiseXOR => self.push_op(Op::BitXOR),
            Operator::BitwiseAND => self.push_op(Op::BitAND),
            Operator::LeftShift => self.push_op(Op::ShiftLeft),
            Operator::RightShift => self.push_op(Op::ShiftRight),
            Operator::GreaterThan => self.push_op(Op::GreaterThan),
            Operator::LessThan => self.push_op(Op::LessThan),
            Operator::GreaterThanOrEqual => self.push_op(Op::GreaterThanOrEqual),
            Operator::LessThanOrEqual => self.push_op(Op::LessThanOrEqual),
            Operator::Equal => self.push_op(Op::Eq),
            Operator::NotEqual => self.push_op(Op::Neq),
            _ => unreachable!(),
        }
        self.push_u32(lhsr.id);

        // lhs = accumulator
        match op {
            Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
            | Operator::ModAssign
            | Operator::PowAssign => match lhs {
                Node::Identifier(s) => {
                    self.push_op(Op::AssignIdentifier);
                    let id = self.string_id(s);
                    self.push_u32(id);
                }
                Node::MemberExpression(base, name) => {
                    let value = rscope.register();
                    let obj = rscope.register();
                    self.store_accumulator_in_register(&value);
                    self.visit(base);
                    self.store_accumulator_in_register(&obj);
                    self.load_accumulator_with_register(&value);
                    self.store_named_property(&obj, name);
                }
                Node::ComputedMemberExpression(base, key) => {
                    let value = rscope.register();
                    let obj = rscope.register();
                    let keyr = rscope.register();
                    self.store_accumulator_in_register(&value);
                    self.visit(base);
                    self.store_accumulator_in_register(&obj);
                    self.visit(key);
                    self.store_accumulator_in_register(&keyr);
                    self.load_accumulator_with_register(&value);
                    self.store_computed_property(&obj, &keyr);
                }
                _ => unreachable!(),
            },
            _ => {}
        }
    }

    fn visit_parenthesized_expression(&mut self, expr: &Node) {
        self.visit(expr);
    }

    fn visit_yield(&mut self, expr: &Option<Box<Node>>) {
        if let Some(expr) = expr {
            self.visit(expr);
        } else {
            self.load_null();
        }
        self.push_op(Op::Suspend);
    }

    fn visit_await(&mut self, expr: &Node) {
        self.visit(expr);
        self.push_op(Op::Suspend);
    }

    fn visit_this(&mut self) {
        self.push_op(Op::GetThis);
    }

    fn visit_member_expression(&mut self, target: &Node, key: &str) {
        self.visit(target);
        self.load_named_property(key);
    }

    fn visit_computed_member_expression(&mut self, base: &Node, key: &Node) {
        let rscope = RegisterScope::new(self);
        let obj = rscope.register();
        self.visit(base);
        self.store_accumulator_in_register(&obj);
        self.visit(key);
        self.load_computed_property(&obj);
    }

    fn visit_call(&mut self, callee_node: &Node, args: &[Node], tail: bool) {
        let rscope = RegisterScope::new(self);

        let receiver = rscope.register();
        let callee = rscope.register();

        match callee_node {
            Node::MemberExpression(base, prop) => {
                self.visit(base);
                self.store_accumulator_in_register(&receiver);
                self.load_named_property(prop);
                self.store_accumulator_in_register(&callee);
            }
            Node::ComputedMemberExpression(base, key) => {
                self.visit(base);
                self.store_accumulator_in_register(&receiver);
                self.visit(key);
                self.load_computed_property(&receiver);
                self.store_accumulator_in_register(&callee);
            }
            _ => {
                self.load_null();
                self.store_accumulator_in_register(&receiver);
                self.visit(callee_node);
                self.store_accumulator_in_register(&callee);
            }
        };

        let rarg = self.register_index;
        for arg in args {
            let reg = rscope.register();
            self.visit(arg);
            self.store_accumulator_in_register(&reg);
        }

        // Call <receiver> <callee> <first argument> <# args>
        self.push_op(if tail { Op::TailCall } else { Op::Call });
        self.push_u32(receiver.id);
        self.push_u32(callee.id);
        self.push_u32(rarg);
        self.push_u8(args.len() as u8);
    }

    fn visit_new(&mut self, target: &Node) {
        match target {
            Node::CallExpression(callee, args) => {
                self.visit(callee);
                let rscope = RegisterScope::new(self);
                let callee = rscope.register();
                self.store_accumulator_in_register(&callee);

                let rarg = self.register_index;
                for arg in args {
                    let reg = rscope.register();
                    self.visit(arg);
                    self.store_accumulator_in_register(&reg);
                }

                self.push_op(Op::ConstructWithArgs);
                self.push_u32(callee.id);
                self.push_u32(rarg);
                self.push_u8(args.len() as u8);
            }
            _ => {
                self.visit(target);
                self.push_op(Op::Construct);
            }
        }
    }

    fn visit_function_expression(
        &mut self,
        kind: FunctionKind,
        name: &Option<String>,
        args: &[Node],
        body: &Node,
    ) {
        self.build_function(
            kind,
            match name {
                Some(n) => Some(n.to_string()),
                None => None,
            },
            args,
            body,
        );
    }

    fn visit_function_declaration(
        &mut self,
        kind: FunctionKind,
        name: &str,
        args: &[Node],
        body: &Node,
    ) {
        self.build_function(kind, Some(name.to_string()), args, body);
        self.lexical_initialization(name);
    }

    fn visit_arrow_function(&mut self, kind: FunctionKind, args: &[Node], body: &Node) {
        self.build_function(kind, None, args, body);
    }

    fn build_function(
        &mut self,
        kind: FunctionKind,
        name: Option<String>,
        params: &[Node],
        body: &Node,
    ) {
        let mut end = self.label();

        self.push_op(Op::NewFunction);
        let info = AssemblerFunctionInfo {
            position: self.code.len() + 9,
            kind,
            name,
            parameters: params
                .iter()
                .map(|n: &Node| match n {
                    Node::Identifier(s) => s.to_string(),
                    Node::Initializer(s, ..) => {
                        if let Node::Identifier(s) = &**s {
                            s.to_string()
                        } else {
                            unreachable!();
                        }
                    }
                    _ => unreachable!(),
                })
                .collect::<Vec<String>>(),
        };
        let id = self.function_info.len();
        self.function_info.push(info);
        self.push_u32(id as u32); // 4
        self.jump(&mut end); // 5

        if let Node::Block(scope, stmts) = body {
            for param in params {
                if let Node::Initializer(name, init) = param {
                    if let Node::Identifier(name) = &**name {
                        let mut label = self.label();
                        self.visit_identifier(name);
                        self.jump_if_not_empty(&mut label);
                        self.visit(init);
                        self.overwrite_binding(name);
                        self.mark(&mut label);
                    } else {
                        unreachable!();
                    }
                }
            }
            for (name, mutable) in &scope.bindings {
                self.lexical_declaration(name, *mutable);
            }
            let mut needs_return = true;
            for stmt in stmts {
                self.visit(stmt);
                if let Node::ReturnStatement(..) = stmt {
                    needs_return = false;
                    break;
                }
            }
            if needs_return {
                self.visit(&Node::ReturnStatement(None));
            }
        } else {
            unreachable!();
        }

        self.mark(&mut end);
    }

    fn visit_class_expression(&mut self, name: &str, extends: &Option<Box<Node>>, fields: &[Node]) {
        self.build_class(name, extends, fields);
    }

    fn visit_class_declaration(
        &mut self,
        name: &str,
        extends: &Option<Box<Node>>,
        fields: &[Node],
    ) {
        self.build_class(name, extends, fields);
        self.lexical_initialization(name);
    }

    fn build_class(&mut self, name: &str, extends_o: &Option<Box<Node>>, fields: &[Node]) {
        let rscope = RegisterScope::new(self);
        let extends = rscope.register();
        let key = rscope.register();
        let class = rscope.register();
        let prototype = rscope.register();

        match extends_o {
            Some(extends) => {
                self.visit(extends);
            }
            None => {
                self.load_empty();
            }
        }
        self.store_accumulator_in_register(&extends);

        let constructor = fields.iter().find(|f| {
            if let Node::Initializer(name, ..) = f {
                **name == Node::StringLiteral("constructor".to_string())
            } else {
                unreachable!();
            }
        });

        if let Some(constructor) = constructor {
            if let Node::Initializer(_, value) = constructor {
                self.visit(value);
            } else {
                unreachable!();
            }
        } else {
            self.load_empty();
        }
        self.store_accumulator_in_register(&class);

        self.push_op(Op::CreateEmptyObject);
        self.store_accumulator_in_register(&prototype);
        self.store_named_property(&class, "prototype");

        for field in fields {
            if let Node::Initializer(name, value) = field {
                if **name != Node::StringLiteral("constructor".to_string()) {
                    self.visit(name);
                    self.store_accumulator_in_register(&key);
                    self.visit(value);
                    self.push_op(Op::StoreInObjectLiteral);
                    self.push_u32(prototype.id);
                    self.push_u32(key.id);
                }
            } else {
                unreachable!();
            }
        }

        self.push_op(Op::FinishClass);
        self.push_u32(class.id);
        self.push_u32(extends.id);
        let id = self.string_id(name);
        self.push_u32(id);

        self.load_accumulator_with_register(&class);
    }

    fn visit_lexical_initialization(&mut self, name: &str, init: &Node) {
        self.visit(init);
        self.lexical_initialization(name);
    }

    fn visit_return(&mut self, expr: &Option<Box<Node>>) {
        if let Some(expr) = expr {
            self.visit(expr);
        } else {
            self.load_null();
        }
        self.push_op(Op::Return);
    }

    fn visit_throw(&mut self, expr: &Node) {
        self.visit(expr);
        self.push_op(Op::SetException);
        if let Some(throw_label) = self.throw_label {
            self.push_op(Op::PopTry);
            unsafe {
                self.jump(&mut *throw_label);
            }
        } else {
            self.push_op(Op::ThrowDynamic);
        }
    }

    fn visit_break(&mut self) {
        unsafe {
            self.jump(&mut *self.break_label.unwrap());
        }
    }

    fn visit_continue(&mut self) {
        unsafe {
            self.jump(&mut *self.continue_label.unwrap());
        }
    }

    fn visit_try(
        &mut self,
        tryc: &Node,
        binding: &Option<String>,
        catchc: &Option<Box<Node>>,
        finallyc: &Option<Box<Node>>,
    ) {
        let mut catch = self.label();
        let mut finally = self.label();

        self.push_op(Op::PushTry);
        self.jmp(&mut catch); // interpreter eats this for the try_stack

        let ptl = self.throw_label;
        self.throw_label = Some(&mut catch as *mut Label);
        self.visit(tryc);
        self.throw_label = ptl;

        self.push_op(Op::PopTry);

        self.mark(&mut catch);
        if let Some(catchc) = catchc {
            self.push_op(Op::EnterScope);
            if let Some(binding) = binding {
                self.lexical_declaration(binding, false);
                self.push_op(Op::GetException);
                self.lexical_initialization(binding);
            } else {
                self.push_op(Op::ClearException);
            }
            if let Node::Block(scope, stmts) = &**catchc {
                for (name, mutable) in &scope.bindings {
                    self.lexical_declaration(name, *mutable);
                }
                for stmt in stmts {
                    self.visit(stmt);
                }
            } else {
                unreachable!();
            }
            self.push_op(Op::ExitScope);
        }

        self.mark(&mut finally);
        if let Some(finallyc) = finallyc {
            self.visit(finallyc);
        }

        self.load_null();
    }

    fn visit_export(&mut self, decl: &Node) {
        self.visit(decl);
    }

    fn string_id(&mut self, string: &str) -> u32 {
        let index = self.string_table.iter().position(|s| s == string);
        match index {
            Some(i) => i as u32,
            None => {
                let id = self.string_table.len();
                self.string_table.push(string.to_string());
                id as u32
            }
        }
    }

    fn label(&self) -> Label {
        Label {
            index: None,
            targets: Vec::new(),
        }
    }

    fn mark(&mut self, label: &mut Label) {
        let pos = self.code.len();
        label.index = Some(pos);
        for index in &label.targets {
            self.code[*index] = (pos & 0xff) as u8;
            self.code[*index + 1] = ((pos >> 8) & 0xff) as u8;
            self.code[*index + 2] = ((pos >> 16) & 0xff) as u8;
            self.code[*index + 3] = ((pos >> 24) & 0xff) as u8;
        }
    }

    fn jmp(&mut self, label: &mut Label) {
        if let Some(index) = label.index {
            self.push_u32(index as u32);
        } else {
            let target = self.code.len();
            self.push_u32(0);
            label.targets.push(target);
        }
    }

    fn jump(&mut self, label: &mut Label) {
        self.push_op(Op::Jump);
        self.jmp(label);
    }

    fn jump_if_true(&mut self, label: &mut Label) {
        self.push_op(Op::JumpIfTrue);
        self.jmp(label);
    }

    fn jump_if_false(&mut self, label: &mut Label) {
        self.push_op(Op::JumpIfFalse);
        self.jmp(label);
    }

    fn jump_if_not_empty(&mut self, label: &mut Label) {
        self.push_op(Op::JumpIfNotEmpty);
        self.jmp(label);
    }

    fn push_op(&mut self, op: Op) {
        self.push_u8(op as u8);
    }

    fn push_u8(&mut self, n: u8) {
        self.code.push(n);
    }

    fn push_u32(&mut self, n: u32) {
        self.code.write_u32::<LittleEndian>(n).unwrap();
    }

    fn store_accumulator_in_register(&mut self, r: &Register) {
        self.push_op(Op::StoreAccumulatorInRegister);
        self.push_u32(r.id);
    }

    fn load_accumulator_with_register(&mut self, r: &Register) {
        self.push_op(Op::LoadAccumulatorFromRegister);
        self.push_u32(r.id);
    }

    fn load_empty(&mut self) {
        self.push_op(Op::LoadEmpty);
    }

    fn load_null(&mut self) {
        self.push_op(Op::LoadNull);
    }

    fn load_true(&mut self) {
        self.push_op(Op::LoadTrue);
    }

    fn load_false(&mut self) {
        self.push_op(Op::LoadFalse);
    }

    fn load_f64(&mut self, n: f64) {
        self.push_op(Op::LoadF64);
        self.code.write_f64::<LittleEndian>(n).unwrap();
    }

    fn load_string(&mut self, s: &str) {
        let id = self.string_id(s);
        self.push_op(Op::LoadString);
        self.push_u32(id);
    }

    fn load_symbol(&mut self, s: &str) {
        let id = self.string_id(s);
        self.push_op(Op::LoadSymbol);
        self.push_u32(id);
    }

    fn load_named_property(&mut self, s: &str) {
        self.push_op(Op::LoadNamedProperty);
        let id = self.string_id(s);
        self.push_u32(id);
    }

    fn store_named_property(&mut self, obj: &Register, name: &str) {
        self.push_op(Op::StoreNamedProperty);
        self.push_u32(obj.id);
        let id = self.string_id(name);
        self.push_u32(id);
    }

    fn load_computed_property(&mut self, obj: &Register) {
        self.push_op(Op::LoadComputedProperty);
        self.push_u32(obj.id);
    }

    fn store_computed_property(&mut self, obj: &Register, key: &Register) {
        self.push_op(Op::StoreComputedProperty);
        self.push_u32(obj.id);
        self.push_u32(key.id);
    }

    fn lexical_declaration(&mut self, name: &str, mutable: bool) {
        self.push_op(Op::LexicalDeclaration);
        let id = self.string_id(name);
        self.push_u32(id);
        self.push_u8(mutable as u8);
    }

    fn lexical_initialization(&mut self, name: &str) {
        self.push_op(Op::LexicalInitialization);
        let id = self.string_id(name);
        self.push_u32(id);
    }

    fn overwrite_binding(&mut self, name: &str) {
        self.push_op(Op::OverwriteBinding);
        let id = self.string_id(name);
        self.push_u32(id);
    }
}

impl Default for Assembler {
    fn default() -> Self {
        Assembler::new()
    }
}
