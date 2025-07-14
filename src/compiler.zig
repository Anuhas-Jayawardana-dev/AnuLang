const std = @import("std");
const lex = @import("lexer.zig");
const Lexer = lex.Lexer;
const Token = lex.Token;
const TokenType = lex.TokenType;

const i = @import("instruction.zig");
const Instruction = i.Instruction;
const InstructionType = i.InstructionType;
const Value = i.Value;
const ValueType = i.ValueType;

pub const Local = struct {
    depth: usize = 1,
    id: usize = 0,

    pub fn init(id:usize,depth:usize) Local {
        return .{.depth=depth,.id=id};
    }
};

pub const Function = struct {
    id: usize = 0,
    params: usize = 0,
};

pub const Compiler = struct {
    lexer: Lexer = undefined,
    current: Token = undefined,
    i: usize = 0,
    depth: usize = 0,
    instructions: std.ArrayList(Instruction) = undefined,
    globals: std.StringHashMap(usize) = undefined,
    locals: std.StringHashMap(Local) = undefined,
    functions: std.StringHashMap(Function) = undefined,
    last_if_indices: std.ArrayList(usize) = undefined,

    pub fn init(code:[]const u8) Compiler {
        var compiler:Compiler = .{};
        compiler.lexer = Lexer.init(code);
        compiler.lexer.tokenize();
        
        compiler.current = compiler.lexer.tokens.items[0];
        compiler.instructions = std.ArrayList(Instruction).init(std.heap.page_allocator);
        compiler.last_if_indices = std.ArrayList(usize).init(std.heap.page_allocator);
        
        compiler.globals = std.StringHashMap(usize).init(std.heap.page_allocator);
        compiler.locals = std.StringHashMap(Local).init(std.heap.page_allocator);
        compiler.functions = std.StringHashMap(Function).init(std.heap.page_allocator);

        return compiler;
    }

    pub fn deinit(self:*Compiler) void {
        self.instructions.deinit();
        self.globals.deinit();
        self.locals.deinit();
        self.functions.deinit();
    }

    fn peek(self:*Compiler) Token {
        return (if (self.current.kind == .eof) self.current else self.lexer.tokens.items[self.i + 1]);
    }

    fn expect(self:*Compiler,kind:TokenType,msg:[]const u8) Token {
        if (self.current.kind == kind) {
            const c = self.current;
            self.i += 1;
            self.current = self.lexer.tokens.items[self.i];
            return c;
        }
        std.debug.print("ERROR: in line {d}, expected {s}, but got {s} instead\n",.{self.current.line,msg,self.current.value});
        std.process.exit(0);
        return null;
    }

    fn function(self:*Compiler) void {
        self.begin_scope();
        _ = self.expect(.func,"func");
        const name = self.expect(.id,"identifier");

        self.instructions.append(Instruction.init(.begin_function,Value.number(0),self.current.line)) catch unreachable;

        if (self.functions.contains(name.value)) {
            std.debug.print("ERROR: in line {d}, redefinition of function {s}\n",.{name.line,name.value});
            std.process.exit(0);
        }

        var func:Function = .{};
        func.id = self.instructions.items.len;

        _ = self.expect(.lparen,"(");

        while (self.current.kind != .rparen) {
            const param = self.expect(.id,"identifier");
            
            if (self.globals.contains(name.value)) {
                std.debug.print("ERROR: in line {d}, redefinition of variable {s}\n",.{param.line,param.value});
                std.process.exit(0);
            }

            const id = self.locals.count();
            self.locals.put(param.value,Local.init(id,self.depth)) catch unreachable;
            self.instructions.append(Instruction.init(InstructionType.store,Value.number(id),param.line)) catch unreachable;
            
            func.params += 1;
            if (self.current.kind != .rparen) _ = self.expect(.comma,",");
        }

        _ = self.expect(.rparen,")");
        self.functions.put(name.value,func) catch unreachable;

        while (self.current.kind != .end) {
            self.statement();
        }

        _ = self.expect(.end,"end");

        if (self.instructions.items[self.instructions.items.len - 1].kind != .ret) {
            self.instructions.append(Instruction.init(.ret,Value.nil(),self.current.line)) catch unreachable;
        }

        self.instructions.append(Instruction.init(.end_function,Value.nil(),self.current.line)) catch unreachable;

        self.end_scope();
    }

    fn funcall(self:*Compiler) void {
        const name = self.expect(.id,"identifier");

        if (!self.functions.contains(name.value)) {
            std.debug.print("ERROR: in line {d}, undefiend reference to function '{s}'\n",.{name.line,name.value});
            std.process.exit(0);
        }

        const func = self.functions.get(name.value).?;

        var params:usize = 0;
        _ = self.expect(.lparen,"(");
        while (self.current.kind != .rparen) {
            self.expression(false);
            if (self.current.kind != .rparen) _ = self.expect(.comma,",");
            params += 1;
        }
        
        if (params != func.params) {
            std.debug.print("ERROR: in line {d}, function '{s}' takes {d} parameters, but {d} were given\n",.{name.line,name.value,func.params,params});
            std.process.exit(0);
        }

        _ = self.expect(.rparen,")");

        self.instructions.append(Instruction.init(.jump,Value.number(func.id),self.current.line)) catch unreachable;
    }

    fn expression(self:*Compiler,optional:bool) void {
        var expect_value = true;
        var recent_instruction:?Instruction = null;
        while (true) {
            if (expect_value) {
                if (self.current.kind == .num) {
                    const token = self.expect(self.current.kind,"number");
                    const number = std.fmt.parseInt(usize,token.value,10) catch unreachable;
                    self.instructions.append(Instruction.init(.push,Value.number(number),token.line)) catch unreachable;
                }
                else if (self.current.kind == .string) {
                    const token = self.expect(self.current.kind,"string");
                    self.instructions.append(Instruction.init(.push,Value.string(token.value),token.line)) catch unreachable;
                }
                else if (self.current.kind == .keyword_true or self.current.kind == .keyword_false) {
                    const token = self.expect(self.current.kind,"boolean");
                    const boolean:usize = if(token.kind == .keyword_true) 1 else 0;
                    self.instructions.append(Instruction.init(.push,Value.boolean(boolean),token.line)) catch unreachable;
                }
                else if (self.current.kind == .id) {
                    if (self.peek().kind == .lparen) {
                        self.funcall();
                    } else {
                        const token = self.expect(self.current.kind,"identifier");
                        if (!self.globals.contains(token.value) and !self.locals.contains(token.value)) {
                            std.debug.print("ERROR: in line {d}, undefiend reference to variable '{s}'\n",.{token.line,token.value});
                            std.process.exit(0);
                        }
                        const id = if (self.globals.contains(token.value)) self.globals.get(token.value).? else self.locals.get(token.value).?.id;
                        const kind = if (self.globals.contains(token.value)) InstructionType.gload else InstructionType.load;
                        self.instructions.append(Instruction.init(kind,Value.number(id),token.line)) catch unreachable;
                    }
                } else {
                    if (optional) return else _ = self.expect(.none,"value");
                }

                if (recent_instruction != null) {
                    self.instructions.append(recent_instruction.?) catch unreachable;
                    recent_instruction = null;
                }

            } else {
                if (!self.current.is_operator()) break;
                const op = self.expect(self.current.kind,"operator");
                const itype:InstructionType = switch(op.kind) {
                    .add => InstructionType.add,
                    .sub => InstructionType.sub,
                    .mul => InstructionType.mul,
                    .div => InstructionType.div,
                    .equals => InstructionType.equals,
                    .nequals => InstructionType.nequals,
                    .bang => InstructionType.not,
                    .lt => InstructionType.lt,
                    .gt => InstructionType.gt,
                    .concat => InstructionType.concat,
                    else => InstructionType.add
                };
                recent_instruction = Instruction.init(itype,Value.nil(),op.line);
            }
            expect_value = !expect_value;
        }
    }

    fn cond_body(self:*Compiler,root:bool) void {
        if(!root) {
            self.end_scope();
        }
        self.begin_scope();
        
        if (!root) {
            self.instructions.items[self.last_if_indices.pop().?].value = self.instructions.items.len;    
        }

        self.instructions.append(Instruction.init(.jump_if_nequal, Value.number(0), self.current.line)) catch unreachable;
        self.last_if_indices.append(self.instructions.items.len - 1) catch unreachable;

        while (self.current.kind != .end and self.current.kind != .keyword_elseif and self.current.kind != .keyword_else) {
            self.statement();
        }

        if (self.current.kind == .keyword_else) {
            _ = self.expect(.keyword_else, "else");
                            
            self.instructions.items[self.last_if_indices.pop().?].value = self.instructions.items.len;    
            self.instructions.append(Instruction.init(.jump_if_nequal, Value.number(0), self.current.line)) catch unreachable;
            self.last_if_indices.append(self.instructions.items.len - 1) catch unreachable;

            while (self.current.kind != .end) {
                self.statement();
            }
            _ = self.expect(.end, "end");
        } else if (self.current.kind == .end) {
            self.end_scope();
            _ = self.expect(.end, "end");
        }

        self.end_scope();
    }

    fn create_if_jump(self:*Compiler) void {
        self.instructions.append(Instruction.init(.jump_if_nequal, Value.number(0), self.current.line)) catch unreachable;
        self.last_if_indices.append(self.instructions.items.len - 1) catch unreachable;
    }

    fn patch_jump(self:*Compiler) void {
        self.instructions.items[self.last_if_indices.pop().?].value = Value.number(self.instructions.items.len);
    }

    fn elseif(self:*Compiler) void {
        _ = self.current;
    }

    fn if_statement(self:*Compiler) void {
        _ = self.expect(.keyword_if,"if");
        self.expression(false);
        _ = self.expect(.then, "then");
        self.begin_scope();


        self.create_if_jump();
        while (self.current.kind != .end and self.current.kind != .keyword_else) { self.statement(); }
        
        if (self.current.kind == .keyword_else) {
            _ = self.expect(.keyword_else, "else");
            self.patch_jump();
            self.end_scope();
            self.begin_scope();

            while (self.current.kind != .end) {
                self.statement();
            }

            _ = self.expect(.end, "end");
        } else if (self.current.kind == .keyword_elseif) {
            self.elseif();
        } else {
            self.patch_jump();
            self.end_scope();
            _ = self.expect(.end, "end");
        }
    }

    fn begin_scope(self:*Compiler) void {
        self.depth += 1;
    }
    
    fn end_scope(self:*Compiler) void {
        var it = self.locals.iterator();
        while (it.next()) |entry| {
            const local = self.locals.get(entry.key_ptr.*).?;
            if (local.depth == self.depth) _ = self.locals.remove(entry.key_ptr.*);
        }
        self.depth -= 1;
    }

    fn ret(self:*Compiler) void {
        if (self.depth == 0) {
            std.debug.print("ERROR: in line {d}, attempt to call 'return' in global scope\n",.{self.current.line});
            std.process.exit(0);
        }

        _ = self.expect(.ret,"return");
        self.expression(true);
        self.instructions.append(Instruction.init(.ret,Value.nil(),self.current.line)) catch unreachable;
    }

    fn variable(self:*Compiler) void {
        const name = self.expect(.id,"identifier");
        _ = self.expect(.assign,"=");
        self.expression(false);

        var is_global = false;
        var id:usize = 0;

        if (self.globals.contains(name.value)) {
            is_global = true;
            id = self.globals.get(name.value).?;
        } else if (self.locals.contains(name.value)) {
            is_global = false;
            id = self.locals.get(name.value).?.id;
        } else {
            if (self.depth == 0) is_global = true;
            id = if (self.depth == 0) self.globals.count() else self.locals.count();
            if (self.depth == 0) self.globals.put(name.value,id) catch unreachable else self.locals.put(name.value,Local.init(id,self.depth)) catch unreachable;
        }
 
        const ikind = if (is_global) InstructionType.gstore else InstructionType.store;

        self.instructions.append(Instruction.init(ikind,Value.number(id),name.line)) catch unreachable;
    }

    fn print(self:*Compiler) void {
        _ = self.expect(.print, "print");
        self.expression(false);
        self.instructions.append(Instruction.init(.print, Value.number(0), self.current.line)) catch unreachable;
    }

    fn while_loop(self:*Compiler) void {
        _ = self.expect(.keyword_while, "while");
        const expr_index = self.instructions.items.len;
        self.expression(false);
        _ = self.expect(.keyword_do, "do");
        self.instructions.append(Instruction.init(.jump_if_nequal, Value.number(0), self.current.line)) catch unreachable;
        const jump_index = self.instructions.items.len - 1;

        self.begin_scope();
        while (self.current.kind != .end) {
            self.statement();
        }
        self.end_scope();
        _ = self.expect(.end, "end");
        self.instructions.append(Instruction.init(.jump, Value.number(expr_index), self.current.line)) catch unreachable;
        self.instructions.items[jump_index].value = Value.number(self.instructions.items.len);
    }

    fn statement(self:*Compiler) void {
        switch (self.current.kind) {
            .id => {
                if (self.peek().kind == .assign) {self.variable();return;}
                if (self.peek().kind == .lparen) {self.funcall();return;}
                _ = self.expect(.none,"function call or a variable declaration");
            },
            .print => self.print(),
            .keyword_if => self.if_statement(),
            .ret => self.ret(),
            .keyword_while => self.while_loop(),
            
            else => {
                _ = self.expect(.none,"statement");
            }
        }
        return undefined;
    }

    fn parse(self:*Compiler) void {
        while (self.current.kind != .eof) {
            switch (self.current.kind) {
                .func => self.function(),
                else => {
                    self.statement();
                }
            }
        }
    }

    pub fn get_main_function_id(self:*Compiler) usize {
        if (!self.functions.contains("main")) {
            std.debug.print("ERROR: the main entry point could not be found\n",.{});
            std.process.exit(0);
        }
        return self.functions.get("main").?.id;
    }

    pub fn compile(self:*Compiler) void {
        self.parse();
    }
};