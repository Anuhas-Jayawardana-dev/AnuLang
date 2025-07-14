const std = @import("std");
const c = @import("compiler.zig");
const Compiler = c.Compiler;

const ins = @import("instruction.zig");
const Value = ins.Value;
const ValueType = ins.ValueType;
const Instruction = ins.Instruction;
const InstructionType = ins.InstructionType;

pub const Anu = struct {
    stack: std.ArrayList(Value),
    instructions: []Instruction,
    i:usize = 0,
    halt:bool = false,

    jumps: std.ArrayList(usize),
    globals: std.AutoHashMap(usize,Value),
    locals: std.AutoHashMap(usize,Value),

    pub fn init(compiler:*Compiler) Anu {
        return .{
            .stack = std.ArrayList(Value).init(std.heap.page_allocator),
            .jumps = std.ArrayList(usize).init(std.heap.page_allocator),
            .i = compiler.get_main_function_id(),

            .instructions = compiler.instructions.items,
            .globals = std.AutoHashMap(usize,Value).init(std.heap.page_allocator),
            .locals = std.AutoHashMap(usize,Value).init(std.heap.page_allocator)
        };
    }

    fn push(self:*Anu,value:Value) void {
        self.stack.append(value) catch unreachable;
    }

    fn pop(self:*Anu) Value {
        if (self.stack.items.len == 0) {
            std.debug.print("ERROR: in line {d}, stack underflowed\n",.{self.instructions[self.i].line});
            std.process.exit(0);
        }
        return self.stack.pop().?;
    }

    fn pop_expect_type(self:*Anu,kind:ValueType) Value {
        const v = self.pop();
        if (v.kind != kind) {
            std.debug.print("ERROR: in line {d}, expected type {s} but got {s} instead\n",.{self.instructions[self.i].line,ins.value_type_to_str(kind),ins.value_type_to_str(v.kind)});
            std.process.exit(0);
        }
        return v;
    }

    fn jump(self:*Anu,index:usize) void {
        if (index < 0 or index > self.instructions.len - 1) {
            std.debug.print("ERROR: in line {d}, illegal jump address\n",.{self.instructions[self.i].line});
            std.process.exit(0);
        }
        self.jumps.append(self.i) catch unreachable;
        self.i = index - 1;
    }

    fn jump_if_false(self:*Anu,index:usize) void {
        if (self.pop_expect_type(.boolean).number_val == 0) self.i = index - 1;
    }

    fn equals(self:*Anu) void {
        const a = self.pop();
        const b = self.pop();
        if (a.kind != b.kind) {
            std.debug.print("ERROR: in line {d}, arithmetic between {s} and {s}\n",.{self.instructions[self.i].line,ins.value_type_to_str(a.kind),ins.value_type_to_str(b.kind)});
            std.process.exit(0);
        }
        if (a.kind == .nil) self.push(Value.boolean(1));
        if (a.kind == .number) self.push(Value.boolean(if(a.number_val == b.number_val) 1 else 0));
        if (a.kind == .boolean) self.push(Value.boolean(if(a.number_val == b.number_val) 1 else 0));
        if (a.kind == .string) self.push(Value.boolean(if(std.mem.eql(u8,a.string_val,b.string_val)) 1 else 0));
    }

    fn nequals(self:*Anu) void {
        const a = self.pop();
        const b = self.pop();
        if (a.kind != b.kind) {
            self.push(Value.boolean(1));
        }
        if (a.kind == .nil) self.push(Value.boolean(0));
        if (a.kind == .number) self.push(Value.boolean(if(a.number_val == b.number_val) 0 else 1));
        if (a.kind == .boolean) self.push(Value.boolean(if(a.number_val == b.number_val) 0 else 1));
        if (a.kind == .string) self.push(Value.boolean(if(std.mem.eql(u8,a.string_val,b.string_val)) 0 else 1));
    }

    fn instruct(self:*Anu,i:Instruction) void {
        switch (i.kind) {
            .push => self.push(i.value),
            .pop => _ = self.pop(),
            .store => {
                self.locals.put(i.value.number_val,self.pop()) catch unreachable;
            },
            .load => {
                self.push(self.locals.get(i.value.number_val).?);
            },
            .gstore => {
                self.globals.put(i.value.number_val,self.pop()) catch unreachable;
            },
            .gload => {
                self.push(self.globals.get(i.value.number_val).?);
            },
            .jump => self.jump(i.value.number_val),
            .ret => {
                if (self.jumps.items.len == 0) self.halt = true else self.i = self.jumps.pop().?;
            },

            .concat => {
                const b = self.pop_expect_type(.string);
                const a = self.pop_expect_type(.string);
                const x:[]const []const u8 = &.{a.string_val,b.string_val};
                const string = std.mem.concat(std.heap.page_allocator, u8,x) catch unreachable;
                self.push(Value.string(string));
            },

            .add => {
                const b = self.pop_expect_type(ValueType.number).number_val;
                const a = self.pop_expect_type(ValueType.number).number_val;
                self.push(Value.number(a + b));
            },

            .sub => {
                const b = self.pop_expect_type(ValueType.number).number_val;
                const a = self.pop_expect_type(ValueType.number).number_val;
                self.push(Value.number(a - b));
            },

            .mul => {
                const b = self.pop_expect_type(ValueType.number).number_val;
                const a = self.pop_expect_type(ValueType.number).number_val;
                self.push(Value.number(a * b));
            },

            .div => {
                const b = self.pop_expect_type(ValueType.number).number_val;
                const a = self.pop_expect_type(ValueType.number).number_val;
                self.push(Value.number(a / b));
            },

            .equals => self.equals(),
            .nequals => self.nequals(),
            .lt => {
                const b = self.pop_expect_type(.number).number_val;
                const a = self.pop_expect_type(.number).number_val;
                self.push(Value.boolean(if (a < b) 1 else 0));
            },
            .gt => {
                const b = self.pop_expect_type(.number).number_val;
                const a = self.pop_expect_type(.number).number_val;
                self.push(Value.boolean(if (a > b) 1 else 0));
            },
            .not => {},

            .print => {
                const val = self.pop();
                switch (val.kind) {
                    .nil => std.debug.print("{s}\n",.{"nil"}),
                    .number => std.debug.print("{d}\n",.{val.number_val}),
                    .string => std.debug.print("{s}\n",.{val.string_val}),
                    .boolean => {
                        std.debug.print("{s}\n",.{if(val.number_val == 1) "true" else "false"}); 
                    }
                }
            },

            .jump_if_equal => {if (self.pop_expect_type(.boolean).number_val == 1) {self.jump(i.value.number_val);}},
            .jump_if_nequal => self.jump_if_false(i.value.number_val),
            .begin_function => {},
            .end_function => {},
        }
    }


    pub fn execute(self:*Anu) void {
        // var index:usize = 0;
        // for (self.instructions) |i| {
        //     std.debug.print("{d} -> {any} {d}\n", .{index,i.kind,i.value});
        //     index += 1;
        // }

        while (true) {
            self.instruct(self.instructions[self.i]);
            if (self.halt) break;
            self.i += 1; 
        }
    }
};