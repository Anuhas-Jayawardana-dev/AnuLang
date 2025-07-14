const std = @import("std");
const c = @import("compiler.zig");
const Compiler = c.Compiler;
const Instruction = c.Instruction;

pub const Anu = struct {
    stack: std.ArrayList(usize),
    instructions: []Instruction,
    i:usize = 0,
    halt:bool = false,

    jumps: std.ArrayList(usize),
    globals: std.AutoHashMap(usize,usize),
    locals: std.AutoHashMap(usize,usize),

    pub fn init(compiler:*Compiler) Anu {
        return .{
            .stack = std.ArrayList(usize).init(std.heap.page_allocator),
            .jumps = std.ArrayList(usize).init(std.heap.page_allocator),
            .i = compiler.get_main_function_id(),

            .instructions = compiler.instructions.items,
            .globals = std.AutoHashMap(usize,usize).init(std.heap.page_allocator),
            .locals = std.AutoHashMap(usize,usize).init(std.heap.page_allocator)
        };
    }

    fn push(self:*Anu,value:usize) void {
        self.stack.append(value) catch unreachable;
    }

    fn pop(self:*Anu) usize {
        if (self.stack.items.len == 0) {
            std.debug.print("ERROR: in line {d}, stack underflowed\n",.{self.instructions[self.i].line});
            std.process.exit(0);
        }
        return self.stack.pop().?;
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
        if (self.pop() == 0) self.i = index - 1;
    }

    fn instruct(self:*Anu,i:Instruction) void {
        switch (i.kind) {
            .push => self.push(i.value),
            .pop => _ = self.pop(),
            .store => {
                self.locals.put(i.value,self.pop()) catch unreachable;
            },
            .load => {
                self.push(self.locals.get(i.value).?);
            },
            .gstore => {
                self.globals.put(i.value,self.pop()) catch unreachable;
            },
            .gload => {
                self.push(self.globals.get(i.value).?);
            },
            .jump => self.jump(i.value),
            .ret => {
                if (self.jumps.items.len == 0) self.halt = true else self.i = self.jumps.pop().?;
            },

            .add => {
                const b = self.pop();
                const a = self.pop();
                self.push(a + b);
            },

            .sub => {
                const b = self.pop();
                const a = self.pop();
                self.push(a - b);
            },

            .mul => {
                const b = self.pop();
                const a = self.pop();
                self.push(a * b);
            },

            .div => {
                const b = self.pop();
                const a = self.pop();
                self.push(a / b);
            },

            .equals => {self.push(if (self.pop() == self.pop()) 1 else 0);},
            .nequals => {self.push(if (self.pop() != self.pop()) 1 else 0);},
            .not => {},

            .print => std.debug.print("{d}\n",.{self.pop()}),

            .jump_if_equal => {if (self.pop() == 1) {self.jump(i.value);}},
            .jump_if_nequal => self.jump_if_false(i.value),
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