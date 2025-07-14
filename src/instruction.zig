const std = @import("std");

pub const InstructionType = enum {
    push,pop,store,load,jump,ret,gload,gstore,
    add,sub,mul,div,print,
    begin_function,end_function,
    jump_if_equal,jump_if_nequal,
    not,nequals,equals,lt,gt,concat,
};

pub const ValueType = enum {
    nil,number,string,boolean,
};


pub const Value = struct {
    kind: ValueType = ValueType.nil,
    number_val: usize = 0,
    string_val: []const u8 = "",

    pub fn number(n:usize) Value {
        return .{.kind=.number,.number_val=n};
    }

    pub fn boolean(b:usize) Value {
        return .{.kind=.boolean,.number_val=b}; 
    }

    pub fn nil() Value {
        return .{};
    }

    pub fn string(str:[]const u8) Value {
        return .{.kind=.string,.string_val=str};
    }

    pub fn print(self:*Value) void {
        switch (self.kind) {
            .nil => std.debug.print("{s}\n",.{"nil"}),
            .number => std.debug.print("{d}\n",.{self.number_val}),
            .string => std.debug.print("{s}\n",.{self.string_val}),
            .boolean => {
                std.debug.print("{s}\n",.{if(self.number_val == 1) "true" else "no"}); 
            }
        }
    }
};


pub fn value_type_to_str(v:ValueType) []const u8 {
    if(v == .number) return "number";
    if(v == .string) return "string";
    if(v == .boolean) return "boolean";
    return "nil";
}

pub const Instruction = struct {
    kind: InstructionType,
    value: Value,
    line: usize,

    pub fn init(kind:InstructionType,value:Value,line:usize) Instruction {
        return .{.kind=kind,.value=value,.line=line};
    }
};