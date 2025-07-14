const std = @import("std");
const c = @import("compiler.zig");
const a = @import("anu.zig");

const Compiler = c.Compiler;
const Anu = a.Anu;

fn read_file(filepath:[]const u8) []u8 {
    var file = std.fs.cwd().openFile(filepath,.{}) catch {std.debug.print("ERROR: cannot open file {s}\n",.{filepath});std.process.exit(0);};
    defer file.close();
    const size = file.getEndPos() catch unreachable;
    var buffer = std.heap.page_allocator.alloc(u8,size + 1) catch unreachable;
    buffer[size] = 0;
    _ = file.read(buffer) catch unreachable;
    return buffer;
}

pub fn main() void {
    var compiler = Compiler.init(read_file("test.anu"));
    compiler.compile();

    var anu = Anu.init(&compiler);
    anu.execute();
}