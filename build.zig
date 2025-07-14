const std = @import("std");

pub fn build(b:*std.Build) void {
    const exe = b.addExecutable(.{
        .name = "anulang",
        .target = b.graph.host,
        .root_source_file = b.path("src/main.zig")
    });

    b.installArtifact(exe);
    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run","run this");
    run_step.dependOn(&run_exe.step);
}