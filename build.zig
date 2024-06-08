const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "tessel",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const install = b.addInstallArtifact(exe, .{
        // .dest_dir = .{
        //     .override = .{ .custom = "../" },
        // },
        // .pdb_dir = .{
        //     .override = .{ .custom = "./bin" },
        // },
    });
    b.default_step.dependOn(&install.step);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/unit_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    run_exe_unit_tests.has_side_effects = true;

    const test_step = b.step("test", "Run unit tests");

    test_step.dependOn(&run_exe_unit_tests.step);

    // benchmark

    const bench = b.addExecutable(.{
        .name = "tessel_benchmark",
        .root_source_file = b.path("src/benchmark.zig"),
        .target = target,
        .optimize = optimize,
    });

    const loc = b.addInstallArtifact(bench, .{
        // .dest_dir = .{
        //     .override = .{ .custom = "./" },
        // },
        // .pdb_dir = .{
        //     .override = .{ .custom = "./bin" },
        // },
    });
    b.default_step.dependOn(&loc.step);

    const benchmark_command = b.addRunArtifact(bench);

    benchmark_command.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        benchmark_command.addArgs(args);
    }
    const bench_step = b.step("benchmark", "Run fibonacci(35) benchmark the app");
    bench_step.dependOn(&benchmark_command.step);
}
